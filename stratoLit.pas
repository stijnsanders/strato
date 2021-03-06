unit stratoLit;

interface

uses stratoDecl, stratoSphere, stratoParseDecl, stratoTokenizer;

type
  TParserLookupDecl=function:xNode of object;

function ParseLiteral(Parser:TStratoParserDecl;LookUpDecl:TParserLookupDecl;
  st0:TStratoToken;NeedValue:PInteger):xNode;

implementation

uses SysUtils, stratoTools, stratoLogic;

//TODO: expand into full interpreter?

function ParseLiteral(Parser:TStratoParserDecl;LookUpDecl:TParserLookupDecl;
   st0:TStratoToken;NeedValue:PInteger):xNode;

  function kn(Key:xKey;Node:xNode):xKeyValue;
  begin
    Result.k:=Key;
    Result.n:=xRef(0);//handled by sphere
    if (Node.sphere=nil) or (Node.sphere=Parser.Sphere) then
      Result.i:=0
    else
      Result.i:=SphereIndex(Node.sphere);
    Result.v:=Node.index;
  end;


type
  TPrecedence=(
    pParentheses,
      p_Something,
    pLogicalOr,
    pLogicalXor,
    pLogicalAnd,
    pBitwiseOr,
    pBitwiseXor,
    pBitwiseAnd,
    pEqual,
    pComparative,
    pShift,
    pAddSub,
    pMulDiv,
    pUnary,
    pTypeOf,pSizeOf
  );
const
  stackGrowSize=$10;
var
  st:TStratoToken;
  vt:xNode;
  vv:UTF8String;
  stackSize,stackIndex:integer;
  stack:array of record
    p:TPrecedence;
    vt:xNode;
    v1,v2:UTF8String;
  end;
  SrcPos:xSrcPos;

  procedure Combine(pp:TPrecedence);
  var
    p0:TPrecedence;
    v0,ww:UTF8String;
    wt:xNode;
    done:boolean;
    i,j:int64;
  begin
    if not vt.IsNone then
     begin
      done:=false;
      while not(done) and (stackIndex<>0) and (stack[stackIndex-1].p>=pp) do
       begin
        dec(stackIndex);
        p0:=stack[stackIndex].p;
        v0:=stack[stackIndex].v1;
        ww:=stack[stackIndex].v2;
        wt:=stack[stackIndex].vt;
        stack[stackIndex].v2:='';
        case p0 of
          pParentheses:
            done:=true;//always only one (need to parse ")" correctly)
          pUnary:
            case v0[1] of
              '-':vv:='-'+ww;//assert vt=IntrinsicTypes[itNumber]
              '!','~'://not
                if SameType(vt,IntrinsicTypes[itBoolean]) then
                  if ww='0' then vv:='1' else vv:='0'
                else
                if SameType(vt,IntrinsicTypes[itNumber]) then
                 begin
                  i:=ParseInteger(vv);
                  vv:=IntToStr8(not(i));
                 end
                else
                  Parser.Source.Error('unsupported type for ''not''');
              else
                Parser.Source.Error('unknown unary operator');
            end;
          pSizeOf:
            vv:=IntToStr8(ByteSize(vt));
          pMulDiv,pAddSub,pShift,
          pLogicalOr,pLogicalXor,pLogicalAnd,//TODO:
          pBitwiseOr,pBitwiseXor,pBitwiseAnd,
          pEqual,pComparative:
            if SameType(vt,IntrinsicTypes[itNumber]) then
             begin
              i:=ParseInteger(vv);
              j:=ParseInteger(ww);
              case v0[1] of
                '*':vv:=IntToStr8(i *   j);
                '/':vv:=IntToStr8(i div j);
                '%':vv:=IntToStr8(i mod j);
                '+':vv:=IntToStr8(i +   j);
                '-':vv:=IntToStr8(i -   j);
                '&':vv:=IntToStr8(i and j);
                '|':vv:=IntToStr8(i or  j);
                'X':vv:=IntToStr8(i xor j);
                '=':
                 begin
                  vt:=IntrinsicTypes[itBoolean];
                  if i=j then vv:='1' else vv:='0';
                 end;
                '<':
                 begin
                  vt:=IntrinsicTypes[itBoolean];
                  if i<j then vv:='1' else vv:='0';
                 end;
                'l':
                 begin
                  vt:=IntrinsicTypes[itBoolean];
                  if i<=j then vv:='1' else vv:='0';
                 end;
                '>':
                 begin
                  vt:=IntrinsicTypes[itBoolean];
                  if i>j then vv:='1' else vv:='0';
                 end;
                'g':
                 begin
                  vt:=IntrinsicTypes[itBoolean];
                  if i>=j then vv:='1' else vv:='0';
                 end;
                's':
                  if v0='shl' then
                    vv:=IntToStr8(i shl j)
                  else
                    vv:=IntToStr8(i shr j);
                'r':
                  if v0='rol' then
                    vv:=IntToStr8((i shl j) or (i shr (
                      SystemWordSize*8-j)))
                  else
                    vv:=IntToStr8((i shr j) or (i shl (
                      SystemWordSize*8-j)));
                else
                  Parser.Source.Error('unknown operator');
              end;
             end
            else
              case v0[1] of
                '+':vv:=ww+vv;
                '=':
                 begin
                  vt:=IntrinsicTypes[itBoolean];
                  //TODO: CanCast?
                  if SameType(vt,wt) and (vv=ww) then vv:='1' else vv:='0';
                 end;
                else
                  Parser.Source.Error('unsupported type for operator');
              end;
          else
            Parser.Source.Error('unexpected constant operator precedence');
        end;
       end;
     end;
  end;

  procedure Push(p:TPrecedence;const ww:UTF8String);
  begin
    Combine(p);
    if stackIndex=stackSize then //grow
     begin
      inc(stackSize,stackGrowSize);
      SetLength(stack,stackSize);
     end;
    stack[stackIndex].p:=p;
    stack[stackIndex].v1:=ww;
    stack[stackIndex].v2:=vv;
    stack[stackIndex].vt:=vt;
    inc(stackIndex);
    vt.none;
    vv:='';
  end;

var
  p:xRef;
  q:PxKeyValue;
begin
  st:=st_Unknown;
  stackSize:=0;
  stackIndex:=0;
  vt.none;
  vv:='';
  SrcPos:=Parser.Source.SrcPos;
  while (st=st_Unknown) or (stackIndex<>0) do
   begin
    if st=st_Unknown then st:=st0 else st:=Parser.Source.Token;
    case st of
    stIdentifier:
     begin
      p:=LookUpDecl.index;
      if p=0 then
        Parser.Source.Error('undefined constant value')
      else
       begin
        case Parser.Sphere[p].k of
          nConstant,nVar:
           begin
            q:=Parser.Sphere.v(p,iValue);
            if q.i<>0 then
              Parser.Source.Error('content value from different sphere not supported');
            p:=q.v;
           end
          else
            p:=0;
        end;
        if p=0 then
         begin
          Parser.Source.Error('unsupported constant value');
          vt.none;
          vv:='';
         end
        else
         begin
          vt:=Parser.Sphere.r(p,iType);
          q:=Parser.Sphere.v(p,iValue);
          if q.i<>0 then
            Parser.Source.Error('content value from different sphere not supported');
          vv:=Parser.Sphere.BinaryData(q.v);
         end;
        end;
     end;
    stStringLiteral:
     begin
      vt:=IntrinsicTypes[itString];
      vv:=Parser.Source.GetStrs;
     end;
    stNumericLiteral:
     begin
      vt:=IntrinsicTypes[itNumber];
      vv:=Parser.Source.GetID(SrcPos);
     end;
    stOpAdd://unary
      Push(pUnary,'+');
    stOpSub://unary
      Push(pUnary,'-');
    stTilde:
      Push(pUnary,'~');
    stOpSizeOf://unary
      Push(pSizeOf,'');
    {//TODO:
    stCOpen://JSON?
    stBOpen://array
    }
    stPOpen://expression (tuple?)
      Push(pParentheses,'');
    stPClose:
      if stackIndex=0 then
        Parser.Source.Error('unexpected token')
      else
        Combine(pParentheses);
    else
     begin
      Parser.Source.Error('unsupported literal syntax');
      vt:=IntrinsicTypes[itType];//break out of loop
     end;
    end;
    if not(vt.IsNone) then
      if Parser.Source.IsNextBetween(stOpAssign,stOp_Last) then
       begin
        st:=Parser.Source.Token;
        case st of
        stOpAdd:Push(pAddSub,'+');
        stOpSub:Push(pAddSub,'-');
        stOpMul:Push(pMulDiv,'*');
        stOpDiv:Push(pMulDiv,'/');
        stOpMod:Push(pMulDiv,'%');
        stOpShl:Push(pShift,'shl');
        stOpShr:Push(pShift,'shr');
        stThreeLT:Push(pShift,'rol');
        stThreeGT:Push(pShift,'ror');
        stOpAnd:
          if SameType(vt,IntrinsicTypes[itBoolean]) then
            Push(pLogicalAnd,'&')
          else
            Push(pBitwiseAnd,'&');
        stOpOr:
          if SameType(vt,IntrinsicTypes[itBoolean]) then
            Push(pLogicalOr,'|')
          else
            Push(pBitwiseOr,'|');
        stOpNot:Push(pUnary,'!');
        stTilde:Push(pUnary,'~');
        stOpXor:
          if SameType(vt,IntrinsicTypes[itBoolean]) then
            Push(pLogicalXor,'X')
          else
            Push(pBitwiseXor,'X');
        stOpEQ:Push(pEqual,'=');
        stOpLT:Push(pComparative,'<');
        stOpLTE:Push(pComparative,'l');
        stOpGT:Push(pComparative,'>');
        stOpGTE:Push(pComparative,'g');
        else Parser.Source.Error('unsupported constant operator');
        end;
       end
      else
        Combine(p_Something);//something between pParentheses and operators
   end;

  if NeedValue=nil then
   begin
    if vt.IsNone then
      Parser.Source.Error('literal of undetermined type');
    Result:=Parser.Add(nLiteral,4,
      [kv(vSrcPos,0,SrcPos)
      ,kn(iType,vt)
      ,kv(iValue,0,Parser.Sphere.AddBinaryData(vv))
      ]);
   end
  else
   begin
    Result.none;
    if SameType(vt,IntrinsicTypes[itNumber]) then
      NeedValue^:=ParseInteger(vv)
    else
     begin
      NeedValue^:=0;//default
      Parser.Source.Error('integer constant expected');
     end;
   end;
end;

end.