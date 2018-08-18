unit stratoLit;

interface

uses stratoDecl, stratoSphere, stratoParse, stratoTokenizer;

type
  TParserLookupDecl=function:rItem of object;

function ParseLiteral(Parser:TStratoParser; LookUpDecl:TParserLookupDecl;
  st0:TStratoToken; NeedValue:PInteger):rItem;

implementation

uses SysUtils, stratoLogic, stratoTools;

//TODO: expand into full interpreter?

function ParseLiteral(Parser:TStratoParser; LookUpDecl:TParserLookupDecl;
   st0:TStratoToken; NeedValue:PInteger):rItem;
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
  vt:rItem;
  vv:UTF8String;
  stackSize,stackIndex:integer;
  stack:array of record
    p:TPrecedence;
    vt:rItem;
    v1,v2:UTF8String;
  end;
  SrcPos:xSrcPos;

  procedure Combine(pp:TPrecedence);
  var
    p0:TPrecedence;
    v0,ww:UTF8String;
    wt:rItem;
    done:boolean;
    i,j:int64;
  begin
    if vt.x<>0 then
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
                if vt.x=IntrinsicTypes[itBoolean] then
                  if ww='0' then vv:='1' else vv:='0'
                else
                if vt.x=IntrinsicTypes[itNumber] then
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
            if vt.x=IntrinsicTypes[itNumber] then
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
                  vt.x:=IntrinsicTypes[itBoolean];
                  if i=j then vv:='1' else vv:='0';
                 end;
                '<':
                 begin
                  vt.x:=IntrinsicTypes[itBoolean];
                  if i<j then vv:='1' else vv:='0';
                 end;
                'l':
                 begin
                  vt.x:=IntrinsicTypes[itBoolean];
                  if i<=j then vv:='1' else vv:='0';
                 end;
                '>':
                 begin
                  vt.x:=IntrinsicTypes[itBoolean];
                  if i>j then vv:='1' else vv:='0';
                 end;
                'g':
                 begin
                  vt.x:=IntrinsicTypes[itBoolean];
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
                  vt.x:=IntrinsicTypes[itBoolean];
                  //TODO: CanCast?
                  if (vt.x=wt.x) and (vv=ww) then vv:='1' else vv:='0';
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
    vt.x:=0;
    vv:='';
  end;

var
  p:rItem;
begin
  st:=st_Unknown;
  stackSize:=0;
  stackIndex:=0;
  vt.x:=0;
  vv:='';
  SrcPos:=Parser.Source.SrcPos;
  while (st=st_Unknown) or (stackIndex<>0) do
   begin
    if st=st_Unknown then st:=st0 else st:=Parser.Source.Token;
    case st of
      stIdentifier:
       begin
        p:=LookUpDecl;
        if p.x=0 then
          Parser.Source.Error('undefined constant value')
        else
         begin
          case p.NodeType of
            nConstant,nVar:
              p:=p.r(iValue);
            else
              p.x:=0;
          end;
          if p.x=0 then
           begin
            Parser.Source.Error('unsupported constant value');
            vt.x:=0;
            vv:='';
           end
          else
           begin
            vt:=p.r(iType);
            vv:=BinaryData(p.r(iValue));
           end;
          end;
       end;
      stStringLiteral:
       begin
        vt.x:=IntrinsicTypes[itString];
        vv:=Parser.Source.GetStrs;
       end;
      stNumericLiteral:
       begin
        vt.x:=IntrinsicTypes[itNumber];
        vv:=Parser.Source.GetID(cardinal(SrcPos));
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
        vt.x:=IntrinsicTypes[itType];//break out of loop
       end;
    end;
    if vt.x<>0 then
      if Parser.Source.IsNextBetween(stOpAssign,stOpTypeIs) then
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
            if vt.x=IntrinsicTypes[itBoolean] then
              Push(pLogicalAnd,'&')
            else
              Push(pBitwiseAnd,'&');
          stOpOr:
            if vt.x=IntrinsicTypes[itBoolean] then
              Push(pLogicalOr,'|')
            else
              Push(pBitwiseOr,'|');
          stOpNot:Push(pUnary,'!');
          stTilde:Push(pUnary,'~');
          stOpXor:
            if vt.x=IntrinsicTypes[itBoolean] then
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
    if vt.x=0 then
      Parser.Source.Error('literal of undetermined type');
    Result:=Parser.Add(nLiteral,
      [vSrcPos,SrcPos
      ,iType,vt.x
      ,iValue,AddBinaryData(Parser.SrcIndex,vv)
      ]);
   end
  else
   begin
    Result.x:=0;
    if vt.x=IntrinsicTypes[itNumber] then
      NeedValue^:=ParseInteger(vv)
    else
     begin
      NeedValue^:=0;//default
      Parser.Source.Error('integer constant expected');
     end;
   end;
end;

end.