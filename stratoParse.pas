unit stratoParse;

interface

uses stratoDecl, stratoSphere, stratoSource;

function StratoParseSource(Store: TStratoStore;
  Source: TStratoSource; InlineErrors: boolean): xItem;

implementation

uses SysUtils, stratoTokenizer, stratoRunTime, stratoFn, stratoLogic, Math;

type
  TPrecedence=(
    p___,
    pCodeBlock,
      p_Statement,
    pThrow,pDefer,pCatch,
    pBrackets,
    pParentheses,
    pForCritOpen,pForCritPara,pForPostEval,
      p_CodeBlockDone,
    pIfThen,pIfElse,
    pForBodyFirst,pForBody,

    pUnTypedVar,
    pArgList,
      p_ArgList_Item,
    pAssign,
      p_Juxta,
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
      p_Cast,
    pUnary,
    pTypeOf,pSizeOf,pAddressOf,
    pUnresolved
  );

type
  TStratoParserBase=class(TObject)
  protected
    Store: TStratoStore;
    Sphere: TStratoSphere;
    Source: TStratoSource;
    FNameSpace: xItem;
    FInlineErrors: boolean;

    Locals:array of xItem;//nNameSpace

    procedure ID(var n:xName;var nn:UTF8String;var SrcPos:xSrcPos);
    function LookUpNameSpace(var ns:xItem;var nx:xName):boolean;
    procedure ParseImport;
    function LookUpID(StopAtType:boolean=false):xItem;
    procedure LookUpNext(nx:xName;const nn:UTF8String;
      var p,ns,isNew:xItem);
    function Fn(x:xItem;nx:xName;const nn:UTF8String;SrcPos:xSrcPos):xItem;//nMember
    function ParseLiteral(st0:TStratoToken;NeedValue:PInteger):xItem;//nLiteral
    function LookUpType(const tname:string;Lazy:boolean=false):xItem;//nTypeDecl
    function ParseSignature(ns:xItem;const name:UTF8String;
      CloseToken:TStratoToken;SrcPos:xSrcPos):xItem;//nSignature
    procedure ParseEnumeration(p:xItem);//nEnumeration
    procedure ParseInterfaceDecl(x:xItem);//nInterface
  public
    //constructor Create(ASphere: TStratoSphere; ASource: TStratoSource);
    property NameSpace: xItem read FNameSpace;
  end;

  TStratoParser=class(TStratoParserBase)
  private
    stackSize,stackIndex:cardinal;
    stack:array of record
      pp:TPrecedence;
      p1,p2:xItem;
      SrcPos:xSrcPos;
    end;
    cb,rd:xItem;
    cbInhCalled:boolean;

    procedure Push(pp:TPrecedence;p1,p2:xItem;SrcPos:xSrcPos);
    procedure CodeLookup(nx:xName;var p:xItem;SrcPos:xSrcPos);
    function Combine(zz:TPrecedence;var q:xItem):boolean;
    procedure Juxta(var p:xItem);
    procedure PushBinary(p:TPrecedence;st:TStratoToken;var q:xItem);

    procedure ParseHeader;
    procedure ParseDeclaration;
    procedure ParseRecord;
    procedure ParseLogic;

    procedure CheckPassed(p:xItem);
    function CbStart(pp:xItem):xItem;//nCodeBlock
    procedure CbAdd(p:xItem);
  public
    constructor Create(AStore: TStratoStore; ASource: TStratoSource;
      InlineErrors: boolean);
    procedure Parse;
  end;

function StratoParseSource(Store: TStratoStore;
  Source: TStratoSource; InlineErrors: boolean): xItem; //nNameSpace
var
  p:TStratoParser;
begin
  if Source.IsNext([st_EOF]) then
    Result:=0 //raise?
  else
   begin
    p:=TStratoParser.Create(Store,Source,InlineErrors);
    try
      p.Parse;
      Result:=p.NameSpace;
    finally
      p.Free;
    end;
   end;
end;

{ TStratoParserBase }

{
constructor TStratoParserBase.Create(ASphere: TStratoSphere;
  ASource: TStratoSource);
begin
  inherited Create;
  Sphere:=ASphere;
  Source:=ASource;
  FNameSpace:=0;//default
end;
}

procedure TStratoParserBase.ID(var n:xName;var nn:UTF8String;var SrcPos:xSrcPos);
begin
  nn:=Source.GetID(cardinal(SrcPos));
  n:=Store.Dict.StrIdx(nn);
end;

function TStratoParserBase.LookUpNameSpace(var ns:xItem; var nx:xName):boolean;
var
  nn:UTF8String;
  SrcPos:xSrcPos;
  p:xItem;
begin
  //assert Source.Token was stIdentifier
  ns:=0;
  ID(nx,nn,SrcPos);
  p:=xItem(-1);
  while Store.NextModule(p) and (ns=0) do ns:=Sphere.Lookup(p,nx);
  if (ns<>0) and (Sphere.n(ns,vTypeNr)^<>nNameSpace) then
   begin
    Source.Error('"'+nn+'" is not a namespace');
    ns:=0;
   end;
  Result:=ns<>0;
  //resolve nested namespaces
  while Source.IsNext([stPeriod,stIdentifier]) do
   begin
    if not(Result) then
      if ns=0 then
        ns:=Sphere.Add(Sphere.SourceFile,fSourceFile_NameSpaces,nNameSpace,0,0,[vName,nx])
      else
        ns:=Sphere.Add(ns,fItems,nNameSpace,ns,0,[vName,nx]);
    Source.Token;//stIdentifier
    ID(nx,nn,SrcPos);
    p:=Sphere.Lookup(ns,nx);
    if p=0 then
      Result:=false
    else
    if Sphere.n(p,vTypeNr)^<>nNameSpace then
     begin
      Source.Error('"'+nn+'" is not a namespace');
      Result:=false;
     end
    else
     begin
      ns:=p;
      Result:=true;
     end;
   end;
end;

procedure TStratoParserBase.ParseImport;
var
  ns,p:xItem;
  ss:TStratoSource;
  nx:xName;
  alias:UTF8String;
  fn,fn1:string;
  i,l:integer;
  SrcPos:xSrcPos;
begin
  ns:=0;//default
  fn:='';//default;
  //alias?
  if Source.IsNext([stIdentifier,stDefine]) then
   begin
    alias:=Source.GetID(cardinal(SrcPos));
    Source.Token;//stDefine
   end
  else
    alias:='';
  case Source.Token of
    stIdentifier:
     begin
      if not LookupNameSpace(ns,nx) then
       begin
        //Sphere.Add(ttNameSpace here? see StratoParseSource below
        fn:=string(Sphere.FQN(ns));
        if fn<>'' then fn:=fn+'.';
        fn1:=fn+Store.Dict.Str[nx];
        fn:=fn1+'.xs';
        if not Store.FindFile(fn) then
          Source.Error('import not found "'+fn1+'"');
       end;
     end;
    stStringLiteral:
     begin
      //TODO: resolve relative path, list of paths, system paths
      //TODO: allow duplicates? detect by full path?
      fn:=string(Source.GetStr);
     end;
    else Source.Error('unsupported import subject syntax');
  end;
{
  if Source.IsNext([stAt,stNumericLiteral]) then
   begin
    Source.Token;//stNumericLiteral
    i:=ParseInteger(string(Source.GetID(SrcPos1)))
   end
  else
    i:=0;
}
  Source.Skip(stSemiColon);
  //load and parse
  if fn<>'' then
   begin
    ss:=TStratoSource.Create;
    ss.OnError:=Source.OnError;//?
    ss.LoadFromFile(Store.ResolvePath(fn));
    ns:=StratoParseSource(Store,ss,FInlineErrors);
    Source.ErrorCount:=Source.ErrorCount+ss.ErrorCount;
   end;
  //TODO: if (fn='') and (q<>0) then 'already loaded at...'?
  //register
  if ns<>0 then
    if alias<>'' then //alias
     begin
      if not Sphere.Add(Locals[0],fItems,nImport,
        Store.Dict.StrIdx(alias),Locals[0],SrcPos,[fTarget,ns],p) then
        Source.Error('duplicate identifier "'+alias+'"');
     end
    else
     begin
      //TODO: store locals in Sphere?
      l:=Length(Locals);
      i:=0;
      while (i<>l) and (Locals[i]<>ns) do inc(i);
      if i=l then
       begin
        SetLength(Locals,i+1);
        Locals[i]:=ns;
       end;
     end;
     
  //TODO: if Source.IsNext([stPOpen?stCOpen?
end;

function TStratoParserBase.LookUpID(StopAtType:boolean=false):xItem;
var
  i,j,l:integer;
  nx:xName;
  nn:UTF8String;
  nsx:array of xItem;
  SrcPos:xSrcPos;
  p:xItem;
  function CheckFoundType: boolean;
  begin
    Result:=true;//default
    if StopAtType and (j=1) then
     begin
      i:=0;
      while (i<l) and (nsx[i]=0) do inc(i);
      if (i<l) and ((cardinal(Sphere.n(nsx[i],vTypeNr)^) and $F00)>=$400) then
        Result:=false;
     end
  end;
begin
  Result:=0;//default
  ID(nx,nn,SrcPos);
  //look-up with locals
  l:=Length(Locals);
  SetLength(nsx,l+1);
  j:=0;
  for i:=0 to l-1 do
   begin
    nsx[i]:=Sphere.Lookup(Locals[i],nx);
    if nsx[i]<>0 then inc(j);
   end;
  //look-up with module's root namespace(s)
  p:=xItem(-1);
  nsx[l]:=0;
  while Store.NextModule(p) and (nsx[l]=0) do
    nsx[l]:=Sphere.Lookup(p,nx); //no inc(j) here! see below (case j=0)
  while CheckFoundType and Source.IsNext([stPeriod,stIdentifier]) do
   begin
    Source.Token;//stIdentifier
    ID(nx,nn,SrcPos);
    for i:=0 to l do
      if nsx[i]<>0 then
       begin
        nsx[i]:=Sphere.Lookup(nsx[i],nx);
        if nsx[i]=0 then
          dec(j)
        else
          if Sphere.n(nsx[i],vTypeNr)^=nImport then
            nsx[i]:=Sphere.n(nsx[i],fSubject)^;
       end;
   end;
  //assert Source.Token=stIdentifier
  case j of
    0://none found, any by namespaces?
      Result:=nsx[l];
    1://one thing found
     begin
      i:=0;
      while (i<>l) and (nsx[i]=0) do inc(i);
      Result:=nsx[i];
     end;
    else//multiple found
     begin
      nn:='';
      for i:=0 to l-1 do
        if nsx[i]<>0 then
          nn:=nn+','+Sphere.FQN(nsx[i]);
      Source.Error('multiple declarations "'+
        string(Copy(nn,2,Length(nn)-1))+'"');
     end;
  end;
end;

procedure TStratoParserBase.LookUpNext(nx:xName;const nn:UTF8String;
  var p,ns,isNew:xItem);
var
  i,l:integer;
  q:xItem;
begin
  isNew:=0;//default
  if p=0 then
   begin
    l:=Length(Locals);
    i:=0;
    while (i<>l) and (p=0) do
     begin
      if Locals[i]<>0 then
        p:=Sphere.Lookup(Locals[i],nx);
      inc(i);
     end;
    if p=0 then //still nothing, check namespaces
     begin
      q:=xItem(-1);
      while Store.NextModule(q) and (p=0) do
        p:=Sphere.Lookup(q,nx);
     end;
   end
  else
    p:=Sphere.Lookup(p,nx);
  if p=0 then
   begin
    //create to silence further errors?
    if Sphere.Add(ns,fItems,nNameSpace,nx,ns,0,[],p) then
      isNew:=p
    else
     begin
      Source.Error('duplicate namespace "'+
        string(Sphere.FQN(ns)+'.'+nn)+'"');
      isNew:=Sphere.Lookup(ns,nx);
     end;
   end;
  ns:=p;
end;

function TStratoParserBase.ParseLiteral(st0:TStratoToken;
  NeedValue:PInteger):xItem;//nLiteral
const
  stackGrowSize=$10;
var
  st:TStratoToken;
  vt:xItem;
  vv:UTF8String;
  stackSize,stackIndex:integer;
  stack:array of record
    p:TPrecedence;
    vt:xItem;
    v1,v2:UTF8String;
  end;
  SrcPos:xSrcPos;

  procedure Combine(pp:TPrecedence);
  var
    p0:TPrecedence;
    v0,ww:UTF8String;
    wt:xItem;
    done:boolean;
    i,j:int64;
  begin
    if vt<>0 then
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
              '-':vv:='-'+ww;//assert vt=TypeDecl_number
              '!','~'://not
                if vt=TypeDecl_bool then
                  if ww='0' then vv:='1' else vv:='0'
                else
                if vt=TypeDecl_number then
                 begin
                  i:=ParseInteger(string(vv));
                  vv:=UTF8String(IntToStr(not(i)));
                 end
                else
                  Source.Error('unsupported type for ''not''');
              else
                Source.Error('unknown unary operator');
            end;
          pSizeOf:
            vv:=IntToStr(ByteSize(Sphere,vt));
          pMulDiv,pAddSub,pShift,
          pLogicalOr,pLogicalXor,pLogicalAnd,//TODO:
          pBitwiseOr,pBitwiseXor,pBitwiseAnd,
          pEqual,pComparative:
            if vt=TypeDecl_number then
             begin
              i:=ParseInteger(string(vv));
              j:=ParseInteger(string(ww));
              case v0[1] of
                '*':vv:=UTF8String(IntToStr(i*j));
                '/':vv:=UTF8String(IntToStr(i div j));
                '%':vv:=UTF8String(IntToStr(i mod j));
                '+':vv:=UTF8String(IntToStr(i+j));
                '-':vv:=UTF8String(IntToStr(i-j));
                '&':vv:=UTF8String(IntToStr(i and j));
                '|':vv:=UTF8String(IntToStr(i or j));
                'X':vv:=UTF8String(IntToStr(i xor j));
                '=':
                 begin
                  vt:=TypeDecl_bool;
                  if i=j then vv:='1' else vv:='0';
                 end;
                '<':
                 begin
                  vt:=TypeDecl_bool;
                  if i<j then vv:='1' else vv:='0';
                 end;
                'l':
                 begin
                  vt:=TypeDecl_bool;
                  if i<=j then vv:='1' else vv:='0';
                 end;
                '>':
                 begin
                  vt:=TypeDecl_bool;
                  if i>j then vv:='1' else vv:='0';
                 end;
                'g':
                 begin
                  vt:=TypeDecl_bool;
                  if i>=j then vv:='1' else vv:='0';
                 end;
                's':
                  if v0='shl' then
                    vv:=UTF8String(IntToStr(i shl j))
                  else
                    vv:=UTF8String(IntToStr(i shr j));
                'r':
                  if v0='rol' then
                    vv:=UTF8String(IntToStr((i shl j) or (i shr (SystemWordSize*8-j))))
                  else
                    vv:=UTF8String(IntToStr((i shr j) or (i shl (SystemWordSize*8-j))));
                else
                  Source.Error('unknown operator');
              end;
             end
            else
              case v0[1] of
                '+':vv:=ww+vv;
                '=':
                 begin
                  vt:=TypeDecl_bool;
                  //TODO: CanCast?
                  if (vt=wt) and (vv=ww) then vv:='1' else vv:='0';
                 end;
                else
                  Source.Error('unsupported type for operator');
              end;
          else
            Source.Error('unexpected constant operator precedence');
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
    vt:=0;
    vv:='';
  end;

var
  p,q:xItem;
begin
  st:=st_Unknown;
  stackSize:=0;
  stackIndex:=0;
  vt:=0;
  vv:='';
  SrcPos:=Source.SrcPos;
  while (st=st_Unknown) or (stackIndex<>0) do
   begin
    if st=st_Unknown then st:=st0 else st:=Source.Token;
    case st of
      stIdentifier:
       begin
        q:=LookUpID;
        if q=0 then
          Source.Error('undefined constant value')
        else
         begin
          case Sphere.n(q,vTypeNr)^ of
            nConstant,nVarDecl:
              p:=Sphere.n(q,fValue)^;
            else
              p:=0;
          end;
          if p=0 then
           begin
            Source.Error('unsupported constant value');
            vt:=0;
            vv:='';
           end
          else
           begin
            vt:=Sphere.n(p,fTypeDecl)^;
            vv:=Sphere.GetBinaryData(Sphere.n(p,fValue)^);
           end;
          end;
       end;
      stStringLiteral:
       begin
        vt:=TypeDecl_string;
        vv:=Source.GetStrs;
       end;
      stNumericLiteral:
       begin
        vt:=TypeDecl_number;
        vv:=Source.GetID(cardinal(SrcPos));
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
          Source.Error('unexpected token')
        else
          Combine(pParentheses);
      else
       begin
        Source.Error('unsupported literal syntax');
        vt:=TypeDecl_type;//break out of loop
       end;
    end;
    if vt<>0 then
      if Source.IsNextBetween(stOpAssign,stOpTypeIs) then
       begin
        st:=Source.Token;
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
            if vt=TypeDecl_bool then
              Push(pLogicalAnd,'&')
            else
              Push(pBitwiseAnd,'&');
          stOpOr:
            if vt=TypeDecl_bool then
              Push(pLogicalOr,'|')
            else
              Push(pBitwiseOr,'|');
          stOpNot:Push(pUnary,'!');
          stTilde:Push(pUnary,'~');
          stOpXor:
            if vt=TypeDecl_bool then
              Push(pLogicalXor,'X')
            else
              Push(pBitwiseXor,'X');
          stOpEQ:Push(pEqual,'=');
          stOpLT:Push(pComparative,'<');
          stOpLTE:Push(pComparative,'l');
          stOpGT:Push(pComparative,'>');
          stOpGTE:Push(pComparative,'g');
          else Source.Error('unsupported constant operator');
        end;
       end
      else
        Combine(pAssign);//something between pParentheses and operators
   end;

  if NeedValue=nil then
   begin
    if vt=0 then
      Source.Error('literal of undetermined type');
    Result:=Sphere.Add(nLiteral,0,SrcPos,
      [fTypeDecl,vt
      ,fValue,Sphere.AddBinaryData(vv)
      ]);
   end
  else
   begin
    Result:=0;
    if vt=TypeDecl_number then
      NeedValue^:=ParseInteger(string(vv))
    else
     begin
      NeedValue^:=0;//default
      Source.Error('integer constant expected');
     end;
   end;
end;

function TStratoParserBase.LookUpType(const tname:string;
  Lazy:boolean=false):xItem;//nTypeDecl
var
  p:xItem;
  i,ptr:cardinal;
begin
  //TODO: if stCOpen then ParseRecord here? (ParseJSON??)
  if Source.IsNext([stQuestionMark,stIdentifier]) then
   begin
    Source.Token;//stIdentifier
    p:=LookUpID;
    if Sphere.n(p,vTypeNr)^<>nClass then
      Source.Error('class reference allowed to class only');
    Result:=Sphere.Add(nClassRef,Sphere.n(p,fParent)^,Sphere.n(p,vSrcPos)^,[fSubject,p]);
   end
  else
   begin
    ptr:=0;
    while Source.IsNext([stCaret]) do inc(ptr);
    if Source.IsNext([stIdentifier]) then
      Result:=LookUpID(Lazy)
    else
      Result:=0;
    if Result=0 then
      Source.Error('undefined '+tname)
    else
     begin
      case Sphere.n(Result,vTypeNr)^ of
        nVarDecl:
          Result:=Sphere.n(Result,fTypeDecl)^;//take var's type
        nTypeAlias:
          Result:=Sphere.n(Result,fTypeDecl)^;//assert never another nTypeAlias
        nSignature:
          //TODO: nDelegate? (keep signature and subject)
          Result:=Sphere.Add(nPointer,Sphere.n(Result,fParent)^,Sphere.n(Result,vSrcPos)^,//Source.SrcPos?
            [fTarget,Result])
        else
          if (cardinal(Sphere.n(Result,vTypeNr)^) and $F00)<$400 then
            Source.Error(tname+' is not a type');
      end;
      //array
      if Source.IsNext([stBOpen]) then
        if Source.IsNext([stBClose]) then
          Source.Error('//TODO: dyn array')
        else
         begin
          ParseLiteral(Source.Token,@i);
          Source.Skip(stBClose);//TODO: force
          Result:=Sphere.Add(nArray,Sphere.n(Result,fParent)^,Sphere.n(Result,vSrcPos)^,//Source.SrcPos?
            [vName,Sphere.n(Result,vName)^
            ,fTypeDecl,Result
            ,vByteSize,ByteSize(Sphere,Result)*i
            ]);
         end;
     end;
    while ptr<>0 do
     begin
      dec(ptr);
      Result:=Sphere.Add(nPointer,Sphere.n(Result,fParent)^,Sphere.n(Result,vSrcPos)^,//Source.SrcPos?
        [fSubject,Result]);
     end;
   end;
end;

function TStratoParserBase.ParseSignature(ns:xItem;
  const name:UTF8String;CloseToken:TStratoToken;SrcPos:xSrcPos):xItem;//nSignature
var
  st:TStratoToken;
  p,q,Signature,NoType:xItem;
  argName:UTF8String;
  byref:boolean;

  procedure AddArgument(InitialValue:xItem);
  var
    nn:xTypeNr;
    r:xItem;
  begin
    if byref then nn:=nArgByRef else nn:=nArgument;
    if not Sphere.Add(Signature,fArguments,nn,
      Store.Dict.StrIdx(argName),Signature,SrcPos,
      [fTypeDecl,p,fValue,InitialValue],r) then
      Source.Error('duplicate argument "'+argName+'"');
    byref:=false;
    if (p=0) and (NoType=0) then NoType:=r;
  end;

begin
  //assert one past token stPOpen
  Signature:=Sphere.Add(nSignature,ns,SrcPos,[vName,Store.Dict.StrIdx(name)]);
  Result:=Signature;
  NoType:=0;
  byref:=false;
  st:=stIdentifier;//default (something not stPClose really)
  while (st<>CloseToken) and Source.NextToken(st) do
    case st of

      stCaret:
        if byref then
          Source.Error('unsupported argument syntax')
        else
          byref:=true;

      stIdentifier:
       begin
        argName:=Source.GetID(cardinal(SrcPos));
        p:=0;//default
        q:=0;//default
        st:=Source.Token;
        case st of
          stColon://argument type
           begin
            p:=LookUpType('argument type');
            while NoType<>0 do
             begin
              //assert NoType.TypeDecl=nil
              Sphere.n(NoType,fTypeDecl)^:=p;
              NoType:=Sphere.n(NoType,fNext)^;
             end;
            if Source.IsNext([stDefine]) then //default value
             begin
              q:=ParseLiteral(Source.Token,nil);
              Sphere.n(q,fParent)^:=Signature;
             end;
            AddArgument(q);
            if Source.IsNext([stComma]) or Source.IsNext([stSemiColon]) then
              ;//skip
           end;
          stComma:
            AddArgument(0);
          stDefine://"="
           begin
            q:=ParseLiteral(Source.Token,nil);
            if byref then
              Source.Error('default value on by-reference-argument not supported');
            AddArgument(q);
            if not Source.IsNext([stComma]) then
              Source.Error('argument with default value but no type'+
                ' requires a subsequent argument with type');
           end;
          else Source.Error('unsupported argument syntax');
        end;
       end;

      //TODO: byref? stAt?

      else
        if st<>CloseToken then
          Source.Error('unsupported argument syntax');
    end;
  //TODO: check default values with type
  if Sphere.n(ns,vTypeNr)^<>nNameSpace then //nRecord,nInterface,nTypeDecl
    Sphere.n(Signature,fSubject)^:=ns;
  if Source.IsNext([stColon]) then
    Sphere.n(Signature,fReturnType)^:=LookUpType('returns type')
  else
    if CloseToken=stBClose then
      Source.Error('property requires value type');
end;

procedure TStratoParserBase.ParseEnumeration(p:xItem);//nEnumeration
var
  st:TStratoToken;
  nx:xName;
  nn:UTF8String;
  i:integer;
  q:xItem;
  SrcPos:xSrcPos;
begin
  i:=0;
  st:=stIdentifier;//default (something not stPClose really)
  while (st<>stPClose) and Source.NextToken(st) do
    case st of
      stIdentifier:
       begin
        ID(nx,nn,SrcPos);
        if Source.IsNext([stDefine]) then
          ParseLiteral(Source.Token,@i);
        if Sphere.Add(p,fItems,nConstant,nx,p,SrcPos,[fTypeDecl,p],q) then
         begin
          //TODO: no literals here? cardinal member for nConstant
          Sphere.n(q,fValue)^:=Sphere.Add(nLiteral,p,SrcPos,
            [fValue,Sphere.AddBinaryData(IntToStr(i))]);
          inc(i);
         end
        else
          Source.Error('duplicate enumeration entry "'+nn+'"');
       end;
      stComma,stSemiColon:;//ignore
      stPClose:;//done
      else Source.Error('unsupported enumeration syntax');
    end;
end;

procedure TStratoParserBase.ParseInterfaceDecl(x:xItem);//nInterface
var
  st:TStratoToken;
  nx:xName;
  nn:UTF8String;
  SrcPos:xSrcPos;
  p:xItem;
begin
  //assert previous token stCOpen
  while not(Source.IsNext([stCClose])) and Source.NextToken(st) do
    case st of

      stIdentifier:
       begin
        ID(nx,nn,SrcPos);
        case Source.Token of
          stColon://value
            if not Sphere.Add(x,fItems,nVarDecl,nx,x,SrcPos,
              [fTypeDecl,LookUpType('field type')],p) then
              Source.Error('duplicate interface field "'+nn+'"');
          stPOpen://signature
            StratoFnAdd(Sphere,Source,nOverload,Fn(x,nx,nn,SrcPos),
              ParseSignature(x,nn,stPClose,SrcPos),SrcPos);
          else Source.Error('unsupported interface field syntax');
        end;
        Source.Skip(stSemiColon);
       end;

      else Source.Error('unsupported interface field syntax');
    end;
end;

{ TStratoParser }

constructor TStratoParser.Create(AStore: TStratoStore;
  ASource: TStratoSource; InlineErrors: boolean);
begin
  inherited Create;
  Store:=AStore;
  Sphere:=nil;//see ParseHeader
  Source:=ASource;
  FInlineErrors:=InlineErrors;
  FNameSpace:=0;//default
end;

const
  stackGrowSize=$100;

procedure TStratoParser.Parse;
begin
  ParseHeader;
  stackIndex:=0;
  stackSize:=stackGrowSize;
  SetLength(stack,stackSize);
  //TODO separate stack with cb/rd and Parse*-pointer
  cb:=0;
  cbInhCalled:=false;//see also CbStart
  rd:=0;
  while not Source.IsNext([st_EOF]) do
    if cb=0 then
      if rd=0 then
        ParseDeclaration
      else
        ParseRecord
    else
      ParseLogic;
  if stackIndex<>0 then
    Source.Error('unexpected end of source ('+IntToStr(stackIndex)+')');
  FreeAndNil(Sphere);
end;

procedure TStratoParser.Push(pp:TPrecedence;p1,p2:xItem;SrcPos:xSrcPos);
begin
  if stackIndex=stackSize then
   begin
    inc(stackSize,stackGrowSize);//grow
    SetLength(stack,stackSize);
   end;
  stack[stackIndex].pp:=pp;
  stack[stackIndex].p1:=p1;
  stack[stackIndex].p2:=p2;
  stack[stackIndex].SrcPos:=SrcPos;
  inc(stackIndex);
end;

procedure TStratoParser.CodeLookup(nx:xName;var p:xItem;SrcPos:xSrcPos);
var
  i,l:integer;
  p0,q,q0,r,s:xItem;
begin
  p0:=p;
  //TODO: nImport, nAlias
  if p<>0 then
    r:=p //see below: search by type
  else
   begin
    r:=0;
    p:=Sphere.Lookup(cb,nx);
    //not found? check stack
    if p=0 then
     begin
      i:=stackIndex;
      while (i<>0) and (p=0) do
       begin
        dec(i);
        case stack[i].pp of
          pCodeBlock:
            p:=Sphere.Lookup(stack[i].p1,nx);
          pForCritPara,pForBody:
            p:=Sphere.Lookup(Sphere.n(stack[i].p1,fDoFirst)^,nx);
          pCatch:
            if (Sphere.n(q,vTypeNr)^=nCatchNamed) and (Sphere.n(q,vExName)^=nx) then
              p:=q;//????????????
        end;
       end;
     end;
    //not found? check under 'this'
    if p=0 then
     begin
      Sphere.First(cb,fVarDecls,q,q0);
      if Sphere.n(q,vTypeNr)^=nThis then
       begin
        p:=q;
        r:=p;//see below: search by type
       end;
     end;
   end;
  if r<>0 then
   begin
    q:=p;
    case Sphere.n(p,vTypeNr)^ of
      nClass:
       begin
        r:=p;
        p:=0;
        while (p=0) and (r<>0) do
         begin
          p:=Sphere.Lookup(r,nx);
          if p=0 then r:=Sphere.n(r,fInheritsFrom)^;
         end;
       end;
      nRecord:
        p:=Sphere.Lookup(p,nx);
      //TODO: more?
      else
        p:=0;
    end;
    //nothing, is it typed? search typedecl
    if p=0 then
     begin
      r:=ResType(Sphere,q);
      if (r<>0) and (Sphere.n(r,vTypeNr)^=nArray) then
        r:=0;//r:=Sphere.n(r,fElementType)?
      if r<>0 then
       begin
        case Sphere.n(r,vTypeNr)^ of
          nClass:
           begin
            s:=r;
            r:=0;
            while (r=0) and (s<>0) do
             begin
              r:=Sphere.Lookup(s,nx);
              if r=0 then s:=Sphere.n(s,fInheritsFrom)^;
             end;
           end;
          nRecord:
            r:=Sphere.Lookup(r,nx);
          //TODO: more?
          else
            r:=0;
        end;
       end;
      if r<>0 then
        p:=Sphere.Add(nField,cb,SrcPos,[fSubject,q,fTarget,r]);
     end;
   end;
  //not found? check locals
  if (p=0) and (p0=0) then
   begin
    l:=Length(Locals);
    i:=0;
    while (i<>l) and (p=0) do
     begin
      if Locals[i]<>0 then
        p:=Sphere.Lookup(Locals[i],nx);
      inc(i);
     end;
    if p=0 then //still nothing, check namespaces
     begin
      q:=xItem(-1);
      while Store.NextModule(q) and (p=0) do
        p:=Sphere.Lookup(q,nx);
     end;
   end;
end;

function TStratoParser.Combine(zz:TPrecedence;var q:xItem):boolean;
var
  z,z00:TPrecedence;
  p1,p2,r:xItem;
  SrcPos:xSrcPos;
  done:boolean;
  nn:xTypeNr;
begin
  if (q<>0) or (zz<=pParentheses) then
   begin
    done:=false;
    while not(done) and (stackIndex<>0) and (stack[stackIndex-1].pp>=zz) do
     begin
      dec(stackIndex);
      z:=stack[stackIndex].pp;
      p1:=stack[stackIndex].p1;
      p2:=stack[stackIndex].p2;
      SrcPos:=stack[stackIndex].SrcPos;
      {$IFDEF DEBUG}
      stack[stackIndex].pp:=p___;
      stack[stackIndex].p1:=0;
      stack[stackIndex].p2:=0;
      stack[stackIndex].SrcPos:=0;
      {$ENDIF}
      z00:=z;
      case z of
        pCodeBlock://nCodeBlock
          //see also stCClose in main loop!
          done:=true;//always only one (need to parse "}" correctly)
        pIfThen://nSelection
         begin
          p1:=Sphere.Add(nSelection,cb,SrcPos,[fDoIf,p1,fDoThen,q]);
          StratoSelectionCheckType(Sphere,p1);
          z:=pIfElse;//now parse 'else' bit
         end;
        pIfElse:
         begin
          Sphere.n(p1,fDoElse)^:=q;
          StratoSelectionCheckType(Sphere,p1);
         end;
        pArgList:
         begin
          if q<>0 then StratoFnCallAddArgument(Sphere,p2,q,Source.SrcPos);
          if p1=cb then
           begin
            //"@@@(...)": call inherited
            while Sphere.n(p1,vTypeNr)^=nCodeBlock do
             begin
              q:=p1;
              p1:=Sphere.n(p1,fParent)^;
             end;
            q:=Sphere.n(Sphere.n(q,fVarDecls)^,fNext)^;
            if q=0 then Source.Error('"@@" not found');
            p1:=StratoFnCallBySignature(Sphere,
              Sphere.n(p1,vTypeNr)^,q,p2,cb,SrcPos);
            if p1=0 then Source.Error('unexpected "@@@"');
           end
          else
           begin
            q:=p1;
            case Sphere.n(p1,vTypeNr)^ of
              nClass:
               begin
                //calling destructor? (detect prefix '-' or '~')
                if (stackIndex<>0) and (stack[stackIndex-1].pp=pUnary) and
                  (TStratoToken(Sphere.n(stack[stackIndex-1].p1,vOperator)^)
                    in [stOpSub,stTilde]) and (TypeDecl_object<>0) then
                 begin
                  nn:=nDestructor;
                  dec(stackIndex);
                  {$IFDEF DEBUG}
                  stack[stackIndex].pp:=p___;
                  stack[stackIndex].p1:=0;
                  stack[stackIndex].p2:=0;
                  stack[stackIndex].SrcPos:=0;
                  {$ENDIF}
                 end
                else
                  nn:=nConstructor;
                p1:=StratoFnCallBySignature(Sphere,nn,p1,p2,cb,SrcPos);
               end;

              //nMember:?
              else
                p1:=StratoFnCallBySignature(Sphere,nOverload,p1,p2,cb,SrcPos);
            end;
            if p1=0 then
              if SameType(Sphere,ResType(Sphere,q),TypeDecl_bool) then
               begin
                p1:=q;
                z:=pIfThen //evaluate as a selection
               end
              else
                Source.Error('no function overload found with these arguments');
           end;
          done:=true;//always only one (need to parse ")" correctly)
         end;
        pParentheses:
         begin
          done:=true;//always only one (need to parse ")" correctly)
          p1:=q;
         end;
        pBrackets:
         begin
          if q<>0 then StratoFnCallAddArgument(Sphere,p2,q,Source.SrcPos);
          p1:=StratoFnCallBySignature(Sphere,nPropertyGet,p1,p2,cb,SrcPos);
          if p1=0 then
            Source.Error('no property overload found with these arguments');
          done:=true;//always only one (need to parse "]" correctly)
         end;

        //nIteration,nIterPostEval
        pForCritOpen:
         begin
          if not SameType(Sphere,ResType(Sphere,q),TypeDecl_bool) then
            Source.Error('iteration criterium does not evaluate to boolean');
          Sphere.n(p1,fDoIf)^:=q;
          z:=pForBody;
         end;
        pForCritPara:
          if Sphere.n(p1,fDoIf)^=0 then
           begin
            if SameType(Sphere,ResType(Sphere,q),TypeDecl_bool) then
              Sphere.n(p1,fDoIf)^:=q
            else
              if (Sphere.n(p1,fDoFirst)^=0) and (zz<>pParentheses) then
                Sphere.n(p1,fDoFirst)^:=q
              else
                Source.Error('iteration criterium does not evaluate to boolean');
            if zz=pParentheses then
              z:=pForBody
            else
              z00:=p___;//z:=pForCritPara;
           end
          else
           begin
            if q<>0 then
              if Sphere.n(p1,fDoThen)^=0 then
                Sphere.n(p1,fDoThen)^:=q
              else
                Source.Error('unexpected iteration syntax');
            z:=pForBody;
           end;
        pForBodyFirst:
         begin
          //assert Sphere.n(q,vTypeNr)=nCodeBlock;
          Sphere.n(p1,fBody)^:=q;
          z:=pForPostEval;//see also Juxta
         end;
        pForBody:
         begin
          //assert Sphere.n(q,vTypeNr)=nCodeBlock
          Sphere.n(p1,fBody)^:=q;
          //TODO: patch p2?
         end;
        pForPostEval:
         begin
          if not SameType(Sphere,ResType(Sphere,q),TypeDecl_bool) then
            Source.Error('iteration criterium does not evaluate to boolean');
          Sphere.n(p1,fDoIf)^:=q;
         end;

        //nUnaryOp
        pUnary:
          p1:=Sphere.Add(nUnaryOp,cb,SrcPos,
            [vOperator,p2
            ,fRight,q
            ,fReturnType,ResType(Sphere,q)
            ]);
        pSizeOf:
          p1:=Sphere.Add(nUnaryOp,cb,SrcPos,
            [vOperator,p2
            ,fRight,q
            ,fReturnType,TypeDecl_number
            ]);
        pTypeOf:
         begin
          r:=ResType(Sphere,q);
          if Sphere.n(r,vTypeNr)^=nClass then
            r:=Sphere.Add(nClassRef,cb,SrcPos,[fSubject,r])
          else
            r:=TypeDecl_type;
          p1:=Sphere.Add(nUnaryOp,cb,SrcPos,
            [vOperator,p2
            ,fRight,q
            ,fReturnType,r
            ]);
         end;
        pAddressOf:
         begin
          if IsAddressable(Sphere,q) then
           begin
            r:=ResType(Sphere,q);
            while Sphere.n(r,vTypeNr)^=nArray do
              r:=Sphere.n(r,fTypeDecl)^;
            r:=Sphere.Add(nPointer,cb,SrcPos,[fSubject,r]);
           end
          else
           begin
            Source.Error('invalid address-of subject');
            r:=0;
           end;
          p1:=Sphere.Add(nAddressOf,cb,Source.SrcPos,
            [fSubject,q
            ,fReturnType,r
            ]);
         end;

        //nBinaryOp
        pMulDiv,pAddSub,pShift,
        pLogicalOr,pLogicalXor,pLogicalAnd,
        pBitwiseOr,pBitwiseXor,pBitwiseAnd:
         begin
          p1:=Sphere.Add(nBinaryOp,cb,SrcPos,
            [vOperator,p2
            ,fLeft,p1
            ,fRight,q
            ]);
          if not StratoOperatorCheckType(Sphere,p1) then
            Source.Error('binary operator operand type mismatch');
         end;
        pEqual,pComparative:
         begin
          p1:=Sphere.Add(nBinaryOp,cb,SrcPos,
            [vOperator,p2
            ,fLeft,p1
            ,fRight,q
            ,fReturnType,TypeDecl_bool
            ]);
          if not StratoComparativeCheckType(Sphere,p1) then
            Source.Error('binary operator operand type mismatch');
         end;

        //nAssign
        pAssign:
         begin
          r:=ResType(Sphere,q);
          if r=0 then
            Source.Error('invalid assignment value')
          else
           begin
            if (stackIndex<>0) and (stack[stackIndex-1].pp=pUnTypedVar) then
             begin
              Sphere.n(stack[stackIndex-1].p1,fTypeDecl)^:=r;
              inc(Sphere.n(cb,vByteSize)^,ByteSize(Sphere,r));
             end;

            case StratoAssignmentCheckType(Sphere,p1,r) of
              1:Source.Error('assignment receiver not addressable');
              2:Source.Error('assignment type mismatch');
            end;

            //assigning an object reference? reference counting!
            if Sphere.n(q,vTypeNr)^=nClass then
             begin
              if p2<>xValue(stOpAssign) then
                Source.Error('invalid assignment type for object reference');
              if TypeDecl_object=0 then
                Source.Error('base class for reference counting not defined')
              else
               begin
                //TODO: check not zero then release
                //TODO: call _addref (with StratoFnCallFindSignature ?)
                //TODO: defer release refcount
               end;
             end;
           end;
          p1:=Sphere.Add(nAssign,cb,SrcPos,[vOperator,p2,fTarget,p1,fValue,q]);
         end;

        pUnTypedVar:
         begin
          //see also pAssignment above and stColon below
          //assert Sphere.t(p)=ttVar
          if Sphere.n(p1,fTypeDecl)^=0 then
            Source.Error('no type for local var "'+string(Sphere.FQN(p1))+'"');
          p1:=q;//patch through
         end;
        pUnresolved:
          p1:=StratoCheckMemberNoArguments(Sphere,p1,q,cb,SrcPos);

        pDefer:
          p1:=Sphere.Add(nDeferred,cb,SrcPos,[fItems,q]);
        pThrow:
          p1:=Sphere.Add(nThrow,cb,SrcPos,[fExceptionConstructor,q]);
        pCatch:
          Sphere.n(p1,fBody)^:=q;
        //else ?
      end;
      if z=z00 then
        q:=p1
      else
       begin
        //re-push
        stack[stackIndex].pp:=z;
        stack[stackIndex].p1:=p1;
        stack[stackIndex].p2:=p2;
        //stack[stackIndex].SrcPos:=SrcPos;
        inc(stackIndex);
        q:=0;
        done:=true;
       end;
     end;
   end;
  Result:=q<>0;
end;

procedure TStratoParser.Juxta(var p:xItem);
var
  pp:TPrecedence;
  qx:PxValue;
begin
  if Combine(p_Juxta,p) then
    if SameType(Sphere,ResType(Sphere,p),TypeDecl_bool) then
     begin
      //not in iteration?
      if stackIndex=0 then pp:=p___ else pp:=stack[stackIndex-1].pp;
      case pp of
        pForCritOpen:
         begin
          Sphere.n(stack[stackIndex-1].p1,fDoIf)^:=p;
          stack[stackIndex-1].pp:=pForBody;
         end;
        pForCritPara:
         begin
          qx:=Sphere.n(stack[stackIndex-1].p1,fDoIf);
          if qx^=0 then qx^:=p else
            Source.Error('unexpected iterator syntax');
         end;
        pForBodyFirst:
         begin
          Sphere.n(stack[stackIndex-1].p1,fDoIf)^:=p;
          stack[stackIndex-1].pp:=pForPostEval;
         end;
        else //start selection
          Push(pIfThen,p,0,Sphere.n(p,vSrcPos)^);
      end;
     end
    else
      if p<>0 then
        Source.Error('missing operator or semicolon');
  p:=0;
end;

procedure TStratoParser.PushBinary(p:TPrecedence;st:TStratoToken;
  var q:xItem);
begin
  if not Combine(p,q) then
    Source.Error('no left side defined for binary operator')
  else
    Push(p,q,xValue(st),Source.SrcPos);
  q:=0;
end;

procedure TStratoParser.CheckPassed(p:xItem);
var
  b:boolean;
begin
  if p<>0 then
   begin
    b:=false;
    case Sphere.n(p,vTypeNr)^ of
      nVarDecl,nFCall,nSCall,nVCall,nIteration,nIterPostEval,nAssign,
      nDeferred,nThrow,nCatchAll,nCatchTypes,nCatchNamed,
      nDestructor://,nPropGetCall,nPropSetCall:
        b:=true;
      nCodeBlock,nSelection:
        b:=true;//b:=p.EvaluatesTo=0;//TODO: descend into?
      nBinaryOp:
        b:=TStratoToken(Sphere.n(p,vOperator)^) in [stOpAssign..stOpAssignAnd];
      nUnaryOp:
        b:=TStratoToken(Sphere.n(p,vOperator)^) in [stOpInc,stOpDec];
      //more?
    end;
    if not b then
      Source.Error('statement without calls or assignments');
   end;
end;


function TStratoParser.CbStart(pp:xItem):xItem;
begin
  //switch to ParseLogic
  //assert cb=0
  cb:=pp;
  cbInhCalled:=false;
  //more?
  Result:=pp;
end;

procedure TStratoParser.CbAdd(p:xItem);
var
  q:xItem;
begin
  if p<>0 then
   begin
    if (Sphere.n(p,fParent)^=cb) and (Sphere.n(p,fNext)^=0) then
      q:=p
    else
     begin
      //member of another chain, create an alias
      raise Exception.Create('//TODO: nAlias');
      //Sphere.Add(nAlias,cb,SrcPos,[fTarget,p]);
      end;
    {$IFDEF DEBUG}
    if Sphere.n(p,fNext)^<>0 then
      raise Exception.Create('broken chain detected');
    {$ENDIF}
    Sphere.Append(cb,fItems,q);
   end;
end;

procedure TStratoParser.ParseHeader;
var
  ns,p,p0:xItem;
  b:boolean;
  nx:xName;
  nn:UTF8String;
  SrcPos:xSrcPos;
begin
  Sphere:=TStratoSphere.Create(Store,Source);
  if FInlineErrors then Source.OnError:=Sphere.InlineError;

  //namespace
  if Source.IsNext([stIdentifier]) then
   begin
    SrcPos:=Source.SrcPos;
    b:=LookUpNameSpace(ns,nx);
{
    while Source.IsNext([stAt]) do
      case Source.Token of
        stNumericLiteral:
          Sphere.MarkIndex(ParseInteger(string(Source.GetID(SrcPos1))))
        stIdentifier:
          if not LookUpNameSpace(module,nx,SrcPos1) then
            module:=Sphere.Add(nNameSpace,?,SrcPos1,[vName,nx]);
        else
          Source.Error('unknown namespace load modifier syntax');
      end;
}
    if not b then
     begin
      //create namespace
      p:=Sphere.Add(nNameSpace,ns,SrcPos,[vName,nx]);
      if ns=0 then
        Sphere.Append(Sphere.SourceFile,fSourceFile_NameSpaces,p)
      else
        Sphere.Append(ns,fItems,p);
      ns:=p;
     end;
   end
  else
   begin
    //default: use file name
    nn:=UTF8String(ChangeFileExt(ExtractFileName(Source.FilePath),''));
      //(''''+StringReplace(Source.FilePath,'''','''''',[rfReplaceAll])+'''');?
    nx:=Store.Dict.StrIdx(nn);
    //TODO: proper detect duplicate!
    if not Sphere.Add(Sphere.SourceFile,fSourceFile_NameSpaces,
      nNameSpace,nx,0,Source.SrcPos,[],ns) then
      Source.Error('duplicate namespace "'+string(nn)+'"');
   end;
  FNameSpace:=ns;
  SetLength(Locals,3);
  Locals[0]:=ns;
  Locals[1]:=0;//see stHRule:ttPrivate
  //runtime from first module
  p:=xItem(-1);
  if Store.NextModule(p) then Sphere.First(p,fSourceFile_NameSpaces,p,p0);
  if p=ns then p:=0;
  Locals[2]:=p;
end;

procedure TStratoParser.ParseDeclaration;
var
  nx:xName;
  nn,fqn:UTF8String;
  ns,p,q,r,r0:xItem;
  st:TStratoToken;
  i:cardinal;
  SrcPos:xSrcPos;
begin
  while (cb=0) and (rd=0) and Source.NextToken(st) do
  case st of

    stThreeLT: //import a namespace
      ParseImport;

    stIdentifier: //declaration
     begin
      //lookup
      p:=0;
      q:=0;
      ID(nx,nn,SrcPos);
      fqn:=nn;
      ns:=Locals[1];
      if ns=0 then ns:=Locals[0];
      while Source.IsNext([stPeriod,stIdentifier]) do
       begin
        LookUpNext(nx,nn,p,ns,q);
        Source.Token;//stIdentifier
        ID(nx,nn,SrcPos);
        fqn:=fqn+'.'+nn;
       end;
      if q<>0 then Source.Error('unknown namespace "'+string(fqn)+'"');

      //operator override?
      if Source.IsNext([stPeriod,stStringLiteral]) then
       begin
        LookUpNext(nx,nn,p,ns,q);
        Source.Token;//stStringLiteral
        //ID(n,nn); but with GetStr:
        nn:=Source.GetStr;
        nx:=Store.Dict.StrIdx(nn);
       end;

      st:=Source.Token;
      case st of

        stColon:
         begin
          q:=LookUpType('type');
          //property
          if Source.IsNext([stCOpen]) then
           begin
            r:=StratoFnAdd(Sphere,Source,nPropertyGet,
              Fn(ns,nx,nn,SrcPos),
              Sphere.Add(nSignature,ns,SrcPos,
                [fSubject,p
                ,vName,nx//+'_get'?
                ,fReturnType,q
                ]),SrcPos);
            if Source.IsNext([stCClose]) then
             begin
              //forward only
              if Source.IsNext([stCOpen,stCClose]) then //empty setter also? skip
                Source.Token;//stCClose
              //TODO: check declared somewhere later
             end
            else
              Sphere.n(r,fBody)^:=CbStart(
                StratoFnCodeBlock(Sphere,r,ns,q,nx,Source.SrcPos));
            p:=0;
           end
          else
          //class
          if Source.IsNext([stDefine,stCOpen]) then
           begin
            if q=0 then
              Source.Error('undeclared base class')
            else
            if Sphere.n(q,vTypeNr)^<>nClass then
              Source.Error('base class must be a class');
            if not Sphere.Add(ns,fItems,nClass,nx,ns,SrcPos,[fInheritsFrom,q],p) then
              Source.Error('duplicate identifier "'+nn+'"');
            if q<>0 then inc(Sphere.n(p,vByteSize)^,Sphere.n(q,vByteSize)^);
            Source.Token;//stCOpen
            rd:=p;//switch to ParseRecord
           end
          else
          //variable
           begin
            if not Sphere.Add(ns,fItems,nVarDecl,nx,ns,SrcPos,[fTypeDecl,q],p) then
              Source.Error('duplicate identifier "'+nn+'"');
            if Source.IsNext([stDefine]) then
              Sphere.n(p,fValue)^:=ParseLiteral(Source.Token,nil);
            //TODO: check InitialValue.EvaluatesTo with EvaluatesTo
            if ns<>0 then
              case Sphere.n(ns,vTypeNr)^ of
                nNameSpace:
                  q:=Sphere.Add(Sphere.SourceFile,fSourceFile_Globals,
                    nGlobal,ns,SrcPos,[fVarDecl,p,vBytesize,ByteSize(Sphere,p)]);
                nClass,nRecord:
                  inc(Sphere.n(ns,vByteSize)^,ByteSize(Sphere,q));
                  //TODO: support @ offset
                else
                  Source.Error('unexpected variable parent');
              end;
           end;
          if cb=0 then Source.Skip(stSemiColon);
         end;

        stDefine://type, constant or enum
          if Source.IsNext([stPOpen,stIdentifier]) then
           begin
            if not Sphere.Add(ns,fItems,nEnumeration,nx,ns,SrcPos,[],p) then
              Source.Error('duplicate identifier "'+nn+'"');
            ParseEnumeration(p);
           end
          else
           begin
            //type or constant declaration
            st:=Source.Token;
            case st of
              stIdentifier:
               begin
                p:=LookUpID;
                if p=0 then
                  Source.Error('unknown type or constant')
                else
                  case Sphere.n(p,vTypeNr)^ of
                    nVarDecl:
                      if Sphere.n(p,fValue)^=0 then
                        Source.Error('var without initial value')
                      else
                       begin
                        if not Sphere.Add(ns,fItems,nConstant,nx,ns,SrcPos,
                          [fValue,Sphere.n(p,fValue)^
                          ,fTypeDecl,Sphere.n(p,fTypeDecl)^
                          ],q) then
                          Source.Error('duplicate identifier "'+nn+'"');
                       end;
                    nConstant:
                      if Sphere.n(p,fValue)^=0 then
                        Source.Error('constant without value')
                      else
                       begin
                        if not Sphere.Add(ns,fItems,nConstant,nx,ns,SrcPos,
                          [fValue,Sphere.n(p,fValue)^
                          ,fTypeDecl,Sphere.n(Sphere.n(p,fValue)^,fTypeDecl)^
                          ],q) then
                          Source.Error('duplicate identifier "'+nn+'"');
                       end;
                    nLiteral:
                      if not Sphere.Add(ns,fItems,nConstant,nx,ns,SrcPos,
                        [fValue,p
                        ,fTypeDecl,Sphere.n(p,fTypeDecl)^
                        ],q) then
                        Source.Error('duplicate identifier "'+nn+'"');
                    nTypeDecl,nRecord,nEnumeration:
                      if Source.IsNext([stBOpen]) then //array
                        if Source.IsNext([stBClose]) then //dyn array
                         begin
                          Source.Error('//TODO: dyn arrays');
                         end
                        else
                         begin
                          ParseLiteral(Source.Token,@i);
                          //TODO: multidimensional arrays, array of array
                          Source.Skip(stBClose);//TODO: force
                          if not Sphere.Add(ns,fItems,nArray,nx,ns,SrcPos,[fTypeDecl,p],q) then
                            Source.Error('duplicate identifier "'+nn+'"');
                          if p<>0 then
                            Sphere.n(p,vByteSize)^:=ByteSize(Sphere,p)*i;
                         end
                      else //type alias
                        if not Sphere.Add(ns,fItems,nTypeAlias,nx,ns,SrcPos,[fTypeDecl,p],q) then
                          Source.Error('duplicate identifier "'+nn+'"');
                    else
                      Source.Error('unsupported type or constant reference');
                  end;
               end;
              stStringLiteral,stNumericLiteral,
              stBOpen,stPOpen,stOpSizeOf://constant
               begin
                q:=ParseLiteral(st,nil);
                if not Sphere.Add(ns,fItems,nConstant,nx,ns,SrcPos,[fValue,q],p) then
                  Source.Error('duplicate identifier "'+nn+'"');
                if q<>0 then
                 begin
                  if Source.IsNext([stColon]) then //here or in ParseLiteral?
                    Sphere.n(q,fTypeDecl)^:=LookUpType('literal type');
                  Sphere.n(p,fTypeDecl)^:=Sphere.n(q,fTypeDecl)^;
                 end;
               end;
              stCOpen://record (aka struct)
               begin
                if not Sphere.Add(ns,fItems,nRecord,nx,ns,SrcPos,[],p) then
                  Source.Error('duplicate identifier "'+nn+'"');
                rd:=p;//switch to ParseRecord
               end;
              stCaret:
                if not Sphere.Add(ns,fItems,nPointer,nx,ns,SrcPos,
                  [fSubject,LookUpType('pointer type')],p) then
                  Source.Error('duplicate identifier "'+nn+'"');
              stQuestionMark:
               begin
                p:=LookUpType('class reference type');
                if (p<>0) and (Sphere.n(p,vTypeNr)^<>nClass) then
                  Source.Error('invalid class reference subject');
                if not Sphere.Add(ns,fItems,nClassRef,nx,ns,SrcPos,[fSubject,p],q) then
                  Source.Error('duplicate identifier "'+nn+'"');
               end;
{
              stOpSizeOf:
               begin
                if not Sphere.Add(ns,fItems,nConstant,n,ns,SrcPos,[fTypeDecl,TypeDecl_Number],p) then
                  Source.Error('duplicate identifier "'+nn+'"');
                if Source.IsNext([stNumericLiteral]) then
                 begin
                  q:=TypeDecl_number;
                  Source.GetID;//skip
                 end
                else
                  q:=LookUpType('sizeof type');
                if q=0 then
                  Source.Error('unknown sizeof type')
                else
                 begin
                  i:=ByteSize(Sphere,q);
                  //TODO: ttSizeOf?
                  Sphere.n(p,fValue)^:=Sphere.Add(nLiteral,0,SrcPos,
                    [fTypeDecl,TypeDecl_number
                    ,fValue,Sphere.AddBinaryData(IntToStr(i))
                    ]);
                 end;
               end;
}
              else
                Source.Error('unsupported type or constant');
             end;
           end;

        stPOpen://parameter list
         begin
          p:=ParseSignature(ns,nn,stPClose,SrcPos);
          //TODO: detect Source.IsNext([stSemiColon,stAOpen])?
          case Source.Token of
            stSemiColon:
             begin
              q:=Sphere.Lookup(ns,nx);
              if q=0 then //just a signature? add to namespace
                Sphere.Append(ns,fItems,p)
              else
                case Sphere.n(q,vTypeNr)^ of
                  nMember:
                    StratoFnAdd(Sphere,Source,nOverload,q,p,SrcPos);
                  nSignature:
                   begin
                    //another forward signature? create ttMember here
                    r:=Sphere.Add(nMember,ns,SrcPos,[vName,nx]);
                    ReplaceNode(Sphere,ns,q,r);
                    StratoFnAdd(Sphere,Source,nOverload,r,q,Sphere.n(q,vSrcPos)^);
                    StratoFnAdd(Sphere,Source,nOverload,r,p,SrcPos);
                   end
                  else
                    Source.Error('duplicate identifier "'+nn+'"');
                end;
             end;
            stCOpen://code block
             begin
              q:=Sphere.Lookup(ns,nx);
              if q=0 then
                p:=StratoFnAdd(Sphere,Source,nOverload,
                  Fn(ns,nx,nn,SrcPos),p,SrcPos)
              else
                case Sphere.n(q,vTypeNr)^ of
                  nMember:
                    p:=StratoFnAdd(Sphere,Source,nOverload,q,p,SrcPos);
                  nSignature://signature forwarded, replace with ttMember
                   begin
                    r:=Fn(ns,nx,nn,SrcPos);
                    ReplaceNode(Sphere,ns,q,r);
                    //StratoFnAdd checks for SameType(p,q):
                    StratoFnAdd(Sphere,Source,nOverload,r,q,Sphere.n(q,vSrcPos)^);
                    p:=StratoFnAdd(Sphere,Source,nOverload,r,p,SrcPos);
                   end;
                  nClass://constructor
                   begin
                    //p.TypeDecl:=q;
                    Sphere.n(p,fSubject)^:=q;
                    p:=StratoFnAdd(Sphere,Source,nConstructor,q,p,SrcPos);
                   end;
                  else
                   begin
                    Source.Error('duplicate identifier "'+nn+'"');
                    q:=Sphere.Add(nMember,ns,SrcPos,[vName,nx]);
                    p:=StratoFnAdd(Sphere,Source,nOverload,q,p,SrcPos);
                   end;
                end;
              if p<>0 then CbStart(StratoFnOvlCodeBlock(Sphere,Source,p));
              p:=0;
             end;
            else Source.Error('unsupported signature syntax');
          end;
         end;

        stOpAssign://":="
          if Source.IsNext([stCOpen]) then
           begin
            //accept only one object:={}
            if not Sphere.Add(ns,fItems,nClass,nx,ns,SrcPos,[],p) then
              Source.Error('duplicate identifier');
            if TypeDecl_object=0 then
              TypeDecl_object:=p //TODO: store somewhere FBlock[0]?
            else
              Source.Error('only one master base class allowed');
            rd:=p;//switch to ParseRecord
           end
          else
            Source.Error('unsupported declaration syntax');

        stBOpen://"["
         begin
          q:=StratoFnAdd(Sphere,Source,nPropertyGet,Fn(ns,nx,nn,SrcPos),
            ParseSignature(ns,nn,stBClose,SrcPos),SrcPos);
          if Source.IsNext([stCOpen]) then
            Sphere.n(q,fBody)^:=CbStart(StratoFnOvlCodeBlock(Sphere,Source,q));
         end;

        //stCOpen://"{"
        //?

        else
          Source.Error('unexpected stray identifier');
      end;
     end;

    stStringLiteral,stNumericLiteral:
     begin
      Source.Error('unexpected literal');
      ParseLiteral(st,nil);
     end;

    stQuestionMark: //interface
     begin
      p:=0;
      r:=0;
      ID(nx,nn,SrcPos);
      fqn:=nn;
      ns:=Locals[1];
      if ns=0 then ns:=Locals[0];
      while Source.IsNext([stPeriod,stIdentifier]) do
       begin
        LookUpNext(nx,nn,p,xItem(ns),r);
        Source.Token;//stIdentifier
        ID(nx,nn,SrcPos);
        fqn:=fqn+'.'+nn;
       end;
      if r<>0 then Source.Error('unknown namespace "'+string(fqn)+'"');

      st:=Source.Token;
      case st of
        stPOpen:
          if Source.IsNextID([stPClose,stCOpen]) or
            Source.IsNextID([stPClose,stDefine,stCOpen]) then
           begin //inherit this interface
            q:=LookUpID;
            if q=0 then
              Source.Error('undeclared base interface');
            Source.Skip(stPClose);
            Source.Skip(stDefine);//if?
            Source.Skip(stCOpen);
            if not Sphere.Add(ns,fItems,nInterface,nx,ns,Source.SrcPos,[fInheritsFrom,q],p) then
              Source.Error('duplicate identifier');
            ParseInterfaceDecl(p);
           end;
        stCOpen:
         begin
          if not Sphere.Add(ns,fItems,nInterface,nx,ns,Source.SrcPos,[],p) then
            Source.Error('duplicate identifier "'+nn+'"');
          ParseInterfaceDecl(p);
         end;
        else
          Source.Error('unsupported interface syntax');
      end;
     end;

    stOpSub,stTilde://'-','~': destructor?
      if Source.IsNextID([stPOpen,stPClose,stCOpen]) then
       begin
        //lookup
        p:=0;
        r:=0;
        ID(nx,nn,SrcPos);
        fqn:=nn;
        ns:=Locals[1];
        if ns=0 then ns:=Locals[0];
        while Source.IsNext([stPeriod,stIdentifier]) do
         begin
          LookUpNext(nx,nn,p,xItem(ns),r);
          Source.Token;//stIdentifier
          ID(nx,nn,SrcPos);
          fqn:=fqn+'.'+nn;
         end;
        if r<>0 then Source.Error('unknown namespace "'+string(fqn)+'"');
        //ParseSignature? destructor doesn't have arguments/overloads
        Source.Skip(stPOpen);
        Source.Skip(stPClose);
        Source.Skip(stCOpen);
        //find class destructor is for
        q:=Sphere.Lookup(ns,nx);
        if q=0 then
         begin
          Source.Error('destructor for unknown class');
          q:=Sphere.Add(nClass,0,0,[vName,nx]);
         end
        else
          if Sphere.n(q,vTypeNr)^<>nClass then
           begin
            Source.Error('destructor only supported on class');
            q:=Sphere.Add(nClass,0,0,[vName,nx]);
           end;
        //check any destructor already
        Sphere.First(q,fItems,r,r0);
        while (r<>0) and (Sphere.n(r,vTypeNr)^<>nDestructor) do Sphere.Next(r,r0);
        if r<>0 then
          Source.Error('duplicate destructor');
        //add
        p:=Sphere.Add(q,fItems,nDestructor,q,SrcPos,
          [fSignature,Sphere.Add(nSignature,q,SrcPos,[vName,nx,fSubject,q])
          ]);
        Sphere.n(p,fBody)^:=CbStart(
          StratoFnCodeBlock(Sphere,p,q,0,0,Source.SrcPos));
        p:=0;
       end
      else
        Source.Error('unexpected token');

    stCOpen:
     begin
      SrcPos:=Source.SrcPos;
      ns:=Locals[0];
      cb:=Sphere.Add(nCodeBlock,ns,SrcPos,[]);
      with Store.SourceFile(cb)^ do
        if InitializationBlock=0 then
          InitializationBlock:=cb
        else
        if FinalizationBlock=0 then
          FinalizationBlock:=cb
        else Source.Error(
          'Initialization and finalization code already declared.');
      p:=0;
     end;

    stHRule:
      if Locals[1]=0 then
       begin
        ns:=Locals[0];
        Locals[1]:=Sphere.Add(ns,fItems,nPrivate,ns,SrcPos,[]);
       end
      else
        Source.Error('already in private visibility');

    stColon:
      if Source.IsNext([stIdentifier]) then
       begin
        ID(nx,nn,SrcPos);
        p:=Sphere.Add(Locals[0],fItems,nTypeDecl,Locals[0],SrcPos,[vName,nx]);
        if Source.IsNext([stAt,stNumericLiteral]) then
         begin
          Source.Token;//stNumericLiteral
          Sphere.n(p,vByteSize)^:=
            ParseInteger(string(Source.GetID(cardinal(SrcPos))));
         end;
        Source.Skip(stSemiColon);
        p:=0;
       end
      else
        Source.Error('identifier expected');

    stSemiColon://;//stray semicolon? ignore
      if Source.IsNext([stSemiColon,stSemiColon]) then
       begin
        asm int 3 end;//for debugging the parser
        Source.Skip(stSemiColon);
        Source.Skip(stSemiColon);
       end;

    //stPOpen?
    //stThreeColons://TODO: switch/extend namespace?

    st_Unknown:Source.Error('unknown token');
    else Source.Error('unexpected token');
  end;
end;

procedure TStratoParser.ParseRecord;
type
  PCardinal=^cardinal;
var
  p,q,r:xItem;
  offset,i,j,s:cardinal;
  bs:PxValue;
  st:TStratoToken;
  b,neg:boolean;
  fn:UTF8String;
  nx:xName;
  SrcPos,SrcPos1:xSrcPos;
begin
  while (cb=0) and (rd<>0) and Source.NextToken(st) do
  case st of

    stIdentifier:
     begin
      offset:=OffsetUseDefault;//default
      p:=0;//default
      fn:=Source.GetID(cardinal(SrcPos));
      if Source.IsNext([stColon]) then
        if Source.IsNext([stCOpen]) then //TODO move this into LookUpType?
         begin
          p:=Sphere.Add(nRecord,rd,SrcPos,[vName,Store.Dict.StrIdx(fn)]);
          rd:=p;//push? see stCClose below
          //TODO: add struct/typedecl itself to something? x?ns?
         end
        else
          p:=LookUpType('field type');
      //offset
      if Source.IsNext([stAt]) then
       begin
        offset:=0;
        //TODO: replace following with ParseLiteral?
        neg:=false;
        b:=true;
        while b and Source.NextToken(st) do
         begin
          i:=0;//default;
          j:=1;//default;
          case st of
            stNumericLiteral:
              if not TryStrToInt(string(Source.GetID(cardinal(SrcPos1))),
                integer(i)) then
                Source.Error('record field offset not an integer');
            stOpSub:neg:=true;
            stOpAdd:neg:=false;
            stIdentifier:
             begin
              q:=Sphere.Lookup(rd,
                Store.Dict.StrIdx(Source.GetID(cardinal(SrcPos1))));
              if q=0 then
                Source.Error('record field not found')
              else
                i:=Sphere.n(q,vOffset)^;
             end;
            stOpSizeOf:
              if Source.IsNext([stNumericLiteral]) then
               begin
                Source.Skip(st);//Source.GetID;
                i:=SystemWordSize;//ByteSize(Sphere,TypeDecl_number);
               end
              else
                i:=ByteSize(Sphere,LookUpType('offset type'));
            //stSemiColon:b:=false; else Source.Error?
            else b:=false;
          end;
          if Source.IsNext([stOpMul,stNumericLiteral]) then
           begin
            Source.Token;//stNumericLiteral
            if TryStrToInt(string(Source.GetID(cardinal(SrcPos1))),integer(j)) then
              i:=i*j
            else
              Source.Error('record field offset factor not an integer');
           end;
          if i<>0 then
            if neg then
             begin
              dec(offset,i);
              neg:=false;
             end
            else
              inc(offset,i);
         end;
       end;
      //TODO: else if Source.IsNext([stCOpen]) then tt:=ttProperty?
      Source.Skip(stSemiColon);

      //register field with record
      nx:=Store.Dict.StrIdx(fn);
      if not Sphere.Add(rd,fItems,nVarDecl,nx,rd,SrcPos,[fTypeDecl,p],r) then
        Source.Error('duplicate record field "'+fn+'"');
      if p=0 then s:=0 else s:=ByteSize(Sphere,p);
      bs:=Sphere.n(rd,vByteSize);
      if offset=OffsetUseDefault then
       begin
        Sphere.n(r,vOffset)^:=bs^;
        inc(bs^,s);
       end
      else
       begin
        if integer(offset)<0 then
         begin
          if rd<>TypeDecl_object then
           begin
            offset:=-integer(offset);
            Source.Error('negative record field offset not allowed');
           end;
         end
        else
         begin
          i:=offset+s;
          if i>bs^ then bs^:=i;
         end;
        Sphere.n(r,vOffset)^:=offset;
       end;
     end;

    //stQuestionMark: nested interface?
    //more?

    stCClose:
     begin
      //'pop'
      p:=Sphere.n(rd,fParent)^;
      if Sphere.n(p,vTypeNr)^=nRecord then rd:=p else rd:=0;
     end;

    stPOpen:
      Source.Error('unsupported record field syntax, declare methods outside of data section');

    else Source.Error('unsupported record field syntax');
  end;
end;

procedure TStratoParser.ParseLogic;
var
  nx:xName;
  nn,fqn:UTF8String;
  p,q,r:xItem;
  st:TStratoToken;
  i,j:cardinal;
  SrcPos:xSrcPos;
  bs:PxValue;
begin
  SrcPos:=Source.SrcPos;//default
  while (cb<>0) and Source.NextToken(st) do
  case st of

    stIdentifier:
     begin
      Juxta(p);
      ID(nx,nn,SrcPos);
      fqn:=nn;
      CodeLookup(nx,p,SrcPos);
      while Source.IsNext([stPeriod,stIdentifier]) do
       begin
        Source.Token;//stIdentifier
        ID(nx,nn,SrcPos);
        fqn:=fqn+'.'+nn;
        CodeLookup(nx,p,SrcPos);
       end;
      if p=0 then
       begin
        Source.Error('undeclared identifier "'+string(fqn)+'"');
        //TODO: silence further errors
        p:=Sphere.Add(nNameSpace,0,SrcPos,
          [vName,Store.Dict.StrIdx('!!!'+nn)//nx
          ]);//silence further errors
       end;
      Push(pUnresolved,p,0,SrcPos);
     end;

    stPeriod://"."
      if p=0 then
        Source.Error('unexpected "."')
      else
        if Source.IsNext([stIdentifier]) then
         begin
          ID(nx,nn,SrcPos);
          fqn:=Format('$%.8x.%s',[p,nn]);
          CodeLookup(nx,p,SrcPos);
          while Source.IsNext([stPeriod,stIdentifier]) do
           begin
            Source.Token;//stIdentifier
            ID(nx,nn,SrcPos);
            fqn:=fqn+'.'+nn;
            CodeLookup(nx,p,SrcPos);
           end;
          if p=0 then
           begin
            Source.Error('undeclared identifier "'+string(fqn)+'"');
            //TODO: silence further errors
            p:=Sphere.Add(nNameSpace,0,SrcPos,
              [vName,Store.Dict.StrIdx('!!!'+nn)//nx
              ]);//silence further errors
           end;
          Push(pUnresolved,p,0,SrcPos);
         end
        else
          Source.Error('unsupported syntax');

    stStringLiteral:
     begin
      Juxta(p);
      p:=Sphere.Add(nLiteral,cb,Source.SrcPos,[fTypeDecl,TypeDecl_string]);
      if (stackIndex<>0) and ((stack[stackIndex-1].pp=pIfThen)
        or (stack[stackIndex-1].pp=pIfElse)) then
        Sphere.n(p,fValue)^:=Sphere.AddBinaryData(Source.GetStr)
      else
        Sphere.n(p,fValue)^:=Sphere.AddBinaryData(Source.GetStrs);
     end;
    stNumericLiteral:
     begin
      Juxta(p);
      p:=Sphere.Add(nLiteral,cb,Source.SrcPos,
        [fValue,Sphere.AddBinaryData(Source.GetID(cardinal(SrcPos)))]);
      if Source.IsNext([stColon]) then
        q:=LookUpType('literal type',true)
      else
        q:=TypeDecl_number;
      Sphere.n(p,fTypeDecl)^:=q;
      if q=0 then Source.Error('unknown literal type');
     end;

    stColon:
      if p=0 then
        if not(Source.IsNext([stIdentifier,stPeriod]))
          and Source.IsNext([stIdentifier]) then
         begin
          //new local variable(s)
          ID(nx,nn,SrcPos);
          if not Sphere.Add(cb,fVarDecls,nVarDecl,nx,cb,SrcPos,
            [vOffset,Sphere.n(cb,vByteSize)^],p) then
            Source.Error('duplicate identifier "'+string(nn)+'"');
          Push(pUnTypedVar,p,0,SrcPos);
         end
        else
          Source.Error('no value to cast')
      else
       begin
        //Combine(p?,p);
        if (stackIndex<>0) and (stack[stackIndex-1].pp=pUnTypedVar) then //local variable(s) type decl?
         begin
          p:=0;
          q:=LookUpType('type',true);
          i:=stackIndex;
          while (i<>0) and (stack[i-1].pp=pUnTypedVar) do dec(i);
          if stackIndex<>i then
           begin
            j:=i;
            while i<stackIndex do
             begin
              r:=stack[i].p1;//assert nVarDecl
              if p=0 then p:=r;
              Sphere.n(r,fTypeDecl)^:=q;//assert was 0
              if q<>0 then
               begin
                bs:=Sphere.n(cb,vByteSize);
                Sphere.n(r,vOffset)^:=bs^;
                inc(bs^,ByteSize(Sphere,q));
               end;
              {$IFDEF DEBUG}
              stack[i].pp:=p___;
              stack[i].p1:=0;
              stack[i].p2:=0;
              stack[i].SrcPos:=0;
              {$ENDIF}
              inc(i);
             end;
            stackIndex:=j;
           end;
          if (p<>0) and Source.IsNext([stSemiColon]) then
            p:=0;//don't add as statement
         end
        else //cast
         begin
          Combine(p_Cast,p);//p_juxta?
          if ResType(Sphere,p)=0 then
            Source.Error(Format('can''t cast value "%s" %s',[Sphere.FQN(p),xDisplay(Sphere,p)]));
          //TODO: check ByteSize's equal?
          p:=Sphere.Add(nCast,cb,Source.SrcPos,[fSubject,p,fTypeDecl,LookUpType('cast type',true)]);
          //TODO: PushBinary?
         end;
       end;

    stPOpen:
      if p=0 then
        Push(pParentheses,0,0,Source.SrcPos)
      else
       begin
        if (stackIndex<>0) and (stack[stackIndex-1].pp=pUnresolved) then
         begin
          stack[stackIndex-1].pp:=pArgList;
          stack[stackIndex-1].p1:=p;
          stack[stackIndex-1].SrcPos:=Source.SrcPos;
         end
        else
          Push(pArgList,p,0,Source.SrcPos);//could be pIfThen later!
        p:=0;
       end;
    stPClose:
      if stackIndex=0 then
        Source.Error('unsupported syntax')
      else
        Combine(pParentheses,p);

    stComma:
     begin
      Combine(p_ArgList_Item,p);
      q:=0;
      if (stackIndex=0) or (p=0) then
        Source.Error('unsupported syntax')
      else
        case stack[stackIndex-1].pp of
          pArgList,pBrackets:
            StratoFnCallAddArgument(Sphere,stack[stackIndex-1].p2,p,Source.SrcPos);
          pUnTypedVar:
            if p=stack[stackIndex-1].p1 then
              if not(Source.IsNext([stIdentifier,stPeriod]))
                and Source.IsNext([stIdentifier]) then
               begin
                //new local variable(s)
                ID(nx,nn,SrcPos);
                if not Sphere.Add(cb,fVarDecls,nVarDecl,nx,cb,SrcPos,
                  [vOffset,Sphere.n(cb,vByteSize)^],q) then
                  Source.Error('duplicate identifier "'+string(nn)+'"');
                Push(pUnTypedVar,q,0,SrcPos);
               end
              else
                Source.Error('unsupported syntax')
            else
              if (Sphere.n(p,vTypeNr)^=nAssign) and
                (TStratoToken(Sphere.n(p,vOperator)^)=stOpAssign) then
                CbAdd(p)
              else
                Source.Error('unexpected syntax in local variable declaration');
          else
            Source.Error('unsupported syntax');
        end;
      p:=q;
     end;

    stCOpen://"{"
      if Source.IsNext([stIdentifier,stColon]) or
        Source.IsNext([stStringLiteral,stColon]) or
        Source.IsNext([stNumericLiteral,stColon]) then
       begin
        //TODO: JSON
        Source.Error('//TODO:JSON');
       end
      else
       begin
        Juxta(p);
        //start code block
        SrcPos:=Source.SrcPos;
        Push(pCodeBlock,cb,0,SrcPos);
        cb:=Sphere.Add(nCodeBlock,cb,SrcPos,[]);

        //iteration block?
        if (stackIndex<>0) and (stack[stackIndex-1].pp=pForBody) then
         begin
          q:=Sphere.n(stack[stackIndex-1].p1,fDoFirst)^;
          if q<>0 then
           begin
            Sphere.n(cb,fVarDecls)^:=Sphere.n(q,fVarDecls)^;
            Sphere.n(q,fVarDecls)^:=0;
            //TODO: convert to read-only?
           end;
         end;

        p:=0;
       end;
    stCClose://"}"
     begin
      Combine(p_Statement,p);
      if p<>0 then
       begin
        //resulting value
        q:=Sphere.n(cb,fParent)^;
        case Sphere.n(q,vTypeNr)^ of
          nOverload,nPropertyGet:
           begin
            r:=Sphere.n(Sphere.n(q,fSignature)^,fReturnType)^;
            if not SameType(Sphere,ResType(Sphere,p),r) then
              Source.Error('result value type mismatch');
            //assign to result value
            nx:=Sphere.n(q,vName)^;
            p:=Sphere.Add(nAssign,cb,Source.SrcPos,
              [vOperator,xValue(stOpAssign)
              ,fTarget,Sphere.Lookup(p,nx)//nMember
              ,fValue,p
              ]);
           end
          else
           begin
            CheckPassed(p);
            //??? p.TypeDecl:=ResType(Sphere,p);
           end;
        end;
        CbAdd(p);
       end;
      while (stackIndex<>0) and (stack[stackIndex-1].pp<>pCodeBlock) do
       begin
        dec(stackIndex);
        if stack[stackIndex].pp=pIfElse then
          Source.Error('if-then without else')
        else
          Source.Error('unexpected incomplete syntax');//+??[stack[stackIndex].p]);
        {$IFDEF DEBUG}
        stack[stackIndex].pp:=p___;
        stack[stackIndex].p1:=0;
        stack[stackIndex].p2:=0;
        stack[stackIndex].SrcPos:=0;
        {$ENDIF}
       end;
      p:=cb;//keep a copy
      if stackIndex=0 then
       begin
        //return to declarations
        cb:=0;//clear here for any CbStart below

        //code block done: checks
        r:=Sphere.n(p,fParent)^;
        if r<>0 then
         begin
          //constructor block done? check inherited called
          if (Sphere.n(r,vTypeNr)^=nConstructor) and not(cbInhCalled)
            and (Sphere.n(Sphere.n(r,fParent)^,fParent)^<>TypeDecl_object) then
           begin
            q:=StratoFnCallBySignature(Sphere,nConstructor,
              Sphere.n(Sphere.n(Sphere.n(r,fParent)^,fParent)^,fInheritsFrom)^,
              Sphere.n(Sphere.n(r,fSignature)^,fArguments)^,
              p,Sphere.n(p,vSrcPos)^);
            if (q=0) and (Sphere.n(r,fFirstArgVar)^<>0) then
              //try again for inherited constructor without arguments
              q:=StratoFnCallBySignature(Sphere,nConstructor,
                Sphere.n(Sphere.n(Sphere.n(r,fParent)^,fParent)^,fInheritsFrom)^,
                0,p,Sphere.n(p,vSrcPos)^);
            if q=0 then
              Source.Error('unable to find base constructor')
            else
             begin
              //arguments
              StratoFnArgByValues(Sphere,q,r,Sphere.n(p,fParent)^);
              //insert first into code block
              Sphere.Prepend(p,fItems,q);
             end;
           end
          else

          //destructor block done? check inherited called
          if (Sphere.n(r,vTypeNr)^=nDestructor) and not(cbInhCalled)
            and (Sphere.n(r,fParent)^<>TypeDecl_object) then
           begin
            q:=StratoFnCallBySignature(Sphere,nDestructor,
              Sphere.n(Sphere.n(r,fParent)^,fInheritsFrom)^,
              0,p,Source.SrcPos);
            if q=0 then
              Source.Error('unable to find base destructor')
            else
              Sphere.Append(p,fItems,q);
           end
          else

          //property getter done? parse setter
          if Sphere.n(r,vTypeNr)^=nPropertyGet then
           begin
            //TODO: if not(cbInhCalled)
            if Source.IsNext([stCOpen]) then
             begin
              //TODO: construct setter signature? (use the same for now)
              q:=StratoFnAdd(Sphere,Source,nPropertySet,
                Sphere.n(r,fParent)^,Sphere.n(r,fSignature)^,
                Source.SrcPos);//r.Parent.SrcPos?
              CbStart(StratoFnOvlCodeBlock(Sphere,Source,q));
             end
            else
              Source.Skip(stSemiColon);//closing property declaration
           end
          else

          //TODO: ttPropertySet and not cbInhCalled
          //if (rx.ThingType=ttPropertySet) and not(cbInhCalled) then
          //else

          ;

         end;
       end
      else
       begin
        //pop from stack
        dec(stackIndex);
        //assert stack[stackIndex].p=pCodeBlock
        cb:=stack[stackIndex].p1;
        {$IFDEF DEBUG}
        stack[stackIndex].pp:=p___;
        stack[stackIndex].p1:=0;
        stack[stackIndex].p2:=0;
        stack[stackIndex].SrcPos:=0;
        {$ENDIF}

        Combine(p_CodeBlockDone,p);

        //detect within "&({}{})" syntax
        if (stackIndex<>0) and (stack[stackIndex-1].pp=pForCritPara) then
         begin
          q:=stack[stackIndex-1].p1;
          if Sphere.n(q,fDoFirst)^=0 then
            Sphere.n(q,fDoFirst)^:=p
          else
          if Sphere.n(q,fDoThen)^=0 then
            Sphere.n(q,fDoThen)^:=p
          else
            Source.Error('unexpected iteration syntax');
         end
        else

        //add to parent chain
        if (p<>0) and (ResType(Sphere,p)=0) then CbAdd(p);
       end;
      p:=0;
     end;
    stSemiColon:
     begin
      Combine(p_Statement,p);
      if p<>0 then
       begin
        CheckPassed(p);
        CbAdd(p);
       end;
      p:=0;
     end;

    stDefine:
     begin
      Combine(pAssign,p);
      Source.Error('use either ":=" or "=="');
      p:=0;
     end;

    stOpAssign,
    stOpAssignAdd,stOpAssignSub,
    stOpAssignMul,stOpAssignDiv,stOpAssignMod,
    stOpAssignOr,stOpAssignAnd:
     begin
      Combine(pAssign,p);
      if p=0 then
        Source.Error('no left side defined for assignment')
      else
       begin
        Push(pAssign,p,xValue(st),Source.SrcPos);
        p:=0;
       end;
     end;
    stOpEQ,stOpNEQ:
      PushBinary(pEqual,st,p);
    stOpLT,stOpLTE,stOpGT,stOpGTE,stOpTypeIs:
      PushBinary(pComparative,st,p);
    //TODO: stOpLT: if not Combine(pComparative,p) then support inline HTML?
    stOpAdd:
     begin
      Combine(pAddSub,p);
      if p=0 then //unary operator
        Push(pUnary,0,xValue(st),Source.SrcPos)
      else
        PushBinary(pAddSub,st,p);
     end;
    stOpSub:
     begin
      Combine(pAddSub,p);
      if p=0 then
        if Source.IsNext([stAtAt,stPOpen,stPClose]) then
          p:=StratoFnCallDestructor(Sphere,Source,cb)
        else
          Push(pUnary,0,xValue(st),Source.SrcPos)
      else
        PushBinary(pAddSub,st,p);
     end;
    stOpMul,stOpDiv,stOpMod:
      PushBinary(pMulDiv,st,p);
    stOpShl,stOpShr,stThreeLT://stThreeGT: see below
      PushBinary(pShift,st,p);
    stOpAnd:
      if SameType(Sphere,ResType(Sphere,p),TypeDecl_bool) then
        PushBinary(pLogicalAnd,st,p)
      else
        PushBinary(pBitwiseAnd,st,p);
    stOpOr:
      if SameType(Sphere,ResType(Sphere,p),TypeDecl_bool) then
        PushBinary(pLogicalOr,st,p)
      else
        PushBinary(pBitwiseOr,st,p);
    stOpXor:
      if SameType(Sphere,ResType(Sphere,p),TypeDecl_bool) then
        PushBinary(pLogicalXor,st,p)
      else
        PushBinary(pBitwiseXor,st,p);
    stOpNot:
      Push(pUnary,0,xValue(st),Source.SrcPos);
    stTilde:
      if Source.IsNext([stAtAt,stPOpen,stPClose]) then
        p:=StratoFnCallDestructor(Sphere,Source,cb)
      else
        Push(pUnary,0,xValue(st),Source.SrcPos);

    stOpInc,stOpDec:
      if p=0 then
        Source.Error('increment/decrement operators only allowed as suffix')
      else
       begin
        Combine(pUnresolved,p);
        p:=Sphere.Add(nUnaryOp,cb,Source.SrcPos,
          [vOperator,xValue(st)
          ,fReturnType,ResType(Sphere,p)
          ,fRight,p
          ]);
       end;

    stAtAt://"@@": this
     begin
      Juxta(p);
      //see also StratoFnAddOverload
      q:=cb;
      p:=0;
      while q<>0 do
       begin
        //p:=xxLookup(q,Store.Dict.StrIdx('@@'));
        p:=Sphere.n(Sphere.n(q,fVarDecls)^,fNext)^;
        if Sphere.n(p,vTypeNr)^=nThis then
          q:=0
        else
         begin
          p:=0;
          q:=Sphere.n(q,fParent)^;
          if Sphere.n(q,vTypeNr)^<>nCodeBlock then q:=0;
         end;
       end;
      if p=0 then
       begin
        Source.Error('"@@" undefined');
        p:=Sphere.Add(nThis,cb,Source.SrcPos,//add anyway to avoid further errors
          [vName,Store.Dict.StrIdx('@@')]);
       end;
     end;
    stTwoWhats://"??": result value
     begin
      Juxta(p);
      q:=cb;
      r:=0;
      while (q<>0) and (Sphere.n(q,vTypeNr)^=nCodeBlock) do
       begin
        r:=q;
        q:=Sphere.n(q,fParent)^;
       end;
      if q=0 then p:=0 else
        case Sphere.n(q,vTypeNr)^ of
          nOverload,nPropertyGet,nPropertySet:
            p:=Sphere.Lookup(r,Sphere.n(Sphere.n(q,fParent)^,vName)^);//nMember
          else p:=0;
        end;
      if p=0 then
       begin
        Source.Error('"??" undefined');
        p:=Sphere.Add(nVarDecl,cb,Source.SrcPos,//add anyway to avoid further errors
          [vName,Store.Dict.StrIdx('??')]);
       end;
     end;

    stAmpersand://"&": iteration
     begin
      Combine(p_Statement,p);//?
      SrcPos:=Source.SrcPos;
      if Source.IsNext([stPOpen]) then //&(){}
        Push(pForCritPara,Sphere.Add(nIteration,cb,SrcPos,[]),0,SrcPos)
      else
      if Source.IsNext([stCOpen]) then //&{}()
       begin
        Push(pForBodyFirst,Sphere.Add(nIterPostEval,cb,SrcPos,[]),0,SrcPos);
        //start code block
        SrcPos:=Source.SrcPos;
        Push(pCodeBlock,cb,0,SrcPos);
        cb:=Sphere.Add(nCodeBlock,cb,SrcPos,[]);
       end
      else //"& x y;" or "&x{}" with x:boolean
        Push(pForCritOpen,Sphere.Add(nIteration,cb,SrcPos,[]),0,SrcPos);
      p:=0;
     end;

    stThreeColons://":::"
     begin
      Combine(p_Statement,p);
      Sphere.Add(nTry,cb,Source.SrcPos,[]);
     end;
    stThreeGT://">>>"
     begin
      Combine(pShift,p);
      if p=0 then //defer
       begin
        Combine(p_Statement,p);
        Push(pDefer,0,0,Source.SrcPos);
       end
      else
        PushBinary(pShift,st,p);//roll right
     end;
    stThreeWhats://"???"
     begin
      Combine(p_Statement,p);
      if Source.IsNext([stPOpen,stIdentifier,stColon,stIdentifier]) then
       begin
        q:=Sphere.Add(nCatchNamed,cb,Source.SrcPos,[]);
        Source.Token;//stIdentifier
        ID(nx,nn,SrcPos);
        Sphere.n(q,vExName)^:=nx;
        r:=Sphere.Add(nVarDecl,cb,Source.SrcPos,[vName,nx]);
        Sphere.n(q,fVarDecl)^:=r;
        Source.Token;//stColon
        Source.Token;//stIdentifier
        Sphere.n(r,fTypeDecl)^:=LookUpType('catch filter');
        bs:=SpherE.n(cb,vByteSize);
        Sphere.n(r,vOffset)^:=bs^;
        inc(bs^,SystemWordSize);//??!!
        Source.Skip(stPClose);//TODO: enforce
        //TODO: check type is by reference (or SystemWordSize?)
       end
      else
      if Source.IsNext([stPOpen]) then
       begin
        q:=Sphere.Add(nCatchTypes,cb,Source.SrcPos,[]);
        repeat
          Sphere.Add(q,fItems,nTypeAlias,q,Source.SrcPos,[fSubject,LookUpType('catch filter')]);
        until not Source.IsNext([stComma]);
        Source.Skip(stPClose);
       end
      else
        q:=Sphere.Add(nCatchAll,cb,Source.SrcPos,[]);
      Push(pCatch,q,0,Source.SrcPos);
     end;
    stThreeBangs://"!!!"
     begin
      Juxta(p);
      Push(pThrow,0,0,Source.SrcPos);
     end;

    stBOpen://"["
      if p=0 then
        Source.Error('unsupported syntax')//TODO: range
      else
       begin
        //TODO: check p is of type array?
        Push(pBrackets,p,0,Source.SrcPos);
        p:=0;
       end;
    stBClose://"]"
      if stackIndex=0 then
        Source.Error('unexpected token')
      else
        Combine(pBrackets,p);

    stAt://"@"
     begin
      Juxta(p);
      Push(pAddressOf,0,0,Source.SrcPos);
     end;

    stQuestionMark://"?"
     begin
      if p<>0 then Source.Error('unsupported syntax');//TODO: trinary "x?y:z"?
      p:=0;
      //type-of this in constructor?
      if Source.IsNext([stAtAt]) then
       begin
        q:=cb;
        r:=Sphere.n(q,fParent)^;
        while Sphere.n(r,vTypeNr)^=nCodeBlock do
         begin
          q:=r;
          r:=Sphere.n(r,fParent)^;
         end;
        if Sphere.n(r,vTypeNr)^=nConstructor then
         begin
          p:=Sphere.Lookup(q,Sphere.Store.Dict.StrIdx('?@@'));
          if p=0 then Source.Error('constructor class reference not found');
         end;
       end;
      if p=0 then
        Push(pTypeOf,0,xValue(st),Source.SrcPos);
     end;
    stOpSizeOf://"@?"
     begin
      Combine(pUnresolved,p);
      if p<>0 then Source.Error('unsupported syntax');
      Push(pSizeOf,0,xValue(st),Source.SrcPos);
     end;

    stCaret://"^"
      if p=0 then
        Source.Error('unsupported syntax')
      else
       begin
        r:=ResType(Sphere,p);
        case Sphere.n(r,vTypeNr)^ of
          nPointer://TODO: xSignature? xOverload?
            p:=Sphere.Add(nDereference,cb,Source.SrcPos,
              [fSubject,p
              ,fTypeDecl,Sphere.n(r,fSubject)^
              ]);
          else
            Source.Error('dereference expected on pointer');
        end;
       end;

    stAtAtAt://"@@@": inherited
     begin
      Juxta(p);
      SrcPos:=Source.SrcPos;
      if Source.IsNext([stPOpen]) then
       begin
        //StratoFnCallFindInherited: see combine pArgList
        Push(pArgList,cb,0,SrcPos);
        cbInhCalled:=true;
        p:=0;
       end
      else
        Source.Error('manipulating inherited pointer not allowed');
     end;

    else Source.Error('unsupported syntax');//'unexpected token');
  end;
end;

function TStratoParserBase.Fn(x:xItem;nx:xName;const nn:UTF8String;SrcPos:xSrcPos):xItem;//nMember
var
  p:xItem;
begin
  p:=Sphere.Lookup(x,nx);
  if (p=0) or (Sphere.n(p,vTypeNr)^<>nMember) then
   begin
    if p<>0 then Source.Error('duplicate identifier "'+nn+'"');
    p:=Sphere.Add(nMember,x,SrcPos,[vName,nx]);
    Sphere.Append(x,fItems,p);
   end;
  Result:=p;
end;

end.
