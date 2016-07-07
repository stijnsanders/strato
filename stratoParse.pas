unit stratoParse;

interface

uses stratoDecl, stratoSphere, stratoSource;

function StratoParseSource(Sphere: TStratoSphere;
  Source: TStratoSource): TStratoIndex;

implementation

uses SysUtils, stratoTokenizer, stratoRunTime, stratoFn, stratoLogic;

type
  TPrecedence=(
    p___,
    pRecord,
    pCodeBlock,
      p_Statement,
    pThrow,pDefer,pCatch,
    pBrackets,
    pParentheses,
    pForBodyFirst,pForFirst,pForCrit,pForThen,pForCritDone,pForCritOnly,
      p_CodeBlockDone,
    pForBody,pIfThen,pIfElse,

    pUnTypedVar,
    pArgList,
      p_ArgList_Item,
    pAssignment,
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
    Sphere: TStratoSphere;
    Source: TStratoSource;
    FNameSpace: TStratoIndex;

    Locals:array of TStratoIndex;
    src:TStratoIndex;

    procedure ID(var n:TStratoName;var nn:UTF8String;var SrcPos:cardinal);
    function LookUpNameSpace(var ns:TStratoIndex;var n:TStratoName;
      var SrcPos: cardinal):boolean;
    procedure ParseImport;
    function LookUpID(StopAtType:boolean=false):TStratoIndex;
    procedure LookUpNext(n:TStratoName;const nn:UTF8String;
      var p,ns,isNew:TStratoIndex);
    function Fn(x:TStratoIndex;n:TStratoName;const nn:UTF8String;
      SrcPos:cardinal):TStratoIndex;
    function ParseLiteral(st0:TStratoToken;
      OutputIntVal:boolean=false):TStratoIndex;
    function LookUpType(const tname:string;Lazy:boolean=false):TStratoIndex;
    function ParseSignature(ns:TStratoIndex;const name:UTF8String;
      CloseToken:TStratoToken;SrcPos:cardinal):TStratoIndex;
    procedure ParseEnumeration(p:TStratoIndex);
    procedure ParseInterfaceDecl(x:TStratoIndex);
  public
    //constructor Create(ASphere: TStratoSphere; ASource: TStratoSource);
    property NameSpace: TStratoIndex read FNameSpace;
  end;

  TStratoParser=class(TStratoParserBase)
  private
    stackSize,stackIndex:cardinal;
    stack:array of record
      p:TPrecedence;
      t:TStratoIndex;
    end;
    cb,rd:TStratoIndex;
    cbInhCalled:boolean;
    mark1,mark2:TStratoIndex;

    procedure Push(p:TPrecedence;t:TStratoIndex);
    procedure CodeLookup(n:TStratoName;var p:TStratoIndex;SrcPos:cardinal);
    function Combine(zz:TPrecedence;var q:TStratoIndex):boolean;
    procedure Juxta(var p:TStratoIndex);
    procedure PushBinary(p:TPrecedence;st:TStratoToken;var q:TStratoIndex);

    procedure ParseHeader;
    procedure ParseDeclaration;
    procedure ParseRecord;
    procedure ParseLogic;

    procedure CheckPassed(p:TStratoIndex);
    function CbStart(pp: TStratoIndex): TStratoIndex;
    procedure CbAdd(p:TStratoIndex);
  public
    constructor Create(ASphere: TStratoSphere; ASource: TStratoSource);
    procedure Parse;
  end;

function StratoParseSource(Sphere: TStratoSphere;
  Source: TStratoSource): TStratoIndex;
var
  p:TStratoParser;
begin
  if Source.Done then Result:=0 else
   begin
    p:=TStratoParser.Create(Sphere,Source);
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

procedure TStratoParserBase.ID(var n:TStratoName;var nn:UTF8String;
  var SrcPos:cardinal);
begin
  nn:=Source.GetID(SrcPos);
  n:=Sphere.Dict.StrIdx(nn);
end;

function TStratoParserBase.LookUpNameSpace(var ns:TStratoIndex;
  var n:TStratoName;var SrcPos:cardinal):boolean;
var
  nn:UTF8String;
  p:TStratoIndex;
begin
  ID(n,nn,SrcPos);
  //Sphere.Lookup? take ttNameSpace only
  ns:=Sphere.Lookup(pHeader,tf_FirstNameSpace,n);
  if not Sphere.t(ns) in [0,ttNameSpace] then
   begin
    Source.Error('"'+nn+'" is not a namespace');
    ns:=0;
   end;
  Result:=ns<>0;
  //resolve nested namespaces
  p:=0;
  while Source.IsNext([stPeriod,stIdentifier]) do
   begin
    if ns=0 then
     begin
      //not found, create
      if ns=0 then
       begin
        ns:=Sphere.Add(ttNameSpace,
          [tfName,n
          ,tfParent,p
          ,tf_NameSpace_SourceFile,src//?
          ,tfSrcPos,SrcPos//?
          ]);
        if p=0 then
          Sphere.Append(pHeader,tf_FirstNameSpace,ns)
        else
          Sphere.Append(p,tfFirstItem,ns);
       end;
      p:=ns;
      Result:=false;
     end;
    ID(n,nn,SrcPos);
    ns:=Sphere.Lookup(p,tfFirstItem,n);
    if not Sphere.t(ns) in [0,ttNameSpace] then
     begin
      Source.Error('"'+nn+'" is not a namespace');
      ns:=0;
     end;
   end;
  if not(Result) then ns:=p; 
end;

procedure TStratoParserBase.ParseImport;
var
  ns,p:TStratoIndex;
  ss:TStratoSource;
  n:TStratoName;
  alias:UTF8String;
  fn:string;
  i,l:integer;
  SrcPos,SrcPos1:cardinal;
begin
  ns:=0;//default
  fn:='';//default;
  //alias?
  if Source.IsNext([stIdentifier,stDefine]) then
   begin
    alias:=Source.GetID(SrcPos);
    Source.Token;//stDefine
   end
  else
    alias:='';
  case Source.Token of
    stIdentifier:
     begin
      //TODO: load from standard library !!!
      if LookupNameSpace(ns,n,SrcPos) then
       begin
        if Sphere.r(ns,tf_NameSpace_SourceFile)=0 then
          Sphere.s(ns,tf_NameSpace_SourceFile,src);
       end
      else
       begin
        //Sphere.Add(ttNameSpace here? see StratoParseSource below
        fn:=string(Sphere.FQN(ns));
        if fn<>'' then fn:=fn+'.';
        fn:=Sphere.BasePath+fn+Sphere.Dict[n]+'.xs';
        if not FileExists(fn) then
         begin
          fn:='';
          Source.Error('unknown namespace "'+string(Sphere.FQN(ns))+'"');
         end;
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
  if Source.IsNext([stAt,stNumericLiteral]) then
    i:=ParseInteger(string(Source.GetID(SrcPos1)))
  else
    i:=0;
  Source.Skip(stSemiColon);
  //load and parse
  if fn<>'' then
   begin
    if i=0 then l:=0 else l:=Sphere.MarkIndex(i);
    ss:=TStratoSource.Create;
    ss.OnError:=Source.OnError;//?
    ss.LoadFromFile(fn);
    ns:=StratoParseSource(Sphere,ss);
    Source.ErrorCount:=Source.ErrorCount+ss.ErrorCount;
    if i<>0 then Sphere.MarkIndex(l);
   end;
  //TODO: if (fn='') and (q<>0) then 'already loaded at...'?
  //register
  if ns<>0 then
    if alias<>'' then //alias
     begin
      if not Sphere.Add(Locals[0],tfFirstItem,ttImport,
        [tfName,Sphere.Dict.StrIdx(alias)
        ,tfSrcPos,SrcPos
        ,tfParent,Locals[0]
        ,tfTarget,ns
        ],p) then
        Source.Error('duplicate identifier "'+alias+'"')
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
end;

function TStratoParserBase.LookUpID(StopAtType:boolean=false):TStratoIndex;
var
  i,j,l:integer;
  n:TStratoName;
  nn:UTF8String;
  nsx:array of TStratoIndex;
  SrcPos:cardinal;
  function CheckFoundType: boolean;
  begin
    Result:=true;//default
    if StopAtType and (j=1) then
     begin
      i:=0;
      while (i<l) and (nsx[i]=0) do inc(i);
      if (i<l) and ((Sphere.t(nsx[i]) and tt__IsType)<>0) then
        Result:=false;
     end
  end;
begin
  //assert Source.IsNext([stIdentifier]);
  Result:=0;//default
  ID(n,nn,SrcPos);
  l:=Length(Locals);
  SetLength(nsx,l+1);
  j:=0;
  for i:=0 to l-1 do
   begin
    if Locals[i]=0 then nsx[i]:=0 else
      nsx[i]:=Sphere.Lookup(Locals[i],tfFirstItem,n);
    if nsx[i]<>0 then inc(j);
   end;
  nsx[l]:=Sphere.Lookup(pHeader,tf_FirstNameSpace,n);
  //no inc(j), see below (case j=0)
  while CheckFoundType and
    Source.IsNext([stPeriod,stIdentifier]) do //and (j<>0)? no: makes it greedy
   begin
    ID(n,nn,SrcPos);
    for i:=0 to l do
      if nsx[i]<>0 then
       begin
        if (Sphere.t(nsx[i]) and tt__Resolvable)=0 then
          nsx[i]:=0
        else
          nsx[i]:=Sphere.Lookup(nsx[i],tfFirstItem,n);
        if nsx[i]=0 then
          dec(j)
        else
          if Sphere.t(nsx[i])=ttImport then
            nsx[i]:=Sphere.r(nsx[i],tfTarget);
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

procedure TStratoParserBase.LookUpNext(n:TStratoName;const nn:UTF8String;
  var p,ns,isNew:TStratoIndex);
var
  i,l:integer;
begin
  isNew:=0;//default
  if p=0 then
   begin
    l:=Length(Locals);
    i:=0;
    while (i<>l) and (p=0) do
     begin
      if Locals[i]<>0 then
        p:=Sphere.Lookup(Locals[i],tfFirstItem,n);
      inc(i);
     end;
    if p=0 then //still nothing, check namespaces
      p:=Sphere.Lookup(pHeader,tf_FirstNameSpace,n);
   end
  else
    if (Sphere.t(p) and tt__Resolvable)<>0 then
      p:=Sphere.Lookup(p,tfFirstItem,n)
    else
      p:=0;//see placeholder below
  if (p=0) or ((Sphere.t(p) and tt__Resolvable)=0) then
   begin
    //create to silence further errors?
    if Sphere.Add(ns,tfFirstItem,ttNameSpace,
      [tfName,n
      ,tfParent,ns
      ,tf_NameSpace_SourceFile,src
      //,tfSrcPos
      ],p) then
      isNew:=p
    else
     begin
      Source.Error('duplicate namespace "'+
        string(Sphere.FQN(ns)+'.'+nn)+'"');
      isNew:=Sphere.Lookup(ns,tfFirstItem,n);
     end;
   end;
  ns:=p;
end;

function TStratoParserBase.ParseLiteral(st0:TStratoToken;
  OutputIntVal:boolean):TStratoIndex;
const
  stackGrowSize=$10;
var
  st:TStratoToken;
  vt:TSTratoIndex;
  v:UTF8String;
  stackSize,stackIndex:integer;
  stack:array of record
    p:TPrecedence;
    vt:TStratoIndex;
    v1,v2:UTF8String;
  end;
  SrcPos:cardinal;

  procedure Combine(pp:TPrecedence);
  var
    p0:TPrecedence;
    v0,w:UTF8String;
    wt:TStratoIndex;
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
        w:=stack[stackIndex].v2;
        wt:=stack[stackIndex].vt;
        stack[stackIndex].v2:='';
        case p0 of
          pParentheses:
            done:=true;//always only one (need to parse ")" correctly)
          pUnary:
            case v0[1] of
              '-':v:='-'+w;//assert vt=TypeDecl_number
              '!','~'://not
                if vt=TypeDecl_bool then
                  if w='0' then v:='1' else v:='0'
                else
                if vt=TypeDecl_number then
                 begin
                  i:=ParseInteger(string(v));
                  v:=UTF8String(IntToStr(not(i)));
                 end
                else
                  Source.Error('unsupported type for ''not''');
              else
                Source.Error('unknown unary operator');
            end;
          pSizeOf:
            v:=IntToStr(ByteSize(Sphere,vt));
          pMulDiv,pAddSub,pShift,
          pLogicalOr,pLogicalXor,pLogicalAnd,//TODO:
          pBitwiseOr,pBitwiseXor,pBitwiseAnd,
          pEqual,pComparative:
            if vt=TypeDecl_number then
             begin
              i:=ParseInteger(string(v));
              j:=ParseInteger(string(w));
              case v0[1] of
                '*':v:=UTF8String(IntToStr(i*j));
                '/':v:=UTF8String(IntToStr(i div j));
                '%':v:=UTF8String(IntToStr(i mod j));
                '+':v:=UTF8String(IntToStr(i+j));
                '-':v:=UTF8String(IntToStr(i-j));
                '&':v:=UTF8String(IntToStr(i and j));
                '|':v:=UTF8String(IntToStr(i or j));
                'X':v:=UTF8String(IntToStr(i xor j));
                '=':
                 begin
                  vt:=TypeDecl_bool;
                  if i=j then v:='1' else v:='0';
                 end;
                '<':
                 begin
                  vt:=TypeDecl_bool;
                  if i<j then v:='1' else v:='0';
                 end;
                'l':
                 begin
                  vt:=TypeDecl_bool;
                  if i<=j then v:='1' else v:='0';
                 end;
                '>':
                 begin
                  vt:=TypeDecl_bool;
                  if i>j then v:='1' else v:='0';
                 end;
                'g':
                 begin
                  vt:=TypeDecl_bool;
                  if i>=j then v:='1' else v:='0';
                 end;
                's':
                  if v0='shl' then
                    v:=UTF8String(IntToStr(i shl j))
                  else
                    v:=UTF8String(IntToStr(i shr j));
                'r':
                  if v0='rol' then
                    v:=UTF8String(IntToStr((i shl j) or (i shr (SystemWordSize*8-j))))
                  else
                    v:=UTF8String(IntToStr((i shr j) or (i shl (SystemWordSize*8-j))));
                else
                  Source.Error('unknown operator');
              end;
             end
            else
              case v0[1] of
                '+':v:=w+v;
                '=':
                 begin
                  vt:=TypeDecl_bool;
                  //TODO: CanCast?
                  if (vt=wt) and (v=w) then v:='1' else v:='0';
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

  procedure Push(p:TPrecedence;const vv:UTF8String);
  begin
    Combine(p);
    if stackIndex=stackSize then //grow
     begin
      inc(stackSize,stackGrowSize);
      SetLength(stack,stackSize);
     end;
    stack[stackIndex].p:=p;
    stack[stackIndex].v1:=vv;
    stack[stackIndex].v2:=v;
    stack[stackIndex].vt:=vt;
    inc(stackIndex);
    vt:=0;
    v:='';
  end;

var
  p:TStratoIndex;
begin
  st:=st_Unknown;
  Result:=0;//default
  stackSize:=0;
  stackIndex:=0;
  vt:=0;
  v:='';
  SrcPos:=Source.SrcPos;
  while (Result=0) or (stackIndex<>0) do
   begin
    if st=st_Unknown then st:=st0 else st:=Source.Token;
    case st of
      stIdentifier:
       begin
        Result:=LookUpID;
        if Result=0 then
         begin
          Source.Error('undefined constant value');
          Result:=Sphere.Add(ttVar,[]);//avoid further errors
         end
        else
          case Sphere.t(Result) of
            ttConstant,ttVar:
             begin
              Result:=Sphere.r(Result,tfInitialValue);
              if (Result<>0) and (Sphere.t(Result)=ttLiteral) then
               begin
                p:=Sphere.r(Result,tfInitialValue);
                if p=0 then
                  Source.Error('constant without value')
                else
                 begin
                  v:=Sphere.GetBinaryData(p);
                  vt:=Sphere.r(Result,tfEvaluatesTo);
                  Result:=p;
                 end;
               end;
             end;
            else
             begin
              Source.Error('unsupported constant value');
              Result:=0;
              vt:=0;
              v:='';
             end;
          end;
       end;
      stStringLiteral:
       begin
        vt:=TypeDecl_string;
        v:=Source.GetStrs;
       end;
      stNumericLiteral:
       begin
        vt:=TypeDecl_number;
        v:=Source.GetID(SrcPos);
       end;
      {//TODO:
      stAOpen://JSON?
      stBOpen://array
      stPOpen://tuple? expression?
      }
      stOpAdd://unary
        Push(pUnary,'+');
      stOpSub://unary
        Push(pUnary,'-');
      stTilde:
        Push(pUnary,'~');
      stOpSizeOf://unary
        Push(pSizeOf,'');
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
       begin
        Combine(p_ArgList_Item);//something between pParentheses and the operators
        if stackIndex=0 then
          if OutputIntVal then
            if vt=TypeDecl_number then
              Result:=ParseInteger(string(v))
            else
             begin
              Source.Error('integer constant expected');
              Result:=0;
             end
          else
           begin
            Result:=Sphere.Add(ttLiteral,
              [tfSrcPos,SrcPos
              ,tfEvaluatesTo,vt
              ,tfInitialValue,Sphere.AddBinaryData(v)
              ]);
            if vt=0 then
              Source.Error('literal of undetermined type');
           end;
       end;
   end;
  if OutputIntVal and (vt=0) then
    Source.Error('missing integer constant');
end;

function TStratoParserBase.LookUpType(const tname:string;
  Lazy:boolean=false):TStratoIndex;
var
  p:TStratoIndex;
  i,ptr:cardinal;
begin
  if Source.IsNext([stQuestionMark,stIdentifier]) then
   begin
    p:=LookUpID;
    Result:=Sphere.Add(ttClassRef,
      [tfParent,Sphere.r(p,tfParent)
      ,tfSrcPos,Sphere.v(p,tfSrcPos)//Source.SrcPos
      ,tfByteSize,SystemWordSize
      ,tfEvaluatesTo,p
      ]);
    if Sphere.t(p)<>ttClass then
      Source.Error('class reference allowed to class only');
    //TODO: support ttClassRef?
   end
  else
   begin
    ptr:=0;
    while Source.IsNext([stCaret]) do inc(ptr);
    //TODO: case Source.Token of?
    if Source.IsNext([stIdentifier]) then
      Result:=LookUpID(Lazy)
    else
      Result:=0;
    if Result=0 then Source.Error('undefined '+tname) else
     begin
      case Sphere.t(Result) of
        ttVar:
          Result:=Sphere.r(Result,tfEvaluatesTo);//take var's type
        ttAlias:
          Result:=Sphere.r(Result,tfTarget);//assert never another ttAlias
        ttSignature:
          //TODO: ttDelegate? (keep signature and subject)
          Result:=Sphere.Add(ttPointer,
            [tfParent,Sphere.r(Result,tfParent)
            ,tfSrcPos,Sphere.v(Result,tfSrcPos)//Source.SrcPos;
            ,tfByteSize,SystemWordSize
            ,tfEvaluatesTo,Result
            ]);
        else
          if (Sphere.t(Result) and tt__IsType)=0 then
            Source.Error(tname+' is not a type');
      end;
      //array
      if Source.IsNext([stBOpen]) then
        if Source.IsNext([stBClose]) then
          Source.Error('//TODO: dyn array')
        else
         begin
          i:=cardinal(ParseLiteral(Source.Token,true));
          Source.Skip(stBClose);//TODO: force
          Result:=Sphere.Add(ttArray,
            [tfParent,Sphere.r(Result,tfParent)
            ,tfSrcPos,Sphere.v(Result,tfSrcPos)//Source.SrcPos
            ,tfName,Sphere.v(Result,tfName)
            ,tfSubject,Result//element type
            ,tfByteSize,ByteSize(Sphere,Result)*i
            ]);
         end;
     end;
    if Result<>0 then
      while ptr<>0 do
       begin
        dec(ptr);
        //TODO: store somewhere?
        p:=Sphere.Add(ttPointer,
          [tfParent,Sphere.r(Result,tfParent)
          ,tfSrcPos,Sphere.v(Result,tfSrcPos)//Source.SrcPos
          ,tfByteSize,SystemWordSize
          ,tfEvaluatesTo,Result
          ]);
        Result:=p;
       end;
   end;
end;

function TStratoParserBase.ParseSignature(ns:TStratoIndex;
  const name:UTF8String;CloseToken:TStratoToken;SrcPos:cardinal):TStratoIndex;
var
  st:TStratoToken;
  p,q,Signature,NoType:TStratoIndex;
  argName:UTF8String;
  byref:boolean;

  procedure AddArgument(InitialValue:TStratoIndex);
  var
    tt:TStratoThingType;
    r:TStratoIndex;
  begin
    if byref then tt:=ttArgByRef else tt:=ttArgument;
    byref:=false;
    if Sphere.Add(Signature,tfFirstArgument,tt,
      [tfName,Sphere.Dict.StrIdx(argName)
      ,tfParent,Signature
      ,tfSrcPos,SrcPos
      ,tfEvaluatesTo,p
      ,tfInitialValue,InitialValue
      ],r) then
     begin
      if (p=0) and (NoType=0) then NoType:=r;
     end
    else
      Source.Error('duplicate argument "'+argName+'"');
  end;

begin
  //assert one past token stPOpen
  Signature:=Sphere.Add(ttSignature,
    [tfName,Sphere.Dict.StrIdx(name)
    ,tfParent,ns
    ,tfSrcPos,SrcPos
    ]);
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
        argName:=Source.GetID(SrcPos);
        p:=0;//default
        q:=0;//default
        st:=Source.Token;
        case st of
          stColon: //argument type
           begin
            p:=LookUpType('argument type');
            while NoType<>0 do
             begin
              //assert Sphere.r(NoType,tfEvaluatesTo)=0
              Sphere.s(NoType,tfEvaluatesTo,p);
              NoType:=Sphere.r(NoType,tfNext);
             end;
            if Source.IsNext([stDefine]) then //default value
              q:=ParseLiteral(Source.Token);
            AddArgument(q);
            if Source.IsNext([stComma]) or Source.IsNext([stSemiColon]) then ;//skip
           end;
          stComma:
            AddArgument(0);
          stDefine:
           begin
            q:=ParseLiteral(Source.Token);
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
  if Sphere.t(ns)<>ttNameSpace then //strRecord,strTypeDecl
    Sphere.s(Signature,tfTarget,ns);
  if Source.IsNext([stColon,stIdentifier]) then
    Sphere.s(Signature,tfEvaluatesTo,LookUpType('returns type'))
  else
    if CloseToken=stBClose then
      Source.Error('property requires value type');
end;

procedure TStratoParserBase.ParseEnumeration(p:TStratoIndex);
var
  st:TStratoToken;
  n:TStratoName;
  nn:UTF8String;
  q:TStratoIndex;
  e,SrcPos:cardinal;
begin
  e:=0;
  st:=stIdentifier;//default (something not stPClose really)
  while (st<>stPClose) and Source.NextToken(st) do
    case st of
      stIdentifier:
       begin
        ID(n,nn,SrcPos);
        if Source.IsNext([stDefine]) then
          e:=cardinal(ParseLiteral(Source.Token,true));
        if Sphere.Add(p,tfFirstItem,ttConstant,
          [tfName,n
          ,tfParent,p
          ,tfSrcPos,SrcPos
          ,tfEvaluatesTo,p
          ,tfInitialValue,e
          ],q) then
          inc(e)
        else
          Source.Error('duplicate enumeration entry "'+nn+'"');
       end;
      stComma,stSemiColon:;//ignore
      stPClose:;//done
      else Source.Error('unsupported enumeration syntax');
    end;
end;

procedure TStratoParserBase.ParseInterfaceDecl(x:TStratoIndex);
var
  st:TStratoToken;
  n:TStratoName;
  nn:UTF8String;
  SrcPos:cardinal;
begin
  //assert previous token stAOpen
  while not(Source.IsNext([stAClose])) and Source.NextToken(st) do
    case st of

      stIdentifier:
       begin
        ID(n,nn,SrcPos);
        case Source.Token of
          stColon:
            case Source.Token of
              stIdentifier:
                if Sphere.Add(x,tfFirstItem,ttVar,
                  [tfName,n
                  ,tfParent,x
                  ,tfSrcPos,SrcPos
                  ,tfEvaluatesTo,LookUpType('field type')
                  ])=0 then
                  Source.Error('duplicate interface field "'+nn+'"');
              //more?
              else Source.Error('unsupported interface field type syntax');
            end;
          stPOpen://signature
            StratoFnAdd(Sphere,Source,ttOverload,Fn(x,n,nn,SrcPos),
              ParseSignature(x,nn,stPClose,SrcPos),src,SrcPos);
          else Source.Error('unsupported interface field syntax');
        end;
        Source.Skip(stSemiColon);
       end;

      else Source.Error('unsupported interface field syntax');
    end;
end;

{ TStratoParser }

constructor TStratoParser.Create(ASphere: TStratoSphere;
  ASource: TStratoSource);
begin
  inherited Create;
  Sphere:=ASphere;
  Source:=ASource;
  FNameSpace:=0;//default
  mark1:=0;
  mark2:=0;
  src:=0;//moved to ParseHeader
end;

const
  stackGrowSize=$100;

procedure TStratoParser.Parse;
begin
  ParseHeader;
  stackIndex:=0;
  stackSize:=stackGrowSize;
  SetLength(stack,stackSize);
  cb:=0;
  cbInhCalled:=false;//see also CbStart
  rd:=0;
  while not Source.Done do
    if cb=0 then
      if rd=0 then
        ParseDeclaration
      else
        ParseRecord
    else
      ParseLogic;
  if stackIndex<>0 then
    Source.Error('unexpected end of source ('+IntToStr(stackIndex)+')');
  if mark1<>0 then
    Sphere.MarkIndex(mark2);
end;

procedure TStratoParser.Push(p:TPrecedence;t:TStratoIndex);
begin
  if stackIndex=stackSize then
   begin
    inc(stackSize,stackGrowSize);//grow
    SetLength(stack,stackSize);
   end;
  stack[stackIndex].p:=p;
  stack[stackIndex].t:=t;
  inc(stackIndex);
end;

procedure TStratoParser.CodeLookup(n:TStratoName;var p:TStratoIndex;
  SrcPos:cardinal);
var
  i,l:integer;
  p0,q,r,s:TStratoIndex;
begin
  p0:=p;
  //TODO: ttImport, ttAlias
  if p<>0 then
    r:=p //see below: search by type
  else
   begin
    r:=0;
    p:=Sphere.Lookup(cb,tfFirstItem,n);
    //not found? check stack
    if p=0 then
     begin
      i:=stackIndex;
      while (i<>0) and (p=0) do
       begin
        dec(i);
        case stack[i].p of
          pRecord,pCodeBlock:
            p:=Sphere.Lookup(stack[i].t,tfFirstItem,n);
          pCatch:
           begin
            q:=Sphere.r(stack[i].t,tfFirstArgument);
            if (q<>0) and (Sphere.v(q,tfName)=n) then p:=q;
           end;
        end;
       end;
     end;
    //not found? check under 'this'
    if (p=0) and (Sphere.t(Sphere.r(cb,tfFirstItem))=ttThis) then
     begin
      p:=Sphere.r(cb,tfFirstItem);
      r:=p;//see below: search by type
     end;
   end;
  if r<>0 then
   begin
    q:=p;
    if (Sphere.t(p) and tt__Resolvable)=0 then p:=0 else
      p:=Sphere.Lookup(p,tfFirstItem,n);
    //nothing, is it typed? search typedecl
    if p=0 then
     begin
      r:=ResType(Sphere,q);
      if (r<>0) and (Sphere.t(r)=ttArray) then
        r:=0;//r:=Sphere[r].ElementType;
      if r<>0 then
       begin
        s:=Sphere.Lookup(r,tfFirstItem,n);
        while (s=0) and (r<>0) and (Sphere.t(r)=ttClass) do
         begin
          r:=Sphere.r(r,tfInheritsFrom);
          if r=0 then
            s:=0
          else
            s:=Sphere.Lookup(r,tfFirstItem,n);
         end;
        r:=s;
       end;
      if r<>0 then
       begin
        p:=Sphere.Add(ttField,
          [tfParent,cb
          ,tfSubject,q
          ,tfSrcPos,SrcPos
          ,tfTarget,r
          ]);
        if (Sphere.t(r) and tt__Typed)<>0 then
          Sphere.s(p,tfEvaluatesTo,Sphere.r(r,tfEvaluatesTo));
       end;
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
        p:=Sphere.Lookup(Locals[i],tfFirstItem,n);
      inc(i);
     end;
    if p=0 then //still nothing, check namespaces
      p:=Sphere.Lookup(pHeader,tf_FirstNameSpace,n);
   end;
end;

function TStratoParser.Combine(zz:TPrecedence;var q:TStratoIndex):boolean;
var
  z,z00:TPrecedence;
  p,r:TStratoIndex;
  done:boolean;
  tt:TStratoThingType;
begin
  if (q<>0) or (zz<=pParentheses) then
   begin
    done:=false;
    while not(done) and (stackIndex<>0) and (stack[stackIndex-1].p>=zz) do
     begin
      dec(stackIndex);
      z:=stack[stackIndex].p;
      p:=stack[stackIndex].t;
      {$IFDEF DEBUG}
      stack[stackIndex].p:=p___;
      stack[stackIndex].t:=0;
      {$ENDIF}
      z00:=z;
      case z of
        pRecord://tRecord
          done:=true;//???
        pCodeBlock://ttCodeBlock
          //see also stAClose in main loop!
          done:=true;//always only one (need to parse "}" correctly)
        pIfThen://ttSelection
         begin
          Sphere.s(p,tfDoThen,q);
          StratoSelectionCheckType(Sphere,p);
          z:=pIfElse;//now parse 'else' bit
         end;
        pIfElse:
         begin
          Sphere.s(p,tfDoElse,q);
          StratoSelectionCheckType(Sphere,p);
         end;
        pArgList://ttFnCall
         begin
          if q<>0 then
            StratoFnCallAddArgument(Sphere,p,q);
          //calling destructor? (detect prefix '-' or '~')
          if (stackIndex<>0) and (stack[stackIndex-1].p=pUnary) and
            (TStratoToken(Sphere.v(stack[stackIndex-1].t,tfOperator))
              in [stOpSub,stTilde]) and (TypeDecl_object<>0) then
           begin
            tt:=ttDestructor;
            dec(stackIndex);
           end
          else
            tt:=ttOverload;
          //subject stored in px.Target when Push(pArgList
          if not StratoFnCallFindSignature(Sphere,p,tt) then
            Source.Error('no function overload found with these arguments');
          done:=true;//always only one (need to parse ")" correctly)
         end;
        pParentheses:
         begin
          done:=true;//always only one (need to parse ")" correctly)
          p:=q;
         end;
        pBrackets:
         begin
          if q<>0 then
            StratoFnCallAddArgument(Sphere,p,q);
          if not StratoFnCallFindSignature(Sphere,p,ttPropertyGet) then
           begin
            //assert Sphere.t(p)=ttArrayIndex
            q:=ResType(Sphere,Sphere.r(p,tfTarget));
            if (q=0) or (Sphere.t(q)<>ttArray) then
              Source.Error('no array or property to index into');
            q:=Sphere.r(p,tfFirstArgument);
            //TODO: accept any numeric type?
            //while q<>0 do //TODO: multi-dim arrays
            if ResType(Sphere,q)<>TypeDecl_number then
              Source.Error('argument index not a number')
            else if Sphere.r(q,tfNext)<>0 then
              Source.Error('multiple array index not supported');//yet?
           end;
          done:=true;//always only one (need to parse "]" correctly)
         end;

        //ttIteration,ttIterationPE
        pForBodyFirst:
         begin
          if (q<>0) and (Sphere.t(q)=ttCodeBlock)
            and (Sphere.r(q,tfEvaluatesTo)<>0) then
            Source.Error('unexpected iteration body with return value');
          Sphere.s(p,tfBody,q);
          z:=pForFirst;//now parse criterium
         end;
        pForFirst://see also stAClose
          if zz=pParentheses then //already closing? take this as crit
           begin
            Sphere.s(p,tfDoIf,q);
            if Sphere.r(p,tfBody)=0 then z:=pForBody;
           end
          else
           begin
            Sphere.s(p,tfDoFirst,q);
            z:=pForCrit;
           end;
        pForCrit,pForCritOnly:
         begin
          if q<>0 then
           begin
            if not SameType(Sphere,ResType(Sphere,q),TypeDecl_bool) then
              Source.Error('iteration criterium does not evaluate to boolean');
            Sphere.s(p,tfDoIf,q);
           end;
          if zz=pParentheses then
           begin
            if Sphere.r(p,tfBody)=0 then z:=pForBody;
           end
          else
            if z<>pForCritOnly then z:=pForThen;
         end;
        pForThen:
         begin
          if q<>0 then Sphere.s(p,tfDoThen,q);//else assert already set by stPClose
          if zz=pParentheses then
           begin
            if Sphere.r(p,tfBody)=0 then z:=pForBody;
           end
          else
            z:=pForCritDone;
         end;
        pForCritDone:
         begin
          if q<>0 then
            Source.Error('unexpected iteration criterium syntax');
          if Sphere.r(p,tfBody)=0 then z:=pForBody;
         end;
        pForBody:
         begin
          if (q<>0) and (Sphere.t(q)=ttCodeBlock)
            and (Sphere.r(q,tfEvaluatesTo)<>0) then
            Source.Error('unexpected iteration body with return value');
          Sphere.s(p,tfBody,q);
         end;

        //ttUnaryOp
        pUnary:
          Sphere.s(p,
            [tfEvaluatesTo,ResType(Sphere,q)
            ,tfRight,q]);
        pSizeOf:
          Sphere.s(p,
            [tfEvaluatesTo,TypeDecl_number
            ,tfRight,q]);//?
        pTypeOf:
         begin
          r:=ResType(sphere,q);
          Sphere.s(p,tfRight,q);
          if (r<>0) and (Sphere.t(r)=ttClass) then
            q:=Sphere.Add(ttClassRef,
              [tfParent,cb
              ,tfByteSize,SystemWordSize
              ,tfEvaluatesTo,r
              ])
          else
            q:=TypeDecl_type;
          Sphere.s(p,tfEvaluatesTo,q);
         end;
        pAddressOf:
         begin
          Sphere.s(p,tfValueFrom,q);
          if IsAddressable(Sphere,q) then
           begin
            q:=ResType(Sphere,q);
            while (q<>0) and (Sphere.t(q)=ttArray) do
              q:=Sphere.r(q,tfSubject);//element type
            Sphere.s(p,tfEvaluatesTo,Sphere.Add(ttPointer,
              [tfParent,cb
              ,tfSrcPos,Source.SrcPos
              ,tfByteSize,SystemWordSize
              ,tfEvaluatesTo,q
              ]));
           end
          else
            Source.Error('invalid address-of subject');
         end;

        //ttBinaryOp
        pMulDiv,pAddSub,pShift,
        pLogicalOr,pLogicalXor,pLogicalAnd,
        pBitwiseOr,pBitwiseXor,pBitwiseAnd:
         begin
          Sphere.s(p,tfRight,q);
          if not StratoOperatorCheckType(Sphere,p) then
            Source.Error('binary operator operand type mismatch');
         end;
        pEqual,pComparative:
         begin
          Sphere.s(p,tfRight,q);
          if not StratoComparativeCheckType(Sphere,p) then
            Source.Error('binary operator operand type mismatch');
          Sphere.s(p,tfEvaluatesTo,TypeDecl_bool);
         end;
        //ttAssign
        pAssignment:
         begin
          case Sphere.t(p) of
            ttAssign:Sphere.s(p,tfValueFrom,q);
            ttPropCall:
              if Sphere.v(p,tfOperator)=cardinal(stOpAssign) then
                Sphere.s(p,tfEvaluatesTo,q)
              else
                //assert p.EvaluatesTo<>nil (see StratoFindPropertySet)
                Sphere.s(Sphere.r(p,tfEvaluatesTo),tfRight,q);
            else Source.Error('invalid assignment type');
          end;
          r:=ResType(Sphere,q);
          if r=0 then
            Source.Error('invalid assignment value')
          else
           begin
            if (stackIndex<>0) and (stack[stackIndex-1].p=pUnTypedVar) then
             begin
              Sphere.s(stack[stackIndex-1].t,tfEvaluatesTo,r);
              Sphere.s(cb,tfByteSize,
                Sphere.v(cb,tfByteSize)+ByteSize(Sphere,r));
             end;
            case Sphere.t(p) of
              ttAssign:q:=ResType(Sphere,Sphere.r(p,tfAssignTo));
              ttPropCall:q:=ResType(Sphere,p);
            end;
            if not SameType(Sphere,r,q) then
              Source.Error('assignment type mismatch');
            //assigning an object reference? reference counting!
            if Sphere.t(r)=ttClass then
             begin
              if Sphere.v(p,tfOperator)<>cardinal(stOpAssign) then
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
         end;

        pUnTypedVar:
         begin
          //see also pAssignment above and stColon below
          //assert Sphere.t(p)=ttVar
          if Sphere.r(p,tfEvaluatesTo)=0 then
            Source.Error('no type for local var "'+string(Sphere.FQN(p))+'"');
          p:=q;
         end;
        pUnresolved:
          p:=StratoCheckMemberNoArguments(Sphere,p,q);

        pDefer,pThrow:Sphere.s(p,tfTarget,q);//ttDeferred,ttThrow
        pCatch:Sphere.s(p,tfBody,q);//ttCatch
        //else ?
      end;
      if z=z00 then
        q:=p
      else
       begin
        stack[stackIndex].p:=z;
        stack[stackIndex].t:=p;
        inc(stackIndex);
        q:=0;
        done:=true;
       end;
     end;
   end;
  Result:=q<>0;
end;

procedure TStratoParser.Juxta(var p:TStratoIndex);
begin
  if Combine(p_Juxta,p) then
    if SameType(Sphere,ResType(Sphere,p),TypeDecl_bool) then
      Push(pIfThen,Sphere.Add(ttSelection,
        [tfParent,cb
        ,tfSrcPos,Source.SrcPos
        ,tfDoIf,p
        ]))
    else
      if Combine(pIfThen,p) then
        Source.Error('missing operator or semicolon');
  p:=0;
end;

procedure TStratoParser.PushBinary(p:TPrecedence;st:TStratoToken;
  var q:TStratoIndex);
begin
  if not Combine(p,q) then
    Source.Error('no left side defined for binary operator')
  else
    Push(p,Sphere.Add(ttBinaryOp,
      [tfParent,cb
      ,tfSrcPos,Source.SrcPos
      ,tfOperator,cardinal(st)
      ,tfLeft,q
      ]));
  q:=0;
end;

procedure TStratoParser.CheckPassed(p:TStratoIndex);
var
  b:boolean;
begin
  if p<>0 then
   begin
    b:=false;
    case Sphere.t(p) of
      ttVar,ttFnCall,ttIteration,ttIterationPE,ttAssign,
      ttDeferred,ttThrow,ttCatch,ttDestructor,ttPropCall:
        b:=true;
      ttCodeBlock,ttSelection:
        b:=true;//b:=px.EvaluatesTo=0;//TODO: descend into?
      ttBinaryOp:b:=TStratoToken(Sphere.v(p,tfOperator)) in
        [stOpAssign..stOpAssignAnd];
      ttUnaryOp:b:=TStratoToken(Sphere.v(p,tfOperator)) in
        [stOpInc,stOpDec];
      //more?
    end;
    if not b then
      Source.Error('statement without calls or assignments');
   end;
end;


function TStratoParser.CbStart(pp: TStratoIndex): TStratoIndex;
begin
  //switch to ParseLogic
  //assert cb=0
  cb:=pp;
  cbInhCalled:=false;
  //more?
  Result:=pp;
end;

procedure TStratoParser.CbAdd(p:TStratoIndex);
var
  q:TStratoIndex;
begin
  if p<>0 then
   begin
    if (Sphere.r(p,tfParent)=cb) and (Sphere.r(p,tfNext)=0) then
      q:=p
    else
      //member of another chain, create an alias
      q:=Sphere.Add(ttAlias,
        [tfParent,cb
        ,tfSrcPos,Source.SrcPos
        ,tfTarget,p
        ]);
    {$IFDEF DEBUG}
    if Sphere.r(p,tfNext)<>0 then
      raise Exception.Create('broken chain detected');
    {$ENDIF}
    Sphere.Append(cb,tfFirstStatement,q);
   end;
end;

procedure TStratoParser.ParseHeader;
var
  ns,p,module:TStratoIndex;
  b:boolean;
  n:TStratoName;
  nn:UTF8String;
  SrcPos,SrcPos1:cardinal;
begin
  module:=0;
  //namespace
  SrcPos:=Source.SrcPos;
  if Source.IsNext([stIdentifier]) then
   begin
    b:=LookUpNameSpace(ns,n,SrcPos);
    while Source.IsNext([stAt]) do
      case Source.Token of
        stNumericLiteral:
         begin
          mark1:=ParseInteger(string(Source.GetID(SrcPos1)));
          mark2:=Sphere.MarkIndex(mark1);
         end;
        stIdentifier:
          if not LookUpNameSpace(module,n,SrcPos1) then
            module:=Sphere.Add(ttNameSpace,
              [tfName,n
              ,tfParent,module
              ,tf_NameSpace_SourceFile,src
              ,tfSrcPos,SrcPos1
              ]);
        else
          Source.Error('unknown namespace load modifier syntax');
      end;
    if not b then
     begin
      //create namespace
      p:=Sphere.Add(ttNameSpace,
        [tfName,n
        ,tfParent,ns
        ,tfSrcPos,SrcPos
        ]);
      if ns=0 then
        Sphere.Append(pHeader,tf_FirstNameSpace,p)
      else
        Sphere.Append(ns,tfFirstItem,p);
      ns:=p;
     end;
   end
  else
   begin
    //default: use file name
    nn:=UTF8String(ChangeFileExt(ExtractFileName(Source.FilePath),''));
      //(''''+StringReplace(Source.FilePath,'''','''''',[rfReplaceAll])+'''');?
    n:=Sphere.Dict.StrIdx(nn);
    if not(Sphere.Add(pHeader,tf_FirstNameSpace,ttNameSpace,
      [tfName,n
      //,tfParent,?
      ,tf_NameSpace_SourceFile,src
      ,tfSrcPos,SrcPos
      ],ns)) then Source.Error('duplicate namespace "'+string(nn)+'"');
   end;
  src:=Sphere.Add(ttSourceFile,
    [tf_SourceFile_FileName,Sphere.AddBinaryData(UTF8String(Source.FilePath))
    ,tf_SourceFile_FileSize,Source.FileSize
    //file date?checksum?
    ,tf_SourceFile_PartOfModule,module
    ,tf_SourceFile_SrcPosLineIndex,Source.LineIndex
    ]);
  if Sphere.r(ns,tf_NameSpace_SourceFile)=0 then
    Sphere.s(ns,tf_NameSpace_SourceFile,src);
  FNameSpace:=ns;
  SetLength(Locals,3);
  Locals[0]:=ns;
  Locals[1]:=0;//see stHRule:ttPrivate
  Locals[2]:=Sphere.r(pHeader,tf_FirstNameSpace);//runtime
end;

procedure TStratoParser.ParseDeclaration;
var
  n:TStratoName;
  nn,fqn:UTF8String;
  ns,p,q,r:TStratoIndex;
  st:TStratoToken;
  i,SrcPos:cardinal;
begin
  while (cb=0) and (rd=0) and Source.NextToken(st) do
  case st of

    stThreeLT: //import a namespace
      ParseImport;

    stIdentifier: //declaration
     begin
      //lookup
      p:=0;
      r:=0;
      ID(n,nn,SrcPos);
      fqn:=nn;
      ns:=Locals[1];
      if ns=0 then ns:=Locals[0];
      while Source.IsNext([stPeriod,stIdentifier]) do
       begin
        LookUpNext(n,nn,p,ns,r);
        ID(n,nn,SrcPos);
        fqn:=fqn+'.'+nn;
       end;
      if r<>0 then Source.Error('unknown namespace "'+string(fqn)+'"');

      //operator override?
      if Source.IsNext([stPeriod,stStringLiteral]) then
       begin
        LookUpNext(n,nn,p,ns,r);
        //ID(n,nn); but with GetStr:
        nn:=Source.GetStr;
        n:=Sphere.Dict.StrIdx(nn);
       end;

      st:=Source.Token;
      case st of

        stColon:
         begin
          case Source.Token of
            stIdentifier:
             begin
              q:=LookUpType('type');
              //property
              if Source.IsNext([stAOpen]) then
               begin
                r:=Sphere.Add(ttSignature,
                  [tfName,n//+'_get'?
                  ,tfParent,ns
                  ,tfTarget,p
                  ,tfEvaluatesTo,q
                  ,tfFirstArgument,0
                  ,tfSrcPos,SrcPos
                  ]);
                r:=StratoFnAdd(Sphere,Source,ttPropertyGet,
                  Fn(ns,n,nn,SrcPos),r,src,SrcPos);
                if Source.IsNext([stAClose]) then
                 begin
                  //forward only
                  if Source.IsNext([stAOpen,stAClose]) then //empty setter also? skip
                    Source.Skip(stAClose);
                  //TODO: check declared somewhere later
                 end
                else
                  Sphere.s(r,tfBody,CbStart(
                    StratoFnCodeBlock(Sphere,r,ns,q,n,Source.SrcPos)));
                p:=0;
               end
              else
              //class
              if Source.IsNext([stDefine,stAOpen]) then
               begin
                if q=0 then
                  Source.Error('undeclared base class')
                else
                if Sphere.t(q)<>ttClass then
                  Source.Error('base class must be a class');
                if not Sphere.Add(ns,tfFirstItem,ttClass,
                  [tfName,n
                  ,tfParent,ns
                  ,tfSrcPos,SrcPos
                  ,tfInheritsFrom,q
                  ],p) then
                  Source.Error('duplicate identifier "'+nn+'"');
                if q<>0 then Sphere.s(p,tfByteSize,Sphere.v(q,tfByteSize));
                Source.Skip(stAOpen);
                rd:=p;//switch to ParseRecord
               end
              else
              //variable
               begin
                if not Sphere.Add(ns,tfFirstItem,ttVar,
                  [tfName,n
                  ,tfParent,ns
                  ,tfSrcPos,SrcPos
                  ,tfEvaluatesTo,q
                  ],p) then
                  Source.Error('duplicate identifier "'+nn+'"');
                if Source.IsNext([stDefine]) then
                  Sphere.s(p,tfInitialValue,ParseLiteral(Source.Token));
                //TODO: check InitialValue.EvaluatesTo with EvaluatesTo
                if ns<>0 then
                 begin
                  case Sphere.t(ns) of
                    ttNameSpace:
                      Sphere.AddGlobalVar(p);//sets px.Offset
                    ttClass,ttRecord:
                     begin
                      //TODO: support @ offset
                      i:=Sphere.v(ns,tfByteSize);
                      Sphere.s(p,tfOffset,i);
                      Sphere.s(ns,tfBytesize,i+ByteSize(Sphere,q));
                     end;
                    else
                      Source.Error('unexpected variable parent');
                  end;
                 end;
               end;
              if cb=0 then Source.Skip(stSemiColon);
             end;
            //more?
            else
             begin
              Source.Error('unsupported type syntax');
              //ns.Add(n, anyway? placeholder?
             end;
          end;
         end;

        stDefine://type, constant or enum
          if Source.IsNext([stPOpen,stIdentifier]) then
           begin
            if not Sphere.Add(ns,tfFirstItem,ttEnumeration,
              [tfName,n
              ,tfParent,ns
              ,tfSrcPos,SrcPos
              ,tfByteSize,SystemWordSize
              ],p) then
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
                  case Sphere.t(p) of
                    ttVar,ttConstant://initial value from var
                      if Sphere.r(p,tfInitialValue)=0 then
                        Source.Error('constant without value')
                      else
                        if not Sphere.Add(ns,tfFirstItem,ttConstant,
                          [tfName,n
                          ,tfParent,ns
                          ,tfSrcPos,SrcPos
                          ,tfInitialValue,Sphere.r(p,tfInitialValue)
                          ,tfEvaluatesTo,Sphere.r(
                            Sphere.r(p,tfInitialValue),tfEvaluatesTo)
                          ],q) then
                          Source.Error('duplicate identifier "'+nn+'"');
                    ttLiteral://constant
                      if not Sphere.Add(ns,tfFirstItem,ttConstant,
                        [tfName,n
                        ,tfParent,ns
                        ,tfSrcPos,SrcPos
                        ,tfInitialValue,p
                        ,tfEvaluatesTo,Sphere.r(p,tfEvaluatesTo)
                        ],q) then
                        Source.Error('duplicate identifier "'+nn+'"');
                    ttTypeDecl,ttRecord,ttEnumeration:
                      if Source.IsNext([stBOpen]) then //array
                        if Source.IsNext([stBClose]) then //dyn array
                         begin
                          Source.Error('//TODO: dyn arrays');
                         end
                        else
                         begin
                          i:=cardinal(ParseLiteral(Source.Token,true));
                          //TODO: multidimensional arrays, array of array
                          Source.Skip(stBClose);//TODO: force
                          if not Sphere.Add(ns,tfFirstItem,ttArray,
                            [tfName,n
                            ,tfParent,ns
                            ,tfSrcPos,SrcPos
                            ,tfSubject,p//element type
                            ],q) then
                            Source.Error('duplicate identifier "'+nn+'"')
                          else
                            if p<>0 then
                              Sphere.s(q,tfByteSize,ByteSize(Sphere,p)*i);
                         end
                      else //type alias
                       begin
                        if not Sphere.Add(ns,tfFirstItem,ttAlias,
                          [tfName,n
                          ,tfParent,ns
                          ,tfSrcPos,SrcPos
                          ,tfTarget,p
                          ],q) then
                          Source.Error('duplicate identifier "'+nn+'"');
                       end;
                    else
                      Source.Error('unsupported type or constant reference');
                  end;
               end;
              stStringLiteral,stNumericLiteral,
              stBOpen,stPOpen,stOpSizeOf://constant
               begin
                q:=ParseLiteral(st);
                if not Sphere.Add(ns,tfFirstItem,ttConstant,
                  [tfName,n
                  ,tfParent,ns
                  ,tfSrcPos,SrcPos
                  ,tfInitialValue,q
                  ],p) then
                  Source.Error('duplicate identifier "'+nn+'"');
                if q<>0 then
                 begin
                  if Source.IsNext([stColon,stIdentifier]) then //here or in ParseLiteral?
                   begin
                    r:=LookUpType('literal type');
                    if r=0 then
                      Source.Error('unknown literal type')
                    else
                      Sphere.s(q,tfEvaluatesTo,r);
                   end;
                  Sphere.s(p,tfEvaluatesTo,Sphere.r(q,tfEvaluatesTo));
                 end;
               end;
              //stPOpen://enumeration: see above
              stAOpen://record (aka struct)
               begin
                if not Sphere.Add(ns,tfFirstItem,ttRecord,
                  [tfName,n
                  ,tfParent,ns
                  ,tfSrcPos,SrcPos
                  ],p) then
                  Source.Error('duplicate identifier "'+nn+'"');
                rd:=p;//switch to ParseRecord
               end;
              stCaret:
               begin
                p:=LookUpType('pointer type');
                if p=0 then
                 begin
                  Source.Error('unknown pointer type');
                  p:=Sphere.Add(ttTypeDecl,
                    [tfName,n
                    ,tfParent,ns
                    ,tfSrcPos,SrcPos
                    ]);
                 end;
                if not Sphere.Add(ns,tfFirstItem,ttPointer,
                  [tfName,n
                  ,tfParent,ns
                  ,tfSrcPos,SrcPos
                  ,tfByteSize,SystemWordSize
                  ,tfEvaluatesTo,p
                  ],q) then
                  Source.Error('duplicate identifier "'+nn+'"');
               end;
              stQuestionMark:
               begin
                p:=LookUpType('class reference type');
                if p=0 then
                  Source.Error('unknown class reference type')
                else
                  if Sphere.t(p)<>ttClass then
                   begin
                    Source.Error('invalid class reference subject');
                    p:=0;
                   end;
                if not Sphere.Add(ns,tfFirstItem,ttClassRef,
                  [tfName,n
                  ,tfParent,ns
                  ,tfSrcPos,SrcPos
                  ,tfByteSize,SystemWordSize
                  ,tfEvaluatesTo,p
                  ],q) then
                  Source.Error('duplicate identifier "'+nn+'"');
               end;
{
              stOpSizeOf:
               begin
                p:=Sphere.AddTo(Sphere[ns].FirstItem,ttConstant,n,px);
                if p=0 then
                 begin
                  Source.Error('duplicate identifier "'+nn+'"');
                  p:=Sphere.Add(ttConstant,px);
                  px.Name:=n;
                 end;
                px.Parent:=ns;
                px.SrcPos:=SrcPos;
                px.EvaluatesTo:=TypeDecl_Number;
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
                  q:=Sphere.Add(ttLiteral,qx);
                  qx.SrcPos:=SrcPos;
                  qx.EvaluatesTo:=TypeDecl_number;
                  qx.InitialValue:=Sphere.AddBinaryData(IntToStr(i));
                  px.InitialValue:=q;
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
              q:=Sphere.Lookup(ns,tfFirstItem,n);
              if q=0 then //just a signature? add to namespace
                Sphere.Append(ns,tfFirstItem,p)
              else
                case Sphere.t(q) of
                  ttMember:
                    StratoFnAdd(Sphere,Source,ttOverload,q,p,src,SrcPos);
                  ttSignature:
                   begin
                    //another forward signature? create ttMember here
                    r:=Sphere.Add(ttMember,
                      [tfName,n
                      ,tfParent,ns
                      ,tfSrcPos,SrcPos
                      ]);
                    ReplaceNode(Sphere,ns,q,r);
                    StratoFnAdd(Sphere,Source,ttOverload,r,q,src,Sphere.v(q,tfSrcPos));
                    StratoFnAdd(Sphere,Source,ttOverload,r,p,src,SrcPos);
                   end
                  else
                    Source.Error('duplicate identifier "'+nn+'"');
                end;
             end;
            stAOpen://code block
             begin
              q:=Sphere.Lookup(ns,tfFirstItem,n);
              if q=0 then
                p:=StratoFnAdd(Sphere,Source,ttOverload,
                  Fn(ns,n,nn,SrcPos),p,src,SrcPos)
              else
                case Sphere.t(q) of
                  ttMember:
                    p:=StratoFnAdd(Sphere,Source,ttOverload,q,p,src,SrcPos);
                  ttSignature://signature forwarded, replace with ttMember
                   begin
                    r:=Fn(ns,n,nn,SrcPos);
                    ReplaceNode(Sphere,ns,q,r);
                    //StratoFnAdd checks for SameType(p,q):
                    StratoFnAdd(Sphere,Source,ttOverload,r,q,
                      src,Sphere.v(q,tfSrcPos));
                    p:=StratoFnAdd(Sphere,Source,ttOverload,r,p,src,SrcPos);
                   end;
                  ttClass://constructor
                   begin
                    //Sphere.s(p,tfEvaluatesTo,q);
                    Sphere.s(p,tfTarget,q);
                    p:=StratoFnAdd(Sphere,Source,ttConstructor,q,p,src,SrcPos);
                   end;
                  else
                   begin
                    Source.Error('duplicate identifier "'+nn+'"');
                    q:=Sphere.Add(ttMember,
                      [tfName,n
                      ,tfParent,ns
                      ,tfSrcPos,SrcPos
                      ]);
                    p:=StratoFnAdd(Sphere,Source,ttOverload,q,p,src,SrcPos);
                   end;
                end;
              if p<>0 then CbStart(StratoFnOvlCodeBlock(Sphere,Source,p));
              p:=0;
             end;
            else Source.Error('unsupported signature syntax');
          end;
         end;

        stOpAssign://":="
          if Source.IsNext([stAOpen]) then
           begin
            //accept only one object:={}
            if not Sphere.Add(ns,tfFirstItem,ttClass,
              [tfName,n
              ,tfParent,ns
              ,tfSrcPos,Source.SrcPos
              ],p) then
              Source.Error('duplicate identifier');
            if TypeDecl_object=0 then
             begin
              TypeDecl_object:=p;
              Name_Inherited:=Sphere.Dict.StrIdx('@@@');
             end
            else
              Source.Error('only one master base class allowed');
            rd:=p;//switch to ParseRecord
           end
          else
            Source.Error('unsupported declaration syntax');

        stBOpen:
         begin
          q:=StratoFnAdd(Sphere,Source,ttPropertyGet,Fn(ns,n,nn,SrcPos),
            ParseSignature(ns,nn,stBClose,SrcPos),src,SrcPos);
          if Source.IsNext([stAOpen]) then
            Sphere.s(q,tfBody,CbStart(StratoFnOvlCodeBlock(Sphere,Source,q)));
         end;

        //stAOpen:?

        else
          Source.Error('unexpected stray identifier');
      end;
     end;

    stStringLiteral,stNumericLiteral:
     begin
      Source.Error('unexpected literal');
      ParseLiteral(st);
     end;

    stQuestionMark: //interface
     begin
      p:=0;
      r:=0;
      ID(n,nn,SrcPos);
      fqn:=nn;
      ns:=Locals[1];
      if ns=0 then ns:=Locals[0];
      while Source.IsNext([stPeriod,stIdentifier]) do
       begin
        LookUpNext(n,nn,p,ns,r);
        ID(n,nn,SrcPos);
        fqn:=fqn+'.'+nn;
       end;
      if r<>0 then Source.Error('unknown namespace "'+string(fqn)+'"');

      st:=Source.Token;
      case st of
        stPOpen:
          if Source.IsNextID([stPClose,stAOpen]) or
            Source.IsNextID([stPClose,stDefine,stAOpen]) then
           begin //inherit this interface
            if Source.IsNext([stIdentifier]) then
              q:=LookUpID
            else
              q:=0;
            if q=0 then
              Source.Error('undeclared base interface');
            Source.Skip(stPClose);
            Source.Skip(stDefine);//if?
            Source.Skip(stAOpen);
            if not Sphere.Add(ns,tfFirstItem,ttInterface,
              [tfName,n
              ,tfParent,ns
              ,tfSrcPos,Source.SrcPos
              ,tfByteSize,SystemWordSize
              ,tfInheritsFrom,q
              ],p) then
              Source.Error('duplicate identifier');
            ParseInterfaceDecl(p);
           end;
        stAOpen:
         begin
          if not Sphere.Add(ns,tfFirstItem,ttInterface,
            [tfName,n
            ,tfParent,ns
            ,tfSrcPos,Source.SrcPos
            ,tfByteSize,SystemWordSize
            ],p) then
            Source.Error('duplicate identifier "'+nn+'"');
          ParseInterfaceDecl(p);
         end;
        else
          Source.Error('unsupported interface syntax');
      end;
     end;

    stOpSub,stTilde://'-','~': destructor?
      if Source.IsNextID([stPOpen,stPClose,stAOpen]) then
       begin
        //lookup
        p:=0;
        r:=0;
        ID(n,nn,SrcPos);
        fqn:=nn;
        ns:=Locals[1];
        if ns=0 then ns:=Locals[0];
        while Source.IsNext([stPeriod,stIdentifier]) do
         begin
          LookUpNext(n,nn,p,ns,r);
          ID(n,nn,SrcPos);
          fqn:=fqn+'.'+nn;
         end;
        if r<>0 then Source.Error('unknown namespace "'+string(fqn)+'"');
        //ParseSignature? destructor doesn't have arguments/overloads
        Source.Skip(stPOpen);
        Source.Skip(stPClose);
        Source.Skip(stAOpen);
        //find class destructor is for
        q:=Sphere.Lookup(ns,tfFirstItem,n);
        if q=0 then
         begin
          Source.Error('destructor for unknown class');
          q:=Sphere.Add(ttClass,[tfName,n]);
         end
        else
          if Sphere.t(q)<>ttClass then
           begin
            Source.Error('destructor only supported on class');
            q:=Sphere.Add(ttClass,[tfName,n]);
           end;
        //check any destructor already
        r:=Sphere.r(q,tfFirstItem);
        while (r<>0) and (Sphere.t(r)<>ttDestructor) do
          r:=Sphere.r(r,tfNext);
        if r<>0 then
          Source.Error('duplicate destructor');
        //add
        p:=Sphere.Add(ttDestructor,
          [tfName,0//n?
          ,tfParent,q
          ,tfSrcPos,SrcPos
          ,tfSignature,Sphere.Add(ttSignature,
            [tfName,n
            ,tfParent,q
            ,tfSrcPos,SrcPos
            ,tfTarget,q
            ])
          ]);
        if r=0 then Sphere.Append(q,tfFirstItem,p);
        Sphere.s(p,tfBody,CbStart(
          StratoFnCodeBlock(Sphere,p,q,0,0,Source.SrcPos)));
        p:=0;
       end
      else
        Source.Error('unexpected token');

    stAOpen:
     begin
      SrcPos:=Source.SrcPos;
      ns:=Locals[0];
      cb:=Sphere.Add(ttCodeBlock,
        [tfParent,ns
        ,tfSrcPos,SrcPos
        ]);
      if Sphere.r(src,tf_SourceFile_InitializationCode)=0 then
       begin
        Sphere.s(src,tf_SourceFile_InitializationCode,cb);
        Sphere.Append(pHeader,tf_FirstInitialization,cb);
       end
      else
      if Sphere.r(src,tf_SourceFile_FinalizationCode)=0 then
       begin
        Sphere.s(src,tf_SourceFile_FinalizationCode,cb);
        Sphere.Prepend(pHeader,tf_FirstFinalization,cb);
       end
      else
        Source.Error('Initialization and finalization code already declared.');
      p:=0;
     end;

    stHRule:
      if Locals[1]=0 then
       begin
        ns:=Locals[0];
        Locals[1]:=Sphere.Add(ns,tfFirstItem,ttPrivate,
          [tfName,0
          ,tfParent,ns
          ,tfSourceFile,src
          //,tfFirstItem:=//see Lookup
          ,tfTarget,ns
          ,tfSrcPos,Source.SrcPos
          ]);
       end
      else
        Source.Error('already in private visibility');

    stSemiColon://;//stray semicolon? ignore
      if Source.IsNext([stSemiColon,stSemiColon]) then
       begin
        asm int 3 end;//for debugging the parser
        Source.Skip(stSemiColon);
        Source.Skip(stSemiColon);
       end;

    //stPOpen?

    st_Unknown:Source.Error('unknown token');
    else Source.Error('unexpected token');
  end;
end;

procedure TStratoParser.ParseRecord;
var
  p,q,r:TStratoIndex;
  offset,i,j,s:cardinal;
  st:TStratoToken;
  b,neg:boolean;
  fn:UTF8String;
  n:TStratoName;
  SrcPos,SrcPos1:cardinal;
begin
  while (cb=0) and (rd<>0) and Source.NextToken(st) do
  case st of

    stIdentifier:
     begin
      offset:=OffsetUseDefault;//default
      p:=0;//default
      fn:=Source.GetID(SrcPos);
      if Source.IsNext([stColon]) then
        case Source.Token of
          stIdentifier:
            p:=LookUpType('field type');
          stQuestionMark:
           begin
            q:=LookUpType('field type');
            p:=Sphere.Add(ttClassRef,
              [tfParent,rd
              ,tfByteSize,SystemWordSize
              ,tfEvaluatesTo,q
              ,tfSrcPos,SrcPos
              ]);
           end;
          stAOpen:
           begin
            p:=Sphere.Add(ttRecord,
              [tfName,Sphere.Dict.StrIdx(fn)
              ,tfParent,rd
              ,tfSrcPos,SrcPos
              ]);
            rd:=p;//push? see stAClose below
            //TODO: add struct/typedecl itself to something? x?ns?
           end;
          //more?
          else Source.Error('unsupported record field type syntax');
        end;
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
              if not TryStrToInt(string(Source.GetID(SrcPos1)),integer(i)) then
                Source.Error('record field offset not an integer');
            stOpSub:neg:=true;
            stOpAdd:neg:=false;
            stIdentifier:
             begin
              q:=Sphere.Lookup(rd,tfFirstItem,
                Sphere.Dict.StrIdx(Source.GetID(SrcPos1)));
              if q=0 then
                Source.Error('record field not found')
              else
                i:=Sphere.v(q,tfOffset);
             end;
            stOpSizeOf:
              if Source.NextToken(st) then
                case st of
                  stIdentifier:
                    i:=ByteSize(Sphere,LookUpType('offset type'));
                  stNumericLiteral:
                   begin
                    Source.Skip(st);//Source.GetID;
                    i:=SystemWordSize;//ByteSize(Sphere,TypeDecl_number);
                   end;
                  else Source.Error('invalid record field offset syntax');
                end;
            //stSemiColon:b:=false; else Source.Error?
            else b:=false;
          end;
          if Source.IsNext([stOpMul,stNumericLiteral]) then
            if TryStrToInt(string(Source.GetID(SrcPos1)),integer(j)) then
              i:=i*j
            else
              Source.Error('record field offset factor not an integer');
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
      //TODO: else if Source.IsNext([stAOpen]) then tt:=ttProperty?
      Source.Skip(stSemiColon);

      //register field with record
      n:=Sphere.Dict.StrIdx(fn);
      if not Sphere.Add(rd,tfFirstItem,ttVar,
        [tfName,n
        ,tfParent,rd
        ,tfEvaluatesTo,p
        ],r) then
        Source.Error('duplicate record field "'+fn+'"');
      if p=0 then s:=0 else s:=ByteSize(Sphere,p);
      if offset=OffsetUseDefault then
       begin
        i:=Sphere.v(rd,tfByteSize);
        Sphere.s(r,tfOffset,i);
        Sphere.s(rd,tfByteSize,i+s);
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
          if i>Sphere.v(rd,tfByteSize) then Sphere.s(rd,tfByteSize,i);
         end;
        Sphere.s(r,tfOffset,offset);
       end;
      Sphere.s(r,tfSrcPos,SrcPos);
     end;

    //stQuestionMark: nested interface?
    //more?

    stAClose:
     begin
      //'pop'
      p:=Sphere.r(rd,tfParent);
      if Sphere.t(p)=ttRecord then rd:=p else rd:=0;
     end;

    stPOpen:Source.Error('unsupported record field syntax, declare methods outside of data section');

    else Source.Error('unsupported record field syntax');
  end;
end;

procedure TStratoParser.ParseLogic;
var
  n:TStratoName;
  nn,fqn:UTF8String;
  p,q,r:TStratoIndex;
  st:TStratoToken;
  i,j,k,SrcPos,bi:cardinal;
begin
  SrcPos:=Source.SrcPos;//default
  {$IFDEF DEBUG}
  bi:=Sphere.MarkIndex(40000);
  {$ELSE}
  bi:=Sphere.MarkIndex($40000);
  {$ENDIF}
  while (cb<>0) and Source.NextToken(st) do
  case st of

    stIdentifier:
     begin
      Juxta(p);
      ID(n,nn,SrcPos);
      fqn:=nn;
      while Source.IsNext([stPeriod,stIdentifier]) do
       begin
        CodeLookup(n,p,SrcPos);
        if p=0 then
         begin
          Source.Error('undeclared identifier "'+string(fqn)+'"');
          //TODO: silence further errors
          p:=Sphere.Add(ttNameSpace,//silence further errors
            [tfName,Sphere.Dict.StrIdx('!!!'+nn)//n
            ,tf_NameSpace_SourceFile,src
            ,tfSrcPos,SrcPos
            ]);
         end;
        ID(n,nn,SrcPos);
        fqn:=fqn+'.'+nn;
       end;
      r:=p;
      CodeLookup(n,p,SrcPos);
      if (p<>0) and (Sphere.t(p)=ttNameSpace) then p:=0;
      if p=0 then
       begin
        //check variable object method/field pointer
        //TODO: this recursive??!! (via stack!)
        if r<>0 then
         begin
          q:=r;
          CodeLookup(n,q,SrcPos);
          while Source.IsNext([stPeriod,stIdentifier]) do
           begin
            ID(n,nn,SrcPos);
            fqn:=fqn+'.'+nn;
            if p<>0 then CodeLookup(n,q,SrcPos);
           end;
          if q<>0 then
            p:=Sphere.Add(ttField,
              [tfParent,cb
              ,tfSubject,r
              ,tfSrcPos,SrcPos
              ,tfTarget,q
              //,tfEvaluatesTo,ResType(p)
              ]);
         end;
        //really found nothing?
        if p=0 then
          if (stackIndex<>0) and (stack[stackIndex-1].p=pUnTypedVar) then
           begin
            if not Sphere.Add(cb,tfFirstItem,ttVar,
              [tfName,n
              ,tfParent,cb
              ,tfSrcPos,SrcPos
              ,tfOffset,Sphere.v(cb,tfByteSize)
              ],p) then
              Source.Error('duplicate identifier "'+string(nn)+'"');
            Push(pUnTypedVar,p);//see stColon below
           end
          else
           begin
            Source.Error('undeclared identifier "'+string(fqn)+'"');
            p:=Sphere.Add(ttVar,[tfName,n]);//silence further errors
           end;
       end;
      if p<>0 then
        case Sphere.t(p) of
          ttField:
            if Sphere.t(Sphere.r(p,tfTarget))=ttMember then
              Push(pUnresolved,p);
          ttMember:
            Push(pUnresolved,Sphere.Add(ttAlias,//ttField?
              [tfParent,cb
              ,tfTarget,p
              ,tfSrcPos,SrcPos
              ]));
        end;
     end;

    stPeriod://"."
     begin
      ID(n,nn,SrcPos);
      fqn:=Sphere.FQN(p)+'.'+nn;
      while Source.IsNext([stPeriod,stIdentifier]) do
       begin
        CodeLookup(n,p,SrcPos);
        if p=0 then
         begin
          Source.Error('undeclared identifier "'+string(fqn)+'"');
          //TODO: silence further errors
          p:=Sphere.Add(ttNameSpace,//silence further errors
            [tfName,Sphere.Dict.StrIdx('!!!'+nn)//n
            ,tf_NameSpace_SourceFile,src
            ,tfSrcPos,SrcPos
            ]);
         end;
        ID(n,nn,SrcPos);
        fqn:=fqn+'.'+nn;
       end;
      CodeLookup(n,p,SrcPos);
      if p=0 then
       begin
        Source.Error('undeclared identifier "'+string(fqn)+'"');
        p:=Sphere.Add(ttVar,[tfName,n]);//silence further errors
       end;
     end;

    stStringLiteral:
     begin
      Juxta(p);
      p:=Sphere.Add(ttLiteral,
        [tfParent,cb
        ,tfSrcPos,Source.SrcPos
        ,tfEvaluatesTo,TypeDecl_string
        ]);
      if (stackIndex<>0) and ((stack[stackIndex-1].p=pIfThen)
        or (stack[stackIndex-1].p=pIfElse)) then
        Sphere.s(p,tfInitialValue,Sphere.AddBinaryData(Source.GetStr))
      else
        Sphere.s(p,tfInitialValue,Sphere.AddBinaryData(Source.GetStrs));
     end;
    stNumericLiteral:
     begin
      Juxta(p);
      p:=Sphere.Add(ttLiteral,
        [tfParent,cb
        ,tfSrcPos,Source.SrcPos
        ,tfInitialValue,Sphere.AddBinaryData(Source.GetID(SrcPos))
        ]);
      if Source.IsNext([stColon,stIdentifier]) then
        q:=LookUpType('literal type',true)
      else
        q:=TypeDecl_number;
      Sphere.s(p,tfEvaluatesTo,q);
      if q=0 then Source.Error('unknown literal type');
     end;

    stColon:
      if p=0 then
        if Source.IsNext([stIdentifier]) and
          not(Source.IsNext([stIdentifier,stPeriod])) then
         begin
          ID(n,nn,SrcPos);
          if not Sphere.Add(cb,tfFirstItem,ttVar,
            [tfName,n
            ,tfParent,cb
            ,tfSrcPos,SrcPos
            ,tfOffset,Sphere.v(cb,tfByteSize)
            ],p) then
            Source.Error('duplicate identifier "'+string(nn)+'"');
          Push(pUnTypedVar,p);//see stColon below
         end
        else
          Source.Error('no value to cast')
      else
       begin
        //Combine(p_ArgList_Item,p);
        if (stackIndex<>0) and (stack[stackIndex-1].p=pUnTypedVar) then //local declaration(s)?
         begin
          p:=0;
          q:=LookUpType('type',true);
          i:=stackIndex;
          while (i<>0) and (stack[i-1].p=pUnTypedVar) do dec(i);
          if stackIndex<>i then
           begin
            j:=i;
            while i<stackIndex do
             begin
              r:=stack[i].t;//assert ttVar
              if p=0 then p:=r;
              Sphere.s(r,tfEvaluatesTo,q);//assert was 0
              if q<>0 then
               begin
                k:=Sphere.v(cb,tfByteSize);
                Sphere.s(r,tfOffset,k);
                Sphere.s(cb,tfByteSize,k+ByteSize(Sphere,q));
               end;
              {$IFDEF DEBUG}
              stack[i].p:=p___;
              stack[i].t:=0;
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
            Source.Error('can''t cast value "'+Sphere.FQN(p)+'" '+
              IntToHex(Sphere.t(p),4));
          p:=Sphere.Add(ttCast,
            [tfParent,cb
            ,tfSrcPos,Source.SrcPos
            ,tfTarget,p
            ,tfEvaluatesTo,LookUpType('cast type',true)
            ]);
         end;
       end;

    stPOpen:
      if p=0 then
        Push(pParentheses,0)
      else
       begin
        //Combine here?
        //start an argument list? (and push pArgList below)?
        r:=p;
        if (p<>0) and (Sphere.t(p)=ttCast) then
          p:=Sphere.r(p,tfEvaluatesTo);
        if p<>0 then
          case Sphere.t(p) of
            ttMember:;//overload
            ttClass:;//constructor
            ttField:;//property
            ttVar:
              case Sphere.t(Sphere.r(p,tfEvaluatesTo)) of
                ttClassRef:;//dynamic constructor
                ttPointer:
                  if Sphere.t(Sphere.r(Sphere.r(
                    p,tfEvaluatesTo),tfEvaluatesTo))<>ttSignature then r:=0;
                else r:=0;
              end;
            //TODO: ttThis?
            //TODO: ttClassRef: only class methods
            //TODO: dedicated function GivesSignature
            else r:=0;
          end;
        if r<>0 then
          Push(pArgList,Sphere.Add(ttFnCall,
            [tfName,n//TODO: from px?
            ,tfParent,cb
            ,tfSrcPos,SrcPos//Source.SrcPos
            ,tfTarget,r
            ]))
        else
        //start a selection?
        if SameType(Sphere,ResType(Sphere,p),TypeDecl_bool) then
          Push(pIfThen,Sphere.Add(ttSelection,
            [tfParent,cb
            ,tfSrcPos,Source.SrcPos
            ,tfDoIf,p
            ]))
        else
        //nothing found!
         begin
          if (stackIndex<>0) and (stack[stackIndex-1].p=pUnTypedVar) and
            (stack[stackIndex-1].t=p) then
           begin
            Source.Error('unknown function "'+
              string(Sphere.Dict[Sphere.v(p,tfName)])+'"');
            dec(stackIndex);
           end
          else
            Source.Error('function expected "'+string(Sphere.FQN(p))+'"');
          //create one to silence errors
          Push(pArgList,Sphere.Add(ttFnCall,
            [tfName,n//?
            ,tfParent,cb
            ,tfSrcPos,SrcPos//Source.SrcPos
            ]));
         end;
        p:=0;
       end;
    stPClose:
      if stackIndex=0 then
        Source.Error('unexpected token')
      else
        Combine(pParentheses,p);

    stComma:
     begin
      Combine(p_ArgList_Item,p);
      if (stackIndex=0) or (p=0) then
        Source.Error('unexpected token')
      else
      if (stack[stackIndex-1].p in [pArgList,pBrackets]) then
        //assert Sphere[p].ThingType=ttFnCall,ttField,ttArrayIndex,ttPropCall
        StratoFnCallAddArgument(Sphere,stack[stackIndex-1].t,p)
      else
      if stack[stackIndex-1].p=pUnTypedVar then
       begin
        if p<>stack[stackIndex-1].t then
          if (Sphere.t(p)=ttAssign) and
            (TStratoToken(Sphere.v(p,tfOperator))=stOpAssign) then
            CbAdd(p)
          else
            Source.Error('unexpected syntax in local variable declaration');
       end
      else
        Source.Error('unsupported syntax');
      p:=0;
     end;

    stAOpen:
      if Source.IsNext([stIdentifier,stColon]) or
        Source.IsNext([stStringLiteral,stColon]) or
        Source.IsNext([stNumericLiteral,stColon]) then
       begin
        //TODO: JSON
        Source.Error('//TODO:JSON');
       end
      else
       begin
        Combine(p_Juxta,p);
        //check part of &({}{}) syntax
        if (stackIndex<>0) and (stack[stackIndex-1].p=pForCrit) then
         begin
          stack[stackIndex-1].p:=pForThen;
          Sphere.s(stack[stackIndex-1].t,tfDoIf,p);
          if not SameType(Sphere,ResType(Sphere,p),TypeDecl_bool) then
            Source.Error('iteration criterium does not evaluate to boolean');
          p:=0;
         end
        else
          Juxta(p);
        //start code block
        Push(pCodeBlock,cb);
        cb:=Sphere.Add(ttCodeBlock,
          [tfParent,cb
          ,tfSrcPos,Source.SrcPos
          ]);
        p:=0;
       end;
    stAClose:
     begin
      Combine(p_Statement,p);
      if p<>0 then
       begin
        //resulting value
        q:=Sphere.r(cb,tfParent);
        r:=Sphere.rr(q,[tfTarget,tfEvaluatesTo]);//ttSignature
        if (Sphere.t(q) in [ttOverload,ttPropertyGet]) and (r<>0) then
         begin
          if not SameType(Sphere,ResType(Sphere,p),r) then
            Source.Error('result value type mismatch');
          //assign to result value
          p:=Sphere.Add(ttAssign,
            [tfOperator,cardinal(stOpAssign)
            ,tfParent,cb
            ,tfSrcPos,Source.SrcPos
            ,tfAssignTo,Sphere.Lookup(p,tfFirstItem,Sphere.v(q,tfName))//ttMember
            ,tfValueFrom,p
            ]);
          //assert p.AssignTo<>0
         end
        else
         begin
          CheckPassed(p);
          Sphere.s(p,tfEvaluatesTo,ResType(Sphere,p));
         end;
        CbAdd(p);
       end;
      while (stackIndex<>0) and (stack[stackIndex-1].p<>pCodeBlock) do
       begin
        dec(stackIndex);
        if stack[stackIndex].p=pIfElse then
          Source.Error('if-then without else')
        else
          Source.Error('unexpected incomplete syntax');//+??[stack[stackIndex].p]);
        {$IFDEF DEBUG}
        stack[stackIndex].p:=p___;
        stack[stackIndex].t:=0;
        {$ENDIF}
       end;
      if stackIndex=0 then
       begin
        //return to declarations
        p:=cb;
        cb:=0;//clear here for any CbStart below

        //code block done: checks
        r:=Sphere.r(p,tfParent);
        if r<>0 then
         begin
          //constructor block done? check inherited called
          if (Sphere.t(r)=ttConstructor) and not(cbInhCalled)
            and (Sphere.rr(r,[tfParent,tfParent])<>TypeDecl_object) then
           begin
            r:=StratoFnCallFindInherited(Sphere,ttConstructor,
              Sphere.r(r,tfParent),r,0);
            if (r=0) and (Sphere.r(r,tfFirstArgument)<>0) then
              //default to inherited constructor without arguments
              r:=StratoFnCallFindInherited(Sphere,ttConstructor,
                Sphere.r(r,tfParent),0,0);
            if r=0 then
              Source.Error('unable to find base constructor')
            else
             begin
              q:=Sphere.Add(ttFnCall,
                [tfName,Name_Inherited
                ,tfParent,p
                ,tfSrcPos,Source.SrcPos
                ,tfTarget,r //no ttField here, no instance yet!
                ]);
              //arguments
              StratoFnArgByValues(Sphere,q,
                Sphere.r(r,tfFirstArgument),
                Sphere.rr(p,[tfParent,tfFirstArgument]));
              //insert first into code block
              Sphere.s(q,tfNext,Sphere.r(p,tfFirstStatement));
              Sphere.s(p,tfFirstStatement,q);
             end;
           end
          else

          //destructor block done? check inherited called
          if (Sphere.t(r)=ttDestructor) and not(cbInhCalled)
            and (Sphere.r(r,tfParent)<>TypeDecl_object) then
           begin
            r:=StratoFnCallFindInherited(Sphere,ttDestructor,
              Sphere.r(r,tfParent),0,0);//,r.FirstArgument,0);
            if r=0 then
              Source.Error('unable to find base destructor')
            else
              //append to code block
              Sphere.Append(p,tfFirstStatement,Sphere.Add(ttFnCall,
                [tfName,Name_Inherited
                ,tfParent,p
                ,tfSrcPos,Source.SrcPos
                ,tfTarget,Sphere.Add(ttField,
                  [tfParent,p
                  ,tfSubject,Sphere.r(p,tfFirstItem)//assert ttThis
                  ,tfSrcPos,Source.SrcPos
                  ,tfTarget,r
                  ])
                //,tfFirstArgument,?
                ]));
           end
          else

          //property getter done? parse setter
          if Sphere.t(r)=ttPropertyGet then
           begin
            //TODO: if not(cbInhCalled)
            if Source.IsNext([stAOpen]) then
             begin
              //TODO: construct setter signature? (use the same for now)
              q:=StratoFnAdd(Sphere,Source,ttPropertySet,
                Sphere.r(r,tfParent),Sphere.r(r,tfSignature),
                src,Source.SrcPos);//rx.Parent.SrcPos?
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
        p:=cb;
        dec(stackIndex);
        //assert stack[stackIndex].p=pCodeBlock
        cb:=stack[stackIndex].t;
        {$IFDEF DEBUG}
        stack[stackIndex].p:=p___;
        stack[stackIndex].t:=0;
        {$ENDIF}

        //check part of &({}{}) syntax:
        if stackIndex<>0 then
          case stack[stackIndex-1].p of
            pForFirst:
             begin
              //CheckPassed(p);
              stack[stackIndex-1].p:=pForCrit;
              //DoFirst
              Sphere.s(stack[stackIndex-1].t,tfDoElse,p);//assert was 0
              //re-link declared items //TODO: detect duplicate ids?
              //assert px.ThingType=ttCodeBlock
              MoveChain(Sphere,p,cb);
              Sphere.s(cb,tfByteSize,
                Sphere.v(cb,tfByteSize)+Sphere.v(p,tfByteSize));
              Sphere.s(p,tfByteSize,0);
              p:=0;
             end;
            //pForCrit,pForCritOnly:assert never
            pForThen:
             begin
              stack[stackIndex-1].p:=pForCritDone;
              Sphere.s(stack[stackIndex-1].t,tfDoThen,p);//assert was 0
              p:=0;
             end;
            pForBodyFirst:
             begin
              if (p<>0) and (Sphere.t(p)=ttCodeBlock)
                and (Sphere.r(p,tfEvaluatesTo)<>0) then
                Source.Error('unexpected iteration body with return value');
              if Source.IsNext([stPOpen]) then //parentheses?
                stack[stackIndex-1].p:=pForFirst
              else //assert boolean expression
                stack[stackIndex-1].p:=pForCritOnly;
              Sphere.s(stack[stackIndex-1].t,tfBody,p);//assert was 0
              //assert px.ThingType=ttCodeBlock
              MoveChain(Sphere,p,cb);
              Sphere.s(cb,tfByteSize,
                Sphere.v(cb,tfByteSize)+Sphere.v(p,tfByteSize));
              Sphere.s(p,tfByteSize,0);
              p:=0;
             end;
            else
              Combine(p_CodeBlockDone,p);
          end;

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
      Combine(pAssignment,p);
      Source.Error('use either ":=" or "=="');
      p:=0;
     end;

    stOpAssign,
    stOpAssignAdd,stOpAssignSub,
    stOpAssignMul,stOpAssignDiv,stOpAssignMod,
    stOpAssignOr,stOpAssignAnd:
     begin
      Combine(pAssignment,p);
      if p=0 then
        Source.Error('no left side defined for assignment')
      else
       begin
        q:=0;//default
        r:=p;
        if p<>0 then
         begin
          //TODO: read-only when?
          while (p<>0) and (r<>0) do
           begin
            case Sphere.t(p) of
              ttVar:p:=0;//is assignable
              ttThis:
               begin
                //only in base object's constructor!
                r:=Sphere.rr(cb,[tfParent,tfParent]);
                if Sphere.t(r)=ttConstructor then
                  r:=Sphere.r(r,tfParent)
                else
                  r:=0;
                if r=TypeDecl_object then p:=0 else r:=0;
               end;
              ttCast:p:=0;//TODO: check about dirty casts
              ttArrayIndex:p:=0;
              ttField:
                if Sphere.r(p,tfTarget)=0 then r:=0 else
                  case Sphere.t(Sphere.r(p,tfTarget)) of
                    ttPropertyGet:  //see also StratoCheckPropertySet
                      p:=0;         //  called from Combine(pAssignment
                    ttPropertySet:
                      p:=0;
                    else
                      p:=Sphere.r(p,tfTarget);//?
                  end;
              ttPropCall:
                if StratoFindPropertySet(Sphere,r,p,
                  cardinal(st),Source.SrcPos) then
                 begin
                  q:=r;
                  p:=0;
                 end
                else
                  r:=0;//Source.Error('read only property'?
              //TODO: more?
              else r:=0;
            end;
           end;
         end;
        if (r=0) or (p<>0) then
          Source.Error('invalid assignment target');
        if q=0 then
          q:=Sphere.Add(ttAssign,
            [tfOperator,cardinal(st)
            ,tfParent,cb
            ,tfSrcPos,Source.SrcPos
            ,tfAssignTo,r
            ]);
        Push(pAssignment,q);
        p:=0;
       end;
     end;
    stOpEQ,stOpNEQ:
      PushBinary(pEqual,st,p);
    stOpLT,stOpLTE,stOpGT,stOpGTE,stOpTypeIs:
      PushBinary(pComparative,st,p);
    stOpAdd,stOpSub:
     begin
      Combine(pAddSub,p);
      if p=0 then //unaryOperator
        Push(pUnary,Sphere.Add(ttUnaryOp,
          [tfParent,cb
          ,tfSrcPos,Source.SrcPos
          ,tfOperator,cardinal(st)
          ]))
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
    stOpNot,stTilde:
      Push(pUnary,Sphere.Add(ttUnaryOp,
        [tfParent,cb
        ,tfSrcPos,Source.SrcPos
        ,tfOperator,cardinal(st)
        ]));

    stOpInc,stOpDec:
      if p=0 then
        Source.Error('increment/decrement operators only allowed as suffix')
      else
        p:=Sphere.Add(ttUnaryOp,
          [tfParent,cb
          ,tfSrcPos,Source.SrcPos
          ,tfOperator,cardinal(st)
          ,tfEvaluatesTo,ResType(Sphere,p)
          ,tfRight,p
          ]);
    stOpSizeOf:
      if p=0 then
        Push(pSizeOf,Sphere.Add(ttUnaryOp,
          [tfParent,cb
          ,tfSrcPos,Source.SrcPos
          ,tfOperator,cardinal(st)
          ]))
      else
        Source.Error('sizeof operator only allowed as prefix');

    stAtAt://"@@": this
     begin
      Juxta(p);
      //see also StratoFnAddOverload
      q:=cb;
      p:=0;
      while (q<>0) and (p=0) do
       begin
        p:=Sphere.Lookup(q,tfFirstItem,Sphere.Dict.StrIdx('@@'));
        if p=0 then
         begin
          q:=Sphere.r(q,tfParent);
          if (q<>0) and (Sphere.t(q)<>ttCodeBlock) then q:=0;
         end;
       end;
      if p=0 then
       begin
        Source.Error('"@@" undefined');
        p:=Sphere.Add(ttThis,//add anyway to avoid further errors
          [tfName,Sphere.Dict.StrIdx('@@')
          ,tfParent,cb
          ,tfSrcPos,Source.SrcPos
          ]);
       end
      else
       begin
        //destructor call?
        if (stackIndex<>0) and (stack[stackIndex-1].p=pUnary) and
          (Sphere.v(stack[stackIndex-1].t,tfOperator)=cardinal(stOpSub)) and
          Source.IsNext([stPOpen,stPClose]) then
         begin
          //find destructor (see also StratoFnCallFindInherited
          p:=Sphere.r(p,tfEvaluatesTo);
          q:=0;
          while (p<>0) and (q=0) do
           begin
            q:=Sphere.r(p,tfFirstItem);
            while (q<>0) and (Sphere.t(q)<>ttDestructor) do
              q:=Sphere.r(q,tfNext);
            if q=0 then p:=Sphere.r(p,tfInheritsFrom);
           end;
          if q=0 then
            Source.Error('unable to determine destructor');
          //then call it
          p:=stack[stackIndex-1].t;
          Sphere.s(p,[tfThingType,ttFnCall
            ,tfParent,cb
            ,tfSrcPos,Source.SrcPos
            ,tfTarget,q
            ]);
          Source.Skip(stPOpen);
          Source.Skip(stPClose);
          dec(stackIndex);
          {$IFDEF DEBUG}
          stack[stackIndex].p:=p___;
          stack[stackIndex].t:=0;
          {$ENDIF}
         end;
       end;
     end;
    stTwoQuestionMarks://"??": result value
     begin
      Juxta(p);
      q:=cb;
      r:=0;
      while (q<>0) and (Sphere.t(q)=ttCodeBlock) do
       begin
        r:=q;
        q:=Sphere.r(q,tfParent);
       end;
      if q=0 then p:=0 else
        case Sphere.t(q) of
          ttOverload,ttPropertyGet,ttPropertySet:
            p:=Sphere.Lookup(r,tfFirstItem,
              Sphere.v(Sphere.r(q,tfParent),tfName));//ttMember
          else p:=0;
        end;
      if p=0 then
       begin
        Source.Error('"??" undefined');
        p:=Sphere.Add(ttVar,//add anyway to avoid further errors
          [tfName,Sphere.Dict.StrIdx('??')
          ,tfParent,cb
          ,tfSrcPos,Source.SrcPos
          ]);
       end;
     end;

    stAmpersand://"&": iteration
      case Source.Token of
        stPOpen://&(){}
          Push(pForFirst,Sphere.Add(ttIteration,
            [tfParent,cb
            ,tfSrcPos,Source.SrcPos
            ]));
        stAOpen://&{}()
         begin
          Push(pForBodyFirst,Sphere.Add(ttIterationPE,
            [tfParent,cb
            ,tfSrcPos,Source.SrcPos
            ]));
          //start code block: (see also stAOpen)
          Push(pCodeBlock,cb);
          cb:=Sphere.Add(ttCodeBlock,
            [tfParent,cb
            ,tfSrcPos,Source.SrcPos
            ]);
          p:=0;
         end;
        else Source.Error('unsupported iteration syntax');
      end;

    stTry://":::"
     begin
      Combine(p_Statement,p);
      Sphere.Add(ttTry,
        [tfParent,cb
        ,tfSrcPos,Source.SrcPos
        ]);
     end;
    stThreeGT://">>>"
     begin
      Combine(pShift,p);
      if p=0 then //defer
       begin
        Combine(p_Statement,p);
        Push(pDefer,Sphere.Add(ttDeferred,
          [tfParent,cb
          ,tfSrcPos,Source.SrcPos
          ]));
       end
      else
        PushBinary(pShift,st,p);//roll right
     end;
    stCatch://"???"
     begin
      Combine(p_Statement,p);
      q:=Sphere.Add(ttCatch,
        [tfParent,cb
        ,tfSrcPos,Source.SrcPos
        ]);
      Push(pCatch,q);
      if Source.IsNext([stPOpen,stIdentifier,stColon,stIdentifier]) then
       begin
        ID(n,nn,SrcPos);
        r:=Sphere.Add(ttVar,
          [tfName,n
          ,tfParent,cb
          ,tfSrcPos,SrcPos
          ]);
        Sphere.s(q,tfFirstArgument,r);
        Source.Skip(stColon);
        Sphere.s(q,tfTarget,LookUpType('catch filter'));
        Sphere.s(r,tfEvaluatesTo,Sphere.r(q,tfTarget));
        Sphere.s(r,tfOffset,Sphere.v(cb,tfByteSize));//?
        Source.Skip(stPClose);//TODO: enforce
        //TODO: check type is by reference (or SystemWordSize?)
       end
      else
      if Source.IsNext([stPOpen,stIdentifier]) then
       begin
        Sphere.s(q,tfDoIf,LookUpType('catch filter'));
        Source.Skip(stPClose);//TODO: enforce
       end;
     end;
    stThrow://"!!!"
     begin
      Juxta(p);
      Push(pThrow,Sphere.Add(ttThrow,
        [tfParent,cb
        ,tfSrcPos,Source.SrcPos
        ]));
     end;

    stBOpen://"["
      if p=0 then
        Source.Error('unsupported syntax')
      else
       begin
        r:=ResType(Sphere,p);
        if (r<>0) and (Sphere.t(r)=ttArray) then
          r:=Sphere.r(r,tfSubject);//element type
        //else error?
        Push(pBrackets,Sphere.Add(ttArrayIndex,
          [tfParent,cb
          ,tfTarget,p
          ,tfSrcPos,SrcPos//p.SrcPos?Source.SrcPos?
          ,tfEvaluatesTo,r
          ]));
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
      Push(pAddressOf,Sphere.Add(ttAddressOf,
        [tfParent,cb
        ,tfSrcPos,Source.SrcPos
        ]));
     end;

    stQuestionMark://"?"
      Push(pTypeOf,Sphere.Add(ttUnaryOp,
        [tfParent,cb
        ,tfSrcPos,Source.SrcPos
        ,tfOperator,cardinal(st)
        ]));

    stCaret://"^"
      if p=0 then
        Source.Error('unsupported syntax')
      else
       begin
        if ((Sphere.t(p) and tt__Typed)<>0)
          and (Sphere.t(Sphere.r(p,tfEvaluatesTo))=ttPointer)
          then
          p:=Sphere.Add(ttDereference,
            [tfParent,cb
            ,tfSrcPos,Source.SrcPos
            ,tfEvaluatesTo,Sphere.rr(p,[tfEvaluatesTo,tfEvaluatesTo])
            ,tfValueFrom,p
            ])
        else
          Source.Error('dereference expected on pointer');
       end;

    stAtAtAt://"@@@": inherited
     begin
      Juxta(p);
      if Source.IsNext([stPOpen]) then
       begin
        //inherited from what?
        i:=0;
        while (i<stackIndex) and (stack[i].p<>pCodeBlock) do inc(i);
        if i=stackIndex then p:=cb else p:=stack[i].t;
        //start FnCall, see Combine: pArgList
        q:=Sphere.Add(ttFnCall,
          [tfName,Name_Inherited
          ,tfParent,cb
          ,tfSrcPos,Source.SrcPos
          ]);
        if Sphere.t(Sphere.r(p,tfParent))=ttConstructor then
          //inherited constructor not relative to this (since null at time of call)
          Sphere.s(q,tfTarget,Sphere.r(p,tfParent))
        else
          //relative to this
          Sphere.s(q,tfTarget,Sphere.Add(ttField,
            [tfParent,cb
            ,tfSubject,Sphere.r(p,tfFirstItem)//assert ttThis
            ,tfSrcPos,Source.SrcPos
            ,tfTarget,Sphere.r(p,tfParent)//Sphere[].Signature;
            ]));
        Push(pArgList,q);
        cbInhCalled:=true;
        p:=0;
       end
      else
        Source.Error('manipulating inherited pointer not allowed');
     end;

    else Source.Error('unsupported syntax');//'unexpected token');
  end;
  Sphere.MarkIndex(bi);
end;

function TStratoParserBase.Fn(x:TStratoIndex;n:TStratoName;const nn:UTF8String;
  SrcPos:cardinal):TStratoIndex;
var
  p:TStratoIndex;
begin
  p:=Sphere.Lookup(x,tfFirstItem,n);
  if (p=0) or (Sphere.t(p)<>ttMember) then
   begin
    if p<>0 then Source.Error('duplicate identifier "'+nn+'"');
    p:=Sphere.Add(ttMember,
      [tfName,n
      ,tfParent,x
      ,tfSrcPos,SrcPos
      ]);
    Sphere.Append(x,tfFirstItem,p);
   end;
  Result:=p;
end;

end.
