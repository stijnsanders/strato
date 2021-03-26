unit stratoParseDecl;

interface

uses stratoParseBase, stratoSphere, stratoTokenizer, stratoDecl, stratoPred;

type
  TStratoParserDecl=class(TStratoParserBase)
  private
    Imports:array of record
      ns:xNode;
      alias:xName;
    end;

    luxSize,luxIndex:cardinal;
    lux:array of xNode;

    ns,rd:xRef;

    function Fn(f:xRef;nx:xName):xNode;

    procedure ParseHeader;
    procedure DeclareIntrinsicTypes(ns:xRef);

    function ParseSignature(ns:xRef;nx:xName;CloseToken:TStratoToken;
      SrcPos:xSrcPos):xNode;
    procedure ParseEnumeration(p:xNode);
    procedure ParseInterfaceDecl(p:xNode);

    procedure ReplaceNode(Parent,Subject,ReplaceWith:xRef);
  protected
    SyntaxClass:TSyntaxClass;
    cbInheritedCalled:boolean;

    procedure ID(var n:xName;var nn:UTF8String;var SrcPos:xSrcPos);
    function ParseLiteral(st0:TStratoToken;NeedValue:PInteger):xNode;

    procedure LookUpDecl_First(nx:xName);
    procedure LookUpDecl_Next(nx:xName);
    function LookUpDecl_Any:xNode;
    procedure LookUpDecl_Lazy(var p:xRef;var nx:xName;var SrcPos:xSrcPos;
      var fqn:UTF8String);

    function LookUpDecl:xNode;
    function LookUpDecl_Type(const tname:string):xNode;

    procedure ParseDeclaration;
    procedure ParseImport;
    procedure ParseRecord;

    function CbStart(pp:xNode;IsVirtual:boolean):xRef;

  public
    procedure AfterConstruction; override;
    procedure Parse; override;
  end;

implementation

{ TStratoParserDecl }

uses SysUtils, stratoTools, stratoLit, stratoLogic, stratoFn,
  stratoParse, stratoSource;

procedure TStratoParserDecl.AfterConstruction;
begin
  inherited;
  //DeclareIntrinsicTypes here?
end;

procedure TStratoParserDecl.DeclareIntrinsicTypes(ns: xRef);

  function A(const Name:UTF8String;s:integer):xNode;
  begin
    Result:=Add(ns,lChildren,nType,8,
      [kv(iParent,0,ns)
      ,kv(dName,0,Sphere.AddName(Name))
      ,kv(vByteSize,0,s)
      ]);
  end;

  procedure S(t:TStratoIntrinsicType;p:xNode);
  begin
    IntrinsicTypes[t]:=p;
  end;

begin
  //assert SpheresCount=0
  //assert IntrinsicTypes[] not set yet

  S(itVoid,A('void',0));
  S(itType,A('type',SystemWordSize));
  S(itPointer,A('pointer',SystemWordSize));
  S(itBoolean,A('bool',SystemWordSize));
  S(itNumber,A('number',SystemWordSize));
  A('i8',1);
  A('i16',2);
  A('i32',4);
  A('i64',8);
  A('u8',1);
  A('u16',2);
  A('u32',4);
  //Type_intLast:=
  A('u64',8);

  A('f32',4);//TODO: floating-point support
  A('f64',8);

  IntrinsicTypes[itObject].none;//see StratoParseSource: allow only one object()={}
  //S(itHash,A('hash',SystemWordSize));//TODO:
  //S(itRange,A('range',SystemWordSize));//TODO:
  S(itString,A('string',SystemWordSize));
  //S(itVariant,A('variant',16));//TODO: OLE compatible variants
end;

procedure TStratoParserDecl.ID(var n:xName;var nn:UTF8String;var SrcPos:xSrcPos);
begin
  nn:=Source.GetID(SrcPos);
  n:=Sphere.AddName(nn);
end;

function TStratoParserDecl.Fn(f:xRef;nx:xName):xNode;
var
  p:xNode;
begin
  p:=Sphere.Lookup(f,nx,lChildren);
  if (p.IsNone) or (p.Key<>nNameSpace) then
   begin
    if not(p.IsNone) then Source.Error('duplicate identifier "'+
      string(Sphere.GetName(nx))+'"');
    p:=Add(f,lChildren,nNameSpace,4,
      [kv(iParent,0,f)
      ,kv(dName,0,nx)
      //kv(vSrcPos,0,SrcPos)?
      ]);
   end;
  Result:=p;
end;

procedure TStratoParserDecl.ReplaceNode(Parent,Subject,ReplaceWith:xRef);
//var
//  p,p0,q:rItem;
begin
  //assert Subject<>nil
  //assert ReplaceWith not pointed to
  //assert ReplaceWith.Parent=Subject.Parent



  asm int 3 end;
  raise Exception.Create('//TODO: modify list entry node?');


(*
  {$IFDEF DEBUG}
  if ReplaceWith.r(iNext).x<>0 then
    raise Exception.Create('broken chain detected');
  {$ENDIF}
  ListFirst(Parent,lItems,p,p0);
  if p.x=0 then
   begin
    Parent.s(lItems,ReplaceWith);
    ReplaceWith.s(iNext,ReplaceWith);
   end
  else
   begin
    q.x:=0;
    while (p.x<>0) and (p.x<>Subject.x) do
     begin
      q.x:=p.x;
      ListNext(p,p0);
     end;
    if p.x=0 then
      raise Exception.CreateFmt(
        'ReplaceNode called with subject %s not on chain %s',
        [ItemToStr(Subject),ItemToStr(Parent)])
    else
     begin
      ReplaceWith.s(iNext,Subject.r(iNext));
      Subject.s(iNext,xx0);
      if q.x=0 then q:=p0;
      q.s(iNext,ReplaceWith);
      if Subject.x=p0.x then Parent.s(lItems,ReplaceWith);
     end;
   end;
*)
end;

function TStratoParserDecl.LookUpDecl:xNode;
var
  nx:xName;
  nn:UTF8String;
  SrcPos:xSrcPos;
begin
  //assert Source.Token was stIdentifier
  ID(nx,nn,SrcPos);
  LookUpDecl_First(nx);
  while Source.IsNext([stPeriod,stIdentifier]) do
   begin
    Source.Token;//stIdentifier
    ID(nx,nn,SrcPos);
    LookUpDecl_Next(nx);
   end;
  Result:=LookUpDecl_Any;
end;

procedure TStratoParserDecl.LookUpDecl_First(nx:xName);
var
  p,q:xNode;
  i:cardinal;
const
  luxGrowStep=8;//?
begin
  p:=Sphere.Lookup(ns,nx,lChildren);
  if p.index=0 then
    luxIndex:=0
  else
   begin
    if luxSize=0 then
     begin
      luxSize:=luxGrowStep;
      SetLength(lux,luxSize);
     end;
    lux[0]:=p;
    luxIndex:=1;
   end;
  if Length(Imports)<>0 then
    for i:=0 to Length(Imports)-1 do
     begin
      p.none;//default
      q:=Imports[i].ns;
      if Imports[i].alias=nx then
       begin
        p:=q;
        //q:=Imports[i].ns;
       end
      else
        p:=q.Lookup(Sphere.GetName(nx));
      if not p.IsNone then
       begin
        if luxIndex=luxSize then
         begin
          inc(luxSize,luxGrowStep);
          SetLength(lux,luxSize);
         end;
        lux[luxIndex]:=p;
        inc(luxIndex);
       end;
     end;
  //TODO: if nsi=0 then check (global) namespaces? (of dependencies only?)
end;

procedure TStratoParserDecl.LookUpDecl_Next(nx:xName);
var
  i:cardinal;
begin
  //assert caller does something like
  //  while Source.IsNext([stPeriod,stIdentifier]) do
  //   begin
  //    Source.Token;//stIdentifier
  //    ID(nx,nn,SrcPos);
  //assert Source.Token was stIdentifier
  if luxIndex<>0 then
    for i:=0 to luxIndex-1 do
      if not lux[i].IsNone then
        lux[i]:=lux[i].Lookup(sphere.GetName(nx));
end;

function TStratoParserDecl.LookUpDecl_Any:xNode;
var
  i,j,k:cardinal;
  nn:UTF8String;
begin
  j:=0;
  k:=0;
  if luxIndex<>0 then
    for i:=0 to luxIndex-1 do
      if not lux[i].IsNone then
       begin
        inc(j);
        k:=i;
       end;
  case j of
    0://none found
      Result.none;
    1://single thing found
      Result:=lux[k];
    else //multiple found
     begin
      nn:='';
      for i:=0 to luxIndex-1 do
        if not lux[i].IsNone then
          nn:=nn+','+lux[i].sphere.FQN(lux[i].index);
      Source.Error('multiple declarations "'+
        string(Copy(nn,2,Length(nn)-1))+'"');
      Result.none;
     end;
  end;
end;

procedure TStratoParserDecl.LookUpDecl_Lazy(var p:xRef;var nx:xName;
  var SrcPos:xSrcPos;var fqn:UTF8String);
var
  nn:UTF8String;
  q:xNode;
begin
  ID(nx,nn,SrcPos);
  fqn:=nn;
  p:=ns;
  while Source.IsNext([stPeriod,stIdentifier]) do
   begin
    q:=Sphere.Lookup(p,nx,lChildren);
    if q.IsNone then //force namespace //TODO: forward decls
      q:=Add(p,lChildren,nNameSpace,4,
        [kv(dName,0,nx)
        ,kv(iParent,0,p)
        ,kv(vSrcPos,0,SrcPos)
        ]);
    if p=ns then
      LookUpDecl_First(nx)
    else
      LookUpDecl_Next(nx);
    p:=Local(q);
    //next
    Source.Token;//stIdentifier
    ID(nx,nn,SrcPos);
    fqn:=fqn+'.'+nn;
   end;
end;

function TStratoParserDecl.LookUpDecl_Type(const tname:string):xNode;
var
  p,q:xRef;
  ptr,i:cardinal;
begin
  //TODO: if stCOpen then ParseRecord here? (ParseJSON??)
  if Source.IsNext([stQuestionMark,stIdentifier]) then
   begin
    Source.Token;//stIdentifier
    p:=Local(LookUpDecl);
    q:=p;
    if Sphere[q].k<>nClass then
      Source.Error('class reference allowed to class only');
    Result:=Add(nClassRef,4,
      [kv(iTarget,0,p)
      ,kn(iParent,Sphere.r(p,iParent))//?
      ,kv(vSrcPos,0,Source.SrcPos)//?
      ]);
   end
  else
   begin
    ptr:=0;
    while Source.IsNext([stCaret]) do inc(ptr);
    if Source.IsNext([stIdentifier]) then
      Result:=LookUpDecl
    else
      Result.none;
    if Result.IsNone then
      Source.Error('undefined '+tname)
    else
     begin
      case Result.Key of
        nType,nRecord,nArray,nEnum,nPointer,nClass,nClassRef,nInterface:
          ;//is a type
        nVar,nVarByRef,nVarReadOnly:
          Result:=Result.r(iType);//take var's type
        nSignature:
         begin
          //TODO: nDelegate? (keep signature and subject)
          Result:=Add(nPointer,4,
             [kn(iTarget,Result)
             ,kn(iParent,Result.r(iParent))
             ,kv(vSrcPos,0,Source.SrcPos)//?
             ]);
         end;
        else
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
          Result:=Add(nArray,8,
            [kv(dName,0,Sphere.AddName(Result.sphere.GetName(Result.v(dName))))//TODO:?
            ,kn(iType,Result)
            ,kv(vByteSize,0,ByteSize(Result)*i)
            ,kv(vSrcPos,0,Source.SrcPos)//?
            ]);
         end;
     end;
    while ptr<>0 do
     begin
      dec(ptr);
      Result:=Add(nPointer,4,
        [kn(iTarget,Result)
        ,kn(iParent,Result.r(iParent))
        ,kv(vSrcPos,0,Source.SrcPos)//?
        ]);
     end;
   end;
end;

procedure TStratoParserDecl.Parse;
begin
  inherited;
  luxIndex:=0;
  luxSize:=0;

  ParseHeader;

  //see inheritants
end;

procedure TStratoParserDecl.ParseHeader;
var
  nx:xName;
  nn:UTF8String;
  SrcPos:xSrcPos;
begin
  //namespace
  if Source.IsNext([stIdentifier]) then
   begin
    ID(nx,nn,SrcPos);
    ns:=Add(0,lChildren,nNameSpace,4,
      [kv(vSrcPos,0,SrcPos)
      ,kv(dName,0,nx)
      ]).index;
    while Source.IsNext([stPeriod,stIdentifier]) do
     begin
      Source.Token;//stIdentifier
      ID(nx,nn,SrcPos);
      ns:=Add(ns,lChildren,nNameSpace,4,
        [kv(iParent,0,ns)
        ,kv(vSrcPos,0,SrcPos)
        ,kv(dName,0,nx)
        ]).index;
     end;
{
    while Source.IsNext([stAt]) do
      case Source.Token of
        stNumericLiteral:
          Sphere.MarkIndex(ParseInteger(Source.GetID(SrcPos1)))
        stIdentifier:
          if not LookUpNameSpace(module,nx,SrcPos1) then
            module:=Sphere.Add(nNameSpace,[iParent,?,vSrcPos,SrcPos1,vName,nx]);
        else
          Source.Error('unknown namespace load modifier syntax');
      end;
}

   end
  else
   begin
    //default: use file name
    nn:=UTF8String(ChangeFileExt(ExtractFileName(Source.FilePath),''));
      //(''''+StringReplace(Source.FilePath,'''','''''',[rfReplaceAll])+'''');?
    nx:=Sphere.AddName(nn);
    ns:=Add(0,lChildren,nNameSpace,4,[kv(dName,0,nx)]).index;
   end;
  Sphere.SetVal(0,iSphere_Local,0,ns);
  //runtime from first module
  if SpheresCount=0 then //TODO: lock global! first one takes it all
   begin
    DeclareIntrinsicTypes(ns);
    SetLength(Imports,0);
   end
  else
   begin
    SetLength(Imports,1);
    Imports[0].ns:=Spheres[0].r(0,iSphere_Local);//"Strato"?
    Imports[0].alias:=0;
    //TODO: list dependency (+ check cyclic)
   end;
  SyntaxClass:=scDeclarative;
end;

procedure TStratoParserDecl.ParseDeclaration;
var
  nx:xName;
  nn,fqn:UTF8String;
  p,q,q1,r,r0,r1,r2:xNode;
  st:TStratoToken;
  i:cardinal;
  SrcPos:xSrcPos;
begin
  while (SyntaxClass=scDeclarative) and Source.NextToken(st) do
  case st of

  stThreeLT:
    ParseImport;

  stIdentifier: //declaration
   begin
    p.sphere:=Sphere;
    p.index:=0;
    LookUpDecl_Lazy(p.index,nx,SrcPos,fqn);

    //operator override?
    if Source.IsNext([stPeriod,stStringLiteral]) then
     begin
      //TODO: lookup over Imports...
      q:=Sphere.Lookup(p.index,nx,lChildren);
      if q.IsNone then
       begin
        Source.Error('operator declaration on unknown type');
        q:=Add(p.index,lChildren,nNameSpace,6,
          [kn(iParent,p)
          ,kv(vSrcPos,0,SrcPos)
          ,kv(dName,0,nx)
          ]);
       end;
      if p.index=ns then
        LookUpDecl_First(nx)
      else
        LookUpDecl_Next(nx);
      p:=q;
      Source.Token;//stStringLiteral
      //ID(nx,nn,SrcPos); but with GetStr:
      nn:=Source.GetStr;
      nx:=Sphere.AddName(nn);
      SrcPos:=Source.SrcPos;
     end;

    st:=Source.Token;
    case st of

    stColon:
     begin
      q:=LookUpDecl_Type('type');

      //property "p.x:q{"
      if Source.IsNext([stCOpen]) then
       begin
        r:=StratoFnAdd(Self,nPropGet,Fn(Local(p),nx),
          Add(nSignature,8,
            [kn(iParent,p)
            ,kv(vSrcPos,0,SrcPos)
            ,kn(iSubject,p)
            ,kv(dName,0,nx)//+'_get'?
            ,kn(iReturnType,q)
            ]),SrcPos);
       if Source.IsNext([stCClose]) then
         begin
          //forward only
          if Source.IsNext([stCOpen,stCClose]) then //empty setter also? skip
            Source.Token;//stCClose
          //TODO: check declared somewhere later
         end
        else
          CbStart(r,true);
       end
      else

      //class "p.x:q={"
      if Source.IsNext([stDefine,stCOpen]) then
       begin
        if q.IsNone then
          Source.Error('undeclared base class')
        else
         begin
          if not Add(Local(p),lChildren,nClass,8,nx,
            [kn(iParent,p)
            ,kv(vSrcPos,0,SrcPos)
            ,kn(iInheritsFrom,q)
            ],p) then
            Source.ErrorN('duplicate identifier',nn);
          if q.Key=nClass then
            Sphere.a(Local(p),vByteSize,q.v(vByteSize))
          else
            Source.Error('base class must be a class');
         end;
        Source.Token;//stCOpen
        rd:=Local(p);//switch to ParseRecord
        SyntaxClass:=scDeclarative_Record;
       end
      else

      //variable "p.x:q"
      if not Add(Local(p),lChildren,nVar,8,nx,
        [kn(iParent,p)
        ,kv(vSrcPos,0,SrcPos)
        ,kn(iType,q)
        ],r) then
        Source.ErrorN('duplicate identifier',nn);
      if Source.IsNext([stDefine]) then
        Sphere.SetRef(Local(r),iValue,ParseLiteral(Source.Token,nil));
      //TODO: check InitialValue.EvaluatesTo with EvaluatesTo
      case p.Key of
        nNameSpace:
          Sphere.Append(0,lSphere_Globals,Local(r));
        nClass,nRecord:
          Sphere.a(Local(p),vByteSize,q.v(vByteSize));
        else
          Source.Error('unexpected variable parent');
      end;

     end;

    stDefine://type, constant or enum "p.x="
      if Source.IsNext([stPOpen,stIdentifier]) then
       begin
        if not Add(Local(p),lChildren,nEnum,6,nx,
          [kn(iParent,p)
          ,kv(vSrcPos,0,SrcPos)
          ],p) then
          Source.ErrorN('duplicate identifier',nn);
        ParseEnumeration(p);
       end
      else
       begin
        //type or constant declaration
        st:=Source.Token;
        case st of

        stIdentifier://"p.x=q"
         begin
          q:=LookUpDecl;
          if q.IsNone then
            Source.Error('unknown type or constant')
          else
            case q.Key of
              nVar:
               begin
                r:=r.r(iType);
                q:=q.r(iValue);
                if q.IsNone then
                  Source.Error('var without initial value')
                else
                  if not Add(Local(p),lChildren,nConstant,8,nx,
                    [kn(iParent,p)
                    ,kv(vSrcPos,0,SrcPos)
                    ,kn(iType,r)
                    ,kn(iValue,q)
                    ],q) then
                    Source.ErrorN('duplicate identifier',nn);
               end;
              nConstant:
               begin
                q:=q.r(iValue);
                if q.IsNone then
                  Source.Error('constant without value')
                else
                  if not Add(Local(p),lChildren,nConstant,8,nx,
                    [kn(iParent,p)
                    ,kv(vSrcPos,0,SrcPos)
                    ,kn(iType,q.r(iType))
                    ,kn(iValue,q)
                    ],q) then
                    Source.ErrorN('duplicate identifier',nn);
               end;
              nLiteral:
                if not Add(Local(p),lChildren,nConstant,8,nx,
                  [kn(iParent,p)
                  ,kv(vSrcPos,0,SrcPos)
                  ,kn(iType,q.r(iType))
                  ,kn(iValue,q)
                  ],q) then
                  Source.ErrorN('duplicate identifier',nn);
              nType,nRecord,nEnum:
                if Source.IsNext([stBOpen]) then //array
                  if Source.IsNext([stBClose]) then //dyn array
                    Source.Error('//TODO: dyn arrays')
                  else
                   begin
                    ParseLiteral(Source.Token,@i);
                    //TODO: multidimensional arrays, array of array
                    if Source.Token<>stBClose then
                      Source.Error('Closing bracket expected');
                    //TODO: check 2GB overflow
                    if q.IsNone then i:=0 else
                      i:=ByteSize(q)*i;
                    if not Add(Local(p),lChildren,nArray,8,nx,
                      [kn(iParent,p)
                      ,kv(vSrcPos,0,SrcPos)
                      ,kn(iType,q)
                      ,kv(vByteSize,0,i)
                      ],r) then
                      Source.ErrorN('duplicate identifier',nn);
                   end
                else //type alias
                  if not Add(Local(p),lChildren,nTypeAlias,6,nx,
                    [kn(iParent,p)
                    ,kv(vSrcPos,0,SrcPos)
                    ,kn(iType,q)
                    ],q) then
                    Source.ErrorN('duplicate identifier',nn);
              else
                Source.Error('unsupported type or constant reference');
            end;
         end;

        stStringLiteral,stNumericLiteral,
        stBOpen,stPOpen,stOpSizeOf://constant
         begin
          q:=ParseLiteral(st,nil);
          if not Add(Local(p),lChildren,nConstant,6,nx,
            [kn(iParent,p)
            ,kv(vSrcPos,0,SrcPos)
            ,kn(iValue,q)
            ],p) then
            Source.ErrorN('duplicate identifier',nn);
          if Source.IsNext([stColon]) then //here or in ParseLiteral?
            Sphere.SetRef(Local(q),iType,LookUpDecl_Type('literal type'));
          Sphere.SetRef(Local(p),iType,q.r(iType));
         end;

        stCOpen://record (aka struct) "p.x={"
         begin
          if not Add(Local(p),lChildren,nRecord,6,nx,
            [kn(iParent,p)
            ,kv(vSrcPos,0,SrcPos)
            ],q) then
            Source.ErrorN('duplicate identifier',nn);
          rd:=Local(q);//switch to ParseRecord
          SyntaxClass:=scDeclarative_Record;
         end;

        stCaret://pointer type "p.x=^q"
          if not Add(Local(p),lChildren,nPointer,6,nx,
            [kn(iParent,p)
            ,kv(vSrcPos,0,SrcPos)
            ,kn(iTarget,LookUpDecl_Type('pointer type'))
            ],q) then
            Source.ErrorN('duplicate identifier',nn);

        stQuestionMark://class reference "p.x=?q"
         begin
          q1:=LookUpDecl_Type('class reference type');
          if q1.Key<>nClass then
            Source.Error('invalid class reference subject');
          if not Add(Local(p),lChildren,nClassRef,6,nx,
            [kn(iParent,p)
            ,kv(vSrcPos,0,SrcPos)
            ,kn(iTarget,q1)
            ],q) then
            Source.ErrorN('duplicate identifier',nn);
         end;

        else
          Source.Error('unsupported type or constant');
        end;
       end;

    stPOpen://parameter list "p.x("
     begin
      q:=ParseSignature(Local(p),nx,stPClose,SrcPos);
      case Source.Token of
        stSemiColon:
         begin
          r:=Sphere.Lookup(Local(p),nx,lChildren);
          if r.IsNone then //just a signature? add to namespace
            Sphere.Append(Local(p),lChildren,Local(q))
          else
            case r.Key of
              nNameSpace:
                StratoFnAdd(Self,nOverload,r,q,SrcPos);
              nSignature:
               begin
                //another forward signature? create ttMember here
                r0:=Add(nNameSpace,4,
                  [kn(iParent,p)
                  ,kv(dName,0,nx)
                  ]);
                ReplaceNode(Local(p),Local(r),Local(r0));
                StratoFnAdd(Self,nOverload,r0,r,r.v(vSrcPos));
                StratoFnAdd(Self,nOverload,r0,q,SrcPos);
               end
              else
                Source.ErrorN('duplicate identifier',nn);
            end;
          //TODO: Source.IsNext([stAOpen])?
         end;
        stCOpen://code block
         begin
          r:=Sphere.Lookup(Local(p),nx,lChildren);
          if r.IsNone then
            p:=StratoFnAdd(Self,nOverload,Fn(Local(p),nx),q,SrcPos)
          else
            case r.Key of
              nNameSpace:
                p:=StratoFnAdd(Self,nOverload,r,q,SrcPos);
              nSignature://signature forwarded, replace with ttMember
               begin
                r0:=Add(nNameSpace,4,
                  [kn(iParent,p)
                  ,kv(dName,0,nx)
                  ]);
                ReplaceNode(Local(p),Local(r),Local(r0));
                //StratoFnAdd checks for SameType(p,q):
                StratoFnAdd(Self,nOverload,r0,r,r.v(vSrcPos));
                p:=StratoFnAdd(Self,nOverload,r0,q,SrcPos);
               end;
              nClass://constructor
               begin
                Sphere.SetRef(Local(q),iSubject,r);
                p:=StratoFnAdd(Self,nCtor,r,q,SrcPos);
               end;
              else
               begin
                Source.ErrorN('duplicate identifier',nn);
                r0:=Add(nNameSpace,4,
                  [kn(iParent,p)
                  ,kv(dName,0,nx)
                  ]);
                p:=StratoFnAdd(Self,nOverload,r0,q,SrcPos);
               end;
            end;
          if not(p.IsNone) then CbStart(p,true);
         end;
        else Source.Error('unsupported signature syntax');
      end;
     end;

    stOpAssign://"p.x:="
      if Source.IsNext([stCOpen]) then
       begin
        //accept only one object:={}
        if p.IsNone then p.s(Sphere,ns);
        if not Add(Local(p),lChildren,nClass,6,nx,
          [kn(iParent,p)
          ,kv(vSrcPos,0,SrcPos)
          ],p) then
          Source.Error('duplicate identifier');
        //TODO: global lock?
        if IntrinsicTypes[itObject].IsNone then
          IntrinsicTypes[itObject]:=p
        else
          Source.Error('only one master base class allowed');
        rd:=Local(p);//switch to ParseRecord
        SyntaxClass:=scDeclarative_Record;
       end
      else
        Source.Error('unsupported declaration syntax');

    stBOpen://"p.x["
     begin
      q:=StratoFnAdd(Self,nPropGet,Fn(Local(p),nx),
        ParseSignature(Local(p),nx,stBClose,SrcPos),SrcPos);
      if Source.IsNext([stCOpen]) then
        CbStart(q,true);
     end;

    stCOpen://"p.x{"
     begin
      if Local(p)=ns then q.none else q:=p;
      r:=StratoFnAdd(Self,nOverload,Fn(Local(p),nx),
        Add(nSignature,8,
          [kn(iParent,p)
          ,kv(dName,0,nx)
          ,kv(vSrcPos,0,SrcPos)
          ,kn(iSubject,q)
          ]),SrcPos);
      CbStart(r,true);
     end;

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
    p.sphere:=Sphere;
    p.index:=0;
    LookUpDecl_Lazy(p.index,nx,SrcPos,fqn);
    st:=Source.Token;
    case st of
    stPOpen:
      if Source.IsNextID([stPClose,stCOpen]) or
        Source.IsNextID([stPClose,stDefine,stCOpen]) then
       begin //inherit this interface
        q1:=LookUpDecl;
        if q1.IsNone then
          Source.Error('undeclared base interface')
        else if q1.Key<>nInterface then
          Source.Error('interface can only inherit from interface');
        Source.Skip(stPClose);
        Source.Skip(stDefine);//if?
        Source.Skip(stCOpen);
        if not Add(Local(p),lChildren,nInterface,8,nx,
          [kn(iParent,p)
          ,kv(vSrcPos,0,SrcPos)
          ,kn(iInheritsFrom,q1)
          ],p) then
          Source.Error('duplicate identifier');
        ParseInterfaceDecl(p);
       end;
    stCOpen:
     begin
      if not Add(Local(p),lChildren,nInterface,8,nx,
        [kn(iParent,p)
        ,kv(vSrcPos,0,SrcPos)
        ],p) then
        Source.ErrorN('duplicate identifier',nn);
      ParseInterfaceDecl(p);
     end;
    else
      Source.Error('unsupported interface syntax');
    end;
   end;

  stOpSub,stTilde://'-','~': destructor?
    if Source.IsNextID([stPOpen,stPClose,stCOpen]) then
     begin
      p.sphere:=Sphere;
      p.index:=0;
      LookUpDecl_Lazy(p.index,nx,SrcPos,fqn);
      //ParseSignature? destructor doesn't have arguments/overloads
      Source.Skip(stPOpen);
      Source.Skip(stPClose);
      Source.Skip(stCOpen);
      //find class destructor is for
      q:=Sphere.Lookup(p.index,nx,lChildren);
      if q.IsNone then
       begin
        Source.Error('destructor for unknown class');
        q:=Add(nClass,4,[kv(dName,0,nx)]);
       end
      else
       begin
        if q.Key<>nClass then
         begin
          Source.Error('destructor only supported on class');
          q:=Add(nClass,4,[kv(dName,0,nx)]);
         end;
       end;
      //check any destructor already
      r1.Start(q,lChildren);
      while r1.Next(r2) and (r2.Key<>nDtor) do ;
      if not r1.Done then
        Source.Error('duplicate destructor');
      //add
      CbStart(Add(Local(q),lChildren,nDtor,4,
        [kn(iParent,q)
        ,kv(vSrcPos,0,SrcPos)
        ]),true);
     end
    else
      Source.Error('unexpected token');

  stCOpen:
   begin
    q.s(Sphere,CbStart(Add(nCodeBlock,8,
      [kn(iParent,Sphere.r(0,iSphere_Local))
      ,kv(vSrcPos,0,Source.SrcPos)
      ]),false));
    if Sphere.r(0,iSphere_InitializationBlock).IsNone then
      Sphere.SetRef(0,iSphere_InitializationBlock,q)
    else
    if Sphere.r(0,iSphere_FinalizationBlock).IsNone then
      Sphere.SetRef(0,iSphere_FinalizationBlock,q)
    else
      Source.Error('Initialization and finalization code already declared.');
   end;

{//TODO: data hiding
  stHRule:
    if Locals[1]=0 then
     begin
      ns:=Locals[0];
      Locals[1]:=Sphere.Add(ns,fItems,nPrivate,
        [iParent,ns
        ,vSrcPos,SrcPos
        ]);
     end
    else
      Source.Error('already in private visibility');
}

  stColon:
    if Source.IsNext([stIdentifier]) then
     begin
      ID(nx,nn,SrcPos);
      p:=Add(ns,lChildren,nType,6,
        [kv(iParent,0,ns)
        ,kv(vSrcPos,0,SrcPos)
        ,kv(dName,0,nx)
        ]);
      if Source.IsNext([stAt,stNumericLiteral]) then
       begin
        Source.Token;//stNumericLiteral
        Sphere.SetVal(Local(p),vByteSize,0,ParseInteger(Source.GetID(SrcPos)));
       end;
      Source.Skip(stSemiColon);
     end
    else
      Source.Error('identifier expected');

  stSemiColon://";"
   begin
    {$IFDEF DEBUG}
    if Source.IsNext([stSemiColon,stSemiColon]) then
     begin
      Source.Skip(stSemiColon);
      Source.Skip(stSemiColon);
      asm int 3 end;//for debugging the parser
     end;
    {$ENDIF}
   end;

  st_Unknown:Source.Error('unknown token');
  else Source.Error('unexpected token');
  end;
end;

procedure TStratoParserDecl.ParseImport;
var
  ns:xNode;
  nx:xName;
  nn,alias:UTF8String;
  fn,fn1:string;
  SrcPos:xSrcPos;
  ss:TStratoSource;
  pp:TStratoParserDecl;
  i:cardinal;
begin
  fn:='';//default
  ns.none;//default
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
    ID(nx,nn,SrcPos);
    fn:=UTF8ToString(nn)+'.';
    ns.sphere:=Sphere;
    if not Add(0,lChildren,nNameSpace,6,nx,
      [kv(vSrcPos,0,SrcPos)
      ],ns) then Source.Error('importing duplicate namespace');
    while Source.IsNext([stPeriod,stIdentifier]) do
     begin
      Source.Token;//stIdentifier
      ID(nx,nn,SrcPos);
      fn:=fn+UTF8ToString(nn)+'.';
      if not Add(Local(ns),lChildren,nNameSpace,6,nx,
        [kn(iParent,ns)
        ,kv(vSrcPos,0,SrcPos)
        ],ns) then Source.Error('importing duplicate namespace');
     end;
    fn:=fn+'xso';
    if not FindKnownFile(fn) then
      Source.Error('import not found "'+fn+'"');
   end;
  stStringLiteral:
   begin
    //TODO: resolve relative path, list of paths, system paths
    //TODO: allow duplicates? detect by full path?
    SrcPos:=Source.SrcPos;
    fn:=UTF8ToString(Source.GetStr);
   end;
  else Source.Error('unsupported import subject syntax');
  end;
{
  if Source.IsNext([stAt,stNumericLiteral]) then
   begin
    Source.Token;//stNumericLiteral
    i:=ParseInteger(Source.GetID(SrcPos1))
    ...
   end
  else
    i:=0;
}
  Source.Skip(stSemiColon);
  if fn<>'' then
   begin
    fn:=ResolveKnownPath(fn);
    fn1:=StripKnownPath(fn);
    i:=0;
    while (i<SpheresCount) and (CompareText(fn1,
      UTF8ToString(Spheres[i].BinaryData(Spheres[i].r(0,iSphere_FileName).index)))<>0) do inc(i);
    if i<SpheresCount then
      ns:=Spheres[i].r(0,iSphere_Local)
    else
     begin
      //TODO: dispatch into (multi-threaded) stack/queue
      //load and parse
      ss:=TStratoSource.Create;
      try
        ss.OnError:=Source.OnError;//?
        ss.LoadFromFile(fn);
        if ss.IsNext([st_EOF]) then
          ns.none //raise?
        else
         begin
          pp:=TStratoParser.Create(TStratoSphere.Create,ss,@Source.OnError<>nil);
          try
            pp.Parse;
            AddSphere(pp.Sphere);
            ns:=pp.Sphere.r(0,iSphere_Local);
            nn:=pp.Sphere.GetName(pp.Sphere.n(ns.index));
          finally
            pp.Free;
          end;
         end;
        Source.ErrorCount:=Source.ErrorCount+ss.ErrorCount;
      finally
        ss.Free;
      end;
     end;
   end;
  //register
  if not ns.IsNone then
   begin
    i:=Length(Imports);
    SetLength(Imports,i+1);
    Imports[i].ns:=ns;
    Imports[i].alias:=Sphere.AddName(alias);
   end;
  //else Source.Error('nothing to import');?

  //TODO: if Source.IsNext([stPOpen?stCOpen?
end;

procedure TStratoParserDecl.ParseRecord;
type
  PCardinal=^cardinal;
const
  OffsetUseDefault=cardinal(-11111);
var
  p,p1,p0,q,r,pUntyped:xNode;
  pv:PxKeyValue;
  offset,i,j:cardinal;
  st:TStratoToken;
  b,neg:boolean;
  fn:UTF8String;
  nx:xName;
  SrcPos,SrcPos1:xSrcPos;
begin
  //assert rd set when SyntaxClass:=scDeclarative_Record
  pUntyped.none;//see stComma below
  while (SyntaxClass=scDeclarative_Record) and Source.NextToken(st) do
  case st of

  stIdentifier:
   begin
    offset:=OffsetUseDefault;//default
    p.none;//default
    fn:=Source.GetID(SrcPos);
    if Source.IsNext([stColon]) then
      if Source.IsNext([stCOpen]) then //TODO move this into LookUpType?
       begin
        p:=Add(nRecord,6,
          [kv(iParent,0,rd)
          ,kv(vSrcPos,0,SrcPos)
          ,kv(dName,0,Sphere.AddName(fn))
          ]);
        rd:=Local(p);//push? see stCClose below
        //TODO: add struct/typedecl itself to something? x?ns?
       end
      else
        p:=LookUpDecl_Type('field type');
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
          if not TryStrToInt(string(Source.GetID(SrcPos1)),
            integer(i)) then
            Source.Error('record field offset not an integer');
        stOpSub:neg:=true;
        stOpAdd:neg:=false;
        stIdentifier:
         begin
          q:=sphere.Lookup(rd,sphere.AddName(Source.GetID(SrcPos1)),lChildren);
          if q.IsNone then
            Source.Error('record field not found')
          else
            i:=q.v(vOffset);
         end;
        stOpSizeOf:
          if Source.IsNext([stNumericLiteral]) then
           begin
            Source.Skip(st);//Source.GetID;
            i:=SystemWordSize;//ByteSize(Sphere,Type_number);
           end
          else
            i:=ByteSize(LookUpDecl_Type('offset type'));
        //stSemiColon:b:=false; else Source.Error?
        else b:=false;
        end;
        if Source.IsNext([stOpMul,stNumericLiteral]) then
         begin
          Source.Token;//stNumericLiteral
          if TryStrToInt(string(Source.GetID(SrcPos1)),integer(j)) then
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
    if Source.IsNext([stSemiColon]) then
      if p.IsNone then
       begin
        Source.Error('record field requires type declaration');
        pUntyped.none;
       end;

    //register field with record
    nx:=Sphere.AddName(fn);
    if not Add(rd,lChildren,nVar,8,nx,
      [kv(iParent,0,rd)
      ,kv(vSrcPos,0,SrcPos)
      ,kn(iType,p)
      ,kv(vOffset,0,offset)
      ],r) then
      Source.ErrorN('duplicate record field',fn);
    if pUntyped.IsNone then pUnTyped:=r;

    if not(p.IsNone) and not(pUntyped.IsNone) then
     begin
      p1.s(Sphere,rd);
      p0.Start(p1,lChildren);
      while p0.Next(r) and not(r.IsSame(pUntyped)) do ;
      repeat
        offset:=r.v(vOffset);
        if offset=OffsetUseDefault then
          offset:=Sphere.a(rd,vByteSize,ByteSize(p))
        else
          if integer(offset)<0 then
           begin
            if not((IntrinsicTypes[itObject].sphere=Sphere) and (IntrinsicTypes[itObject].index=rd)) then
             begin
              offset:=-integer(offset);
              Source.Error('negative record field offset not allowed');
             end;
           end
          else
           begin
            i:=offset+ByteSize(p);
            pv:=Sphere.v(rd,vByteSize);
            if pv=nil then
              Sphere.SetVal(rd,vByteSize,0,i)
            else
              if i>pv.v then pv.v:=i;
           end;
        Sphere.SetVal(Local(r),vOffset,0,offset);
        Sphere.SetRef(Local(r),iType,p);
      until not p0.Next(r);
      pUntyped.none;
     end;

   end;

  stComma:
    if pUntyped.IsNone then Source.Error('unexpected ","');

  //stQuestionMark: nested interface?
  //more?

  stCClose:
   begin
    //'pop'
    SyntaxClass:=scDeclarative;
    //TODO: nested records???
   end;

  stPOpen:
    Source.Error('unsupported record field syntax, declare methods outside of data section');

  else Source.Error('unsupported record field syntax');
  end;
end;

function TStratoParserDecl.ParseLiteral(st0: TStratoToken;
  NeedValue: PInteger): xNode;
begin
  //into separate unit to avoid confusion with "stack"
  Result:=stratoLit.ParseLiteral(Self,LookUpDecl,st0,NeedValue);
end;

function TStratoParserDecl.ParseSignature(ns:xRef;nx:xName;
  CloseToken:TStratoToken;SrcPos:xSrcPos):xNode;//nSignature
var
  st:TStratoToken;
  p,q,r,p0,Signature,pUntyped:xNode;
  argName:UTF8String;
  byref:boolean;

  procedure AddArgument(InitialValue:xNode);
  var
    r:xNode;
  begin
    if byref then
     begin
      if not Add(Local(Signature),lArguments,nSigArgByRef,6,Sphere.AddName(argName),
        [kn(iType,p)
        ,kn(iParent,Signature)
        ,kv(vSrcPos,0,SrcPos)
        ],r) then
        Source.ErrorN('duplicate argument',argName);
      if not InitialValue.IsNone then
        Source.Error('default value on argument by reference not supported');
     end
    else
     begin
      if not Add(Local(Signature),lArguments,nSigArg,6,Sphere.AddName(argName),
        [kn(iParent,Signature)
        ,kv(vSrcPos,0,SrcPos)
        ,kn(iType,p)
        ,kn(iValue,InitialValue)
        ],r) then
        Source.ErrorN('duplicate argument',argName);
     end;
    byref:=false;
    if p.IsNone and pUntyped.IsNone then pUntyped:=r;
  end;

begin
  //assert one past token stPOpen
  Signature:=Add(nSignature,6,
    [kv(dName,0,nx)
    ,kv(iParent,0,ns)
    ,kv(vSrcPos,0,SrcPos)
    ]);
  Result:=Signature;
  pUntyped.none;
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
    p.none;//default
    q.none;//default
    st:=Source.Token;
    case st of
    stColon://argument type
     begin
      p:=LookUpDecl_Type('argument type');
      if not pUntyped.IsNone then
       begin
        p0.Start(Signature,lArguments);
        while p0.Next(r) and not(r.IsSame(pUntyped)) do ;
        repeat
          //assert Sphere.r(pUntyped,iType)=0
          Sphere.SetRef(Local(r),iType,p);
        until not p0.Next(r);
        pUntyped.none;
       end;
      if Source.IsNext([stDefine]) then //default value
       begin
        q:=ParseLiteral(Source.Token,nil);
        Sphere.SetRef(Local(q),iParent,Signature);
       end;
      AddArgument(q);
      if Source.IsNext([stComma]) or Source.IsNext([stSemiColon]) then
        ;//skip
     end;
    stComma:
      AddArgument(none);
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
  if Sphere[ns].k<>nNameSpace then //nRecord,nInterface,nTypeDecl
    Sphere.SetVal(Local(Signature),iSubject,0,ns);
  if Source.IsNext([stColon]) then
    Sphere.SetRef(Local(Signature),iReturnType,LookUpDecl_Type('returns type'))
  else
    if CloseToken=stBClose then
      Source.Error('property requires value type');
end;

procedure TStratoParserDecl.ParseEnumeration(p:xNode);//nEnumeration
var
  st:TStratoToken;
  nx:xName;
  nn:UTF8String;
  i:integer;
  q:xNode;
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
    if Add(Local(p),lChildren,nConstant,6,nx,
      [kn(iType,p)
      ,kv(vSrcPos,0,SrcPos)
      ,kn(iParent,p)
      ],q) then
     begin
      //TODO: no literals here? cardinal member for nConstant
      Sphere.SetRef(Local(q),iValue,Add(nLiteral,4,
        [kv(vSrcPos,0,SrcPos)
        ,kn(iType,IntrinsicTypes[itNumber])
        ,kv(iValue,0,Sphere.AddBinaryData(IntToStr8(i)))
        ]));
      inc(i);
     end
    else
      Source.ErrorN('duplicate enumeration entry',nn);
   end;
  stComma,stSemiColon:;//ignore
  stPClose:;//done
  else Source.Error('unsupported enumeration syntax');
  end;
end;

procedure TStratoParserDecl.ParseInterfaceDecl(p:xNode);//nInterface
var
  st:TStratoToken;
  nx:xName;
  nn:UTF8String;
  SrcPos:xSrcPos;
  q:xNode;
begin
  //assert previous token stCOpen
  while not(Source.IsNext([stCClose])) and Source.NextToken(st) do
  case st of

  stIdentifier:
   begin
    ID(nx,nn,SrcPos);
    case Source.Token of
      stColon://value //TODO: force property get/set?
        if not Add(Local(p),lChildren,nVar,6,nx,
          [kn(iType,LookUpDecl_Type('field type'))
          ,kn(iParent,p)
          ,kv(vSrcPos,0,SrcPos)
          ],q) then
          Source.ErrorN('duplicate interface field',nn);
      stPOpen://signature
        StratoFnAdd(Self,nOverload,Fn(Local(p),nx),
          ParseSignature(Local(p),nx,stPClose,SrcPos),SrcPos);
      else Source.Error('unsupported interface field syntax');
    end;
    Source.Skip(stSemiColon);
   end;

  else Source.Error('unsupported interface field syntax');
  end;
end;

function TStratoParserDecl.CbStart(pp:xNode;IsVirtual:boolean):xRef;
var
  a0,p1,p0,Signature,p,q,v:xNode;
  k:xKey;
  o:cardinal;
begin
  //prepared code-block?
  if pp.Key=nCodeBlock then
    p0:=pp
  else
   begin
    p0:=Add(nCodeBlock,8,
      [kn(iParent,pp)
      ,kv(vSrcPos,0,Source.SrcPos)
      ]);
    //assert pp.Body=0
    Sphere.SetRef(Local(pp),iBody,p0);
    //populate code block
    //this "@@"
    if pp.Key=nDtor then
     begin
      Signature.none;
      p:=pp.r(iParent);
     end
    else
     begin
      //assert pp.NodeType in [nOverload,nCtor,nPropGet,nPropSet]
      Signature:=pp.r(iSignature);
      p:=Signature.r(iSubject);
     end;
    if not(p.IsNone) then
      Add(Local(p0),lCodeBlock_Locals,nThis,6,
        [kn(iParent,p0)
        ,kn(iType,p)
        ,kv(vOffset,0,Sphere.a(Local(p0),vByteSize,SystemWordSize))
        ]);
    if not(Signature.IsNone) then
     begin
      //return value?
      p:=Signature.r(iReturnType);
      if not(p.IsNone) then
        if pp.Key=nCtor then
          //with a constructor, store the effective class type here
          Add(Local(p0),lCodeBlock_Locals,nVarReadOnly,8,
            [kn(iParent,p0)
            ,kv(vSrcPos,0,Source.SrcPos)
            ,kv(dName,0,Sphere.AddName('?@@'))
            ,kv(vOffset,0,Sphere.a(Local(p0),vByteSize,SystemWordSize))
            ,kn(iType,Add(nClassRef,4,
              [kn(iParent,p0)
              ,kv(vSrcPos,0,Source.SrcPos)
              ,kn(iTarget,IntrinsicTypes[itObject])
              ]))
            ])
        else
          Add(Local(p0),lCodeBlock_Locals,nVar,8,//TODO: nVarByRef here
            [kn(iParent,p0)
            ,kv(vSrcPos,0,pp.v(vSrcPos))
            ,kv(dName,0,pp.r(iParent).v(dName))//nMember's name
            ,kv(vOffset,0,Sphere.a(Local(p0),vByteSize,ByteSize(p)))
            ,kn(iType,p)
            ]);
      //arguments
      a0.Start(Signature,lArguments);
      while a0.Next(q) do
       begin
        if not Sphere.Lookup(Local(p0),Sphere.n(Local(q)),lCodeBlock_Locals).IsNone then
          Source.Error('duplicate identifier "'+string(
            Sphere.GetName(Sphere.n(Local(q))))+'"');
        p:=q.r(iType);
        if q.Key=nSigArgByRef then k:=nVarByRef else
          k:=nVarReadOnly;
        if k=nVarByRef then
          o:=Sphere.a(Local(p0),vByteSize,SystemWordSize)
        else
          if p.IsNone then
            o:=0//raise?
          else
            o:=Sphere.a(Local(p0),vByteSize,ByteSize(p));
        v:=Add(Local(p0),lCodeBlock_Locals,k,8,
          [kn(iParent,p0)
          ,kv(vSrcPos,0,q.v(vSrcPos))
          ,kv(dName,0,Sphere.n(Local(q)))
          ,kv(vOffset,0,o)
          ,kn(iType,p)
          ]);
        Sphere.SetRef(Local(q),iArgVar,v);
       end;
     end;
   end;

  //switch to ParseLogic
  //assert cb.x=0
  if IsVirtual then cbInheritedCalled:=false;
  Push(pCodeBlock,p0,none,0);
  SyntaxClass:=scImperative;
  Result:=Local(pp);
end;

end.
