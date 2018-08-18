unit stratoParse;

interface

uses stratoDecl, stratoPred, stratoTokenizer, stratoSphere, stratoSource;

type
  TStratoParser=class(TObject)
  private
    FSrc: cardinal;
    FBase: rItem;
    FSource: TStratoSource;
    FInlineErrors: boolean;

    Imports:array of record
      ns:rItem;
      alias:xName;
    end;

    function Fn(f:rItem;nx:xName):rItem;//iMember
    function ParseLiteral(st0:TStratoToken;NeedValue:PInteger):rItem;//iLiteral
    function ParseSignature(ns:rItem;nx:xName;CloseToken:TStratoToken;
      SrcPos:xSrcPos):rItem;//iSignature
    procedure ParseEnumeration(p:rItem);//iEnum
    procedure ParseInterfaceDecl(p:rItem);//iInterface

    procedure ReplaceNode(Parent,Subject,ReplaceWith:rItem);
    procedure InlineError(Sender: TObject; Line, LPos: cardinal;
      const ErrorMsg: string);
    procedure DeclareIntrinsicTypes(ns:rItem);

  public
    constructor Create(ASource: TStratoSource; InlineErrors: boolean);
    procedure Parse;

    function Add(NodeType:xTypeNr;const FieldValues:array of xValue):rItem;
      overload;
    function Add(ListOwner:rItem;ListField:xTypeNr;NodeType:xTypeNr;
      const FieldValues:array of xValue):rItem; overload;
    function Add(ListOwner:rItem;ListField:xTypeNr;NodeType:xTypeNr;Name:xName;
      const FieldValues:array of xValue;var NewItem:rItem):boolean; overload;
    procedure Append(ListOwner: rItem; ListField: xTypeNr; ListItem: rItem);
    procedure Prepend(ListOwner: rItem; ListField: xTypeNr; ListItem: rItem);

    function AddName(const x:UTF8String):xName;

    property Source: TStratoSource read FSource;
    property SrcIndex: cardinal read FSrc;

  //Declarative section

  private
    luxSize,luxIndex:cardinal;
    lux:array of rItem;

    procedure ParseHeader;
    procedure ParseImport;
    procedure ParseDeclaration;
    procedure ParseRecord;

  protected
    cb,rd:rItem;
    cbInhCalled:boolean;

    procedure ID(var n:xName;var nn:UTF8String;var SrcPos:xSrcPos);

    function CbStart(pp:rItem):rItem;//nCodeBlock

    procedure LookUpDecl_First(nx:xName);
    procedure LookUpDecl_Next(nx:xName);
    function LookUpDecl_Any: rItem;
    procedure LookUpDecl_Lazy(var p: rItem; var nx: xName;
      var SrcPos: xSrcPos; var fqn: UTF8String);

    function LookUpDecl:rItem;
    function LookUpDecl_Type(const tname:string):rItem;

  public
    function IntrinsicType(it:TStratoIntrinsicType):rItem;

  //Imperative section

  private
    stackSize,stackIndex:cardinal;
    stack:array of record
      pr:TPrecedence;
      p1,p2:xItem;
      sp:xSrcPos;
    end;
    stackPushed:boolean;

    procedure Push(pr:TPrecedence;p1,p2:rItem;sp:xSrcPos);
    procedure Pop(var pr:TPrecedence;var p1:rItem;var p2:rItem;var sp:xSrcPos);
    function Peek:TPrecedence;
    function Peek1:rItem;

    procedure ParseLogic;

    procedure LookUpLogic(nx:xName;var p:rItem;var pt:rItem;SrcPos:xSrcPos);

    function CombineTop(var p:rItem;var pt:rItem):xSrcPos;
    function Combine(zz:TPrecedence;var p:rItem;var pt:rItem):xSrcPos;
    procedure Juxta(var p:rItem;var pt:rItem);
    procedure PushUnary(st:TStratoToken;var p:rItem;var pt:rItem);
    procedure PushBinary(pr:TPrecedence;st:TStratoToken;var p:rItem;
      var pt:rItem);

    procedure CheckPassed(p:rItem);
    function IsType(p:rItem):boolean;
    procedure CbAdd(p:rItem);

    procedure CheckType_Selection(p,pt:rItem);
    procedure CheckType_Operator(p,pt:rItem);
    procedure CheckType_Comparison(p,pt:rItem);
    procedure CheckType_Assignment(p,pt,qt:rItem);
    procedure CheckType_Range(p1,p2:rItem);
  end;

implementation

uses SysUtils, stratoTools, stratoFn, stratoLit, stratoLogic;

const
  stackGrowSize=$100;

{ TStratoParser }

constructor TStratoParser.Create(ASource: TStratoSource;
  InlineErrors: boolean);
var
  sx:PxSourceFile;
begin
  inherited Create;
  FSource:=ASource;
  if FSource=nil then
   begin
    FSrc:=0;
    FBase.x:=SourceFilesCount * StratoSphereBlockBase;
   end
  else
   begin
    FSrc:=AddSourceFile(sx);
    FBase.x:=FSrc * StratoSphereBlockBase;
    sx.FileName:=AddBinaryData(FSrc,
      UTF8String(StripKnownPath(FSource.FilePath)));
    sx.FileSize:=FSource.FileSize;
    //TODO: hash, timestamp?
    sx.SrcPosLineIndex:=FSource.LineIndex;
    //sx.BlockIndex:=//see AddNode
   end;

  //TODO: link parser to sourcefile? make others read-only!
  //see also dependencies

  FInlineErrors:=InlineErrors;
  if FInlineErrors then FSource.OnError:=InlineError;
end;

function TStratoParser.Add(NodeType:xTypeNr;
  const FieldValues:array of xValue):rItem;
var
  p1,p2,i,j,k,l:cardinal;
  {$IFDEF DEBUG}
  src:pointer;
  {$ENDIF}
begin
  {$IFDEF DEBUG}
  asm
    mov eax,[ebp+4]
    mov src,eax
  end;
  //dec(src,5);?
  {$ENDIF}
  Result.x:=AddNode(FSrc,NodeType,0);
  p1:=Result.x div StratoSphereBlockBase;
  p2:=Result.x mod StratoSphereBlockBase;
  l:=Length(FieldValues);
  if l<>0 then
   begin
    {$IFDEF DEBUG}
    if (l and 1)=1 then
      raise Exception.Create('unexpected FieldValues odd length');
    if NodeType<n_TypeNr_Low then j:=0 else j:=xTypeDefX[NodeType];
    if j=0 then
      raise EStratoFieldIndexNotFound.CreateFmt(
        '%s: fields not defined',[NodeTypeToStr(NodeType)]) at src;
    {$ELSE}
    j:=xTypeDefX[NodeType];
    {$ENDIF}
    i:=0;
    while i<>l do
     begin
      k:=j;
      while (xTypeDef[k]<f_FieldsMax) and (xTypeDef[k]<>FieldValues[i]) do
        inc(k);
      if xTypeDef[k]<f_FieldsMax then
        Blocks[p1][p2+k-j+1]:=FieldValues[i+1]
      else
        raise EStratoFieldIndexNotFound.CreateFmt(
          '%s: field index not found %s',
          [NodeTypeToStr(NodeType),NodeFieldToStr(FieldValues[i])])
          {$IFDEF DEBUG}at src{$ENDIF};
      inc(i,2);
     end;
   end;
end;

function TStratoParser.Add(ListOwner:rItem;ListField:xTypeNr;NodeType:xTypeNr;
  const FieldValues:array of xValue):rItem;
var
  p:rItem;
begin
  Result:=Add(NodeType,FieldValues);
  //append: n(ListOwner,ListField) points to last element in list
  //        last element's fNext point to top of list
  p:=ListOwner.r(ListField);
  if p.x=0 then
    Result.s(iNext,Result)
  else
   begin
    Result.s(iNext,p.r(iNext));
    p.s(iNext,Result);
   end;
  ListOwner.s(ListField,Result);
end;

function TStratoParser.Add(ListOwner:rItem;ListField:xTypeNr;NodeType:xTypeNr;
  Name:xName;const FieldValues:array of xValue;var NewItem:rItem):boolean;
var
  p,q:rItem;
begin
  p:=ListOwner.r(ListField);
  if p.x=0 then
   begin
    NewItem:=Add(NodeType,FieldValues);
    NewItem.s(iNext,NewItem);
    NewItem.s(iName,Name);
    ListOwner.s(ListField,NewItem);
    Result:=true;
   end
  else
   begin
    q:=p.r(iNext);
    while (q.x<>p.x) and (q.v(iName)<>Name) do q:=q.r(iNext);
    if q.v(iName)=Name then
     begin
      //NewItem:=0;//?
      NewItem:=Add(NodeType,FieldValues);//create placeholder entry to avoid further errors
      Result:=false;
     end
    else
     begin
      q:=p.r(iNext);
      NewItem:=Add(NodeType,FieldValues);
      NewItem.s(iNext,q);
      NewItem.s(iName,Name);
      p.s(iNext,NewItem);
      ListOwner.s(ListField,NewItem);
      Result:=true;
     end;
   end;
end;

procedure TStratoParser.Append(ListOwner: rItem; ListField: xTypeNr;
  ListItem: rItem);
var
  p,q:rItem;
begin
  p:=ListOwner.r(ListField);
  q:=ListItem.r(iNext);
  if q.x<>0 then raise Exception.Create(
    'Append: broken list detected, item can''t be on two lists '+
    ItemToStr(ListItem));
  if p.x=0 then
    ListItem.s(iNext,ListItem)
  else
   begin
    ListItem.s(iNext,p.r(iNext));
    p.s(iNext,ListItem);
   end;
  ListOwner.s(ListField,ListItem);
end;

procedure TStratoParser.Prepend(ListOwner: rItem; ListField: xTypeNr;
  ListItem: rItem);
var
  p,q:rItem;
begin
  p:=ListOwner.r(ListField);
  q:=ListItem.r(iNext);
  if q.x<>0 then raise Exception.Create(
    'Append: broken list detected, item can''t be on two lists '+
    ItemToStr(ListItem));
  if p.x=0 then
   begin
    ListItem.s(iNext,ListItem);
    ListOwner.s(ListField,ListItem);
   end
  else
   begin
    ListItem.s(iNext,p.r(iNext));
    p.s(iNext,ListItem);
   end;
end;

function TStratoParser.AddName(const x:UTF8String):xName;
var
  i,j,l,v:cardinal;
  p,p0,p1,q,q1:rItem;
  qf:xTypeNr;
  kx:xValue;
begin
  i:=0;
  l:=Length(x);
  q.x:=FSrc * StratoSphereBlockBase;
  qf:=lSourceFile_Dictionary;
  while i<l do
   begin
    //4 bytes
    v:=0;
    for j:=0 to 3 do
      if i=l then
        v:=v shl 8
      else
       begin
        inc(i);
        v:=(v shl 8) or byte(x[i]);
       end;
    //lookup
    ListFirst(q,qf,p,p0);
    if p.x=0 then kx:=0 else kx:=p.v(vKey);
    p1.x:=0;
    while (p.x<>0) and (kx<v) do
     begin
      p1:=p;
      ListNext(p,p0);
      if p.x=0 then kx:=0 else kx:=p.v(vKey);
     end;
    if p.x=0 then
     begin
      //at end of list: append new
      p:=Add(n_NameData,[iParent,q.x,vKey,v]);
      Append(q,qf,p);
     end
    else if kx>v then
     begin
      //no matching node: insert new
      p:=Add(n_NameData,[iParent,q.x,vKey,v,iNext,p.x]);
      if p1.x=0 then
       begin
        q1:=q.r(qf);
        q1.s(iNext,p);//first in list
       end
      else
        p1.s(iNext,p);
     end;
    q:=p;
    qf:=lItems;
   end;
  Result:=q.x;
end;

procedure TStratoParser.InlineError(Sender: TObject; Line, LPos: cardinal;
  const ErrorMsg: string);
begin
  AddBinaryData(FSrc,UTF8Encode(Format('### %s(%d:%d): %s',
    [(Sender as TStratoSource).FilePath,Line,LPos,ErrorMsg])));
end;

procedure TStratoParser.DeclareIntrinsicTypes(ns:rItem);

  function A(const Name:UTF8String;s:integer):xItem;
  begin
    Result:=Add(ns,lItems,nType,
      [iParent,ns.x
      ,iName,AddName(Name)
      ,vByteSize,s
      ]).x;
  end;

begin
  //assert Store.SphereCount=0
  //assert n(ns,iName)=AddName('Strato')

  IntrinsicTypes[itVoid]:=A('void',0);
  IntrinsicTypes[itType]:=A('type',SystemWordSize);
  IntrinsicTypes[itPointer]:=A('pointer',SystemWordSize);
  IntrinsicTypes[itBoolean]:=A('bool',SystemWordSize);
  IntrinsicTypes[itNumber]:=A('number',SystemWordSize);
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

  IntrinsicTypes[itObject]:=0;//see StratoParseSource: allow only one object()={}
  //IntrinsicTypes[itHash]:=A('hash',SystemWordSize);//TODO:
  //IntrinsicTypes[itRange]:=A('range',SystemWordSize);//TODO:
  IntrinsicTypes[itString]:=A('string',SystemWordSize);
  //IntrinsicTypes[itVariant]:=A('variant',16);//TODO: OLE compatible variants
end;

procedure TStratoParser.ID(var n:xName;var nn:UTF8String;var SrcPos:xSrcPos);
begin
  nn:=Source.GetID(cardinal(SrcPos));
  n:=AddName(nn);
end;

procedure TStratoParser.ReplaceNode(Parent,Subject,ReplaceWith:rItem);
var
  p,p0,q:rItem;
begin
  //assert Subject<>nil
  //assert ReplaceWith not pointed to
  //assert ReplaceWith.Parent=Subject.Parent
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
end;

function TStratoParser.Fn(f:rItem;nx:xName):rItem;
var
  p:rItem;
begin
  p:=Lookup(f,nx);
  if (p.x=0) or (p.NodeType<>nMember) then
   begin
    if p.x<>0 then Source.Error('duplicate identifier "'+
      string(GetName(nx))+'"');
    p:=Add(nMember,
      [iParent,f.x
      ,iName,nx
      ]);
    Append(f,lItems,p);
   end;
  Result:=p;
end;

procedure TStratoParser.Parse;
begin
  //initialization
  stackSize:=stackGrowSize;
  SetLength(stack,stackSize);
  luxIndex:=0;
  luxSize:=0;

  //parse header then body
  ParseHeader;

  cb.x:=0;
  cbInhCalled:=false;//see also CbStart
  rd.x:=0;

  while not Source.IsNext([st_EOF]) do
    if cb.x=0 then
      if rd.x=0 then
        ParseDeclaration
      else
        ParseRecord
    else
      ParseLogic;
{

    case pc of
      pcDeclarative:
        ParseDeclaration;
      pcRecord:
        ParseRecord;
      pcImperative:
        ParseLogic;
      else
        Source.Error('Parse class not supported');
    end;
    }
  //TODO: flag sourcefile done
end;

procedure TStratoParser.ParseHeader;
var
  ns,p,p0:rItem;
  nx:xName;
  nn:UTF8String;
  SrcPos:xSrcPos;
begin
  //namespace
  if Source.IsNext([stIdentifier]) then
   begin
    ID(nx,nn,SrcPos);
    ns:=Add(FBase,lSourceFile_NameSpaces,nNameSpace,
      [vSrcPos,SrcPos
      ,iName,nx
      ]);
    while Source.IsNext([stPeriod,stIdentifier]) do
     begin
      Source.Token;//stIdentifier
      ID(nx,nn,SrcPos);
      ns:=Add(ns,lItems,nNameSpace,
        [iParent,ns.x
        ,vSrcPos,SrcPos
        ,iName,nx]);
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
    nx:=AddName(nn);
    ns:=Add(FBase,lSourceFile_NameSpaces,nNameSpace,[iName,nx]);
   end;
  SourceFiles[FSrc].Local:=ns.x;
  //runtime from first module
  if (FSrc=0) and (SourceFilesCount=1) then
   begin
    DeclareIntrinsicTypes(ns);
    SetLength(Imports,0);
   end
  else
   begin
    SetLength(Imports,1);
    ListFirst(xx0,lSourceFile_NameSpaces,p,p0);//not FBase!
    Imports[0].ns:=p;//'Strato'
    Imports[0].alias:=0;
    //TODO: list dependency (+ check cyclic)
   end;
end;

procedure TStratoParser.ParseDeclaration;
var
  nx:xName;
  nn,fqn:UTF8String;
  p,q,r,r0:rItem;
  st:TStratoToken;
  i:cardinal;
  SrcPos:xSrcPos;
begin
  while (cb.x=0) and (rd.x=0) and Source.NextToken(st) do
  case st of

    stThreeLT: //import a namespace
      ParseImport;

    stIdentifier: //declaration
     begin
      LookUpDecl_Lazy(p,nx,SrcPos,fqn);

      //operator override?
      if Source.IsNext([stPeriod,stStringLiteral]) then
       begin
        //TODO: lookup over Imports...
        q:=Lookup(p,nx);
        if q.x=0 then
         begin
          Source.Error('operator declaration on unknown type');
          q:=Add(p,lItems,nNameSpace,
            [iParent,p.x
            ,vSrcPos,SrcPos
            ,iName,nx
            ]);
         end;
        if p.x=SourceFiles[FSrc].Local then
          LookUpDecl_First(nx)
        else
          LookUpDecl_Next(nx);
        p:=q;
        Source.Token;//stStringLiteral
        //ID(nx,nn,SrcPos); but with GetStr:
        nn:=Source.GetStr;
        nx:=AddName(nn);
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
            r:=StratoFnAdd(Self,nPropGet,Fn(p,nx),
              Add(nSignature,
                [iParent,p.x
                ,vSrcPos,SrcPos
                ,iSubject,p.x
                ,iName,nx//+'_get'?
                ,iReturnType,q.x
                ]),SrcPos);
           if Source.IsNext([stCClose]) then
             begin
              //forward only
              if Source.IsNext([stCOpen,stCClose]) then //empty setter also? skip
                Source.Token;//stCClose
              //TODO: check declared somewhere later
             end
            else
              CbStart(r);
           end
          else

          //class "p.x:q={"
          if Source.IsNext([stDefine,stCOpen]) then
           begin
            if q.x=0 then
              Source.Error('undeclared base class')
            else
             begin
              if not Add(p,lItems,nClass,nx,
                [iParent,p.x
                ,vSrcPos,SrcPos
                ,iInheritsFrom,q.x
                ],p) then
                Source.ErrorN('duplicate identifier',nn);
              if q.NodeType=nClass then
                p.a(vByteSize,q.v(vByteSize))
              else
                Source.Error('base class must be a class');
             end;
            Source.Token;//stCOpen
            rd:=p;//switch to ParseRecord
           end
          else

          //variable "p.x:q"
           begin
            if not Add(p,lItems,nVar,nx,
              [iParent,p.x
              ,vSrcPos,SrcPos
              ,iType,q.x
              ],r) then
              Source.ErrorN('duplicate identifier',nn);
            if Source.IsNext([stDefine]) then
              r.s(iValue,ParseLiteral(Source.Token,nil));
            //TODO: check InitialValue.EvaluatesTo with EvaluatesTo
            case p.NodeType of
              nNameSpace:
                Add(FBase,lSourceFile_Globals,nGlobal,
                  [iParent,p.x
                  ,vSrcPos,SrcPos
                  ,iTarget,r.x
                  //,vBytesize,ByteSize(q)//TODO: pre-calculated here?
                  ]);
              nClass,nRecord:
                p.a(vByteSize,ByteSize(q));
                //TODO: support @ offset
              else
                Source.Error('unexpected variable parent');
            end;
           end;

          //done
          if (cb.x=0) and (rd.x=0) then Source.Skip(stSemiColon);
         end;

        stDefine://type, constant or enum "p.x="
          if Source.IsNext([stPOpen,stIdentifier]) then
           begin
            if not Add(p,lItems,nEnum,nx,
              [iParent,p.x
              ,vSrcPos,SrcPos
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
                if q.x=0 then
                  Source.Error('unknown type or constant')
                else
                  case q.NodeType of
                    nVar:
                     begin
                      r:=q.r(iType);
                      q:=q.r(iValue);
                      if q.x=0 then
                        Source.Error('var without initial value')
                      else
                        if not Add(p,lItems,nConstant,nx,
                          [iParent,p.x
                          ,vSrcPos,SrcPos
                          ,iValue,q.x
                          ,iType,r.x
                          ],q) then
                          Source.ErrorN('duplicate identifier',nn);
                     end;
                    nConstant:
                     begin
                      q:=q.r(iValue);
                      if q.x=0 then
                        Source.Error('constant without value')
                      else
                        if not Add(p,lItems,nConstant,nx,
                          [iParent,p.x
                          ,vSrcPos,SrcPos
                          ,iValue,q.x
                          ,iType,q.r(iType).x
                          ],q) then
                          Source.ErrorN('duplicate identifier',nn);
                     end;
                    nLiteral:
                      if not Add(p,lItems,nConstant,nx,
                        [iParent,p.x
                        ,vSrcPos,SrcPos
                        ,iValue,q.x
                        ,iType,q.r(iType).x
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
                          if q.x=0 then i:=0 else i:=ByteSize(q)*i;
                          if not Add(p,lItems,nArray,nx,
                            [iParent,p.x
                            ,vSrcPos,SrcPos
                            ,iType,q.x
                            ,vByteSize,i
                            ],r) then
                            Source.ErrorN('duplicate identifier',nn);
                         end
                      else //type alias
                        if not Add(p,lItems,nTypeAlias,nx,
                          [iParent,p.x
                          ,vSrcPos,SrcPos
                          ,iType,q.x
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
                if not Add(p,lItems,nConstant,nx,
                  [iParent,p.x
                  ,vSrcPos,SrcPos
                  ,iValue,q.x
                  ],p) then
                  Source.ErrorN('duplicate identifier',nn);
                if Source.IsNext([stColon]) then //here or in ParseLiteral?
                  q.s(iType,LookUpDecl_Type('literal type'));
                p.s(iType,q.r(iType));
               end;

              stCOpen://record (aka struct) "p.x={"
               begin
                if not Add(p,lItems,nRecord,nx,
                  [iParent,p.x
                  ,vSrcPos,SrcPos
                  ],q) then
                  Source.ErrorN('duplicate identifier',nn);
                rd:=q;//switch to ParseRecord
               end;

              stCaret://pointer type "p.x=^q"
                if not Add(p,lItems,nPointer,nx,
                  [iParent,p.x
                  ,vSrcPos,SrcPos
                  ,iTarget,LookUpDecl_Type('pointer type').x
                  ],q) then
                  Source.ErrorN('duplicate identifier',nn);

              stQuestionMark://class reference "p.x=?q"
               begin
                q:=LookUpDecl_Type('class reference type');
                if q.NodeType<>nClass then
                  Source.Error('invalid class reference subject');
                if not Add(p,lItems,nClassRef,nx,
                  [iParent,p.x
                  ,vSrcPos,SrcPos
                  ,iTarget,q.x
                  ],q) then
                  Source.ErrorN('duplicate identifier',nn);
               end;

{//moved into ParseLiteral...
              stOpSizeOf://constant from type size "p.x=@?q"
               begin
                if not Add(p,lItems,nConstant,nx,
                  [iParent,p
                  ,vSrcPos,SrcPos
                  ,iType,IntrinsicType(itNumber)
                  ],p) then
                  Source.ErrorN('duplicate identifier',nn);
                if Source.IsNext([stNumericLiteral]) then
                 begin
                  q:=IntrinsicType(itNumber);
                  Source.GetID;//skip
                 end
                else
                  q:=LookUpType('sizeof type');
                if q=0 then
                  Source.Error('unknown sizeof type')
                else
                 begin
                  i:=ByteSize(q);
                  //TODO: stSizeOf?
                  p.s(iValue,Add(nLiteral,
                    [vSrcPos,SrcPos
                    ,iType,IntrinsicType(itNumber)
                    ,iValue,AddBinaryData(IntToStr(i))
                    ]);
                 end;
               end;
}

              else
                Source.Error('unsupported type or constant');
             end;
           end;

        stPOpen://parameter list "p.x("
         begin
          q:=ParseSignature(p,nx,stPClose,SrcPos);
          case Source.Token of
            stSemiColon:
             begin
              r:=Lookup(p,nx);
              if r.x=0 then //just a signature? add to namespace
                Append(p,lItems,q)
              else
                case r.NodeType of
                  nMember:
                    StratoFnAdd(Self,nOverload,r,q,SrcPos);
                  nSignature:
                   begin
                    //another forward signature? create ttMember here
                    r0:=Add(nMember,
                      [iParent,p.x
                      ,iName,nx
                      ]);
                    ReplaceNode(p,r,r0);
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
              r:=Lookup(p,nx);
              if r.x=0 then
                p:=StratoFnAdd(Self,nOverload,Fn(p,nx),q,SrcPos)
              else
                case r.NodeType of
                  nMember:
                    p:=StratoFnAdd(Self,nOverload,r,q,SrcPos);
                  nSignature://signature forwarded, replace with ttMember
                   begin
                    r0:=Add(nMember,
                      [iParent,p.X
                      ,iName,nx
                      ]);
                    ReplaceNode(p,r,r0);
                    //StratoFnAdd checks for SameType(p,q):
                    StratoFnAdd(Self,nOverload,r0,r,r.v(vSrcPos));
                    p:=StratoFnAdd(Self,nOverload,r0,q,SrcPos);
                   end;
                  nClass://constructor
                   begin
                    q.s(iSubject,r);
                    p:=StratoFnAdd(Self,nCtor,r,q,SrcPos);
                   end;
                  else
                   begin
                    Source.ErrorN('duplicate identifier',nn);
                    r0:=Add(nMember,
                      [iParent,p.x
                      ,iName,nx
                      ]);
                    p:=StratoFnAdd(Self,nOverload,r0,q,SrcPos);
                   end;
                end;
              if p.x<>0 then CbStart(p);
             end;
            else Source.Error('unsupported signature syntax');
          end;
         end;

        stOpAssign://"p.x:="
          if Source.IsNext([stCOpen]) then
           begin
            //accept only one object:={}
            if not Add(p,lItems,nClass,nx,
              [iParent,p.x
              ,vSrcPos,SrcPos
              ],p) then
              Source.Error('duplicate identifier');
            if (FSrc=0) and (SourceFilesCount=1)
              and (IntrinsicTypes[itObject]=0)
              then
              IntrinsicTypes[itObject]:=p.x
            else
              Source.Error('only one master base class allowed');
            rd:=p;//switch to ParseRecord
           end
          else
            Source.Error('unsupported declaration syntax');

        stBOpen://"p.x["
         begin
          q:=StratoFnAdd(Self,nPropGet,Fn(p,nx),
            ParseSignature(p,nx,stBClose,SrcPos),SrcPos);
          if Source.IsNext([stCOpen]) then
            CbStart(q);
         end;

        stCOpen://"p.x{"
         begin
          if p.x=SourceFiles[FSrc].Local then
            q.x:=0
          else
            q:=p;
          r:=StratoFnAdd(Self,nOverload,Fn(p,nx),
            Add(nSignature,
              [iParent,p.x
              ,vSrcPos,SrcPos
              ,iSubject,q.x
              ,iName,nx
              ]),SrcPos);
          CbStart(r);
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
      LookUpDecl_Lazy(p,nx,SrcPos,fqn);
      st:=Source.Token;
      case st of
        stPOpen:
          if Source.IsNextID([stPClose,stCOpen]) or
            Source.IsNextID([stPClose,stDefine,stCOpen]) then
           begin //inherit this interface
            q:=LookUpDecl;
            if q.x=0 then
              Source.Error('undeclared base interface')
            else if q.NodeType<>nInterface then
              Source.Error('interface can only inherit from interface');
            Source.Skip(stPClose);
            Source.Skip(stDefine);//if?
            Source.Skip(stCOpen);
            if not Add(p,lItems,nInterface,nx,
              [iParent,p.x
              ,vSrcPos,SrcPos
              ,iInheritsFrom,q.x
              ],p) then
              Source.Error('duplicate identifier');
            ParseInterfaceDecl(p);
           end;
        stCOpen:
         begin
          if not Add(p,lItems,nInterface,nx,
            [iParent,p.x
            ,vSrcPos,SrcPos
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
        LookUpDecl_Lazy(p,nx,SrcPos,fqn);
        //ParseSignature? destructor doesn't have arguments/overloads
        Source.Skip(stPOpen);
        Source.Skip(stPClose);
        Source.Skip(stCOpen);
        //find class destructor is for
        q:=Lookup(p,nx);
        if q.x=0 then
         begin
          Source.Error('destructor for unknown class');
          q:=Add(nClass,[iName,nx]);
         end
        else
         begin
          if q.NodeType<>nClass then
           begin
            Source.Error('destructor only supported on class');
            q:=Add(nClass,[iName,nx]);
           end;
         end;
        //check any destructor already
        ListFirst(q,lItems,r,r0);
        while (r.x<>0) and (r.NodeType<>nDtor) do
          ListNext(r,r0);
        if r.x<>0 then
          Source.Error('duplicate destructor');
        //add
        CbStart(Add(q,lItems,nDtor,
          [iParent,q.x
          ,vSrcPos,SrcPos
          ]));
       end
      else
        Source.Error('unexpected token');

    stCOpen:
     begin
      CbStart(Add(nCodeBlock,
        [iParent,SourceFiles[FSrc].Local
        ,vSrcPos,Source.SrcPos
        ]));
      with SourceFile(FSrc)^ do
        if InitializationBlock=0 then
          InitializationBlock:=cb.x
        else
        if FinalizationBlock=0 then
          FinalizationBlock:=cb.x
        else Source.Error(
          'Initialization and finalization code already declared.');
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
        q.x:=SourceFiles[FSrc].Local;
        p:=Add(q,lItems,nType,
          [iParent,q.x
          ,vSrcPos,SrcPos
          ,iName,nx
          ]);
        if Source.IsNext([stAt,stNumericLiteral]) then
         begin
          Source.Token;//stNumericLiteral
          p.s(vByteSize,ParseInteger(Source.GetID(cardinal(SrcPos))));
         end;
        Source.Skip(stSemiColon);
       end
      else
        Source.Error('identifier expected');

    stSemiColon://;//stray semicolon? ignore
      {$IFDEF DEBUG}
      if Source.IsNext([stSemiColon,stSemiColon]) then
       begin
        Source.Skip(stSemiColon);
        Source.Skip(stSemiColon);
        asm int 3 end;//for debugging the parser
       end
      {$ENDIF}
      ;

    //stPOpen?
    //stThreeColons://TODO: switch/extend namespace?

    st_Unknown:Source.Error('unknown token');
    else Source.Error('unexpected token');
  end;
end;

procedure TStratoParser.ParseRecord;
type
  PCardinal=^cardinal;
const
  OffsetUseDefault=cardinal(-1);
var
  p,q,r,pUntyped:rItem;
  offset,i,j:cardinal;
  st:TStratoToken;
  b,neg:boolean;
  fn:UTF8String;
  nx:xName;
  SrcPos,SrcPos1:xSrcPos;
begin
  pUntyped.x:=0;//see stComma below
  while (cb.x=0) and (rd.x<>0) and Source.NextToken(st) do
  case st of

    stIdentifier:
     begin
      offset:=OffsetUseDefault;//default
      p.x:=0;//default
      fn:=Source.GetID(cardinal(SrcPos));
      if Source.IsNext([stColon]) then
        if Source.IsNext([stCOpen]) then //TODO move this into LookUpType?
         begin
          p:=Add(nRecord,
            [iParent,rd.x
            ,vSrcPos,SrcPos
            ,iName,AddName(fn)
            ]);
          rd:=p;//push? see stCClose below
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
              if not TryStrToInt(string(Source.GetID(cardinal(SrcPos1))),
                integer(i)) then
                Source.Error('record field offset not an integer');
            stOpSub:neg:=true;
            stOpAdd:neg:=false;
            stIdentifier:
             begin
              q:=Lookup(rd,AddName(Source.GetID(cardinal(SrcPos1))));
              if q.x=0 then
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
      if Source.IsNext([stSemiColon]) then
        if p.x=0 then
         begin
          Source.Error('record field requires type declaration');
          pUntyped.x:=0;
         end;

      //register field with record
      nx:=AddName(fn);
      if not Add(rd,lItems,nVar,nx,
        [iParent,rd.x
        ,vSrcPos,SrcPos
        ,iType,p.x
        ,vOffset,offset
        ],r) then
        Source.ErrorN('duplicate record field',fn);
      if pUntyped.x=0 then pUntyped:=r;

      if p.x<>0 then
        while pUntyped.x<>0 do
         begin
          offset:=pUntyped.v(vOffset);
          if offset=OffsetUseDefault then
            offset:=rd.a(vByteSize,ByteSize(p))
          else
            if integer(offset)<0 then
             begin
              if rd.x<>IntrinsicType(itObject).x then
               begin
                offset:=-integer(offset);
                Source.Error('negative record field offset not allowed');
               end;
             end
            else
             begin
              i:=offset+ByteSize(p);
              if i>rd.v(vByteSize) then rd.s(vByteSize,i);
             end;
          if pUntyped.x<>r.x then pUntyped.s(iType,p);
          pUntyped.s(vOffset,offset);

          ListNext(pUntyped,r);
         end;

     end;

    stComma:
      if pUntyped.x=0 then Source.Error('unexpected ","');

    //stQuestionMark: nested interface?
    //more?

    stCClose:
     begin
      //'pop'
      p:=rd.r(iParent);
      if p.NodeType=nRecord then rd:=p else rd.x:=0;
     end;

    stPOpen:
      Source.Error('unsupported record field syntax, declare methods outside of data section');

    else Source.Error('unsupported record field syntax');
  end;
end;

function TStratoParser.ParseLiteral(st0:TStratoToken;
  NeedValue:PInteger):rItem;//iLiteral
begin
  //into separate unit to avoid confusion with "stack"
  Result:=stratoLit.ParseLiteral(Self,LookUpDecl,st0,NeedValue);
end;

function TStratoParser.ParseSignature(ns:rItem;nx:xName;
  CloseToken:TStratoToken;SrcPos:xSrcPos):rItem;//nSignature
var
  st:TStratoToken;
  p,q,Signature,NoType:rItem;
  argName:UTF8String;
  byref:boolean;

  procedure AddArgument(InitialValue:rItem);
  var
    r:rItem;
  begin
    if byref then
     begin
      if not Add(Signature,lArguments,nSigArgByRef,AddName(argName),
        [iType,p.x
        ,iParent,Signature.x
        ,vSrcPos,SrcPos
        ],r) then
        Source.ErrorN('duplicate argument',argName);
      if InitialValue.x<>0 then
        Source.Error('default value on argument by reference not supported');
     end
    else
     begin
      if not Add(Signature,lArguments,nSigArg,AddName(argName),
        [iType,p.x
        ,iValue,InitialValue.x
        ,iParent,Signature.x
        ,vSrcPos,SrcPos
        ],r) then
        Source.ErrorN('duplicate argument',argName);
     end;
    byref:=false;
    if (p.x=0) and (NoType.x=0) then NoType:=r;
  end;

begin
  //assert one past token stPOpen
  Signature:=Add(nSignature,
    [iName,nx
    ,iParent,ns.x
    ,vSrcPos,SrcPos
    ]);
  Result:=Signature;
  NoType.x:=0;
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
        p.x:=0;//default
        q.x:=0;//default
        st:=Source.Token;
        case st of
          stColon://argument type
           begin
            p:=LookUpDecl_Type('argument type');
            while NoType.x<>0 do
             begin
              //assert NoType.TypeDecl=nil
              NoType.s(iType,p);
              NoType:=NoType.r(iNext);
             end;
            if Source.IsNext([stDefine]) then //default value
             begin
              q:=ParseLiteral(Source.Token,nil);
              q.s(iParent,Signature);
             end;
            AddArgument(q);
            if Source.IsNext([stComma]) or Source.IsNext([stSemiColon]) then
              ;//skip
           end;
          stComma:
            AddArgument(xx0);
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
  if ns.NodeType<>nNameSpace then //nRecord,nInterface,nTypeDecl
    Signature.s(iSubject,ns);
  if Source.IsNext([stColon]) then
    Signature.s(iReturnType,LookUpDecl_Type('returns type'))
  else
    if CloseToken=stBClose then
      Source.Error('property requires value type');
end;

procedure TStratoParser.ParseEnumeration(p:rItem);//nEnumeration
var
  st:TStratoToken;
  nx:xName;
  nn:UTF8String;
  i:integer;
  q:rItem;
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
        if Add(p,lItems,nConstant,nx,
          [iType,p.x
          ,vSrcPos,SrcPos
          ,iParent,p.x
          ],q) then
         begin
          //TODO: no literals here? cardinal member for nConstant
          q.s(iValue,Add(nLiteral,
            [vSrcPos,SrcPos
            ,iType,IntrinsicType(itNumber).x
            ,iValue,AddBinaryData(FSrc,IntToStr8(i))
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

procedure TStratoParser.ParseInterfaceDecl(p:rItem);//nInterface
var
  st:TStratoToken;
  nx:xName;
  nn:UTF8String;
  SrcPos:xSrcPos;
  q:rItem;
begin
  //assert previous token stCOpen
  while not(Source.IsNext([stCClose])) and Source.NextToken(st) do
    case st of

      stIdentifier:
       begin
        ID(nx,nn,SrcPos);
        case Source.Token of
          stColon://value //TODO: force property get/set?
            if not Add(p,lItems,nVar,nx,
              [iType,LookUpDecl_Type('field type').x
              ,iParent,p.x
              ,vSrcPos,SrcPos
              ],q) then
              Source.ErrorN('duplicate interface field',nn);
          stPOpen://signature
            StratoFnAdd(Self,nOverload,Fn(p,nx),
              ParseSignature(p,nx,stPClose,SrcPos),SrcPos);
          else Source.Error('unsupported interface field syntax');
        end;
        Source.Skip(stSemiColon);
       end;

      else Source.Error('unsupported interface field syntax');
    end;
end;

procedure TStratoParser.ParseImport;
var
  ns:rItem;
  nx:xName;
  nn,alias:UTF8String;
  fn,fn1:string;
  SrcPos:xSrcPos;
  ss:TStratoSource;
  pp:TStratoParser;
  i:cardinal;
begin
  fn:='';//default
  ns.x:=0;//default
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
      ID(nx,nn,SrcPos);
      fn:=UTF8ToString(nn)+'.';
      ns:=Add(FBase,lSourceFile_NameSpaces,nNameSpace,
        [iName,nx
        ,vSrcPos,SrcPos
        ]);
      while Source.IsNext([stPeriod,stIdentifier]) do
       begin
        Source.Token;//stIdentifier
        ID(nx,nn,SrcPos);
        fn:=fn+UTF8ToString(nn)+'.';
        ns:=Add(ns,lItems,nNameSpace,
          [iParent,ns.x
          ,iName,nx
          ,vSrcPos,SrcPos
          ]);
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
    while (i<SourceFilesCount) and (CompareText(fn1,
      UTF8ToString(BinaryData(xxr(SourceFiles[i].FileName))))<>0) do inc(i);
    if i<SourceFilesCount then
      ns.x:=SourceFiles[i].Local
    else
     begin
      //TODO: dispatch into (multi-threaded) stack/queue
      //load and parse
      ss:=TStratoSource.Create;
      try
        ss.OnError:=Source.OnError;//?
        ss.LoadFromFile(fn);
        if ss.IsNext([st_EOF]) then
          ns.x:=0 //raise?
        else
         begin
          pp:=TStratoParser.Create(ss,FInlineErrors);
          try
            pp.Parse;
            ns.X:=SourceFiles[pp.SrcIndex].Local;
            nn:=GetName(ns.v(iName));
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
  if ns.x<>0 then
   begin
    i:=Length(Imports);
    SetLength(Imports,i+1);
    Imports[i].ns:=ns;
    Imports[i].alias:=AddName(alias);
   end;
  //else Source.Error('nothing to import');?

  //TODO: if Source.IsNext([stPOpen?stCOpen?
end;

function TStratoParser.IsType(p:rItem):boolean;
begin
  Result:=false;//default
  while p.NodeType=nField do p:=p.r(iTarget);
  case p.NodeType of
    nType,nSignature,nArray,nRecord,nEnum,nPointer,
    nClass,nClassRef,nInterface:
      Result:=true;
    //nOverload,nCtor://TODO
    //  Result:=true;//signature!!!
    //TODO: nDtor: create signature on the fly?
    //nPropGet://TODO:IsType(Store.n(Store.n(p,iSignature),iReturnType);
    //nMember: find overload without arguments? //TODO

    nTypeAlias:
      Result:=true;//?
  end;
end;

function TStratoParser.IntrinsicType(it:TStratoIntrinsicType):rItem;
begin
  Result.x:=IntrinsicTypes[it];
  if Result.x=0 then Source.Error('intrinsic type not declared');//+?[it]
end;

function TStratoParser.LookUpDecl:rItem;
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

procedure TStratoParser.LookUpDecl_First(nx:xName);
var
  p,q:rItem;
  i:cardinal;
const
  luxGrowStep=8;//?
begin
  p:=Lookup(xxr(SourceFiles[FSrc].Local),nx);
  if p.x=0 then
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
      p.x:=0;//default
      q:=Imports[i].ns;
      if Imports[i].alias=nx then
       begin
        p:=q;
        //q:=Imports[i].ns;
       end
      else
        p:=Lookup(q,nx);
      if p.x<>0 then
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

procedure TStratoParser.LookUpDecl_Next(nx:xName);
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
      if lux[i].x<>0 then
        lux[i]:=Lookup(lux[i],nx);
end;

function TStratoParser.LookUpDecl_Any: rItem;
var
  i,j,k:cardinal;
  nn:UTF8String;
begin
  j:=0;
  k:=0;
  if luxIndex<>0 then
    for i:=0 to luxIndex-1 do
      if lux[i].x<>0 then
       begin
        inc(j);
        k:=i;
       end;
  case j of
    0://none found
      Result.x:=0;
    1://single thing found
      Result:=lux[k];
    else //multiple found
     begin
      nn:='';
      for i:=0 to luxIndex-1 do
        if lux[i].x<>0 then
          nn:=nn+','+FQN(lux[i]);
      Source.Error('multiple declarations "'+
        string(Copy(nn,2,Length(nn)-1))+'"');
      Result.x:=0;
     end;
  end;
end;

procedure TStratoParser.LookUpDecl_Lazy(var p: rItem; var nx: xName;
  var SrcPos: xSrcPos; var fqn: UTF8String);
var
  nn:UTF8String;
  q,p1:rItem;
begin
  ID(nx,nn,SrcPos);
  fqn:=nn;
  p1.x:=SourceFiles[FSrc].Local;
  p:=p1;
  while Source.IsNext([stPeriod,stIdentifier]) do
   begin
    q:=Lookup(p,nx);
    if q.x=0 then //force namespace //TODO: forward decls
      q:=Add(p,lItems,nNameSpace,
        [iParent,p.x
        ,vSrcPos,SrcPos
        ,iName,nx
        ]);
    if p.x=p1.x then
      LookUpDecl_First(nx)
    else
      LookUpDecl_Next(nx);
    p:=q;
    //next
    Source.Token;//stIdentifier
    ID(nx,nn,SrcPos);
    fqn:=fqn+'.'+nn;
   end;
end;

function TStratoParser.LookUpDecl_Type(const tname:string):rItem;
var
  p,q:rItem;
  ptr,i:cardinal;
begin
  //TODO: if stCOpen then ParseRecord here? (ParseJSON??)
  if Source.IsNext([stQuestionMark,stIdentifier]) then
   begin
    Source.Token;//stIdentifier
    p:=LookUpDecl;
    q:=p;
    if q.NodeType<>nClass then
      Source.Error('class reference allowed to class only');
    Result:=Add(nClassRef,
      [iTarget,p.x
      ,iParent,p.r(iParent).x//?
      ,vSrcPos,Source.SrcPos//?
      ]);
   end
  else
   begin
    ptr:=0;
    while Source.IsNext([stCaret]) do inc(ptr);
    if Source.IsNext([stIdentifier]) then
      Result:=LookUpDecl
    else
      Result.x:=0;
    if Result.x=0 then
      Source.Error('undefined '+tname)
    else
     begin
      case Result.NodeType of
        nType,nRecord,nArray,nEnum,nPointer,nClass,nClassRef,nInterface:
          ;//is a type
        nVar,nVarByRef,nVarReadOnly:
          Result:=Result.r(iType);//take var's type
        nSignature:
         begin
          //TODO: nDelegate? (keep signature and subject)
          Result:=Add(nPointer,
             [iTarget,Result.x
             ,iParent,Result.r(iParent).x
             ,vSrcPos,Source.SrcPos//?
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
          Result:=Add(nArray,
            [iName,AddName(GetName(Result.v(iName)))
            ,iType,Result.x
            ,vByteSize,ByteSize(Result)*i
            ,vSrcPos,Source.SrcPos//?
            ]);
         end;
     end;
    while ptr<>0 do
     begin
      dec(ptr);
      Result:=Add(nPointer,
        [iTarget,Result.x
        ,iParent,Result.r(iParent).x
        ,vSrcPos,Source.SrcPos//?
        ]);
     end;
   end;
end;

function TStratoParser.CbStart(pp:rItem):rItem;
var
  p0,Signature,p,q,v,a,a0:rItem;
  tt:xTypeNr;
  o:cardinal;
begin
  //prepared code-block?
  if pp.NodeType=nCodeBlock then
    p0:=pp
  else
   begin
    p0:=Add(nCodeBlock,
      [iParent,pp.x
      ,vSrcPos,Source.SrcPos
      ]);
    //assert pp.Body=0
    pp.s(iBody,p0);
    //populate code block
    //this "@@"
    if pp.NodeType=nDtor then
     begin
      Signature.x:=0;
      p:=pp.r(iParent);
     end
    else
     begin
      //assert pp.NodeType in [nOverload,nCtor,nPropGet,nPropSet]
      Signature:=pp.r(iSignature);
      p:=Signature.r(iSubject);
     end;
    if p.x<>0 then
      Add(p0,lLocals,nThis,
        [iParent,p0.x
        ,iType,p.x
        ,vOffset,p0.a(vByteSize,SystemWordSize)
        ]);
    if Signature.x<>0 then
     begin
      //return value?
      p:=Signature.r(iReturnType);
      if p.x<>0 then
        if pp.NodeType=nCtor then
          //with a constructor, store the effective class type here
          Add(p0,lLocals,nVarReadOnly,
            [iParent,p0.x
            ,vSrcPos,Source.SrcPos
            ,iName,AddName('?@@')
            ,vOffset,p0.a(vByteSize,SystemWordSize)
            ,iType,Add(nClassRef,
              [iParent,p0.x
              ,vSrcPos,Source.SrcPos
              ,iTarget,IntrinsicType(itObject).x
              ]).x
            ])
        else
          Add(p0,lLocals,nVar,//TODO: nVarByRef here
            [iParent,p0.x
            ,vSrcPos,pp.v(vSrcPos)
            ,iName,pp.rr(iParent,iName).x //nMember's name
            ,vOffset,p0.a(vByteSize,ByteSize(p))
            ,iType,p.x
            ]);
      //arguments
      ListFirst(Signature,lArguments,a,a0);
      if a.x<>0 then
       begin
        q:=pp.r(iFirstArgVar);
        while a.x<>0 do
         begin
          if Lookup(p0,a.v(iName)).x<>0 then
            Source.Error('duplicate identifier "'+string(
              GetName(a.v(iName)))+'"');
          p:=a.r(iType);
          if a.NodeType=nSigArgByRef then tt:=nVarByRef else
            tt:=nVarReadOnly;
          if tt=nVarByRef then
            o:=p0.a(vByteSize,SystemWordSize)
          else
            if p.x=0 then
              o:=0//raise?
            else
              o:=p0.a(vByteSize,ByteSize(p));
          v:=Add(p0,lLocals,tt,
            [iParent,p0.x
            ,vSrcPos,a.v(vSrcPos)
            ,iName,a.v(iName)
            ,vOffset,o
            ,iType,p.x
            ]);
          //store first arg value on function overload index
          if q.x=0 then
           begin
            q:=v;
            pp.s(iFirstArgVar,q);
           end;
          //next argument
          ListNext(a,a0);
         end;
       end;  
     end;
   end;

  //switch to ParseLogic
  //assert cb=0
  cb:=p0;
  cbInhCalled:=false;
  //more?
  Result:=pp;
end;

procedure TStratoParser.CbAdd(p:rItem);
begin
  if p.x<>0 then
   begin
    {$IFDEF DEBUG}
    if p.r(iNext).x<>0 then
      raise Exception.Create('broken chain detected');
    {$ENDIF}
    //TODO: nAlias here?
    Append(cb,lItems,p);
   end;
end;

procedure TStratoParser.Push(pr:TPrecedence;p1,p2:rItem;sp:xSrcPos);
begin
  stackPushed:=true;//see Combine
  if stackIndex=stackSize then
   begin
    inc(stackSize,stackGrowSize);//grow
    SetLength(stack,stackSize);
   end;
  stack[stackIndex].pr:=pr;
  stack[stackIndex].p1:=p1.x;
  stack[stackIndex].p2:=p2.x;
  stack[stackIndex].sp:=sp;
  inc(stackIndex);
end;

procedure TStratoParser.Pop(var pr:TPrecedence;var p1:rItem;var p2:rItem;
  var sp:xSrcPos);
begin
  {$IFDEF DEBUG}
  if stackIndex=0 then raise Exception.Create('Can''t pop from empty stack');
  {$ENDIF}
  dec(stackIndex);
  pr  :=stack[stackIndex].pr;
  p1.x:=stack[stackIndex].p1;
  p2.x:=stack[stackIndex].p2;
  sp  :=stack[stackIndex].sp;
  {$IFDEF DEBUG}
  stack[stackIndex].pr:=p___;
  stack[stackIndex].p1:=0;
  stack[stackIndex].p2:=0;
  stack[stackIndex].sp:=0;
  {$ENDIF}
end;

function TStratoParser.Peek:TPrecedence; //inline;
begin
 if stackIndex=0 then
   Result:=p___
 else
   Result:=stack[stackIndex-1].pr;
end;

function TStratoParser.Peek1:rItem; //inline;
begin
 if stackIndex=0 then
   Result.x:=0
 else
   Result.x:=stack[stackIndex-1].p1;
end;

procedure TStratoParser.LookUpLogic(nx:xName;var p:rItem;var pt:rItem;
  SrcPos:xSrcPos);
var
  i:cardinal;
  q,q0,r:rItem;
  rt:rItem;
begin
  if p.x<>0 then
   begin
    //see below: search by type
    r:=p;
    rt:=pt;
    p.x:=0;
   end
  else
   begin
    //check code block, then up stack
    r.x:=0;
    rt.x:=0;
    p:=Lookup(cb,nx);
    if p.x=0 then
     begin
      i:=stackIndex;
      while (i<>0) and (p.x=0) do
       begin
        dec(i);
        case stack[i].pr of
          pCodeBlock:
            p:=Lookup(xxr(stack[i].p1),nx);
          pCatch:
           begin
            p.x:=stack[i].p1;
            p:=p.r(iTarget);
            if not((p.NodeType=nVarReadOnly) and (p.v(iName)=nx)) then
              p.x:=0;
           end;
        end;
       end;
     end;
    //not found? check under 'this'
    if p.x=0 then
     begin
      q:=cb;
      while (r.x=0) and (q.NodeType=nCodeBlock) do
       begin
        //ListFirst(q,lLocals,,);
        q0:=q.rr(lLocals,iNext);
        if q0.NodeType=nThis then
         begin
          //see below: search by type
          r:=q0;
          rt:=q0.r(iType);
         end
        else
          q:=q.r(iParent);
       end;
     end;
   end;
  //search under something?
  if r.x<>0 then
   begin
    //assert Parent=0
    //first try direct
    case r.NodeType of
      nClass:
        while (p.x=0) and (r.x<>0) do
         begin
          p:=Lookup(r,nx);
          if p.x=0 then
            r:=r.r(iInheritsFrom);
         end;
      nRecord:
        p:=Lookup(r,nx);
      //TODO: more?
    end;
    //nothing yet, is it typed? search type
    if p.x=0 then //) and (ParentType.p<>0) then
     begin
      case rt.NodeType of
        nClass:
          while (p.x=0) and (rt.x<>0) do
           begin
            p:=Lookup(rt,nx);
            if p.x=0 then
              rt:=rt.r(iInheritsFrom);
           end;
        nRecord:
          p:=Lookup(rt,nx);
        //TODO: more?
      end;
     end
    else
      r.x:=0;
   end;
  //nothing yet? check locals
  if p.x=0 then
   begin
    r.x:=0;
    LookUpDecl_First(nx);
    //no "while Source.IsNext([stPeriod,stIdentifier])" here!
    p:=LookUpDecl_Any;
   end;
  //resolve tyle
  case p.NodeType of
    0:pt.x:=0;
    nType:
      pt:=IntrinsicType(itType);
    nClass:
      pt:=IntrinsicType(itType);//add(nClassRef?
    nMember:
      pt.x:=0;//assert resolved later
    else
      pt:=p.r(iType);
  end;
  //field
  if (p.x<>0) and (r.x<>0) then
    p:=Add(nField,
      [iParent,cb.x
      ,vSrcPos,SrcPos
      ,iSubject,r.x
      ,iTarget,p.x
      //,iType,pt.x
      ]);
end;

function TStratoParser.Combine(zz:TPrecedence;var p:rItem;var pt:rItem):xSrcPos;
begin
  Result:=Source.SrcPos;//default
  stackPushed:=false;
  while not(stackPushed) and (Peek>zz) do
    Result:=CombineTop(p,pt);
end;

function TStratoParser.CombineTop(var p:rItem;var pt:rItem):xSrcPos;
var
  z:TPrecedence;
  p1,p2,q1,q2:rItem;
  SrcPos,SrcPos1:xSrcPos;
begin
  //IMPORTANT: don't call Push from within CombineTop!
  Pop(z,p1,p2,SrcPos);
  Result:=SrcPos;
  case z of

    pCodeBlock:
      Source.Error('missing expected "}"');
    pParentheses:
      Source.Error('missing expected ")"');
    pBrackets:
      Source.Error('missing expected "]"');

    //nSelection
    pIfThen:
      if p.x=0 then
       begin
        //just a floating boolean value, no selection
        p:=p1;
        pt:=p2;
       end
      else
       begin
        //assert Peek=pIfElse
        Push(pIfElse,Add(nSelection,
          [iParent,cb.x
          ,vSrcPos,SrcPos
          ,iPredicate,p1.x
          ,iDoTrue,p.x
          ,iReturnType,pt.x
          ]),xx0,SrcPos);
        p.x:=0;
        pt.x:=0;
       end;
    pIfElse:
     begin
      if p.x=0 then
        //selection without false section
      else
       begin
        p1.s(iDoFalse,p);
        CheckType_Selection(p1,pt);
       end;
      p:=p1;
      //pt:=p1.r(iReturnType);
     end;

    pRange:
     begin
      p1.s(iRight,p);
      CheckType_Range(p2,pt);
      p:=p1;
      //pt.x:=pt.x;//?
     end;

    //nIteration,nIterPostEval
    pIterationX:
     begin
      //assert Peek=pIterationZ
      if SameType(pt,IntrinsicType(itBoolean)) then
       begin
        if p1.x<>0 then
          Source.Error('unexpected iterator for iteration with boolean predicate');
        Push(pIterationZ,Add(nIteration,
          [iParent,cb.x
          ,vSrcPos,SrcPos
          ,iPredicate,p.x
          //,iBody,//see pIterationZ
          //,iReturnType,//see pIterationZ
          ]),xx0,SrcPos);
       end
      else if p.NodeType=nRange then
       begin
        if Peek=pUnTypedVar then
         begin
          Pop(z,q1,q2,SrcPos1);
          if q1.x<>q2.x then
            Source.Error('unexpected multiple iterators');//TODO: ?
          q1.s(iType,pt);//assert q1=p1
          q1.s(vOffset,cb.a(vByteSize,ByteSize(pt)));
         end;
        Push(pIterationZ,Add(nIteration,
          [iParent,cb.x
          ,vSrcPos,SrcPos
          ,iPredicate,Add(nRangeIndex,
            [iParent,cb.x
            ,vSrcPos,SrcPos
            ,iLeft,p1.x //iterator
            ,iRight,p.x //range
            ]).x
          ]),xx0,SrcPos);
        //TODO: more checks on nRangeIndex?
       end
      else if p.x=0 then
       begin
        if p1.x<>0 then
          Source.Error('unexpected iterator for iteration without predicate');
        Push(pIterationY,xx0,xx0,SrcPos);
       end
      else
        Push(pIterationZ,Add(nIterPostEval,
          [iParent,cb.x
          ,vSrcPos,SrcPos
          //,iPredicate,//see pIterationZ
          ,iBody,p.x
          ,iReturnType,pt.x
          ]),xx0,SrcPos);
      p.x:=0;
      pt.x:=0;
     end;
    pIterationY:
     begin
      if p1.x<>0 then
        Source.Error('unexpected iterator for iteration with boolean predicate');
      Push(pIterationZ,Add(nIteration,
        [iParent,cb.x
        ,vSrcPos,SrcPos
        ,iPredicate,p.x
        //,iBody,//see pIterationZ
        //,iReturnType,//see pIterationZ
        ]),xx0,SrcPos);
      p.x:=0;
      pt.x:=0;
     end;
    pIterationZ:
      if p1.NodeType=nIterPostEval then
       begin
        if SameType(pt,IntrinsicType(itBoolean)) then
          p1.s(iPredicate,p)
        else
          Source.Error('boolean expression expected for iteration predicate');
        p:=p1;
        pt:=p1.r(iReturnType);
       end
      else
       begin
        p1.s(iBody,p);
        p1.s(iReturnType,pt);
        p:=p1;
        //pt:=pt;//pt:=p1.s(iReturnT
       end;

    //nUnaryOp
    pUnary:
     begin
      if p.x=0 then Source.Error('unary operand missing');
      //TODO: lookup
      p:=Add(nUnaryOp,
        [iParent,cb.x
        ,vSrcPos,SrcPos
        ,vOperator,p1.x//xValue(st)
        ,iRight,p.x
        ,iReturnType,pt.x
        ]);
      //pt:=pt;
     end;
    pSizeOf:
     begin
      if p.x=0 then Source.Error('size-of operand missing');
      q1:=IntrinsicType(itNumber);
      p:=Add(nUnaryOp,
        [iParent,cb.x
        ,vSrcPos,SrcPos
        ,vOperator,p1.x//xValue(st)
        ,iRight,p.x
        ,iReturnType,q1.x
        ]);
      pt:=q1;
     end;
    pTypeOf:
     begin
      if p.x=0 then Source.Error('type-of operand missing');
      if pt.NodeType=nClass then
        q1:=Add(nClassRef,
          [iParent,cb.x
          ,vSrcPos,SrcPos
          ,iTarget,pt.x
          ])
      else
        q1:=IntrinsicType(itType);
      p:=Add(nUnaryOp,
        [iParent,cb.x
        ,vSrcPos,SrcPos
        ,vOperator,p1.x//xValue(st)
        ,iRight,p.x
        ,iReturnType,q1.x
        ]);
      pt:=q1;
     end;
    pAddressOf:
     begin
      //is addressable?
      if IsAddressable(p) then
       begin
        while pt.NodeType=nArray do
          pt:=pt.r(iType);
        q1:=Add(nPointer,
          [iParent,cb.x
          ,vSrcPos,SrcPos
          ,iTarget,pt.x
          ]);
       end
      else
       begin
        Source.Error('invalid address-of subject');
        q1.x:=0;
       end;
      p:=Add(nAddressOf,
        [iParent,cb.x
        ,vSrcPos,SrcPos
        ,iSubject,p.x
        ,iReturnType,q1.x
        ]);
      pt:=q1;
     end;

    //nBinaryOp
    pMulDiv,pAddSub,pShift,
    pLogicalOr,pLogicalXor,pLogicalAnd,
    pBitwiseOr,pBitwiseXor,pBitwiseAnd:
     begin
      if p.x=0 then Source.Error('binary operand missing');
      p1.s(iRight,p);
      CheckType_Operator(p1,pt);
      p:=p1;
      //pt:=pt?
     end;
    pEqual,pComparative:
     begin
      if p.x=0 then Source.Error('comparison operand missing');
      p1.s(iRight,p);
      CheckType_Comparison(p1,pt);
      p:=p1;
      pt:=IntrinsicType(itBoolean);
     end;

    //nAssign
    pAssign:
      if p.x=0 then
        Source.Error('invalid assignment value')
      else
       begin
        if Peek=pUnTypedVar then
         begin
          Pop(z,q1,q2,SrcPos1);
          if q1.x<>q2.x then
            Source.Error('unexpected multiple assignees');//TODO: deconstruction (or tuples?)
          q1.s(iType,pt);
          q1.s(vOffset,cb.a(vByteSize,ByteSize(pt)));
          if q1.x=p1.r(iTarget).x then p2.x:=pt.x;//assert p2.x was 0
         end;

        p1.s(iValue,p);
        CheckType_Assignment(p1,pt,p2);

        //assigning an object reference? reference counting!
        if pt.NodeType=nClass then
         begin
          if TStratoToken(p1.v(vOperator))<>stOpAssign then
            Source.Error('invalid assignment type for object reference');
          if IntrinsicType(itObject).x=0 then
            Source.Error('base class for reference counting not defined')
          else
           begin
            //TODO: check not zero then release
            //TODO: call _addref (with StratoFnCallFindSignature ?)
            //TODO: defer release refcount
           end;
         end;

        p:=p1;
        pt.x:=0;//by language design!
       end;

    pUnTypedVar:
     begin
      //see also pAssignment above and stColon below
      //assert p1.NodeType=nVar
      if p1.r(iType).x=0 then
        Source.Error('no type for local var "'+string(FQN(p1))+'"');
     end;

    pCast:
     begin
      if pt.x=0 then //if pt<>IntrinsicType(itType)?
        Source.Error('invalid cast')
      else
        if ByteSize(p)<>ByteSize(p2) then
          Source.Error('cast type size mismatch');
      p1.s(iType,p);
      pt:=p;
      p:=p1;
     end;

    pDefer:
     begin
      p:=Add(nDefer,
        [iParent,cb.x
        ,vSrcPos,SrcPos
        ]);
      pt.x:=0;
     end;
    pThrow:
     begin
      p:=Add(nThrow,
        [iParent,cb.x
        ,vSrcPos,SrcPos
        ,iSubject,p.x
        ]);
      pt.x:=0;
     end;
    pCatch:
     begin
      p1.s(iBody,p);//TODO: check? here or with stCClose??
      p.x:=0;
      pt.x:=0;
     end;
    //else ?
  end;
end;

procedure TStratoParser.Juxta(var p:rItem; var pt:rItem);
var
  SrcPos:xSrcPos;
begin
  if p.x<>0 then
   begin
    SrcPos:=Combine(p_Juxta,p,pt);
    case Peek of
      pIfThen,pIterationX:
        CombineTop(p,pt);
      pIterationZ:
        CombineTop(p,pt);//????
      else
        if SameType(pt,IntrinsicType(itBoolean)) then
          Push(pIfThen,p,pt,SrcPos)
        else
          Source.Error('missing operator or semicolon');
    end;
    p.x:=0;
    pt.x:=0;
   end;
end;

procedure TStratoParser.PushUnary(st:TStratoToken;var p:rItem; var pt:rItem);
begin
  Push(pUnary,xxr(xValue(st)),xx0,Source.SrcPos);
  p.x:=0;
  pt.x:=0;
end;

procedure TStratoParser.PushBinary(pr:TPrecedence;st:TStratoToken;
  var p:rItem;var pt:rItem);
var
  SrcPos:xSrcPos;
begin
  Combine(pr,p,pt);
  if p.x=0 then
    Source.Error('no left side defined for binary operator')
  else
   begin
    SrcPos:=Source.SrcPos;
    p:=Add(nBinaryOp,
      [iParent,cb.x
      ,vSrcPos,SrcPos
      ,vOperator,xValue(st)
      ,iLeft,p.x
      //,iRight,//see Combine
      ,iReturnType,pt.x
      ]);
    Push(pr,p,xx0,SrcPos);
   end;
  p.x:=0;
  pt.x:=0;
end;

procedure TStratoParser.CheckPassed(p:rItem);
var
  b:boolean;
begin
  if p.x<>0 then
   begin
    b:=false;
    case p.NodeType of
      nVar,nVarByRef,nVarReadOnly,
      nFCall,nSCall,nVCall,nICall,
      nIteration,nIterPostEval,nAssign,
      nDefer,nThrow,nCatch,
      nDtor://,nPropGetCall,nPropSetCall:
        b:=true;
      nCodeBlock,nSelection:
        b:=true;//b:=p.EvaluatesTo=0;//TODO: descend into?
      nBinaryOp:
        b:=TStratoToken(p.v(vOperator)) in [stOpAssign..stOpAssignAnd];
      nUnaryOp:
        b:=TStratoToken(p.v(vOperator)) in [stOpInc,stOpDec];
      //more?
    end;
    if not b then
      Source.Error('statement without calls or assignments');
   end;
end;

procedure TStratoParser.ParseLogic;
var
  nx:xName;
  nn,fqn:UTF8String;
  p,p0,p1,p2,p3:rItem;
  pt,q:rItem;
  tt:xTypeNr;
  st:TStratoToken;
  SrcPos,SrcPos1:xSrcPos;
  z:TPrecedence;
  i:cardinal;
begin
  stackIndex:=0;
  p.x:=0;
  pt.x:=0;
  while (cb.x<>0) and Source.NextToken(st) do
  case st of

    stIdentifier:
     begin
      Juxta(p,pt);
      ID(nx,nn,SrcPos);
      LookUpLogic(nx,p,pt,SrcPos);

      if p.x=0 then
        Source.ErrorN('undeclared identifier',nn)
      else
      if (Peek=pCast) and (pt.x<>0) then
        CombineTop(p,pt)
      else
        StratoCheckMemberNoArguments(Self,p,pt,cb,SrcPos);
     end;

    stPeriod://"."
      if p.x=0 then
        Source.Error('unexpected "."')
      else
      if Source.IsNext([stIdentifier]) then
       begin
        //Combine?
        ID(nx,nn,SrcPos);
        LookUpLogic(nx,p,pt,SrcPos);

        if p.x=0 then
          Source.ErrorN('undeclared identifier',nn)//TODO: FQN?
        else
        if (Peek=pCast) and IsType(pt) then
          CombineTop(p,pt)
        else
          StratoCheckMemberNoArguments(Self,p,pt,cb,SrcPos);
          
       end
      else
        Source.Error('unexpected "."');

    stStringLiteral:
     begin
      Juxta(p,pt);
      pt:=IntrinsicType(itString);
      p:=Add(nLiteral,
        [vSrcPos,Source.SrcPos
        ,iType,pt.x
        ]);
      if Peek in [pIfThen,pIfElse] then
        p.s(iValue,AddBinaryData(FSrc,Source.GetStr))
      else
        p.s(iValue,AddBinaryData(FSrc,Source.GetStrs));
     end;

    stNumericLiteral:
     begin
      Juxta(p,pt);
      pt:=IntrinsicType(itNumber); //default
      p:=Add(nLiteral,
        [vSrcPos,Source.SrcPos
        ,iType,pt.x
        ,iValue,AddBinaryData(FSrc,Source.GetID(cardinal(SrcPos)))
        ]);
      if Source.IsNext([stColon]) then
       begin
        pt:=LookUpDecl_Type('literal type');
        if pt.x=0 then Source.Error('unknown literal type');
        p.s(iType,pt);
       end;
     end;

    stColon://":"
      if p.x=0 then
        if not(Source.IsNext([stIdentifier,stPeriod]))
          and Source.IsNext([stIdentifier]) then
         begin
          //new local variable(s)
          ID(nx,nn,SrcPos);
          if not Add(cb,lLocals,nVar,nx,
            [iParent,cb.x
            ,vSrcPos,SrcPos
            //,iType,//see pUnTypedVar
            //,vOffset,//see pUnTypedVar
            ],p) then
            Source.Error('duplicate identifier "'+string(nn)+'"');
          Push(pUnTypedVar,p,p,SrcPos);//see also stComma
          pt.x:=0;
         end
        else
          Source.Error('no value to cast')
      else
        //local variable(s) type decl?
        if Peek=pUnTypedVar then
         begin
          //TODO: tuples...
          pt:=LookUpDecl_Type('type');
          i:=ByteSize(pt);
          Pop(z,p1,p2,SrcPos);
          while p1.x<>0 do
           begin
            p1.s(iType,pt);//assert was 0
            p1.s(vOffset,cb.a(vByteSize,i));
            ListNext(p1,p2);
           end;
          if Source.IsNext([stSemiColon]) then
           begin
            p.x:=0;//don't add as statement
            pt.x:=0;
           end;
         end
        else
         begin
          //cast
          Push(pCast,Add(nCast,
            [iParent,cb.x
            ,vSrcPos,Source.SrcPos
            ,iSubject,p.x
            //,iType,//see Combine pCast
            ]),pt,Source.SrcPos);
          p.x:=0;
          pt.x:=0;
         end;

    stPOpen://"("
     begin
      SrcPos:=Combine(p_POpen,p,pt);
      if Peek<>pIterationZ then
        if SameType(pt,IntrinsicType(itBoolean)) then
         begin
          Push(pIfThen,p,pt,SrcPos);
          p.x:=0;
          pt.x:=0;
         end;
      Push(pParentheses,p,xx0,SrcPos);
      p.x:=0;
      pt.x:=0;
     end;

    stPClose://")"
     begin
      Combine(pParentheses,p,pt);
      if Peek=pParentheses then
       begin
        Pop(z,p1,p2,SrcPos);
        if p1.x=0 then
         begin
          if not(Peek in [pIterationX,pIterationY]) then
            if SameType(pt,IntrinsicType(itBoolean)) then
             begin
              //See also Juxta()
              Push(pIfThen,p,pt,SrcPos);
              p.x:=0;
              pt.x:=0;
             end
            else
              if Peek in [pIfThen,pIfElse] then
                CombineTop(p,pt);
         end
        else
         begin
          StratoFnCallAddArgument(Self,p2,p,pt,Source.SrcPos);
          //calling constructor or destructor?
          p.x:=0;
          pt.x:=0;
          tt:=nOverload;//default
          case p1.NodeType of
            nClass://calling destructor? (detect prefix '-' or '~')
              if (Peek=pUnary) and (TStratoToken(Peek1.v(vOperator))
                in [stOpSub,stTilde]) and (IntrinsicTypes[itObject]<>0) then
               begin
                Pop(z,p0,p0,SrcPos);
                tt:=nDtor;
               end
              else
                tt:=nCtor;
            nSCall:
             begin
              //assert p0=p1 and ps=Sphere
              p1.s(lArguments,p2);
              p:=p1;
              pt.x:=0;//pt:=p1.rr(iSignature,iReturnType);
             end;
            nThis://"@@@(...)": call inherited
              tt:=nThis;
          end;
          if p.x=0 then
            StratoFnCallBySignature(Self,tt,p1,p2,cb,SrcPos,p,pt,tt=nThis);
          if p.x=0 then
            Source.Error('no function overload found with these arguments');
         end;
       end
      else
        Source.Error('unexpected ")"');
     end;

    stComma://","
     begin
      Combine(pUnTypedVar,p,pt);
      case Peek of
        pParentheses,pBrackets:
          if (Peek1.x=0) or (p.x=0) then
            Source.Error('unexpected ","')
          else
            StratoFnCallAddArgument(Self,
              rItem(stack[stackIndex-1].p2),//Peek2^,
              p,pt,Source.SrcPos);
        pUnTypedVar:
          if p.x=Peek1.x then
            if not(Source.IsNext([stIdentifier,stPeriod]))
              and Source.IsNext([stIdentifier]) then
             begin
              Pop(z,p1,p2,SrcPos1);
              //new local variable(s)
              ID(nx,nn,SrcPos);
              if not Add(cb,lLocals,nVar,nx,
                [iParent,cb.x
                ,vSrcPos,SrcPos
                ,vOffset,cb.v(vByteSize)
                ],p2) then
                Source.Error('duplicate identifier "'+string(nn)+'"');
              Push(pUnTypedVar,p1,p2,SrcPos);
              //assert p2 one or more iNext's from p1
             end
            else
              Source.Error('unexpected ","')
          else
            if (p.NodeType=nAssign) and
              (TStratoToken(p.v(vOperator))=stOpAssign) then
              CbAdd(p)
            else
              Source.Error('unexpected syntax in local variable declaration');
        else
          Source.Error('unexpected ","');
      end;
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
        Juxta(p,pt);
        //start code block
        SrcPos:=Source.SrcPos;
        Push(pCodeBlock,cb,xx0,SrcPos);
        cb:=Add(nCodeBlock,
          [iParent,cb.x
          ,vSrcPos,SrcPos
          ]);
       end;

    stCClose://"}"
     begin
      Combine(p_Statement,p,pt);
      if p.x<>0 then
       begin
        //resulting value
        p1:=cb.r(iParent);
        case p1.NodeType of
          nOverload,nPropGet:
           begin
            q:=p1.rr(iSignature,iReturnType);
            if q.x=0 then
             begin
              if p.x<>0 then
                Source.Error('missing closing semicolon');
             end
            else
             begin
              if not SameType(pt,q) then
                Source.Error('result value type mismatch');
              //assign to result value
              nx:=p1.v(iName);
              p:=Add(nAssign,
                [iParent,cb.x
                ,vSrcPos,Source.SrcPos
                ,vOperator,xValue(stOpAssign)
                ,iTarget,Lookup(p,nx).x//nMember
                ,iValue,p.x
                ]);
              pt.x:=0;//by language design!
             end;
           end
          else
           begin
            CheckPassed(p);
            //??? p.iType:=ResType(Sphere,p);
           end;
        end;
        CbAdd(p);
       end;
      while not(Peek in [p___,pCodeBlock]) do
       begin
        Pop(z,p,p0,SrcPos);
        if z=pIfElse then
          Source.Error('if-then without else')
        else
          Source.Error('unexpected "}"');//+xDisplay(p)?
       end;
      p:=cb;//keep a copy
      if stackIndex=0 then
       begin
        //return to declarations
        cb.x:=0;
        //code block done: checks
        p1:=p.r(iParent);
        case p1.NodeType of
          nCtor://constructor block done? check inherited called
            if not cbInhCalled then
             begin
              p2:=p1.rr(iParent,iParent);//nCtors>nClass
              if p2.x<>IntrinsicType(itObject).x then
               begin
                StratoFnCallBySignature(Self,nCtor,
                  p2.r(iInheritsFrom),p1.rr(iSignature,lArguments),//!!
                  p,p.v(vSrcPos),p3,pt,true);
                if (p3.x=0) and (p1.r(iFirstArgVar).x<>0) then
                  //try again for inherited constructor without arguments
                  StratoFnCallBySignature(Self,nCtor,
                    p2.r(iInheritsFrom),xx0,
                    p,p.v(vSrcPos),p3,pt,true);
                if p3.x=0 then
                  Source.Error('unable to find base constructor')
                else
                 begin
                  //arguments
                  StratoFnArgByValues(Self,p3,p.r(iParent));
                  //insert first into code block
                  Prepend(p,lItems,p3);
                 end;
               end;
             end;
          nDtor://destructor block done? check inherited called
            if not cbInhCalled then
             begin
              p2:=p1.r(iParent);//nClass
              if p2.x<>IntrinsicType(itObject).x then
               begin
                StratoFnCallBySignature(Self,nDtor,
                  p2.r(iInheritsFrom),xx0,
                  p,Source.SrcPos,p3,pt,true);
                if p3.x=0 then
                  Source.Error('unable to find base destructor')
                else
                  Append(p,lItems,p3);
               end;
             end;
          nPropGet://property getter done? parse setter
           begin
            //TODO: if not(cbInhCalled)
            if Source.IsNext([stCOpen]) then
             begin
              //TODO: construct setter signature? (use the same for now)
              p2:=StratoFnAdd(Self,nPropSet,
                p1.r(iParent),p1.r(iSignature),
                Source.SrcPos);//r.Parent.SrcPos?
              CbStart(p2);
             end
            else
              Source.Skip(stSemiColon);//closing property declaration
           end;

          //TODO: nPropSet and not cbInhCalled
          //else

        end;
       end
      else
       begin
        //pop from stack
        Pop(z,cb,p0,SrcPos);
        //assert z=pCodeBlock (see above)
        Combine(p_Statement,p,pt);//?
        //add to parent chain
        if (p.x<>0) and (pt.x=0) then CbAdd(p);
       end;
      p.x:=0;
      pt.x:=0;
     end;

    stSemiColon:
     begin
      {$IFDEF DEBUG}
      if Source.IsNext([stSemiColon,stSemiColon]) then
       begin
        Source.Skip(stSemiColon);
        Source.Skip(stSemiColon);
        asm int 3 end;//for debugging the parser
       end;
      {$ENDIF}
      Combine(p_Statement,p,pt);
      if p.x<>0 then
       begin
        CheckPassed(p);
        CbAdd(p);
       end;
      p.x:=0;
      pt.x:=0;
     end;

    stDefine:
     begin
      Source.Error('use either ":=" or "=="');
      p.x:=0;
      pt.x:=0;
     end;

    stOpAssign,
    stOpAssignAdd,stOpAssignSub,
    stOpAssignMul,stOpAssignDiv,stOpAssignMod,
    stOpAssignOr,stOpAssignAnd:
     begin
      //Combine(pAssign,p,pt);
      if Peek=pCast then CombineTop(p,pt);//allow only casts (dirty assignment)
      if p.x=0 then
        Source.Error('no left side defined for assignment')
      else
       begin
        Push(pAssign,Add(nAssign,
          [iParent,cb.x
          ,vSrcPos,Source.SrcPos
          ,vOperator,xValue(st)
          ,iTarget,p.x
          //,iValue,see Combine pAssign
          ]),pt,Source.SrcPos);
        p.x:=0;
        pt.x:=0;
       end;
     end;
    stOpEQ,stOpNEQ:
      PushBinary(pEqual,st,p,pt);
    stOpLT,stOpLTE,stOpGT,stOpGTE,stOpTypeIs:
      PushBinary(pComparative,st,p,pt);
    //TODO: stOpLT: if not Combine(pComparative,p) then support inline HTML?
    stOpAdd:
     begin
      Combine(pAddSub,p,pt);
      if p.x=0 then //unary operator
        PushUnary(st,p,pt)
      else
        PushBinary(pAddSub,st,p,pt);
     end;
    stOpSub:
     begin
      Combine(pAddSub,p,pt);
      if p.x=0 then
        if Source.IsNext([stAtAt,stPOpen,stPClose]) then
         begin
          p:=StratoFnCallDestructor(Self,cb);
          pt.x:=0;//since destructor never returns a value
         end
        else
          PushUnary(st,p,pt)
      else
        PushBinary(pAddSub,st,p,pt);
     end;
    stOpMul,stOpDiv,stOpMod:
      PushBinary(pMulDiv,st,p,pt);
    stOpShl,stOpShr,stThreeLT://stThreeGT: see below
      PushBinary(pShift,st,p,pt);
    stOpAnd:
      if SameType(pt,IntrinsicType(itBoolean)) then
        PushBinary(pLogicalAnd,st,p,pt)
      else
        PushBinary(pBitwiseAnd,st,p,pt);
    stOpOr:
      if SameType(pt,IntrinsicType(itBoolean)) then
        PushBinary(pLogicalOr,st,p,pt)
      else
        PushBinary(pBitwiseOr,st,p,pt);
    stOpXor:
      if SameType(pt,IntrinsicType(itBoolean)) then
        PushBinary(pLogicalXor,st,p,pt)
      else
        PushBinary(pBitwiseXor,st,p,pt);
    stOpNot:
      if p.x=0 then
        PushUnary(st,p,pt)
      else
        Source.Error('Unexpected left operand to unary NOT');
    stTilde:
      if p.x=0 then
        if Source.IsNext([stAtAt,stPOpen,stPClose]) then
          p:=StratoFnCallDestructor(Self,cb)
        else
          PushUnary(st,p,pt)
      else
        Source.Error('Unexpected left operand to unary XOR');

    stOpInc,stOpDec:
      if p.x=0 then
        Source.Error('increment/decrement operators only allowed as suffix')
      else
        p:=Add(nUnaryOp,
          [iParent,cb.x
          ,vSrcPos,Source.SrcPos
          ,vOperator,xValue(st)
          ,iReturnType,pt.x
          ,iRight,p.x  //should be 'iLeft', but most nUnaryOp uses iRight...
          ]);

    stAtAt://"@@": this
     begin
      Juxta(p,pt);
      //see also StratoFnAddOverload
      p1:=cb;
      while p1.x<>0 do
       begin
        //assert nThis first of lLocals
        p:=p1.rr(lLocals,iNext);//ListFirst
        if p.NodeType=nThis then
          p1.x:=0//end loop
        else
         begin
          p.x:=0;
          p1:=p1.r(iParent);
          if p1.NodeType<>nCodeBlock then p1.x:=0;
         end;
       end;
      if p.x=0 then
       begin
        Source.Error('"@@" undefined');
        p:=Add(nThis,//add anyway to avoid further errors
          [iParent,cb.x
          //,iType,?
          //,vOffset,?
          ]);
       end;
      pt:=p.r(iType);
     end;
    stWhatWhat://"??": result value
     begin
      Juxta(p,pt);
      p0.x:=0;
      p1:=cb;
      while (p1.x<>0) and (p1.NodeType=nCodeBlock) do
       begin
        p0:=p1;
        p1:=p1.r(iParent);
       end;
      if p1.x=0 then p.x:=0 else
        case p1.NodeType of
          nOverload,nPropGet,nPropSet:
            p:=Lookup(p0,p1.rr(iParent,iName).x);//nMember
          else
            p.x:=0;
        end;
      if p.x=0 then
       begin
        Source.Error('"??" undefined');
        p:=Add(nVar,//add anyway to avoid further errors
          [iParent,cb.x
          ,vSrcPos,Source.SrcPos
          ,iName,AddName('??')
          ]);
       end;
      pt:=p.r(iType);
     end;

    //stAmpersand:?

    stHashHash://iteration "##"
     begin
      Combine(p_Juxta,p,pt);//?
      Push(pIterationX,p,pt,Source.SrcPos);
      p.x:=0;
      pt.x:=0;
     end;
    stOpRange:
     begin
      Combine(pRange,p,pt);
      //TODO: if p.x=0 then create literal "0"?
      p:=Add(nRange,
        [iParent,cb.x
        ,vSrcPos,Source.SrcPos
        ,iLeft,p.x
        //,iRight,//see Combine
        ,iReturnType,pt.x
        ]);
      Push(pRange,p,pt,SrcPos);
      p.x:=0;
      pt.x:=0;
     end;

    stThreeColons://":::"
     begin
      Combine(p_Statement,p,pt);
      CbAdd(Add(nTry,
        [iParent,cb.x
        ,vSrcPos,Source.SrcPos
        ]));
     end;
    stThreeGT://">>>"
     begin
      Combine(pShift,p,pt);
      if p.x=0 then //defer
       begin
        Combine(p_Statement,p,pt);
        Push(pDefer,xx0,xx0,Source.SrcPos);
       end
      else
        PushBinary(pShift,st,p,pt);//roll right
     end;
    stThreeWhats://"???"
     begin
      Combine(p_Statement,p,pt);
      //"???(e:ExceptionType)"
      SrcPos:=Source.SrcPos;
      if Source.IsNext([stPOpen,stIdentifier,stColon,stIdentifier]) then
       begin
        p1:=Add(nCatch,
          [iParent,cb.x
          ,vSrcPos,SrcPos
          ]);
        Source.Token;//stIdentifier
        ID(nx,nn,SrcPos);
        Source.Token;//stColon
        Source.Token;//stIdentifier
        p1.s(iTarget,Add(cb,lLocals,nVarReadOnly,
          [iParent,cb.x
          ,vSrcPos,SrcPos
          ,iName,nx
          ,iType,LookUpDecl_Type('catch filter').x
          ,vOffset,cb.a(vByteSize,SystemWordSize)//??!!
          ]));
        Source.Skip(stPClose);//TODO: enforce
       end
      else
      if Source.IsNext([stPOpen]) then
       begin
        p1:=Add(nCatch,
          [iParent,cb.x
          ,vSrcPos,SrcPos
          ]);
        repeat
          Add(p1,lTypes,nTypeAlias,
            [iParent,p1.x
            ,vSrcPos,Source.SrcPos
            ,iTarget,LookUpDecl_Type('catch filter').x
            ]);
        until not Source.IsNext([stComma]);
        Source.Skip(stPClose);
       end
      else
        p1:=Add(nCatch,
          [iParent,cb.x
          ,vSrcPos,SrcPos
          ]);
      Push(pCatch,p1,xx0,SrcPos);
     end;
    stThreeBangs://"!!!"
     begin
      Juxta(p,pt);
      Push(pThrow,xx0,xx0,Source.SrcPos);
     end;

    stBOpen://"["
      if p.x=0 then
        Source.Error('unexpected "["')//TODO: lambda
      else
       begin
        //TODO: check p is of type array?
        Push(pBrackets,p,xx0,Source.SrcPos);
        p.x:=0;
        pt.x:=0;
       end;
    stBClose://"]"
     begin
      Combine(pBrackets,p,pt);
      if Peek=pBrackets then
       begin
        Pop(z,p1,p2,SrcPos);
        if p1.x=0 then
         begin
          //TODO:
          Source.Error('unsupported "[]" without subject');//TODO: lambda
         end
        else
         begin
          StratoFnCallAddArgument(Self,p2,p,pt,Source.SrcPos);
          StratoFnCallBySignature(Self,nPropGet,p1,p2,cb,SrcPos,p,pt,false);
          if p.x=0 then
            Source.Error('no property overload found with these arguments');
         end;
       end
      else
        Source.Error('unexpected "]"');
     end;

    stAt://"@"
     begin
      Juxta(p,pt);
      //TODO: if p<>0 then ?
      Push(pAddressOf,xx0,xx0,Source.SrcPos);
     end;

    stQuestionMark://"?"
     begin
      if p.x<>0 then Source.Error('unexpected "?"');//TODO: trinary "x?y:z"?
      p.x:=0;
      pt.x:=0;
      //type-of this in constructor?
      if Source.IsNext([stAtAt]) then
       begin
        p0:=cb;
        p1:=p0.r(iParent);
        while p1.NodeType=nCodeBlock do
         begin
          p0:=p1;
          p1:=p1.r(iParent);
         end;
        if p1.NodeType=nCtor then
         begin
          p:=Lookup(p0,AddName('?@@'));
          pt:=p.r(iType);//IntrinsicType(itType)?
          if p.x=0 then Source.Error('constructor class reference not found');
         end;
       end;
      if p.x=0 then
        Push(pTypeOf,xxr(xValue(st)),xx0,Source.SrcPos);
     end;
    stOpSizeOf://"@?"
     begin
      Juxta(p,pt);
      if p.x<>0 then Source.Error('unexpected "?"');
      Push(pSizeOf,xxr(xValue(st)),xx0,Source.SrcPos);
     end;

    stCaret://"^"
      if p.x=0 then
        Source.Error('unexpected "^"')
      else
       begin
        Juxta(p,pt);
        case pt.NodeType of
          nPointer://TODO: xSignature? xOverload?
           begin
            pt:=pt.r(iTarget);
            p:=Add(nDereference,
              [iParent,cb.x
              ,vSrcPos,Source.SrcPos
              ,iSubject,p.x
              ,iReturnType,pt.x
              ]);
           end;
          else
            Source.Error('dereference expected on pointer');
        end;
       end;

    stAtAtAt://"@@@": inherited
     begin
      Juxta(p,pt);
      SrcPos:=Source.SrcPos;
      if Source.IsNext([stPOpen]) then
       begin
        //see also StratoFnAddOverload
        p1:=cb;
        while p1.x<>0 do
         begin
          //assert nThis first of lLocals
          p:=p1.rr(lLocals,iNext);//ListFirst
          if p.NodeType=nThis then
            p1.x:=0 //end loop
          else
           begin
            p.x:=0;
            p1:=p1.r(iParent);
            if p1.NodeType<>nCodeBlock then p1.x:=0;
           end;
         end;
        if p.x=0 then
         begin
          Source.Error('"@@" undefined');
          p:=Add(nThis,//add anyway to avoid further errors
            [iParent,cb.x
            //,iType,?
            //,vOffset,?
            ]);
         end;
        Push(pParentheses,p,xx0,SrcPos);//see also stPClose
        cbInhCalled:=true;
        p.x:=0;
        pt.x:=0;
       end
      else
        Source.Error('manipulating inherited pointer not allowed');
     end;

    stDollar://"$":abstract system call
     begin
      Juxta(p,pt);
      SrcPos1:=Source.SrcPos;
      if Source.IsNextID([stPOpen]) then
       begin
        ID(nx,nn,SrcPos);
        fqn:=nn;
        while Source.IsNext([stPeriod,stIdentifier]) do
         begin
          Source.Token;//stIdentifier
          ID(nx,nn,SrcPos);
          fqn:=fqn+'.'+nn;
         end;
        Source.Token;//stPOpen
        //TODO: checks?
        Push(pParentheses,Add(nSCall,
          [iParent,cb.x
          ,vSrcPos,SrcPos1
          ,iTarget,AddBinaryData(FSrc,fqn)
          ]),xx0,SrcPos1);
        p.x:=0;
        pt.x:=0;
       end
      else
      if Source.IsNext([stStringLiteral]) then
       begin
        p:=Add(nSCall,
          [iParent,cb.x
          ,vSrcPos,SrcPos1
          ,iTarget,AddBinaryData(FSrc,Source.GetStr)
          ]);
        pt.x:=0;
       end
      else
      if Source.IsNext([stDollar,stNumericLiteral]) then
       begin
        p:=Add(nSCall,
          [iParent,cb.x
          ,vSrcPos,SrcPos1
          ,iTarget,ParseLiteral(Source.Token,nil).x
          ]);
        pt.x:=0;
       end
      else
        Source.Error('unexpected "$"');
     end;

    else
      Source.Error('unsupported syntax');//'unexpected token');
  end;

  if stackIndex<>0 then
    Source.Error('unexpected end of source ('+IntToStr(stackIndex)+')');
end;

procedure TStratoParser.CheckType_Selection(p,pt:rItem);
var
  q:rItem;
begin
  q:=p.r(iReturnType);
  if p.r(iDoTrue).x=0 then
    p.s(iReturnType,pt)
  else
    if (q.x<>0) and not(SameType(q,pt)) then
      if SameType(pt,q) then
        p.s(iReturnType,pt);
end;

procedure TStratoParser.CheckType_Operator(p,pt:rItem);
var
  q:rItem;
begin
  q:=p.r(iReturnType);
  if (q.x=0) or (pt.x=0) then
    Source.Error('binary operator operand type missing')
  else
    if q.x<>pt.x then
      if not(SameType(q,pt)) then
        if SameType(pt,q) then
          p.s(iReturnType,pt)
        else
          Source.Error('binary operator operand type mismatch');
end;

procedure TStratoParser.CheckType_Comparison(p,pt:rItem);
var
  ok:boolean;
  q:rItem;
begin
  ok:=false;//default
  q:=p.r(iReturnType);
  p.s(iReturnType,IntrinsicType(itBoolean));
  if (q.x<>0) and (pt.x<>0) then
   begin
    if TStratoToken(p.v(vOperator))=stOpTypeIs then
      ok:=true
    else
      ok:=(q.x=pt.x)
        or (SameType(q,pt))
        or (SameType(pt,q))//?
    ;
    //TODO: if p1=Type_bool then support 'x<y<z'
   end;
  if not ok then
    Source.Error('comparison operand type mismatch');
end;

procedure TStratoParser.CheckType_Assignment(p,pt,qt: rItem);
var
  q:rItem;
begin
  //Result:=0;//default
  q:=p.r(iTarget);
  if q.NodeType=nCast then //allow dirty cast
   begin
    if IsAddressable(q.r(iSubject)) then
     begin
      //TODO: if nArray
      if not SameType(pt,qt) then
        Source.Error('assignment type mismatch');
     end
    else
      Source.Error('assignment receiver not addressable');
    //TODO: check ByteSize's equal?
    //TODO: switch to enable pointer arith (default off!)
   end
  else
   begin
    if IsAddressable(q) then
     begin
      //while Sphere.n(r,vTypeNr)^=nArray do r:=Sphere.n(r,fTypeDecl)^;
      if not SameType(pt,qt) then
        Source.Error('assignment type mismatch');
     end
    else
      Source.Error('assignment receiver not addressable');
    //TODO: local lookup member ":=" (use TStratoToken(p2)!)
   end;
end;

procedure TStratoParser.CheckType_Range(p1,p2:rItem);
begin
  if not SameType(p1,p2) then
    Source.Error('range terminals type mismatch');
end;

end.
