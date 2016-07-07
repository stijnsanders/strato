unit stratoSphere;

interface

{xxx$D-}
{xxx$L-}

uses SysUtils, Classes, stratoDict, stratoSource, stratoDecl;

type
  TStratoSphere=class(TObject)
  private
    FDict:TStringDictionary;
    FBlock:array of record
      First,Module:TStratoIndex;//cardinal
      Index,Size:cardinal;
      Data:pointer;//PStratoSphereDataBlock see below
    end;
    FBlockIndex,FBlockCount,FBlockSize:cardinal;
    FBasePath:string;
    FGetNodeCacheID:TStratoIndex;
    FGetNodeCacheST:PStratoThing;
    FZeroes:TStratoThing;
    function GetNode(ID: TStratoIndex): PStratoThing;
  public
    constructor Create;
    destructor Destroy; override;
    function MarkIndex(Index:TStratoIndex): TStratoIndex;

    //add
    function Add(ThingType: TStratoThingType;
      const Values:array of cardinal): TStratoIndex; overload;
    function Add(Parent: TStratoIndex; ListField: TStratoField; ThingType: TStratoThingType;
      const Values:array of cardinal): TStratoIndex; overload;
    function Add(Parent: TStratoIndex; ListField: TStratoField; ThingType: TStratoThingType;
      const Values:array of cardinal;
      var Index: TStratoIndex): boolean; overload;

    function Append(Parent:TStratoIndex;List:TStratoField;
      Item:TStratoIndex):TStratoIndex;
    function Prepend(Parent:TStratoIndex;List:TStratoField;
      Item:TStratoIndex):TStratoIndex;

    //get ThingType:
    function t(p:TStratoIndex):TStratoThingType;
    //get reference:
    function r(p:TStratoIndex;f:TStratoField):TStratoIndex; overload;
    function r(p:TStratoIndex;f:TStratoField;var q:TStratoIndex):boolean; overload;
    function rr(p:TStratoIndex;const ff:array of TStratoField):TStratoIndex;
    //get value:
    function v(p:TStratoIndex;f:TStratoField):cardinal;
    //set:
    procedure s(p:TStratoIndex;f:TStratoField;q:TStratoIndex); overload;
    //procedure s(p:TStratoIndex;f:TStratoField;v:cardinal); overload;
    procedure s(p:TStratoIndex;const Values:array of cardinal); overload;

    function Lookup(Parent:TStratoIndex;ListField:TStratoField;
      Name:TStratoName):TStratoIndex;
    function FQN(p:TStratoIndex):UTF8String;
    function AddBinaryData(const x:UTF8String):TStratoIndex;
    function GetBinaryData(p:TStratoIndex):UTF8String;
    function NextIndex(var Index:TStratoIndex):boolean;
    function DebugInfo(p:TStratoIndex):string;
    procedure AddGlobalVar(p:TStratoIndex);
    procedure ReadSettings(const IniPath:string);
    procedure LoadFromFile(const FilePath:string);
    procedure SaveToFile(const FilePath:string);
    procedure Error(p:TStratoIndex;const Msg:string);
    procedure InlineError(Sender:TObject;Line,LPos:cardinal;
      const ErrorMsg:string);
    property Dict:TStringDictionary read FDict;
    //property Node[ID:TStratoIndex]:PStratoThing read GetNode; default;
    property BasePath:string read FBasePath;
  end;

function StratoGetSourceFile(s:TStratoSphere;p:TStratoIndex;
  var q:TStratoIndex;var srcLine,srcColumn:cardinal;var p1,p2:TStratoIndex):boolean;

implementation

uses Windows, stratoRunTime, stratoTokenizer, stratoLogic, stratoFn, Math;

const
  StratoSphereDataBlockSize=$800;//2048
  StratoSphereBlockGrowSize=8;
  StratoSphereFileVersion=$00000106;//0.1.6

type
  TStratoSphereDataBlock=array[0..StratoSphereDataBlockSize-1] of TStratoThing;
  PStratoSphereDataBlock=^TStratoSphereDataBlock;

function BlockAlloc:pointer;
begin
  //New(PStratoSphereDataBlock(FBlock[0].Data));
  //ZeroMemory(FBlock[0].Data,SizeOf(TStratoSphereDataBlock));
  Result:=VirtualAlloc(nil,SizeOf(TStratoSphereDataBlock),
    MEM_RESERVE or MEM_COMMIT,PAGE_READWRITE);//MEM_LARGE_PAGES?
  if Result=nil then RaiseLastOSError;
end;

procedure BlockFree(x:pointer);
begin
  if not VirtualFree(x,0,MEM_RELEASE) then RaiseLastOSError;
end;

{ TStratoSphere }

constructor TStratoSphere.Create;
var
  h:PStratoThing;
begin
  inherited Create;
  FDict:=TStringDictionary.Create;
  FBlockIndex:=0;
  FBlockCount:=1;
  FBlockSize:=StratoSphereBlockGrowSize;
  SetLength(FBlock,FBlockSize);
  FBlock[0].First:=0;
  FBlock[0].Module:=0;
  FBlock[0].Index:=1;
  FBlock[0].Size:=StratoSphereDataBlockSize;
  FBlock[0].Data:=BlockAlloc;
  FGetNodeCacheID:=0;
  FGetNodeCacheST:=nil;

  //header
  h:=@(PStratoSphereDataBlock(FBlock[0].Data)[0]);
  h[0]:=ttFileMarker;//tf_FileMarker
  h[1]:=0;//tf_ThinCount, see SaveToFile
  h[2]:=StratoSphereFileVersion;//tf_Version
  h[3]:=0;//tf_FirstNameSpace, see stratoRunTime
  h[4]:=0;//tf_FirstGlobalVar
  h[5]:=0;//tf_GlobalByteSize
  h[6]:=0;//tf_FirstInitialization
  h[7]:=0;//tf_FirstFinalization
end;

destructor TStratoSphere.Destroy;
begin
  FDict.Free;
  //for i:=FBlockCount-1 downto 0 do Dispose(PStratoSphereDataBlock(FBlock[0].Data));
  //hack: avoid expensive dealloc:
  pointer(FBlock):=nil;//SetLength(FData,0);
  inherited Destroy;
end;

function TStratoSphere.MarkIndex(Index: TStratoIndex): TStratoIndex;
var
  i,j:cardinal;
begin
  //TODO: push/pop (or cleaner separate branches?)
  Result:=FBlock[FBlockIndex].First;//output previous
  {
  if (Index and (StratoSphereDataBlockSize-1))<>0 then
    raise Exception.Create('Block start indexes must be a multiple of block size');
  }
  if Index>=IndexStep1-1 then
    raise Exception.Create('Block start index reserved');
  j:=FBlockCount;
  for i:=0 to FBlockCount-1 do
    if Index=FBlock[i].First then
      j:=i
    else
    if (Index>FBlock[i].First) and
      (Index<FBlock[i].First+FBlock[i].Size) and
      (Index>=FBlock[i].First+FBlock[i].Index) then
      FBlock[i].Size:=FBlock[i].First-Index;
  if j=FBlockCount then
   begin
    if FBlockCount=FBlockSize then
     begin
      inc(FBlockSize,StratoSphereBlockGrowSize);
      SetLength(FBlock,FBlockSize);
     end;
    inc(FBlockCount);
    FBlock[j].First:=Index;
    FBlock[j].Module:=0;//TODO
    FBlock[j].Index:=0;
    FBlock[j].Size:=StratoSphereDataBlockSize;
    FBlock[j].Data:=BlockAlloc;
   end;
  FBlockIndex:=j;
end;

procedure TStratoSphere.ReadSettings(const IniPath: string);
var
  sl:TStringList;
begin
  sl:=TStringList.Create;
  try
    //TODO: something else? per project overrides? command line
    try
      sl.LoadFromFile(IniPath);
    except
      on EFOpenError do ;//silent
    end;
    FBasePath:=IncludeTrailingPathDelimiter(sl.Values['BasePath']);//TODO: relative to ParamStr(0)
    if FBasePath=PathDelim then FBasePath:=ExtractFilePath(ParamStr(0));
  finally
    sl.Free;
  end;
end;

procedure TStratoSphere.Error(p: TStratoIndex; const Msg: string);
var
  q:TStratoIndex;
  i,j:cardinal;
begin
  q:=p;
  while (q<>0) and not(t(q) in [ttNameSpace,ttPrivate,ttOverload,
    ttConstructor,ttPropertyGet,ttPropertySet]) do
    q:=r(q,tfParent);
  if q<>0 then
    case t(q) of
      ttNameSpace:
        q:=r(q,tf_NameSpace_SourceFile);
      ttOverload,ttConstructor,ttPrivate,ttPropertyGet,ttPropertySet:
        q:=r(q,tfSourceFile);
      else
        q:=0;//raise?
    end;
  //TODO: raise?
  if (p=0) or (q=0) then
    Writeln(ErrOutput,Msg)
  else
   begin
    i:=v(p,tfSrcPos);
    j:=v(q,tf_SourceFile_SrcPosLineIndex);
    Writeln(ErrOutput,Format('%s(%d:%d): %s',
      [GetBinaryData(r(q,tf_SourceFile_FileName))
      ,i div j
      ,i mod j
      ,Msg
      ]));
   end;
  ExitCode:=1;
end;

function TStratoSphere.Add(ThingType: TStratoThingType;
  const Values:array of cardinal): TStratoIndex;
var
  p:TStratoIndex;
  pp:PStratoThing;
  i,j,l:cardinal;
  f:TStratoField;
  q:TStratoIndex;
begin
  l:=Length(Values);
  if (l mod 2)<>0 then
    raise Exception.Create('TStratoSphere.Add Values required 2-by-2');
  if FBlock[FBlockIndex].Index=FBlock[FBlockIndex].Size then
   begin
    //block full, determine first index for next block
    p:=FBlock[FBlockIndex].First+FBlock[FBlockIndex].Size;
    j:=StratoSphereDataBlockSize;
    i:=0;
    while i<FBlockCount do
      if (p>=FBlock[i].First) and (p<FBlock[i].First+FBlock[i].Size) then
       begin
        p:=FBlock[i].First+FBlock[i].Size;
        j:=StratoSphereDataBlockSize;
        i:=0;//restart
       end
      else
      if (p+j>=FBlock[i].First) and (p+j<FBlock[i].First+FBlock[i].Size) then
       begin
        j:=FBlock[i].First-p;
        inc(i);
       end
      else
        inc(i);
    //start a new block
    if FBlockCount=FBlockSize then
     begin
      inc(FBlockSize,StratoSphereBlockGrowSize);
      SetLength(FBlock,FBlockSize);
     end;
    FBlockIndex:=FBlockCount;
    inc(FBlockCount);
    FBlock[FBlockIndex].First:=p;
    FBlock[FBlockIndex].Module:=0;//TODO:
    FBlock[FBlockIndex].Index:=0;
    FBlock[FBlockIndex].Size:=j;
    FBlock[FBlockIndex].Data:=BlockAlloc;
   end;
  i:=FBlock[FBlockIndex].Index;
  inc(FBlock[FBlockIndex].Index);
  Result:=FBlock[FBlockIndex].First+i;
  pp:=@(PStratoSphereDataBlock(FBlock[FBlockIndex].Data)[i]);
  //ZeroMemory(q^?
  pp[0]:=ThingType;//tfThingType
  i:=0;
  while (i<l) do
   begin
    f:=Values[i];
    q:=Values[i+1];
    inc(i,2);
    {$IFDEF DEBUG}
    pp[rx(ThingType,f,t,q)]:=q;
    {$ELSE}
    pp[f and $07]:=q;
    {$ENDIF}
   end;
end;

function TStratoSphere.Add(Parent: TStratoIndex; ListField:TStratoField;
  ThingType: TStratoThingType; const Values:array of cardinal): TStratoIndex;
var
  i,l:integer;
  p,q:TStratoIndex;
begin
  //assert Values contains [tfParent,Parent]
  l:=Length(Values);
  i:=0;
  while (i<l) and (Values[i]<>tfName) do inc(i,2);
  if i=l then
    raise Exception.Create('TStratoSphere.Add needs tfName value')
  else
    inc(i);
  p:=r(Parent,ListField);//tfFirstItem
  if p=0 then
   begin
    //first item
    Result:=Add(ThingType,Values);
    s(Parent,Listfield,Result);
   end
  else
   begin
    //assert all named things
    q:=r(p,tfNext);
    while (q<>0) and (v(p,tfName)<>Values[i]) do
     begin
      p:=q;
      q:=r(p,tfNext);
     end;
    if q<>0 then
     begin
      //duplicate ! assert caller handles zero value
      Result:=0;
     end
    else
     begin
      //add here
      Result:=Add(ThingType,Values);
      s(p,tfNext,Result);
      //TODO: sort? build better look-up lists?
     end;
   end;
end;

function TStratoSphere.Add(Parent: TStratoIndex; ListField:TStratoField;
  ThingType: TStratoThingType; const Values:array of cardinal; var Index: TStratoIndex): boolean;
begin
  Index:=Add(Parent,ListField,ThingType,Values);
  if Index=0 then
   begin
    //create anyway to prevent errors on further parsing
    Index:=Add(ThingType,Values);
    //Append?
    Result:=false;
   end
  else
    Result:=true;
end;

function TStratoSphere.Append(Parent:TStratoIndex;List:TStratoField;
  Item:TStratoIndex):TStratoIndex;
var
  p,q:TStratoIndex;
begin
  {$IFDEF DEBUG}
  if r(Item,tfNext)<>0 then
    raise Exception.Create('broken chain detected');
  {$ENDIF}
  p:=r(Parent,List);
  if p=0 then
    s(Parent,List,Item)
  else
   begin
    q:=r(p,tfNext);
    while q<>0 do
     begin
      p:=q;
      q:=r(p,tfNext);
     end;
    s(p,tfNext,Item);
   end;
  Result:=Item;
end;

function TStratoSphere.Prepend(Parent:TStratoIndex;List:TStratoField;
  Item:TStratoIndex):TStratoIndex;
var
  p:TStratoIndex;
begin
  {$IFDEF DEBUG}
  if r(Item,tfNext)<>0 then
    raise Exception.Create('broken chain detected');
  {$ENDIF}
  p:=r(Parent,List);
  if p=0 then
    s(Parent,List,Item)
  else
   begin
    s(Item,tfNext,p);
    s(Parent,List,Item);
   end;
  Result:=Item;
end;

function TStratoSphere.GetNode(ID: TStratoIndex): PStratoThing;
var
  i:cardinal;
begin
  if ID=0 then
    Result:=nil
  else
  if ID=FGetNodeCacheID then
    Result:=FGetNodeCacheST //TODO: thread-safe?
  else
   begin
    if (ID>=FBlock[FBlockIndex].First)
      and (ID<FBlock[FBlockIndex].First+FBlock[FBlockIndex].Index) then
      Result:=@(PStratoSphereDataBlock(FBlock[FBlockIndex].Data)
        [ID-FBlock[FBlockIndex].First])
    else
     begin
      i:=0;
      while (i<FBlockCount) and not((ID>=FBlock[i].First)
        and (ID<FBlock[i].First+FBlock[i].Size)) do inc(i);
      if i=FBlockCount then
        Result:=nil//raise? ptr to all-zero?
      else
        Result:=@(PStratoSphereDataBlock(FBlock[i].Data)[ID-FBlock[i].First]);
     end;
    FGetNodeCacheID:=ID;
    FGetNodeCacheST:=Result;
   end;
  if Result=nil then
    if ID=pHeader then
      Result:=@(PStratoSphereDataBlock(FBlock[0].Data)[0])
    else
     begin
      //reset to zeroes just to be sure (use ZeroMemory?)
      for i:=0 to 7 do FZeroes[i]:=0;
      Result:=@FZeroes;
     end;
end;

function TStratoSphere.t(p: TStratoIndex): TStratoThingType;
begin
  if p=0 then Result:=0 else Result:=GetNode(p)[0];//tfThingtype
end;

function TStratoSphere.r(p: TStratoIndex; f: TStratoField): TStratoIndex;
{$IFDEF DEBUG}
var
  pp:PStratoThing;
begin
  if (f and tf__IsValue)<>0 then
    raise Exception.Create('Use Sphere.v to request value');
  if p=0 then
    Result:=0//raise?
  else
   begin
    pp:=GetNode(p);
    Result:=pp[rx(pp[0],f,t,pp[f and $07])];
   end;
{$ELSE}
begin
  if p=0 then Result:=0 else Result:=GetNode(p)[f and $07];
{$ENDIF}
end;

function TStratoSphere.r(p: TStratoIndex; f: TStratoField;
  var q: TStratoIndex): boolean;
begin
  q:=r(p,f);
  Result:=q<>0;
end;

function TStratoSphere.rr(p:TStratoIndex;
  const ff:array of TStratoField):TStratoIndex;
var
  i,l:integer;
begin
  Result:=p;
  i:=0;
  l:=Length(ff);
  while (Result<>0) and (i<l) do
   begin
    Result:=r(Result,ff[i]);
    inc(i);
   end;
end;

function TStratoSphere.v(p: TStratoIndex; f: TStratoField): cardinal;
{$IFDEF DEBUG}
var
  pp:PStratoThing;
begin
  if (f and tf__IsValue)=0 then
    raise Exception.Create('Use Sphere.r to request reference');
  if p=0 then
    Result:=0//raise?
  else
   begin
    pp:=GetNode(p);
    Result:=pp[rx(pp[0],f,nil,0)];
   end;
{$ELSE}
begin
  if p=0 then Result:=0 else Result:=GetNode(p)[f and $07];
{$ENDIF}
end;

procedure TStratoSphere.s(p: TStratoIndex; f: TStratoField; q: TStratoIndex);
{$IFDEF DEBUG}
var
  pp:PStratoThing;
begin
  pp:=GetNode(p);
  pp[rx(pp[0],f,t,q)]:=q;
{$ELSE}
begin
  GetNode(p)[f and $07]:=q;
{$ENDIF}
end;

procedure TStratoSphere.s(p: TStratoIndex; const Values: array of cardinal);
var
  pp:PStratoThing;
  i,l:cardinal;
  f:TStratoField;
  q:TStratoIndex;
begin
  l:=Length(Values);
  if (l mod 2)<>0 then
    raise Exception.Create('TStratoSphere.Add Values required 2-by-2');
  pp:=GetNode(p);
  i:=0;
  while (i<l) do
   begin
    f:=Values[i];
    q:=Values[i+1];
    inc(i,2);
    {$IFDEF DEBUG}
    pp[rx(pp[0],f,t,q)]:=q;
    {$ELSE}
    pp[f and $07]:=q;
    {$ENDIF}
   end;
end;

function TStratoSphere.Lookup(Parent:TStratoIndex;ListField:TStratoField;
  Name:TStratoName):TStratoIndex;
begin
  Result:=r(Parent,ListField);
  while (Result<>0) and (v(Result,tfName)<>Name) do
    Result:=r(Result,tfNext);
end;

function TStratoSphere.FQN(p: TStratoIndex): UTF8String;
begin
  Result:='';
  while p<>0 do
   begin
    //TODO: tt__Named
    if t(p) in [ttNameSpace,ttTypeDecl,ttRecord,ttEnumeration,
      ttVar,ttConstant,ttImport,ttSignature,ttMember,ttArgument,
      ttPointer,ttVarByRef,ttArgByRef,ttClass,ttInterface,
      ttClassRef] then
      if GetNode(p)[tfName and $07]=0 then//if v(p,tfName)=0 then
        Result:=UTF8String(IntToStr(p))+'.'+Result
      else
        Result:=FDict.Str[v(p,tfName)]+'.'+Result;
    p:=r(p,tfParent);
   end;
  if Result<>'' then SetLength(Result,Length(Result)-1);//trailing "."
end;

const
  //ttBinaryData
  tf_BinaryData_Length =1;
  tf_BinaryData_Start  =2;

function TStratoSphere.AddBinaryData(const x: UTF8String): TStratoIndex;
var
  l,i,bx,bi:cardinal;
  p:PStratoThing;
  q:TStratoIndex;
begin
  l:=Length(x);
  bx:=FBlockIndex;
  bi:=FBlock[bx].Index;
  Result:=Add(ttBinaryData,[]);
  p:=GetNode(Result);
  p[tf_BinaryData_Length]:=l;
  //need more than one node?
  //TODO: calculate, not iterate (also split over blocks?
  i:=l+8;//check including header of first node
  while i>SizeOf(TStratoThing) do
   begin
    q:=Add(ttBinaryData,[]);//to inc FDataIndex
    if FBlockIndex<>bx then
     begin
      //TODO: replace with something cleaner
      //new block started?
      Result:=q;
      p:=GetNode(q);
      FBlock[bx].Index:=bi;
      bx:=FBlockIndex;
      bi:=FBlock[bx].Index;
     end
    else
      dec(i,SizeOf(TStratoThing));
   end;
  //p.???:=FDataIndex-Result;?
  //store data
  Move(x[1],p[tf_BinaryData_Start],l);
end;

function TStratoSphere.GetBinaryData(p: TStratoIndex): UTF8String;
var
  l:integer;
  pp:PStratoThing;
begin
  if p=0 then Result:='' else
   begin
    //assert t(p)=ttBinary
    pp:=GetNode(p);
    l:=pp[tf_BinaryData_Length];
    SetLength(Result,l);
    Move(pp[tf_BinaryData_Start],Result[1],l);
   end;
end;

procedure TStratoSphere.AddGlobalVar(p: TStratoIndex);
var
  x:cardinal;
  q:TStratoIndex;
begin
  //assert p<>0
  //assert t(p)=ttVar
  x:=v(pHeader,tf_GlobalByteSize);
  s(p,tfOffset,x);
  q:=r(p,tfEvaluatesTo);
  if q<>0 then s(pHeader,tf_GlobalByteSize,x+ByteSize(Self,q));
  Append(pHeader,tf_FirstGlobalVar,Add(ttGlobal,[tfTarget,p]));
end;

procedure TStratoSphere.LoadFromFile(const FilePath: string);
var
  f:TFileStream;//TMemoryStream first?
  hh:PStratoThing;
  h:TStratoBlockHeader;
  i,j,l:cardinal;
  x:UTF8String;
begin
  f:=TFileStream.Create(FilePath,fmOpenRead or fmShareDenyWrite);
  try
    hh:=@(PStratoSphereDataBlock(FBlock[0].Data)[0]);
    f.Read(hh^,SizeOf(TStratoThing));
    if hh[0]<>ttFileMarker then //tf_FileMarker
      raise Exception.Create('File is not a sphere data file');
    //TODO: check?
    if hh[2]<>StratoSphereFileVersion then //tf_Version
      raise Exception.Create('File is of an unsupported version');

    FBlock[0].First:=0;//?
    FBlockIndex:=0;
    f.Position:=0;
    h.ThingCount:=hh[1];//tf_ThingCount
    while h.ThingCount<>0 do
     begin
      if FBlockIndex=FBlockCount then
       begin
        if FBlockCount=FBlockSize then
         begin
          inc(FBlockSize,StratoSphereBlockGrowSize);
          SetLength(FBlock,FBlockSize);
         end;
        FBlock[FBlockIndex].Data:=BlockAlloc;
        inc(FBlockCount);
       end;
      //TODO: detect overlaps
      if FBlockIndex<>0 then FBlock[FBlockIndex].First:=h.FirstIndex;
      FBlock[FBlockIndex].Module:=h.Module;
      FBlock[FBlockIndex].Index:=h.ThingCount;
      FBlock[FBlockIndex].Size:=h.ThingCount;//close for new additions
      if h.ThingCount>StratoSphereDataBlockSize then
        raise Exception.Create('Unexpectedly large data block');
      f.Read(PStratoSphereDataBlock(FBlock[FBlockIndex].Data)[0],
        h.ThingCount*SizeOf(TStratoThing));
      inc(FBlockIndex);
      //next?
      f.Read(h,SizeOf(TStratoThing));//assert =SizeOf(TStratoBlockHeader)
     end;
    while FBlockIndex<FBlockCount do
     begin
      //BlockFree??
      FBlock[FBlockIndex].Index:=0;
      inc(FBlockIndex);
     end;

    //FDict.Clear;
    l:=1;
    i:=0;
    while l<>0 do
      if f.Read(l,4)<>4 then l:=0 else
       begin
        SetLength(x,l);
        f.Read(x[1],l);
        inc(i);
        j:=FDict.StrIdx(x);
        if i<>j then
          raise Exception.CreateFmt(
            'error loading dictionary: %d-%d "%s"',[i,j,x]);
       end;
  finally
    f.Free;
    FBlockIndex:=0;//?
    FGetNodeCacheID:=0;
    FGetNodeCacheST:=nil;
  end;
end;

procedure TStratoSphere.SaveToFile(const FilePath: string);
var
  f:TFileStream;//TMemoryStream first?
  i,l,n:cardinal;
  x:UTF8String;
  h:TStratoBlockHeader;
begin
  //ZeroMemory(
  h.FirstIndex:=0;
  h.ThingCount:=0;
  h.xReserved1:=0;
  h.xReserved2:=0;
  h.Module:=0;
  h.xReserved4:=0;
  h.xReserved5:=0;
  h.xReserved6:=0;

  f:=TFileStream.Create(FilePath,fmCreate);
  try
    s(pHeader,tf_ThingCount,FBlock[0].Index);

    i:=0;
    while i<FBlockCount do
     begin
      f.Write(PStratoSphereDataBlock(FBlock[i].Data)[0],
        FBlock[i].Index*SizeOf(TStratoThing));
      inc(i);
      if i=FBlockSize then
       begin
        h.FirstIndex:=0;
        h.Module:=0;
        h.ThingCount:=0;
       end
      else
       begin
        h.FirstIndex:=FBlock[i].First;
        h.Module:=FBlock[i].Module;
        h.ThingCount:=FBlock[i].Index;
       end;
      f.Write(h,SizeOf(TStratoThing));//assert =SizeOf(TStratoBlockHeader)
     end;

    i:=0;
    n:=FDict.StrCount;
    while i<n do
     begin
      inc(i);
      x:=FDict.Str[i];
      l:=Length(x);
      f.Write(l,4);
      f.Write(x[1],l);
     end;

  finally
    f.Free;
  end;
end;

function TStratoSphere.NextIndex(var Index:TStratoIndex):boolean;
var
  i,j,f:cardinal;
begin
  i:=0;//TODO: keep over calls?
  while (i<FBlockCount) and not((Index>=FBlock[i].First)
    and (Index<FBlock[i].First+FBlock[i].Size)) do inc(i);
  inc(Index);
  if i=FBlockCount then Result:=false else
   begin
    f:=FBlock[i].First;
    if Index>=f+FBlock[i].Index then
     begin
      i:=FBlockCount;
      j:=0;
      while (j<FBlockCount) do
       begin
        if (FBlock[j].First>f) and
          ((i=FBlockCount) or (FBlock[j].First<FBlock[i].First)) then i:=j;
        inc(j);
       end;
      if i=FBlockCount then Result:=false else
       begin
        Index:=FBlock[i].First;
        Result:=true;
       end;
     end
    else
      Result:=true;
   end;
end;

function TStratoSphere.DebugInfo(p:TStratoIndex):string;
var
  pp:PStratoThing;
begin
  if p=0 then Result:='' else
   begin
    pp:=GetNode(p);
    Result:=Format('(%.4x) %d,%d,%d,%d,%d',
      [pp[0],pp[3],pp[4],pp[5],pp[6],pp[7]]);
   end;
end;

procedure TStratoSphere.InlineError(Sender: TObject; Line, LPos: cardinal;
  const ErrorMsg: string);
begin
  AddBinaryData(Format('### %s(%d:%d): %s',
    [(Sender as TStratoSource).FilePath,Line,LPos,ErrorMsg]));
end;

{ StratoGetSourceFile }

function StratoGetSourceFile(s:TStratoSphere;p:TStratoIndex;
  var q:TStratoIndex;var srcLine,srcColumn:cardinal;var p1,p2:TStratoIndex):boolean;
var
  pp:PStratoThing;
  x,l:cardinal;
begin
  //previously in stratoDebug.pas
  //assert p<>0
  //assert Sphere[p].SrcPos<>0
  //assert not Sphere[p].ThingType in [ttHeader,ttSourceFile,ttBinaryData]
  q:=p;
  while (q<>0) and not(s.t(q) in [ttNameSpace,ttPrivate,ttOverload,
    ttConstructor,ttPropertyGet,ttPropertySet]) do
    q:=s.r(q,tfParent);
  if q<>0 then
    case s.t(q) of
      ttNameSpace:
        q:=s.r(q,tf_NameSpace_SourceFile);
      ttPrivate,ttOverload,
      ttConstructor,ttPropertyGet,ttPropertySet:
        q:=s.r(q,tfSourceFile);
      else
        q:=0;//raise?
    end;
  pp:=s.GetNode(p);
  p1:=pp[1];//tfParent
  p2:=pp[2];//tfNext
  srcLine:=0;//default
  srcColumn:=0;//default
  if q<>0 then
   begin
    l:=s.v(q,tf_SourceFile_SrcPosLineIndex);
    x:=pp[tfSrcPos and $07];//p:=s.v(p,tfSrcPos);
    if l<>0 then
     begin
      srcLine:=x div l;
      srcColumn:=x mod l;
     end;
   end;
  Result:=q<>0;
end;

end.
