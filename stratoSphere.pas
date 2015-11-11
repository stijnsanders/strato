unit stratoSphere;

{

stratoSphere

declares the TStratoSphere object that manages parsed code into a set of
TStratoThing records

}

interface

{$D-}
{$L-}

uses SysUtils, Classes, stratoDict, stratoDecl;

type
  TStratoSphere=class(TObject)
  private
    FDict:TStringDictionary;
    FBlock:array of record
      First:TStratoIndex;//cardinal
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
    function Add(ThingType: cardinal; var Info: PStratoThing): TStratoIndex;
    function AddTo(var First: TStratoIndex; ThingType: cardinal;
      Name: TStratoName; var Info: PStratoThing): TStratoIndex; overload;
    function AddTo(var First:TStratoIndex;Item:TStratoIndex):boolean; overload;
    function AddTo(var First: TStratoIndex; ThingType: cardinal;
      Name: TStratoName; var Index: TStratoIndex;
      var Info: PStratoThing): boolean; overload;
    procedure Prepend(var First:TStratoIndex;Item:TStratoIndex);
    procedure Append(var First:TStratoIndex;Item:TStratoIndex);
    function Lookup(First:TStratoIndex;Name:TStratoName):TStratoIndex;
    function FQN(p:TStratoIndex):UTF8String;
    function AddBinaryData(const x:UTF8String):TStratoIndex;
    function GetBinaryData(p:TStratoIndex):UTF8String;
    function Header:PStratoHeader;
    function NextIndex(var Index:TStratoIndex):boolean;
    procedure AddGlobalVar(p:TStratoIndex);
    procedure ReadSettings(const IniPath:string);
    procedure LoadFromFile(const FilePath:string);
    procedure SaveToFile(const FilePath:string);
    procedure Error(p:TStratoIndex;const Msg:string);
    procedure InlineError(Sender:TObject;Line,LPos:cardinal;
      const ErrorMsg:string);
    property Dict:TStringDictionary read FDict;
    property Node[ID:TStratoIndex]:PStratoThing read GetNode; default;
    property BasePath:string read FBasePath;
  end;

implementation

uses Windows, stratoLogic, stratoSource;

const
  StratoSphereDataBlockSize=$800;//2048
  StratoSphereBlockGrowSize=8;
  StratoSphereFileVersion=$00000104;//0.1.4

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
  h:PStratoHeader;
begin
  inherited Create;
  FDict:=TStringDictionary.Create;
  FBlockIndex:=0;
  FBlockCount:=1;
  FBlockSize:=StratoSphereBlockGrowSize;
  SetLength(FBlock,FBlockSize);
  FBlock[0].First:=0;
  FBlock[0].Index:=1;
  FBlock[0].Size:=StratoSphereDataBlockSize;
  FBlock[0].Data:=BlockAlloc;
  FGetNodeCacheID:=0;
  FGetNodeCacheST:=nil;

  //header
  h:=@(PStratoSphereDataBlock(FBlock[0].Data)[0]);
  h.FileMarker:=ttFileMarker;
  h.ThingCount:=0;//see SaveToFile
  h.Version:=StratoSphereFileVersion;
  h.FirstNameSpace:=0;//default (see stratoRunTime)
  h.FirstGlobalVar:=0;//default
  h.GlobalByteSize:=0;//default
  h.FirstInitialization:=0;//default
  h.FirstFinalization:=0;//default
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
  px:PStratoThing;
begin
  q:=p;
  while (q<>0) and not(Node[q].ThingType
    in [ttNameSpace,ttPrivate,ttOverload,ttConstructor]) do
    q:=Node[q].Parent;
  if q<>0 then
    case Node[q].ThingType of
      ttNameSpace:
        q:=PStratoNameSpaceData(Node[q]).SourceFile;
      ttOverload,ttConstructor,ttPrivate:
        q:=Node[q].SourceFile;
      else
        q:=0;//raise?
    end;
  //TODO: raise?
  px:=Node[p];
  if (p=0) or (q=0) then
    Writeln(ErrOutput,Msg)
  else
    Writeln(ErrOutput,Format('%s(%d:%d): %s',
      [GetBinaryData(PStratoSourceFile(Node[q]).FileName)
      ,px.SrcPos div PStratoSourceFile(Node[q]).SrcPosLineIndex
      ,px.SrcPos mod PStratoSourceFile(Node[q]).SrcPosLineIndex
      ,Msg
      ]));
  ExitCode:=1;
end;

function TStratoSphere.Add(ThingType: cardinal;
  var Info: PStratoThing): TStratoIndex;
var
  p:TStratoIndex;
  i,j:cardinal;
begin
  if FBlock[FBlockIndex].Index=FBlock[FBlockIndex].Size then
   begin
    //block fill, determine first index for next block
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
    FBlock[FBlockIndex].Index:=0;
    FBlock[FBlockIndex].Size:=j;
    FBlock[FBlockIndex].Data:=BlockAlloc;
   end;
  i:=FBlock[FBlockIndex].Index;
  inc(FBlock[FBlockIndex].Index);
  Result:=FBlock[FBlockIndex].First+i;
  Info:=@(PStratoSphereDataBlock(FBlock[FBlockIndex].Data)[i]);
  //ZeroMemory(Info^?
  Info.ThingType:=ThingType;
end;

function TStratoSphere.AddTo(var First: TStratoIndex; ThingType: cardinal;
  Name: TStratoName; var Info: PStratoThing): TStratoIndex;
var
  p:PStratoThing;
begin
  if First=0 then
   begin
    //first item
    First:=Add(ThingType,Info);
    Info.Name:=Name;
    Result:=First;
   end
  else
   begin
    //assert all named things
    p:=Node[First];
    while (p.Next<>0) and (p.Name<>Name) do p:=Node[p.Next];
    if (p.Next=0) and (p.Name<>Name) then
     begin
      //add
      Result:=Add(ThingType,Info);
      Info.Name:=Name;
      //TODO: sort? build better look-up lists?
      p.Next:=Result;//assert was 0
     end
    else
     begin
      //duplicate ! assert caller handles zero values
      Result:=0;
      Info:=nil;
     end;
   end;
  //assert caller sets Info.Parent
end;

function TStratoSphere.AddTo(var First:TStratoIndex;Item:TStratoIndex):boolean;
var
  n:TStratoName;
  px,qx:PStratoThing;
begin
  //assert caller sets Sphere[Item].Parent
  {$IFDEF DEBUG}
  if Node[Item].Next<>0 then
    raise Exception.Create('broken chain detected');
  {$ENDIF}
  Result:=true;//default
  if First=0 then
    First:=Item
  else
   begin
    n:=Node[Item].Name;
    px:=Node[First];
    qx:=nil;
    while px.ThingType<>0 do
     begin
      qx:=px;
      if px.Name=n then
       begin
        Result:=false;
        FZeroes.ThingType:=0;
        px:=@FZeroes;
       end
      else
        px:=Node[px.Next];
     end;
    if Result then qx.Next:=Item;
   end;
end;

function TStratoSphere.AddTo(var First: TStratoIndex; ThingType: cardinal;
  Name: TStratoName; var Index: TStratoIndex;
  var Info: PStratoThing): boolean;
begin
  Index:=AddTo(First,ThingType,Name,Info);
  if Index=0 then
   begin
    //create anyway to prevent errors on further parsing
    Index:=Add(ThingType,Info);
    Info.Name:=Name;
    Result:=false;
   end
  else
    Result:=true;
end;

procedure TStratoSphere.Prepend(var First: TStratoIndex; Item: TStratoIndex);
begin
  Node[Item].Next:=First;
  First:=Item;
end;

procedure TStratoSphere.Append(var First: TStratoIndex; Item: TStratoIndex);
var
  px:PStratoThing;
begin
  if First=0 then
    First:=Item
  else
   begin
    px:=Node[First];
    while px.Next<>0 do px:=Node[px.Next];
    px.Next:=Item;
   end;
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
      Result:=@(PStratoSphereDataBlock(FBlock[FBlockIndex].Data)[ID-FBlock[FBlockIndex].First])
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
   begin
    //reset to zeroes just to be sure (use ZeroMemory?)
    FZeroes.ThingType:=0;
    FZeroes.Parent:=0;
    FZeroes.Next:=0;
    FZeroes.Name:=0;
    FZeroes.FirstItem:=0;
    FZeroes.ByteSize:=0;
    FZeroes.InheritsFrom:=0;
    FZeroes.SrcPos:=0;
    Result:=@FZeroes;
   end;
end;

function TStratoSphere.Lookup(First:TStratoIndex;Name:TStratoName):TStratoIndex;
begin
  Result:=First;
  while (Result<>0) and (Node[Result].Name<>Name) do
    Result:=Node[Result].Next;
end;

function TStratoSphere.FQN(p: TStratoIndex): UTF8String;
begin
  Result:='';
  while p<>0 do
   begin
    //TODO: tt__Named
    if Node[p].ThingType in [ttNameSpace,ttTypeDecl,ttRecord,ttEnumeration,
      ttVar,ttConstant,ttImport,ttSignature,ttMember,ttArgument,
      ttPointer,ttVarByRef,ttArgByRef,ttClass,ttInterface,
      ttClassRef] then
      if Node[p].Name=0 then
        Result:=UTF8String(IntToStr(p))+'.'+Result
      else
        Result:=FDict.Str[Node[p].Name]+'.'+Result;
    p:=Node[p].Parent;
   end;
  if Result<>'' then SetLength(Result,Length(Result)-1);//trailing "."
end;

function TStratoSphere.AddBinaryData(const x: UTF8String): TStratoIndex;
var
  l,i:cardinal;
  p,q:PStratoBinaryData;
begin
  l:=Length(x);
  Result:=Add(ttBinaryData,PStratoThing(p));
  p.DataLength:=l;//Length
  //need more than one node?
  i:=l+8;//check including header of first node
  //TODO: calculate, not iterate
  while i>SizeOf(TStratoThing) do
   begin
    Add(ttBinaryData,PStratoThing(q));//to inc FDataIndex
    dec(i,SizeOf(TStratoThing));
   end;
  //p.???:=FDataIndex-Result;?
  //store data
  Move(x[1],p.DataStart,l);
end;

function TStratoSphere.GetBinaryData(p: TStratoIndex): UTF8String;
var
  l:integer;
begin
  if p=0 then Result:='' else
   begin
    //assert x<>0 and x<FDataIndex
    //assert Node[x].ThingType:=ttBinary
    l:=PStratoBinaryData(Node[p]).DataLength;
    SetLength(Result,l);
    Move(PStratoBinaryData(Node[p]).DataStart,Result[1],l);
   end;
end;

function TStratoSphere.Header:PStratoHeader;
begin
  //TODO: header separate in data and file?
  Result:=@(PStratoSphereDataBlock(FBlock[0].Data)[0]);
end;

procedure TStratoSphere.AddGlobalVar(p: TStratoIndex);
var
  q:TStratoIndex;
  qx:PStratoThing;
begin
  //assert p<>0
  //assert Node[p].ThingType=ttVar
  Node[p].Offset:=Header.GlobalByteSize;
  if Node[p].EvaluatesTo<>0 then
    inc(Header.GlobalByteSize,ByteSize(Self,Node[p].EvaluatesTo));
  q:=Add(ttGlobal,qx);
  qx.Target:=p;
  Append(Header.FirstGlobalVar,q);
end;

procedure TStratoSphere.LoadFromFile(const FilePath: string);
var
  f:TFileStream;//TMemoryStream first?
  hh:PStratoHeader;
  h:TStratoBlockHeader;
  i,j,l:cardinal;
  x:UTF8String;
begin
  f:=TFileStream.Create(FilePath,fmOpenRead or fmShareDenyWrite);
  try
    hh:=@(PStratoSphereDataBlock(FBlock[0].Data)[0]);
    f.Read(hh^,SizeOf(TStratoThing));
    if hh.FileMarker<>ttFileMarker then
      raise Exception.Create('File is not a sphere data file');
    //TODO: check?
    if hh.Version<>StratoSphereFileVersion then
      raise Exception.Create('File is of an unsupported version');

    FBlock[0].First:=0;//?
    FBlockIndex:=0;
    f.Position:=0;
    h.ThingCount:=hh.ThingCount;
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
  h.xReserved3:=0;
  h.xReserved4:=0;
  h.xReserved5:=0;
  h.xReserved6:=0;

  f:=TFileStream.Create(FilePath,fmCreate);
  try
    Header.ThingCount:=FBlock[0].Index;

    i:=0;
    while i<FBlockCount do
     begin
      f.Write(PStratoSphereDataBlock(FBlock[i].Data)[0],
        FBlock[i].Index*SizeOf(TStratoThing));
      inc(i);
      if i=FBlockSize then
       begin
        h.FirstIndex:=0;
        h.ThingCount:=0;
       end
      else
       begin
        h.FirstIndex:=FBlock[i].First;
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

procedure TStratoSphere.InlineError(Sender: TObject; Line, LPos: cardinal;
  const ErrorMsg: string);
begin
  AddBinaryData(Format('### %s(%d:%d): %s',
    [(Sender as TStratoSource).FilePath,Line,LPos,ErrorMsg]));
end;

end.
