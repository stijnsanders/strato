unit stratoSphere;

interface

{xxx$D-}
{xxx$L-}

uses SysUtils, Classes, stratoDict, stratoSource, stratoDecl;

type
  TStratoStore=class(TObject)
  private
    FDict:TStringDictionary;
    FBlock:array of record
      SourceFile:TStratoIndex;
      NextBlock,Index:cardinal;
      Data:pointer;//PStratoSphereDataBlock see below
    end;
    FBlockCount,FBlockSize:cardinal;
    FPath:array of record
      Key,Path:string;
    end;
    procedure AddPath(const Key, Path: string);
  protected
    function NewBlock(ChainIndex:cardinal;SourceFile:TStratoIndex):cardinal;
    function Add(var BlockIndex:cardinal;var pp:PStratoThing):TStratoIndex;
    function GetNode(ID: TStratoIndex; BlockIndex: cardinal): PStratoThing;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ReadSettings(const IniPath:string);
    procedure LoadFromFile(const FilePath:string);
    procedure SaveToFile(const FilePath:string);
    function StripPath(const FilePath:string):string;
    function FindFile(var FilePath:string):boolean;
    function ResolvePath(const FilePath:string):string;
    function NextModule(var Index:TStratoIndex):boolean;
    function NextIndex(var Index:TStratoIndex):boolean;
    function SourceFile(Module:TStratoIndex):TStratoIndex;
    property Dict:TStringDictionary read FDict;
  end;

  TStratoSphere=class(TObject)
  private
    FStore:TStratoStore;
    FBlockIndex:cardinal;
    FSourceFile:TStratoIndex;
    FGetNodeCacheID:TStratoIndex;
    FGetNodeCacheST:PStratoThing;
    FCheckDepCache:cardinal;
    FZeroes:TStratoThing;
    function GetNode(ID: TStratoIndex): PStratoThing;
    procedure CheckDep(p,q:TStratoIndex);
  public
    constructor Create(Store: TStratoStore; Source: TStratoSource);
    destructor Destroy; override;

    function SourceFile:TStratoIndex;
    function Module:TStratoIndex;

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
    function DebugInfo(p:TStratoIndex):string;
    procedure Error(p:TStratoIndex;const Msg:string);
    procedure InlineError(Sender:TObject;Line,LPos:cardinal;
      const ErrorMsg:string);

    property Store:TStratoStore read FStore;
  end;

function StratoGetSourceFile(s:TStratoSphere;p:TStratoIndex;
  var q:TStratoIndex;var srcLine,srcColumn:cardinal;var p1,p2:TStratoIndex):boolean;

implementation

uses Windows, stratoRunTime, stratoTokenizer, stratoLogic, stratoFn, Math;

const
  StratoSphereDataBlockSize=$800;//2048
  StratoSphereBlockGrowSize=8;
  StratoSphereFileVersion=$00000107;//0.1.7

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

{ TStratoStore }

constructor TStratoStore.Create;
var
  h:PStratoThing;
begin
  inherited Create;
  FDict:=TStringDictionary.Create;
  FBlockCount:=1;
  FBlockSize:=StratoSphereBlockGrowSize;
  SetLength(FBlock,FBlockSize);
  FBlock[0].SourceFile:=0;
  FBlock[0].NextBlock:=0;
  FBlock[0].Index:=1;
  FBlock[0].Data:=BlockAlloc;

  //header
  h:=@(PStratoSphereDataBlock(FBlock[0].Data)[0]);
  h[0]:=ttFileMarker;//tf_FileMarker
  h[1]:=StratoSphereFileVersion;
  h[2]:=0;//ThingCount see SaveToFile
  h[3]:=0;
  h[4]:=0;
  h[5]:=0;
  h[6]:=0;
  h[7]:=0;
end;

destructor TStratoStore.Destroy;
begin
  FDict.Free;
  //for i:=FBlockCount-1 downto 0 do
  //  Dispose(PStratoSphereDataBlock(FBlock[0].Data));
  //hack: avoid expensive dealloc:
  pointer(FBlock):=nil;//SetLength(FData,0);
  inherited;
end;

function TStratoStore.NewBlock(ChainIndex:cardinal;
  SourceFile:TStratoIndex):cardinal;
begin
  Result:=FBlockCount;
  if FBlockCount=FBlockSize then
   begin
    inc(FBlockSize,StratoSphereBlockGrowSize);
    SetLength(FBlock,FBlockSize);
   end;
  inc(FBlockCount);
  FBlock[Result].SourceFile:=SourceFile;
  FBlock[Result].NextBlock:=0;
  FBlock[Result].Index:=0;
  FBlock[Result].Data:=BlockAlloc;
  if FBlock[ChainIndex].SourceFile=SourceFile then
    FBlock[ChainIndex].NextBlock:=Result;
end;

procedure TStratoStore.AddPath(const Key,Path:string);
var
  pp,qq:cardinal;
begin
  pp:=Length(FPath);
  qq:=0;
  while (qq<pp) and (FPath[qq].Key<>Key) do inc(qq);
  if qq<pp then
    raise Exception.Create('Duplicate path configuration "'+Key+'"');
  SetLength(FPath,pp+1);//TODO: stepped grow?
  FPath[pp].Key:=Key;
  if Path='' then FPath[pp].Path:='.\' else
    FPath[pp].Path:=IncludeTrailingPathDelimiter(Path);
end;

procedure TStratoStore.ReadSettings(const IniPath: string);
var
  sl:TStringList;
  i,j,l:integer;
  s:string;
begin
  sl:=TStringList.Create;
  try
    //TODO: something else? per project overrides? command line
    try
      sl.LoadFromFile(IniPath);
    except
      on EFOpenError do ;//silent
    end;
    AddPath('compiler',ExtractFilePath(ParamStr(0)));
    for i:=0 to sl.Count-1 do
     begin
      s:=sl[i];
      if Copy(s,1,5)='path:' then
       begin
        l:=Length(s);
        j:=6;
        while (j<=l) and (s[j]<>'=') do inc(j);
        AddPath(Copy(s,6,j-6),Copy(s,j+1,l-j+1));
       end;
     end;
  finally
    sl.Free;
  end;
end;

function TStratoStore.Add(var BlockIndex: cardinal;
  var pp: PStratoThing): TStratoIndex;
var
  i:cardinal;
begin
  if FBlock[BlockIndex].Index=StratoSphereDataBlockSize then
    BlockIndex:=NewBlock(BlockIndex,FBlock[BlockIndex].SourceFile);
  i:=FBlock[BlockIndex].Index;
  inc(FBlock[BlockIndex].Index);
  pp:=@(PStratoSphereDataBlock(FBlock[BlockIndex].Data)[i]);
  Result:=BlockIndex*StratoSphereDataBlockSize+i;
end;

function TStratoStore.GetNode(ID: TStratoIndex;
  BlockIndex: cardinal): PStratoThing;
var
  i,j:cardinal;
begin
  i:=ID div StratoSphereDataBlockSize;
  j:=ID mod StratoSphereDataBlockSize;
  if i<FBlockCount then
    Result:=@(PStratoSphereDataBlock(FBlock[i].Data)[j])
  else
  if (ID>cardinal(-5)) then //pHeader,pIndexes
    Result:=@(PStratoSphereDataBlock(FBlock[0].Data)[3+integer(ID)])
  else
    Result:=nil;//raise? ptr to all-zero?
end;

procedure TStratoStore.LoadFromFile(const FilePath: string);
var
  f:TFileStream;//TMemoryStream first?
  h:TStratoBlockHeader;
  i,j,l:cardinal;
  x:UTF8String;
begin
  f:=TFileStream.Create(FilePath,fmOpenRead or fmShareDenyWrite);
  try
    f.Read(h,SizeOf(TStratoThing));
    if h.Marker<>ttFileMarker then
      raise Exception.Create('File is not a sphere data file');
    if cardinal(h.SourceFile)<>StratoSphereFileVersion then
      raise Exception.Create('File is of an unsupported version');
    if h.ThingCount<>0 then f.Position:=0;

    i:=0;
    while h.ThingCount<>0 do
     begin
      if i=FBlockCount then
       begin
        if FBlockCount=FBlockSize then
         begin
          inc(FBlockSize,StratoSphereBlockGrowSize);
          SetLength(FBlock,FBlockSize);
         end;
        FBlock[i].Data:=BlockAlloc;
        inc(FBlockCount);
       end;
      //if h.Marker<>ttFileMarker then raise?
      if i<>0 then FBlock[i].SourceFile:=h.SourceFile;
      if i<>0 then FBlock[i].NextBlock:=h.NextBlock;
      FBlock[i].Index:=h.ThingCount;
      if h.ThingCount>StratoSphereDataBlockSize then
        raise Exception.Create('Unexpectedly large data block');
      f.Read(PStratoSphereDataBlock(FBlock[i].Data)[0],
        h.ThingCount*SizeOf(TStratoThing));
      inc(i);
      //next?
      f.Read(h,SizeOf(TStratoThing));//assert =SizeOf(TStratoBlockHeader)
     end;
    while i<FBlockCount do
     begin
      //BlockFree??
      FBlock[i].SourceFile:=0;
      FBlock[i].NextBlock:=0;
      FBlock[i].Index:=0;
      inc(i);
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
  end;
end;

procedure TStratoStore.SaveToFile(const FilePath: string);
var
  f:TFileStream;//TMemoryStream first?
  i,l,n:cardinal;
  x:UTF8String;
  h:TStratoBlockHeader;
begin
  //ZeroMemory(
  h.Marker:=ttFileMarker;
  h.SourceFile:=0;
  h.ThingCount:=0;
  h.NextBlock:=0;
  h.xReserved1:=0;
  h.xReserved2:=0;
  h.xReserved3:=0;
  h.xReserved4:=0;

  f:=TFileStream.Create(FilePath,fmCreate);
  try
    //ThingCount
    PStratoSphereDataBlock(FBlock[0].Data)[0][2]:=FBlock[0].Index;

    i:=0;
    while i<FBlockCount do
     begin
      f.Write(PStratoSphereDataBlock(FBlock[i].Data)[0],
        FBlock[i].Index*SizeOf(TStratoThing));
      inc(i);
      if i=FBlockCount then
       begin
        h.SourceFile:=0;
        h.ThingCount:=0;
        h.NextBlock:=0;
       end
      else
       begin
        h.SourceFile:=FBlock[i].SourceFile;
        h.ThingCount:=FBlock[i].Index;
        h.NextBlock:=FBlock[i].NextBlock;
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

function TStratoStore.NextModule(var Index:TStratoIndex):boolean;
var
  i:cardinal;
begin
  i:=Index div StratoSphereDataBlockSize;
  inc(i);
  while (i<FBlockCount) and not((FBlock[i].Index>1) and
    (PStratoSphereDataBlock(FBlock[i].Data)[0][0]=ttModule)) do
    inc(i);
  Index:=i*StratoSphereDataBlockSize;
  Result:=i<FBlockCount;
end;

function TStratoStore.NextIndex(var Index:TStratoIndex):boolean;
var
  i,j:cardinal;
begin
  inc(Index);
  i:=Index div StratoSphereDataBlockSize;
  j:=Index mod StratoSphereDataBlockSize;
  while (i<FBlockCount) and (j>=FBlock[i].Index) do
   begin
    inc(i);
    j:=0;
    Index:=i*StratoSphereDataBlockSize;
   end;
  Result:=i<FBlockCount;
end;

function TStratoStore.SourceFile(Module:TStratoIndex):TStratoIndex;
var
  i:cardinal;
begin
  i:=Module div StratoSphereDataBlockSize;
  if i<FBlockCount then Result:=FBlock[i].SourceFile else Result:=0;
end;

function TStratoStore.StripPath(const FilePath: string): string;
var
  i,l:integer;
begin
  i:=0;
  l:=Length(FPath);
  while (i<l) and (FPath[i].Path<>Copy(FilePath,1,Length(FPath[i].Path))) do
    inc(i);
  if i<l then
   begin
    l:=Length(FPath[i].Path)-1;
    Result:='$'+FPath[i].Key+Copy(FilePath,l+1,Length(FilePath)-l);
   end
  else
    Result:=FilePath;
end;

function TStratoStore.ResolvePath(const FilePath: string): string;
var
  i,l:integer;
begin
  if (FilePath<>'') and (FilePath[1]='$') then
   begin
    i:=0;
    l:=Length(FPath);
    while (i<l) and (Copy(FilePath,2,Length(FPath[i].Key))<>FPath[i].Key) do
      inc(i);
    if i<l then
     begin
      l:=2+Length(FPath[i].Key);
      Result:=FPath[i].Path+Copy(FilePath,l+1,Length(FilePath)-l);
     end
    else
      Result:=FilePath;
   end
  else
    Result:=FilePath;
end;

function TStratoStore.FindFile(var FilePath: string): boolean;
var
  i,l:integer;
begin
  i:=0;
  l:=Length(FPath);
  while (i<l) and not(FileExists(FPath[i].Path+FilePath)) do inc(i);
  if i<l then
   begin
    FilePath:='$'+FPath[i].Key+PathDelim+FilePath;
    Result:=true;
   end
  else
    Result:=false;
end;

{ TStratoSphere }

constructor TStratoSphere.Create(Store: TStratoStore; Source: TStratoSource);
begin
  inherited Create;
  FStore:=Store;
  FGetNodeCacheID:=0;
  FGetNodeCacheST:=nil;
  FCheckDepCache:=0;
  FBlockIndex:=0;//default
  FSourceFile:=0;//default

  if Source<>nil then
   begin
    FSourceFile:=Add(ttSourceFile,[]);
    Self.s(FSourceFile,
      [tf_SourceFile_FileName,AddBinaryData(UTF8String(
        FStore.StripPath(Source.FilePath)))
      ,tf_SourceFile_FileSize,Source.FileSize
      //file date?checksum?
      ,tf_SourceFile_SrcPosLineIndex,Source.LineIndex
      ]);

    FBlockIndex:=FStore.NewBlock(FBlockIndex,FSourceFile);
    Self.s(FSourceFile,
      [tf_SourceFile_Module,Add(ttModule,[])
      ]);

    //set thread id?
   end;
  //TODO: else read-only?
end;

destructor TStratoSphere.Destroy;
begin
  //clear thread id?
  inherited Destroy;
end;

function TStratoSphere.SourceFile:TStratoIndex;
begin
  if FSourceFile=0 then
    raise Exception.Create('StratoSphere not started on SourceFile');
  Result:=FSourceFile;
end;

function TStratoSphere.Module:TStratoIndex;
begin
  if FSourceFile=0 then
    raise Exception.Create('StratoSphere not started on SourceFile');
  Result:=r(FSourceFile,tf_SourceFile_Module);
end;

procedure TStratoSphere.Error(p: TStratoIndex; const Msg: string);
var
  i,j:cardinal;
  q:TStratoIndex;
begin
  //TODO: raise?
  i:=p div StratoSphereDataBlockSize;
  if (p=0) or (i>=FStore.FBlockCount) then
    Writeln(ErrOutput,Msg)
  else
   begin
    q:=FStore.FBlock[i].SourceFile;
    i:=v(q,tf_SourceFile_SrcPosLineIndex);
    j:=v(p,tfSrcPos);
    Writeln(ErrOutput,Format('%s(%d:%d): %s',
      [GetBinaryData(r(q,tf_SourceFile_FileName))
      ,j div i
      ,j mod i
      ,Msg
      ]));
   end;
  ExitCode:=1;
end;

function TStratoSphere.Add(ThingType: TStratoThingType;
  const Values:array of cardinal): TStratoIndex;
var
  pp:PStratoThing;
  i,l:cardinal;
  f:TStratoField;
  q:TStratoIndex;
begin
  l:=Length(Values);
  if (l mod 2)<>0 then
    raise Exception.Create('TStratoSphere.Add Values required 2-by-2');
  Result:=FStore.Add(FBlockIndex,pp);
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
    Result:=FStore.GetNode(ID,FBlockIndex);
    FGetNodeCacheID:=ID;
    FGetNodeCacheST:=Result;
   end;
  if Result=nil then
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
  //TODO: if (f and tf__IsValue) [separate methods for references/values?]
  pp:=GetNode(p);
  pp[rx(pp[0],f,t,q)]:=q;
{$ELSE}
begin
  GetNode(p)[f and $07]:=q;
{$ENDIF}
  if (f and tf__IsValue)=0 then CheckDep(p,q);
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
    if (f and tf__IsValue)=0 then CheckDep(p,q);
   end;
end;

procedure TStratoSphere.CheckDep(p,q:TStratoIndex);
var
  i:cardinal;
  p1,p2,p3:TStratoIndex;
  pp:array of TStratoIndex;
  ppi,ppj,ppk,ppl:cardinal;
begin
  i:=p div StratoSphereDataBlockSize;
  if i<FStore.FBlockCount then
   begin
    if (i<>0) and (FStore.FBlock[i].SourceFile<>FSourceFile) then
//      raise Exception.CreateFmt('XBLOCK %d[%.3x] %d',[p,f,q]);
AddBinaryData(Format('### XBLOCK %d %d',[p,q]));
    i:=q div StratoSphereDataBlockSize;
    if (i<>0) and (i<FStore.FBlockCount) and
      (FStore.FBlock[i].SourceFile<>FSourceFile) and
      (FCheckDepCache<>i) then
     begin
      p1:=r(FSourceFile,tf_SourceFile_FirstDependency);
      p2:=FStore.FBlock[i].SourceFile;
      while (p1<>0) and (r(p1,tfTarget)<>p2) do
        p1:=r(p1,tfNext);
      if p1=0 then
       begin
        //detect circular dependency
        ppi:=0;
        ppj:=0;
        ppl:=0;
        p1:=p2;
        while p1<>0 do
         begin
          p1:=r(p1,tf_SourceFile_FirstDependency);
          while p1<>0 do
           begin
            p3:=r(p1,tfTarget);
            ppk:=0;
            while (ppk<ppj) and (pp[ppk]<>p3) do inc(ppk);
            if ppk<ppj then
              raise Exception.Create('Circular dependency detected');
            if (ppj=ppl) then
             begin
              inc(ppl,$20);//grow
              SetLength(pp,ppl);
             end;
            pp[ppj]:=p3;
            inc(ppj);
            p1:=r(p1,tfNext);
            while (p1=0) and (ppi<ppj) do
             begin
              p1:=r(pp[ppi],tf_SourceFile_FirstDependency);
              inc(ppi);
             end;
           end;
         end;
        //add dependency
        Append(FSourceFile,tf_SourceFile_FirstDependency,
          Add(ttDependency,
            [tfParent,FSourceFile
            ,tfTarget,p2
            ]));
       end;
      FCheckDepCache:=i;
     end;
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
        Result:=FStore.Dict.Str[v(p,tfName)]+'.'+Result;
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
  bi:=FStore.FBlock[bx].Index;
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
      FStore.FBlock[bx].Index:=bi;
      bx:=FBlockIndex;
      bi:=FStore.FBlock[bx].Index;
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
  i,j:cardinal;
begin
  //assert p<>0
  //assert Sphere[p].SrcPos<>0
  pp:=s.GetNode(p);
  p1:=pp[1];//tfParent
  p2:=pp[2];//tfNext
  srcLine:=0;//default
  srcColumn:=0;//default
  i:=p div StratoSphereDataBlockSize;
  if i>=s.FStore.FBlockCount then q:=0 else
    q:=s.FStore.FBlock[i].SourceFile;
  if q<>0 then
   begin
    i:=s.v(q,tf_SourceFile_SrcPosLineIndex);
    j:=pp[tfSrcPos and $07];//p:=s.v(p,tfSrcPos);
    if i<>0 then
     begin
      srcLine:=j div i;
      srcColumn:=j mod i;
     end;
   end;
  Result:=q<>0;
end;

end.
