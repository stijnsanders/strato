unit stratoTools;

interface

uses stratoDecl, stratoSphere;

procedure AddKnownPath(const Key,Path:string);

function AddSourceFile(var Data:PxSourceFile):cardinal;

procedure ReadSettings(const IniPath: string);
procedure LoadFromFile(const FilePath:string);
procedure SaveToFile(const FilePath:string);

function StripKnownPath(const FilePath: string): string;
function ResolveKnownPath(const FilePath: string): string;
function FindKnownFile(var FilePath: string): boolean;

function AddNode(src:cardinal;NodeType:xTypeNr;Extra:cardinal):xItem;
function AddBinaryData(src:cardinal;const x:UTF8String):xItem;

implementation

uses Windows, SysUtils, Classes;

const
  SourceFilesGrowStep=32;

procedure AddKnownPath(const Key,Path:string);
var
  i:cardinal;
begin
  i:=0;
  while (i<KnownPathsCount) and (KnownPaths[i].Key<>Key) do inc(i);
  if i<KnownPathsCount then
    raise Exception.Create('Duplicate path configuration "'+Key+'"')
  else
   begin
    if KnownPathsCount=KnownPathsSize then
     begin
      inc(KnownPathsSize,4);//grow step
      SetLength(KnownPaths,KnownPathsSize);
     end;
    i:=KnownPathsCount;
    inc(KnownPathsCount);
    KnownPaths[i].Key:=Key;
    KnownPaths[i].Path:=IncludeTrailingPathDelimiter(Path);
   end;
end;

function StripKnownPath(const FilePath: string): string;
var
  i,l:cardinal;
begin
  i:=0;
  while (i<KnownPathsCount) and
    (KnownPaths[i].Path<>Copy(FilePath,1,Length(KnownPaths[i].Path))) do
    inc(i);
  if i<KnownPathsCount then
   begin
    l:=Length(KnownPaths[i].Path)-1;
    Result:='$'+KnownPaths[i].Key+
      Copy(FilePath,l+1,cardinal(Length(FilePath))-l);
   end
  else
    Result:=FilePath;
end;

function ResolveKnownPath(const FilePath: string): string;
var
  i,l:cardinal;
begin
  if (FilePath<>'') and (FilePath[1]='$') then
   begin
    i:=0;
    while (i<KnownPathsCount) and
      (Copy(FilePath,2,Length(KnownPaths[i].Key))<>KnownPaths[i].Key) do
      inc(i);
    if i<KnownPathsCount then
     begin
      l:=2+Length(KnownPaths[i].Key);
      Result:=KnownPaths[i].Path+
        Copy(FilePath,l+1,cardinal(Length(FilePath))-l);
     end
    else
      Result:=FilePath;
   end
  else
    Result:=FilePath;
end;

function FindKnownFile(var FilePath: string): boolean;
var
  i:cardinal;
begin
  i:=0;
  while (i<KnownPathsCount) and
    not(FileExists(KnownPaths[i].Path+FilePath)) do inc(i);
  if i<KnownPathsCount then
   begin
    FilePath:='$'+KnownPaths[i].Key+PathDelim+FilePath;
    Result:=true;
   end
  else
    Result:=false;
end;

function AddSourceFile(var Data:PxSourceFile):cardinal;
begin
  if SourceFilesCount=SourceFilesSize then
   begin
    inc(SourceFilesSize,SourceFilesGrowStep);
    SetLength(SourceFiles,SourceFilesSize);
   end;
  Result:=SourceFilesCount;
  Data:=@SourceFiles[SourceFilesCount];
  Data.BlockIndex:=cardinal(-1);//default
  inc(SourceFilesCount);
end;

procedure ReadSettings(const IniPath: string);
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
    KnownPathsCount:=0;//reset
    AddKnownPath('compiler',ExtractFilePath(ParamStr(0)));
    for i:=0 to sl.Count-1 do
     begin
      s:=sl[i];
      if Copy(s,1,5)='path:' then
       begin
        l:=Length(s);
        j:=6;
        while (j<=l) and (s[j]<>'=') do inc(j);
        AddKnownPath(Copy(s,6,j-6),Copy(s,j+1,l-j+1));
       end
      else
      if Copy(s,1,4)='sws:' then //SystemWordSize
        SystemWordSize:=StrToInt(Copy(s,5,99))
      ;
     end;
  finally
    sl.Free;
  end;
end;

function BlockAlloc:pointer;
begin
  Result:=VirtualAlloc(nil,SizeOf(TStratoSphereBlock),
    MEM_RESERVE or MEM_COMMIT,PAGE_READWRITE);//MEM_LARGE_PAGES?
  if Result=nil then RaiseLastOSError;
end;

procedure BlockFree(x:pointer);
begin
  if not VirtualFree(x,0,MEM_RELEASE) then RaiseLastOSError;
end;

const
  xItemSize=SizeOf(xValue);//assert 4?

procedure LoadFromFile(const FilePath: string);
var
  f:TFileStream;//TMemoryStream first?
  h:TStratoStoreHeader;
  i,l:cardinal;
begin
  f:=TFileStream.Create(FilePath,fmOpenRead or fmShareDenyWrite);
  try
    l:=SizeOf(TStratoStoreHeader);
    if cardinal(f.Read(h,l))<>l then RaiseLastOSError;
    if h.FileMarker<>StratoSphereFileMarker then
      raise Exception.Create('File is not a sphere data file');
    if h.FileVersion<>StratoSphereFileVersion then
      raise Exception.Create('File is of an unsupported version');

    SourceFilesCount:=h.SourceFilesCount;
    if SourceFilesSize<SourceFilesCount then
     begin
      SourceFilesSize:=SourceFilesCount+SourceFilesGrowStep-1;
      dec(SourceFilesSize,SourceFilesSize mod SourceFilesGrowStep);
      SetLength(SourceFiles,SourceFilesSize);
     end;

    l:=SizeOf(xSourceFile);
    if SourceFilesCount<>0 then
    for i:=0 to SourceFilesCount-1 do
      if cardinal(f.Read(SourceFiles[i],l))<>l then RaiseLastOSError;

    BlocksCount:=h.BlocksCount;
    if BlocksSize<BlocksCount then
     begin
      BlocksSize:=BlocksCount+BlocksGrowStep-1;
      dec(BlocksSize,BlocksSize mod BlocksGrowStep);
      SetLength(Blocks,BlocksSize);
     end;

    if BlocksCount<>0 then
    for i:=0 to BlocksCount-1 do
     begin
      //TODO:
      if Blocks[i]=nil then Blocks[i]:=BlockAlloc;
      l:=2*xItemSize;
      if cardinal(f.Read(Blocks[i][0],l))<>l then RaiseLastOSError;
      //assert Blocks[i][0]<SourceFilesCount
      l:=(Blocks[i][1]-2)*xItemSize;
      if l<>0 then
        if cardinal(f.Read(Blocks[i][2],l))<>l then RaiseLastOSError;
     end;

    //assert f.EOF

  finally
    f.Free;
  end;
end;

procedure SaveToFile(const FilePath: string);
var
  f:TFileStream;//TMemoryStream first?
  i,l:cardinal;
  h:TStratoStoreHeader;
begin
  f:=TFileStream.Create(FilePath,fmCreate);
  try
    h.FileMarker:=StratoSphereFileMarker;
    h.FileVersion:=StratoSphereFileVersion;
    h.SourceFilesCount:=SourceFilesCount;
    h.BlocksCount:=BlocksCount;
    l:=SizeOf(TStratoStoreHeader);
    if cardinal(f.Write(h,l))<>l then RaiseLastOSError;

    l:=SizeOf(xSourceFile);
    if SourceFilesCount<>0 then
    for i:=0 to SourceFilesCount-1 do
      if cardinal(f.Write(SourceFiles[i],l))<>l then RaiseLastOSError;

    if BlocksCount<>0 then
    for i:=0 to BlocksCount-1 do
     begin
      l:=Blocks[i][1]*SizeOf(xValue);
      if cardinal(f.Write(Blocks[i][0],l))<>l then RaiseLastOSError;
     end;

  finally
    f.Free;
  end;
end;

function AddNode(src:cardinal;NodeType:xTypeNr;Extra:cardinal):xItem;
var
  cc,i,j:cardinal;
begin
  if src<SourceFilesCount then
   begin
    cc:=(NodeType div n_TypeNr_Base)+1+((Extra+xItemSize-1) div xItemSize);
    if cc>=StratoSphereBlockBase then
      raise Exception.CreateFmt('AddNode %s: size exceeds block size %d',
        [NodeTypeToStr(NodeType),cc]);
    //TODO: check if caller linked to sourcefile
    //if SourceFiles[src].BlockIndex=cardinal(-1) then?
    i:=SourceFiles[src].BlockIndex;
    if (i<BlocksCount) and (Blocks[i][0]<>src) then
      i:=BlocksCount;//raise?
    if (i<BlocksCount) and (Blocks[i][1]+cc>=StratoSphereBlockBase) then
      i:=BlocksCount;//force new
    if i>=BlocksCount then
     begin
      //new block
      if BlocksCount=BlocksSize then
       begin
        inc(BlocksSize,BlocksGrowStep);
        SetLength(Blocks,BlocksSize);
       end;
      i:=BlocksCount;
      inc(BlocksCount);
      Blocks[i]:=BlockAlloc;
      Blocks[i][0]:=src;
      Blocks[i][1]:=2;
      SourceFiles[src].BlockIndex:=i;
     end;
    j:=Blocks[i][1];
    inc(Blocks[i][1],cc);
    Result:=(i * StratoSphereBlockBase)+j;
    Blocks[i][j]:=NodeType;
   end
  else
    raise ERangeError.CreateFmt('SourceFile index out of range: %d;%d',
      [src,SourceFilesCount]);
end;

function AddBinaryData(src:cardinal;const x:UTF8String):xItem;
var
  i,j,l:cardinal;
begin
  l:=Length(x);
  Result:=AddNode(src,n_BinaryData,l);
  i:=Result div StratoSphereBlockBase;
  j:=Result mod StratoSphereBlockBase;
  Blocks[i][j+1]:=l;
  Move(x[1],Blocks[i][j+2],l);
end;

end.
