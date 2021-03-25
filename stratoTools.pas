unit stratoTools;

interface

{$D-}
{$L-}

uses stratoDecl, stratoSphere;

function IntToStr8(x:int64):UTF8String;

procedure AddKnownPath(const Key,Path:string);

procedure ReadSettings(const IniPath: string);
procedure LoadFromFile(const FilePath:string);
procedure SaveToFile(const FilePath:string);

function StripKnownPath(const FilePath: string): string;
function ResolveKnownPath(const FilePath: string): string;
function FindKnownFile(var FilePath: string): boolean;

const
  IntrinsicTypeName:array[TStratoIntrinsicType] of string=(
    'Void',//itVoid
    'Type',//itType
    'Pointer',//itPointer
    'Boolean',//itBoolean
    'Number',//itNumber
    'Object',//itObject
    'String' //itString
  );

implementation

uses Windows, SysUtils, Classes;

function IntToStr8(x:int64):UTF8String;
var
  i,j:integer;
  c:AnsiChar;
begin
  if x=0 then
    Result:='0'
  else
   begin
    SetLength(Result,24);
    i:=0;
    j:=1;
    if x<0 then
     begin
      x:=-x;
      inc(i);
      inc(j);
      Result[i]:='-';
     end;
    while x<>0 do
     begin
      inc(i);
      byte(Result[i]):=$30+(x mod 10);
      x:=x div 10;
     end;
    SetLength(Result,i);
    while j<i do
     begin
      c:=Result[j];
      Result[j]:=Result[i];
      Result[i]:=c;
      dec(i);
      inc(j);
     end;
   end;
end;

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

    SpheresCount:=h.SpheresCount;
    if SpheresSize<SpheresCount then
     begin
      SpheresSize:=SpheresCount+SpheresGrowStep-1;
      dec(SpheresSize,SpheresSize mod SpheresGrowStep);
      SetLength(Spheres,SpheresSize);
     end;

    if SpheresCount<>0 then
      for i:=0 to SpheresCount-1 do
       begin
        if Spheres[i]=nil then
          Spheres[i]:=TStratoSphere.Create;
        Spheres[i].LoadFromStream(f);
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
    h.SpheresCount:=SpheresCount;
    h.Reserved_1:=0;
    l:=SizeOf(TStratoStoreHeader);
    if cardinal(f.Write(h,l))<>l then RaiseLastOSError;

    if SpheresCount<>0 then
      for i:=0 to SpheresCount-1 do
        Spheres[i].SaveToStream(f);
  finally
    f.Free;
  end;
end;

end.
