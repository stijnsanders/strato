unit stratoSphere;

interface

{$D-}

uses SysUtils, Classes, stratoDict, stratoSource, stratoDecl;

type
  TStratoSphere=class(TObject)
  private
    FDict:TStringDictionary;
    FData:array of TStratoThing;
    FDataSize,FDataIndex:cardinal;
    FBasePath:string;
    function AddInternal(ThingType:cardinal;Name:TStratoName):TStratoIndex;
    function GetNode(ID: TStratoIndex): PStratoThing;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(ThingType:cardinal;const Name:UTF8String):TStratoIndex;
    function AddTo(var First:TStratoIndex;ThingType:cardinal;
      const Name:UTF8String):TStratoIndex; overload;
    function AddTo(var First:TStratoIndex;Item:TStratoIndex):boolean; overload;
    function Lookup(First:TStratoIndex;Name:TStratoName):TStratoIndex;
    function FQN(x:TStratoIndex):UTF8String;
    function AddBinaryData(const x:UTF8String):TStratoIndex;
    function GetBinaryData(x:TStratoIndex):UTF8String;
    function Header:PStratoHeader;
    procedure AddGlobalVar(x:TStratoIndex);
    procedure ReadSettings(const IniPath:string);
    procedure LoadFromFile(const FilePath:string);
    procedure SaveToFile(const FilePath:string);
    procedure Error(x:TStratoIndex;const Msg:string);
    property Dict:TStringDictionary read FDict;
    property Node[ID:TStratoIndex]:PStratoThing read GetNode; default;
    property BasePath:string read FBasePath;
    property NodeCount:cardinal read FDataIndex;
  end;

implementation

uses stratoRunTime, stratoTokenizer, stratoLogic, stratoFn;

const
  StratoSphereDataGrowSize=$100;

{ TStratoSphere }

constructor TStratoSphere.Create;
var
  h:PStratoHeader;
begin
  inherited Create;
  FDict:=TStringDictionary.Create;
  FDataSize:=StratoSphereDataGrowSize;
  SetLength(FData,FDataSize);
  //ZeroMemory?

  //header
  h:=@FData[0];
  h.ThingType:=ttHeader;
  h.Name:=0;//?
  h.Version:=$00000001;//0.1
  h.ThingCount:=0;//see SaveToFile
  h.SrcIndexLineMultiplier:=StratoTokenizeLineIndex;
  h.FirstNameSpace:=1;
  h.FirstGlobalVar:=0;
  h.GlobalByteSize:=0;

  //runtime
  FData[1].ThingType:=ttNameSpace;
  FData[1].Name:=FDict.StrIdx('Strato');

  FDataIndex:=2;
end;

destructor TStratoSphere.Destroy;
begin
  FDict.Free;
  inherited Destroy;
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

procedure TStratoSphere.Error(x: TStratoIndex; const Msg: string);
begin
  if (x=0) or (FData[x].Source=0) then
    Writeln(ErrOutput,Msg)
  else
   begin
    Writeln(ErrOutput,Format('%s(%d:%d): %s',
      [GetBinaryData(PStratoSourceFile(@FData[FData[x].Source]).FileName)
      ,FData[x].SrcPos div StratoTokenizeLineIndex
      ,FData[x].SrcPos mod StratoTokenizeLineIndex
      ,Msg
      ]));
   end;
  ExitCode:=1;
end;

function TStratoSphere.AddInternal(ThingType:cardinal;Name:TStratoName):TStratoIndex;
begin
  if FDataIndex=FDataSize then
   begin
    inc(FDataSize,StratoSphereDataGrowSize);
    SetLength(FData,FDataSize);//grow
   end;
  Result:=FDataIndex;
  //ZeroMemory?
  FData[Result].ThingType:=ThingType;
  FData[Result].Name:=Name;
  inc(FDataIndex);
end;

function TStratoSphere.Add(ThingType:cardinal; const Name:UTF8String): TStratoIndex;
begin
  Result:=AddInternal(ThingType,FDict.StrIdx(Name));
end;

function TStratoSphere.AddTo(var First:TStratoIndex;ThingType:cardinal;
  const Name:UTF8String):TStratoIndex;
var
  x:TStratoName;
  n:TStratoIndex;
begin
  //assert caller sets Sphere[Result].Parent
  x:=FDict.StrIdx(Name);
  n:=First;
  if n=0 then
   begin
    //first item
    First:=AddInternal(ThingType,x);
    Result:=First;
   end
  else
   begin
    while (FData[n].Next<>0) and (FData[n].Name<>x) do n:=FData[n].Next;
    if FData[n].Name=x then
      Result:=0 //duplicate !
    else
     begin
      //add
      Result:=AddInternal(ThingType,x);
      //TODO: sort? build better look-up lists?
      FData[n].Next:=Result;
     end;
   end;
end;

function TStratoSphere.AddTo(var First:TStratoIndex;Item:TStratoIndex):boolean;
var
  n:TStratoName;
  p,q:TStratoIndex;
begin
  //assert caller sets Sphere[Item].Parent
  {$IFDEF DEBUG}
  if FData[Item].Next<>0 then
    raise Exception.Create('broken chain detected');
  {$ENDIF}
  Result:=true;//default
  if First=0 then
    First:=Item
  else
   begin
    n:=FData[Item].Name;
    p:=First;
    q:=0;
    while p<>0 do
     begin
      q:=p;
      if FData[p].Name=n then
       begin
        Result:=false;
        p:=0;
       end
      else
        p:=FData[p].Next;
     end;
    if Result then FData[q].Next:=Item;
   end;
end;

function TStratoSphere.GetNode(ID: TStratoIndex): PStratoThing;
begin
  if (ID<>0) and (ID<FDataIndex) then Result:=@FData[ID] else Result:=nil;
end;

function TStratoSphere.Lookup(First:TStratoIndex;Name:TStratoName):TStratoIndex;
begin
  Result:=First;
  while (Result<>0) and (FData[Result].Name<>Name) do
    Result:=FData[Result].Next;
end;

//TODO: IdOf(PStratoThing)?
{
function TStratoSphere.IdOf(p:PStratoThing):TStratoIndex;   
begin
  Result:=(cardinal(pointer(p))-cardinal(pointer((@FData[0])))
    div SizeOf(TStratoThing);
end;
}

function TStratoSphere.FQN(x: TStratoIndex): UTF8String;
var
  n:TStratoIndex;
begin
  if x=0 then Result:='' else
   begin
    if FData[x].Name=0 then
      Result:=UTF8String(IntToStr(x))
    else
      Result:=FDict.Str[FData[x].Name];
    n:=FData[x].Parent;
    while (n<>0) and (n<FDataIndex) do
     begin
      if FData[n].Name=0 then
        Result:=UTF8String(IntToStr(n))+'.'+Result
      else
        Result:=FDict.Str[FData[n].Name]+'.'+Result;
      n:=FData[n].Parent;
     end;
   end;
end;

function TStratoSphere.AddBinaryData(const x: UTF8String): TStratoIndex;
var
  l,i:cardinal;
begin
  l:=Length(x);
  Result:=Add(ttBinaryData,'');
  FData[Result].Name:=l;//Length
  //need more than one node?
  i:=l+8;//check including header of first node
  while i>SizeOf(TStratoThing) do
   begin
    AddInternal(ttBinaryData,0);
    dec(i,SizeOf(TStratoThing));
   end;
  //p.???:=FDataIndex-Result;?
  //store data
  Move(x[1],FData[Result].Parent,l);
end;

function TStratoSphere.GetBinaryData(x: TStratoIndex): UTF8String;
begin
  //assert x<>0 and x<FDataIndex
  //assert FData[x].ThingType:=strBinary
  SetLength(Result,FData[x].Name);
  Move(FData[x].Parent,Result[1],FData[x].Name);
end;

function TStratoSphere.Header:PStratoHeader;
begin
  Result:=@FData[0];
end;

procedure TStratoSphere.AddGlobalVar(x: TStratoIndex);
var
  p,q:TStratoIndex;
begin
  //assert FData[x].ThingType=strVar
  FData[x].Offset:=TStratoHeader(FData[0]).GlobalByteSize;
  if FData[x].EvaluatesTo<>0 then
    inc(TStratoHeader(FData[0]).GlobalByteSize,FData[FData[x].EvaluatesTo].ByteSize);

  q:=AddInternal(ttGlobal,0);
  FData[q].Subject:=x;
  p:=TStratoHeader(FData[0]).FirstGlobalVar;
  if p=0 then
    TStratoHeader(FData[0]).FirstGlobalVar:=q
  else
   begin
    while FData[p].Next<>0 do p:=FData[p].Next;
    FData[p].Next:=q;
   end;
end;

procedure TStratoSphere.LoadFromFile(const FilePath: string);
var
  f:TFileStream;//TMemoryStream first?
  h:PStratoHeader;
  i,j,l:cardinal;
  x:UTF8String;
begin
  f:=TFileStream.Create(FilePath,fmOpenRead or fmShareDenyWrite);
  try
    f.Read(FData[0],SizeOf(TStratoThing));
    h:=@FData[0];
    if h.ThingType<>ttHeader then
      raise Exception.Create('File is not a sphere data file');
    //TODO: check?
    h.Version:=$00000001;//0.1
    h.SrcIndexLineMultiplier:=StratoTokenizeLineIndex;

    FDataSize:=((h.ThingCount div StratoSphereDataGrowSize)
      +1)*StratoSphereDataGrowSize;
    SetLength(FData,FDataSize);
    FDataIndex:=h.ThingCount;

    f.Read(FData[1],(h.ThingCount-1)*SizeOf(TStratoThing));

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
          raise Exception.CreateFmt('error loading dictionary: %d-%d "%s"',
            [i,j,x]);
       end;
  finally
    f.Free;
  end;
end;

procedure TStratoSphere.SaveToFile(const FilePath: string);
var
  f:TFileStream;//TMemoryStream first?
  h:PStratoHeader;
  i,l,n:cardinal;
  x:UTF8String;
begin
  f:=TFileStream.Create(FilePath,fmCreate);
  try
    h:=@FData[0];
    h.ThingCount:=FDataIndex;

    f.Write(FData[0],FDataIndex*SizeOf(TStratoThing));

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

end.
