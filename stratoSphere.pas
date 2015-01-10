unit stratoSphere;

interface

{$D-}

uses SysUtils, Classes, stratoDict, stratoSource, stratoDecl;

type
  TStratoSphere=class(TObject)
  private
    FDict:TStringDictionary;
    FData:array of array of TStratoThing;
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
    procedure Prepend(var First:TStratoIndex;Item:TStratoIndex);
    procedure Append(var First:TStratoIndex;Item:TStratoIndex);
    function Lookup(First:TStratoIndex;Name:TStratoName):TStratoIndex;
    function FQN(x:TStratoIndex):UTF8String;
    function AddBinaryData(const x:UTF8String):TStratoIndex;
    function GetBinaryData(x:TStratoIndex):UTF8String;
    function Header:PStratoHeader;
    procedure AddGlobalVar(p:TStratoIndex);
    procedure ReadSettings(const IniPath:string);
    procedure LoadFromFile(const FilePath:string);
    procedure SaveToFile(const FilePath:string);
    procedure Error(x:TStratoIndex;const Msg:string);
    procedure InlineError(Sender:TObject;Line,LPos:cardinal;
      const ErrorMsg:string);
    property Dict:TStringDictionary read FDict;
    property Node[ID:TStratoIndex]:PStratoThing read GetNode; default;
    property BasePath:string read FBasePath;
    property NodeCount:cardinal read FDataIndex;
  end;

implementation

uses stratoRunTime, stratoTokenizer, stratoLogic, stratoFn, Math;

const
  StratoSphereDataBlockSize=340;//~16KiB/48B
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
  SetLength(FData[0],StratoSphereDataBlockSize);
  //ZeroMemory?

  //header
  h:=@FData[0][0];
  h.FileMarker:=ttHeader;
  h.Name:=0;//?
  h.Version:=$00000001;//0.1
  h.ThingCount:=0;//see SaveToFile
  h.SrcIndexLineMultiplier:=StratoTokenizeLineIndex;
  h.FirstNameSpace:=1;
  h.FirstGlobalVar:=0;
  h.GlobalByteSize:=0;

  //runtime
  FData[0][1].ThingType:=ttNameSpace;
  FData[0][1].Name:=FDict.StrIdx('Strato');

  FDataIndex:=2;
end;

destructor TStratoSphere.Destroy;
begin
  FDict.Free;
  //hack: avoid expensive dealloc:
  pointer(FData):=nil;//SetLength(FData,0);
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
var
  px:PStratoThing;
begin
  px:=Node[x];
  if (x=0) or (px.Source=0) then
    Writeln(ErrOutput,Msg)
  else
   begin
    Writeln(ErrOutput,Format('%s(%d:%d): %s',
      [GetBinaryData(PStratoSourceFile(Node[px.Source]).FileName)
      ,px.SrcPos div StratoTokenizeLineIndex
      ,px.SrcPos mod StratoTokenizeLineIndex
      ,Msg
      ]));
   end;
  ExitCode:=1;
end;

function TStratoSphere.AddInternal(ThingType:cardinal;Name:TStratoName):TStratoIndex;
var
  i,j:cardinal;
begin
  //DivMod(FDataIndex,StratoSphereBlockSize,i,j);
  i:=FDataIndex div StratoSphereDataBlockSize;
  j:=FDataIndex mod StratoSphereDataBlockSize;
  if j=0 then
   begin
    if i=FDataSize then
     begin
      inc(FDataSize,StratoSphereDataGrowSize);
      SetLength(FData,FDataSize);//grow
     end;
    SetLength(FData[i],StratoSphereDataBlockSize);
   end;
  Result:=FDataIndex;
  //ZeroMemory?
  FData[i][j].ThingType:=ThingType;
  FData[i][j].Name:=Name;
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
  p:PStratoThing;
begin
  //assert caller sets Sphere[Result].Parent
  x:=FDict.StrIdx(Name);
  if First=0 then
   begin
    //first item
    First:=AddInternal(ThingType,x);
    Result:=First;
   end
  else
   begin
    p:=Node[First];
    while (p.Next<>0) and (p.Name<>x) do p:=Node[p.Next];
    if p.Name=x then
      Result:=0 //duplicate !
    else
     begin
      //add
      Result:=AddInternal(ThingType,x);
      //TODO: sort? build better look-up lists?
      p.Next:=Result;
     end;
   end;
end;

function TStratoSphere.AddTo(var First:TStratoIndex;Item:TStratoIndex):boolean;
var
  n:TStratoName;
  p,q:PStratoThing;
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
    p:=Node[First];
    q:=nil;
    while p<>nil do
     begin
      q:=p;
      if p.Name=n then
       begin
        Result:=false;
        p:=nil;
       end
      else
        p:=Node[p.Next];
     end;
    if Result then q.Next:=Item;
   end;
end;

procedure TStratoSphere.Prepend(var First: TStratoIndex; Item: TStratoIndex);
begin
  Node[Item].Next:=First;
  First:=Item;
end;

procedure TStratoSphere.Append(var First: TStratoIndex; Item: TStratoIndex);
var
  p:PStratoThing;
begin
  p:=Node[First];
  if p=nil then
    First:=Item
  else
   begin
    while p.Next<>0 do p:=Node[p.Next];
    p.Next:=Item;
   end;
end;

function TStratoSphere.GetNode(ID: TStratoIndex): PStratoThing;
begin
  if ID=0 then Result:=nil else
    if ID<FDataIndex then
      Result:=@FData
        [ID div StratoSphereDataBlockSize]
        [ID mod StratoSphereDataBlockSize]
    else
      Result:=nil;
end;

function TStratoSphere.Lookup(First:TStratoIndex;Name:TStratoName):TStratoIndex;
begin
  Result:=First;
  while (Result<>0) and (Node[Result].Name<>Name) do
    Result:=Node[Result].Next;
end;

function TStratoSphere.FQN(x: TStratoIndex): UTF8String;
var
  n:TStratoIndex;
begin
  if x=0 then Result:='' else
   begin
    if Node[x].Name=0 then
      Result:=UTF8String(IntToStr(x))
    else
      Result:=FDict.Str[Node[x].Name];
    n:=Node[x].Parent;
    while (n<>0) and (n<FDataIndex) do
     begin
      if Node[n].Name=0 then
        Result:=UTF8String(IntToStr(n))+'.'+Result
      else
        Result:=FDict.Str[Node[n].Name]+'.'+Result;
      n:=Node[n].Parent;
     end;
   end;
end;

function TStratoSphere.AddBinaryData(const x: UTF8String): TStratoIndex;
var
  l,i:cardinal;
begin
  l:=Length(x);
  Result:=Add(ttBinaryData,'');
  Node[Result].Name:=l;//Length
  //need more than one node?
  i:=l+8;//check including header of first node
  while i>SizeOf(TStratoThing) do
   begin
    AddInternal(ttBinaryData,0);
    dec(i,SizeOf(TStratoThing));
   end;
  //p.???:=FDataIndex-Result;?
  //store data
  Move(x[1],Node[Result].Parent,l);
end;

function TStratoSphere.GetBinaryData(x: TStratoIndex): UTF8String;
begin
  //assert x<>0 and x<FDataIndex
  //assert Node[x].ThingType:=ttBinary
  SetLength(Result,Node[x].Name);
  Move(Node[x].Parent,Result[1],Node[x].Name);
end;

function TStratoSphere.Header:PStratoHeader;
begin
  Result:=@FData[0][0];
end;

procedure TStratoSphere.AddGlobalVar(p: TStratoIndex);
var
  q:TStratoIndex;
begin
  //assert Node[p].ThingType=ttVar
  Node[p].Offset:=Header.GlobalByteSize;
  if Node[p].EvaluatesTo<>0 then
    inc(Header.GlobalByteSize,Node[Node[p].EvaluatesTo].ByteSize);

  q:=AddInternal(ttGlobal,0);
  Node[q].Subject:=p;
  Append(Header.FirstGlobalVar,q);
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
    f.Read(FData[0][0],SizeOf(TStratoThing));
    h:=@FData[0][0];
    if h.FileMarker<>ttHeader then
      raise Exception.Create('File is not a sphere data file');
    //TODO: check?
    h.Version:=$00000001;//0.1
    h.SrcIndexLineMultiplier:=StratoTokenizeLineIndex;

    FDataIndex:=h.ThingCount+1;
    i:=(FDataIndex div StratoSphereDataBlockSize)+1;
    FDataSize:=((i div StratoSphereDataGrowSize)+1)*StratoSphereDataGrowSize;
    SetLength(FData,FDataSize);

    f.Position:=0;
    i:=0;
    l:=FDataIndex;
    while l>StratoSphereDataBlockSize do
     begin
      SetLength(FData[i],StratoSphereDataBlockSize);
      f.Read(FData[i][0],StratoSphereDataBlockSize*SizeOf(TStratoThing));
      dec(l,StratoSphereDataBlockSize);
      inc(i);
     end;
    if l<>0 then
     begin
      if i<>0 then SetLength(FData[i],StratoSphereDataBlockSize);
      f.Read(FData[i][0],l*SizeOf(TStratoThing));
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
  i,l,n:cardinal;
  x:UTF8String;
begin
  f:=TFileStream.Create(FilePath,fmCreate);
  try
    Header.ThingCount:=FDataIndex-1;

    i:=0;
    l:=FDataIndex;
    while l>StratoSphereDataBlockSize do
     begin
      f.Write(FData[i][0],StratoSphereDataBlockSize*SizeOf(TStratoThing));
      dec(l,StratoSphereDataBlockSize);
      inc(i);
     end;
    if l<>0 then
      f.Write(FData[i][0],l*SizeOf(TStratoThing));

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

procedure TStratoSphere.InlineError(Sender: TObject; Line, LPos: cardinal;
  const ErrorMsg: string);
begin
  AddBinaryData(Format('### %s(%d:%d): %s',
    [(Sender as TStratoSource).FilePath,Line,LPos,ErrorMsg]));
end;

end.
