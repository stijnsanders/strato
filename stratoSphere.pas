unit stratoSphere;

interface

{xxx$D-}

uses SysUtils, Classes, stratoDict, stratoSource, stratoDecl;

type
  TStratoSphere=class(TObject)
  private
    FDict:TStringDictionary;
    FBlock:array of record
      First,Index,Size:cardinal;
      Data:array of TStratoThing;
    end;
    FBlockIndex,FBlockCount,FBlockSize:cardinal;
    FBasePath:string;
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

uses stratoRunTime, stratoTokenizer, stratoLogic, stratoFn, Math;

const
  StratoSphereDataBlockSize=500;//$1000;
  StratoSphereDataGrowSize=100;//$100;

  StratoSphereFileVersion=$00000101;//0.1.1

{ TStratoSphere }

constructor TStratoSphere.Create;
var
  h:PStratoHeader;
begin
  inherited Create;
  FDict:=TStringDictionary.Create;
  FBlockIndex:=0;
  FBlockCount:=1;
  FBlockSize:=StratoSphereDataGrowSize;
  SetLength(FBlock,FBlockSize);
  FBlock[0].First:=0;
  FBlock[0].Index:=1;
  FBlock[0].Size:=StratoSphereDataBlockSize;
  SetLength(FBlock[0].Data,StratoSphereDataBlockSize);
  //ZeroMemory?

  //header
  h:=@FBlock[0].Data[0];
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
  //hack: avoid expensive dealloc:
  pointer(FBlock):=nil;//SetLength(FData,0);
  inherited Destroy;
end;

function TStratoSphere.MarkIndex(Index: TStratoIndex): TStratoIndex;
var
  i:cardinal;
begin
  //TODO: push/pop (or cleaner separate branches?)
  Result:=FBlock[FBlockIndex].First;
  {
  if (Index and (StratoSphereDataGrowSize-1))<>0 then
    raise Exception.Create('Block start indexes must be a multiple of block growth size');
  }
  i:=0;
  while (i<FBlockCount) and not((Index>=FBlock[i].First)
    and (Index<FBlock[i].First+FBlock[i].Size)) do inc(i);
  //TODO: check Index+StratoSphereDataBlockSize outside of current blocks
  if i=FBlockCount then
   begin
    if FBlockCount=FBlockSize then
     begin
      inc(FBlockSize,StratoSphereDataGrowSize);
      SetLength(FBlock,FBlockSize);
     end;
    inc(FBlockCount);
    FBlock[i].First:=Index;
    FBlock[i].Index:=0;
    FBlock[i].Size:=StratoSphereDataBlockSize;
    SetLength(FBlock[i].Data,StratoSphereDataBlockSize);
   end;
  FBlockIndex:=i;
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
  while (q<>0) and
    not(Node[q].ThingType in [ttNameSpace,ttOverload,ttConstructor]) do
    q:=Node[q].Parent;
  if q<>0 then
    case Node[q].ThingType of
      ttNameSpace:q:=PStratoNameSpaceData(Node[q]).SourceFile;
      ttOverload,ttConstructor:q:=Node[q].SourceFile;
      else q:=0;//raise?
    end;
  px:=Node[p];
  if (p=0) or (q=0) then
    Writeln(ErrOutput,Msg)
  else
   begin
    Writeln(ErrOutput,Format('%s(%d:%d): %s',
      [GetBinaryData(PStratoSourceFile(Node[q]).FileName)
      ,px.SrcPos div PStratoSourceFile(Node[q]).SrcPosLineIndex
      ,px.SrcPos mod PStratoSourceFile(Node[q]).SrcPosLineIndex
      ,Msg
      ]));
   end;
  ExitCode:=1;
end;

function TStratoSphere.Add(ThingType: cardinal;
  var Info: PStratoThing): TStratoIndex;
var
  i,j:cardinal;
begin
  if FBlock[FBlockIndex].Index=FBlock[FBlockIndex].Size then
   begin
    i:=0;
    j:=FBlock[FBlockIndex].First+FBlock[FBlockIndex].Size;
    while (i<FBlockCount) and (FBlock[i].First<>j) do inc(i);
    if i<>FBlockCount then
     begin
      //block full, start a new one at the end
      j:=0;
      for i:=0 to FBlockCount-1 do
        if j<FBlock[i].First+StratoSphereDataBlockSize then
          j:=FBlock[i].First+StratoSphereDataBlockSize;
      if FBlockCount=FBlockSize then
       begin
        inc(FBlockSize,StratoSphereDataGrowSize);
        SetLength(FBlock,FBlockSize);
       end;
      FBlockIndex:=FBlockCount;
      inc(FBlockCount);
      FBlock[FBlockIndex].First:=j;
      FBlock[FBlockIndex].Index:=0;
      FBlock[FBlockIndex].Size:=0;
     end;
    inc(FBlock[FBlockIndex].Size,StratoSphereDataBlockSize);
    SetLength(FBlock[FBlockIndex].Data,FBlock[FBlockIndex].Size);
   end;
  i:=FBlock[FBlockIndex].Index;
  inc(FBlock[FBlockIndex].Index);
  Result:=FBlock[FBlockIndex].First+i;
  Info:=@FBlock[FBlockIndex].Data[i];
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
    if p.Next=0 then
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
var
  i:cardinal;
begin
  if ID=0 then
    Result:=nil
  else
    if (ID>=FBlock[FBlockIndex].First)
      and (ID<FBlock[FBlockIndex].First+FBlock[FBlockIndex].Size) then
      Result:=@FBlock[FBlockIndex].Data[ID-FBlock[FBlockIndex].First]
    else
     begin
      i:=0;
      while (i<FBlockCount) and not((ID>=FBlock[i].First)
        and (ID<FBlock[i].First+FBlock[i].Size)) do inc(i);
      if i=FBlockCount then
        Result:=nil//raise? ptr to all-zero?
      else
        Result:=@FBlock[i].Data[ID-FBlock[i].First];
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
      ttVar,ttConstant,ttImport,ttSignature,ttFunction,ttArgument,
      ttPointer,ttVarByRef,ttArgByRef,ttClass,ttInterface,ttProperty] then
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
  Result:=@FBlock[0].Data[0];
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
    inc(Header.GlobalByteSize,Node[Node[p].EvaluatesTo].ByteSize);
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
    f.Read(FBlock[0].Data[0],SizeOf(TStratoThing));
    hh:=@FBlock[0].Data[0];//h:=Header;
    if hh.FileMarker<>ttFileMarker then
      raise Exception.Create('File is not a sphere data file');
    //TODO: check?
    if hh.Version<>StratoSphereFileVersion then
      raise Exception.Create('File is of an unsupported version');

    FBlock[0].Index:=hh.ThingCount;//assert <FBlock[0].Size
    //SetLength(FBlock[0].Data,
    FBlockCount:=0;
    f.Position:=0;
    h.ThingCount:=hh.ThingCount;
    while h.ThingCount<>0 do
     begin
      if FBlockCount<>0 then
       begin
        if FBlockCount=FBlockSize then
         begin
          inc(FBlockSize,StratoSphereDataGrowSize);
          SetLength(FBlock,FBlockSize);
         end;
        FBlock[FBlockCount].First:=h.FirstIndex;
        FBlock[FBlockCount].Index:=h.ThingCount;
        FBlock[FBlockCount].Size:=(h.ThingCount+(StratoSphereDataBlockSize-1))
          and not(StratoSphereDataBlockSize-1);
        SetLength(FBlock[FBlockCount].Data,FBlock[FBlockCount].Size);
       end;
      f.Read(FBlock[FBlockCount].Data[0],h.ThingCount*SizeOf(TStratoThing));
      inc(FBlockCount);
      f.Read(h,SizeOf(TStratoThing));//assert =SizeOf(TStratoBlockHeader)
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
      f.Write(FBlock[i].Data[0],FBlock[i].Index*SizeOf(TStratoThing));
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
