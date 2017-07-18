unit stratoSphere;

interface

{$D-}
{$L-}

uses SysUtils, Classes, stratoDict, stratoSource, stratoDecl;

type
  TStratoStore=class(TObject)
  private
    FDict:TStringDictionary;
    FBlock:array of record
      Data:PxValue;
      ItemCount:cardinal;
      SourceFile:xItem;
      ContinueBlockIdx:cardinal;
    end;
    FBlockCount,FBlockSize:cardinal;
    FPath:array of record
      Key,Path:string;
    end;
    procedure AddPath(const Key, Path: string);
  protected
    function NewBlock(ChainIndex:cardinal):cardinal;
    procedure AddNode(var BlockIndex:cardinal;NodeType:xTypeNr;
      Extra:cardinal;var Item:xItem;var Data:PxValue);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ReadSettings(const IniPath:string);
    procedure LoadFromFile(const FilePath:string);
    procedure SaveToFile(const FilePath:string);
    function StripPath(const FilePath:string):string;
    function FindFile(var FilePath:string):boolean;
    function ResolvePath(const FilePath:string):string;
    function NextItem(var Index:xItem):boolean;
    function NextModule(var Module:xItem):boolean;
    function SourceFile(Index:xItem):PxSourceFile;
    function ListBlocks(Item:xItem):string;
    property Dict:TStringDictionary read FDict;
  end;

  TStratoSphere=class(TObject)
  private
    FStore:TStratoStore;
    FBlockIndex:cardinal;
    FSourceFile:xItem;
    FCheckDepCache:cardinal;
    FAllwaysZero:xItem;
  public
    constructor Create(Store: TStratoStore; Source: TStratoSource);
    destructor Destroy; override;

    function n(Item:xItem;Field:xTypeNr):PxValue;

    function Add(NodeType:xTypeNr;
      Parent:xItem;SrcPos:xSrcPos;const FieldValues:array of xValue):xItem; overload;
    function Add(ListOwner:xItem;ListField:xTypeNr;NodeType:xTypeNr;
      Parent:xItem;SrcPos:xSrcPos;const FieldValues:array of xValue):xItem; overload;
    function Add(ListOwner:xItem;ListField:xTypeNr;NodeType:xTypeNr;Name:xName;
      Parent:xItem;SrcPos:xSrcPos;const FieldValues:array of xValue;
      var NewItem:xItem):boolean; overload;

    procedure Append(ListOwner:xItem;ListField:xTypeNr;ListItem:xItem);
    procedure Prepend(ListOwner:xItem;ListField:xTypeNr;ListItem:xItem);

    function Lookup(Parent:xItem;Name:xName):xItem;
    procedure First(ListOwner:xItem;ListField:xTypeNr;var ListItem,ListDelim:xItem);
    procedure Next(var ListItem:xItem;ListDelim:xItem);
    procedure None(var ListItem,ListDelim:xItem);

    function FQN(p:xItem):UTF8String;
    function AddBinaryData(const x:UTF8String):xItem;
    function GetBinaryData(p:xItem):UTF8String;
    function DebugInfo(p:xItem):string;
    procedure Error(p:xItem;const Msg:string);
    procedure InlineError(Sender:TObject;Line,LPos:cardinal;const ErrorMsg:string);

    property SourceFile:xItem read FSourceFile;
    property Store:TStratoStore read FStore;
  end;

function StratoGetSourceFile(Sphere:TStratoSphere;p:xItem;
  var srcLine,srcColumn:cardinal;var p1,p2:xItem):boolean;

type
  TStratoDebugDisplay=function(s:TStratoSphere;p:xItem):string;
var
  xDisplay:TStratoDebugDisplay;    

implementation

uses Windows, stratoTokenizer;

const
  StratoSphereDataBlockBits=20;
  StratoSphereDataBlockMax=1 shl StratoSphereDataBlockBits;
  StratoSphereDataBlockMask=(1 shl StratoSphereDataBlockBits)-1;
  StratoSphereDataBlockSize=StratoSphereDataBlockMax*xValueSize;
  StratoSphereBlockGrowSize=8;
  StratoSphereFileVersion=$00000200;//0.2.0

function BlockAlloc:pointer;
begin
  Result:=VirtualAlloc(nil,StratoSphereDataBlockSize,
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
  i:cardinal;
  p:xItem;
  sx:PxSourceFile;
begin
  inherited Create;
  FDict:=TStringDictionary.Create;
  FBlockCount:=0;
  FBlockSize:=0;
  //reserve first values to invalidate xItem(0)
  i:=0;
  NewBlock(i);
  AddNode(i,nSourceFile,0,p,PxValue(sx));
  sx.SrcPosLineIndex:=1000;//?
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

function TStratoStore.NewBlock(ChainIndex:cardinal):cardinal;
begin
  Result:=FBlockCount;
  if FBlockCount=FBlockSize then
   begin
    inc(FBlockSize,StratoSphereBlockGrowSize);
    SetLength(FBlock,FBlockSize);
   end;
  inc(FBlockCount);
  if FBlock[Result].Data=nil then
    FBlock[Result].Data:=BlockAlloc;
  FBlock[Result].ItemCount:=0;
  if ChainIndex<Result then
   begin
    FBlock[Result].SourceFile:=FBlock[ChainIndex].SourceFile;
    FBlock[ChainIndex].ContinueBlockIdx:=Result;
   end
  else
    FBlock[Result].SourceFile:=0;
  FBlock[Result].ContinueBlockIdx:=0;
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

procedure TStratoStore.AddNode(var BlockIndex:cardinal;NodeType:xTypeNr;Extra:cardinal;
  var Item:xItem;var Data:PxValue);
var
  cc:cardinal;
begin
  cc:=(cardinal(NodeType) shr 24)+2+((Extra+3) div 4);
  if cc>=StratoSphereDataBlockMax then
    raise Exception.CreateFmt('AddNode %.7x: size exceeds block size %d',[cardinal(NodeType),cc]);
  if (BlockIndex>=FBlockCount) or (FBlock[BlockIndex].ItemCount+cc>StratoSphereDataBlockMax) then
    BlockIndex:=NewBlock(BlockIndex);
  Item:=(BlockIndex shl StratoSphereDataBlockBits) or (FBlock[BlockIndex].ItemCount);
  Data:=FBlock[BlockIndex].Data;
  inc(xPtr(Data),FBlock[BlockIndex].ItemCount*xValueSize);
  Data^:=NodeType;
  inc(FBlock[BlockIndex].ItemCount,cc);
end;

procedure TStratoStore.LoadFromFile(const FilePath: string);
var
  f:TFileStream;//TMemoryStream first?
  h:TStratoFileHeader;
  b:array of TStratoBlockHeader;
  i,j,l:cardinal;
  x:UTF8String;
begin
  f:=TFileStream.Create(FilePath,fmOpenRead or fmShareDenyWrite);
  try
    l:=SizeOf(TStratoFileHeader);
    if cardinal(f.Read(h,l))<>l then RaiseLastOSError;
    if h.FileMarker<>StratoSphereFileMarker then
      raise Exception.Create('File is not a sphere data file');
    if h.FileVersion<>StratoSphereFileVersion then
      raise Exception.Create('File is of an unsupported version');

    SetLength(b,h.BlockCount);
    l:=SizeOf(TStratoBlockHeader)*h.BlockCount;
    if cardinal(f.Read(b[0],l))<>l then RaiseLastOSError;

    if FBlockSize<h.BlockCount then
     begin
      i:=(h.BlockCount+StratoSphereBlockGrowSize-1) div StratoSphereBlockGrowSize;
      FBlockSize:=i*StratoSphereBlockGrowSize;
      SetLength(FBlock,FBlockSize);
     end;
    FBlockCount:=0;
    while FBlockCount<h.BlockCount do
     begin
      if FBlock[FBlockCount].Data=nil then
        FBlock[FBlockCount].Data:=BlockAlloc;
      FBlock[FBlockCount].ItemCount:=b[FBlockCount].ItemCount;
      FBlock[FBlockCount].SourceFile:=b[FBlockCount].SourceFile;
      FBlock[FBlockCount].ContinueBlockIdx:=b[FBlockCount].ContinueBlockIdx;//TODO: check!
      l:=b[FBlockCount].ItemCount*xValueSize;
      if cardinal(f.Read(FBlock[FBlockCount].Data^,l))<>l then RaiseLastOSError;
      inc(FBlockCount);
     end;
    SetLength(b,0); 

    //FDict.Clear;
    l:=1;
    i:=0;
    while l<>0 do
      if f.Read(l,4)<>4 then l:=0 else
       begin
        SetLength(x,l);
        if cardinal(f.Read(x[1],l))<>l then RaiseLastOSError;
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
  b:TStratoBlockHeader;
  i,l,n:cardinal;
  x:UTF8String;
  h:TStratoFileHeader;
begin
  f:=TFileStream.Create(FilePath,fmCreate);
  try
    h.FileMarker:=StratoSphereFileMarker;
    h.FileVersion:=StratoSphereFileVersion;
    h.BlockCount:=FBlockCount;
    h.Reserved1:=0;
    l:=SizeOf(TStratoFileHeader);
    if cardinal(f.Write(h,l))<>l then RaiseLastOSError;

    //TODO: unused blocks?
    i:=0;
    while i<FBlockCount do
     begin
      b.Reserved1:=0;//?
      b.ItemCount:=FBlock[i].ItemCount;
      b.SourceFile:=FBlock[i].SourceFile;
      b.ContinueBlockIdx:=FBlock[i].ContinueBlockIdx;
      l:=SizeOf(TStratoBlockHeader);
      if cardinal(f.Write(b,l))<>l then RaiseLastOSError;
      inc(i);
     end;

    i:=0;
    while i<FBlockCount do
     begin
      l:=FBlock[i].ItemCount*xValueSize;
      if cardinal(f.Write(FBlock[i].Data^,l))<>l then RaiseLastOSError;
      inc(i);
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

function TStratoStore.NextItem(var Index:xItem):boolean;
var
  i,j,l:cardinal;
  p:PxValue;
  t:xTypeNr;
begin
  i:=Index shr StratoSphereDataBlockBits;
  if i>=FBlockCount then
   begin
    Index:=0;
    Result:=false; //raise?
   end
  else
   begin
    j:=Index and StratoSphereDataBlockMask;
    p:=FBlock[i].Data;
    inc(xPtr(p),j*xValueSize);
    t:=xTypeNr(p^);
    l:=((cardinal(t) shr 24) and $F)+2;
    if t=n_BinaryData then
     begin
      inc(xPtr(p),xValueSize);
      inc(l,(p^+3) shr 2);
     end;
    if j+l>=FBlock[i].ItemCount then
     begin
      inc(i);
      //TODO: skip empty blocks
      if i<FBlockCount then
       begin
        Index:=i shl StratoSphereDataBlockBits;
        Result:=true;
       end
      else
       begin
        Index:=0;
        Result:=false;
       end;
     end
    else
     begin
      inc(Index,l);
      Result:=true;
     end;
   end;
end;

function TStratoStore.NextModule(var Module:xItem):boolean;
var
  i:cardinal;
begin
  //assert nSourceFile at start of block (see TStratoSphere.Create)
  if Module=xItem(-1) then
    i:=0
  else
    i:=(Module shr StratoSphereDataBlockBits)+1;
  while (i<FBlockCount) and (xTypeNr(PxValue(FBlock[i].Data)^)<>nSourceFile) do inc(i); //?
  if i<FBlockCount then
   begin
    Module:=i shl StratoSphereDataBlockBits;
    Result:=true;
   end
  else
   begin
    Module:=0;
    Result:=false;
   end;
end;

function TStratoStore.SourceFile(Index:xItem):PxSourceFile;
var
  i:cardinal;
begin
  i:=Index shr StratoSphereDataBlockBits;
  if i>=FBlockCount then
   begin
    //Src:=0;
    Result:=nil; //raise?
   end
  else
   begin
    //assert (FBlock[i].SourceFile and StratoSphereDataBlockMask)=0
    //Src:=FBlock[i].SourceFile;
    Result:=PxSourceFile(FBlock[FBlock[i].SourceFile shr StratoSphereDataBlockBits].Data);
   end;
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

function TStratoStore.ListBlocks(Item:xItem):string;
var
  m:xItem;
  i:cardinal;
begin
  i:=Item shr StratoSphereDataBlockBits;
  if i>=FBlockCount then
    Result:='?' //raise?
  else
   begin
    m:=FBlock[i].SourceFile;
    Result:='';
    for i:=0 to FBlockCount-1 do
      if FBlock[i].SourceFile=m then
        Result:=Format('%s, $%.3x',[Result,i]);
   end;
end;

{ TStratoSphere }

constructor TStratoSphere.Create(Store: TStratoStore; Source: TStratoSource);
var
  s:PxSourceFile;
begin
  inherited Create;
  FStore:=Store;
  FCheckDepCache:=0;
  FBlockIndex:=0;//default
  FSourceFile:=0;//default
  FAllwaysZero:=0;//see function n
  if Source<>nil then
   begin
    FBlockIndex:=FStore.NewBlock(FStore.FBlockCount);
    FSourceFile:=Add(nSourceFile,0,0,[]);
    FStore.FBlock[FBlockIndex].SourceFile:=FSourceFile;//start chain //TODO: FStore method?
    s:=PxSourceFile(FStore.FBlock[FBlockIndex].Data);
    s.SrcPosLineIndex:=Source.LineIndex;
    s.FileName:=AddBinaryData(UTF8String(FStore.StripPath(Source.FilePath)));
    s.FileSize:=Source.FileSize;
    //TODO: hash, timestamp?
    //set thread id?
   end;
  //TODO: else read-only?
end;

destructor TStratoSphere.Destroy;
begin
  //clear thread id?
  inherited Destroy;
end;

procedure TStratoSphere.Error(p: xItem; const Msg: string);
var
  s:PxSourceFile;
  i:cardinal;
  SrcPos:xSrcPos;
begin
  s:=FStore.SourceFile(p);
  if (s=nil) or (s.FileName=0) then
    Writeln(ErrOutput,Msg)
  else
   begin
    SrcPos:=n(p,vSrcPos)^;
    i:=s.SrcPosLineIndex;
    if i=0 then i:=1;
    Writeln(ErrOutput,Format('%s(%d:%d): %s',
      [GetBinaryData(s.FileName)
      ,SrcPos div i
      ,SrcPos mod i
      ,Msg
      ]));
   end;
  ExitCode:=1;
end;

function TStratoSphere.n(Item:xItem;Field:xTypeNr):PxValue;
var
  i,j:cardinal;
  t:xTypeNr;
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
  i:=Item shr StratoSphereDataBlockBits;
  j:=Item and StratoSphereDataBlockMask;
  if (i>=FStore.FBlockCount) or (j>=FStore.FBlock[i].ItemCount) then
   begin
    FAllwaysZero:=0;
    Result:=@FAllwaysZero//?????!!!!!
   end
  else
   begin
    Result:=FStore.FBlock[i].Data;
    inc(xPtr(Result),j*xValueSize);
    case Field of
      vTypeNr:;
      vSrcPos:inc(xPtr(Result),xValueSize);
      fParent:inc(xPtr(Result),xValueSize*2);
      fSourceFile_NameSpaces..fSourceFile_FinalizationBlock:
        if Result^<>nSourceFile then
          raise Exception.Create('SourceFile fields only available on SourceFile')
        else
          inc(xPtr(Result),(Field-fSourceFile_NameSpaces+7)*xValueSize);
      else
        if Item=0 then
         begin
          FAllwaysZero:=0;
          Result:=@FAllwaysZero//?????!!!!!
         end
        else
         begin
          t:=Result^;
          if t<n_Item_Low then i:=0 else i:=xTypeDefX[t];
          if i=0 then raise Exception.CreateFmt('%.7x: fields not defined',
            [cardinal(t)]) {$IFDEF DEBUG}at src{$ENDIF};
          j:=i;
          while (xTypeDef[j]<f_FieldsMax) and (xTypeDef[j]<>Field) do inc(j);
          if xTypeDef[j]<f_FieldsMax then
            inc(xPtr(Result),xValueSize*(j-i+3))
          else
            raise Exception.CreateFmt('%.7x: field index not found %d',
              [cardinal(t),cardinal(Field)]) {$IFDEF DEBUG}at src{$ENDIF};
         end;
    end;
   end;
end;

function TStratoSphere.Add(NodeType:xTypeNr;
  Parent:xItem;SrcPos:xSrcPos;const FieldValues:array of xValue):xItem;
var
  i,j,k,l:integer;
  p,q:PxValue;
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
  //TODO: register dependencies
  FStore.AddNode(FBlockIndex,NodeType,0,Result,p);
  if (cardinal(NodeType) and $F00)<>0 then
   begin
    q:=p;
    inc(xPtr(q),xValueSize);q^:=SrcPos;
    inc(xPtr(q),xValueSize);q^:=Parent;
   end;
  l:=Length(FieldValues);
  if l<>0 then
   begin
    if NodeType<n_Item_Low then j:=0 else j:=xTypeDefX[NodeType];
    if j=0 then raise Exception.CreateFmt('%.7x: field not defined',
      [cardinal(NodeType)]) {$IFDEF DEBUG}at src{$ENDIF};
    i:=0;
    while i<>l do
     begin
      k:=j;
      while (xTypeDef[k]<f_FieldsMax) and (xTypeDef[k]<>FieldValues[i]) do inc(k);
      if xTypeDef[k]<f_FieldsMax then
       begin
        q:=p;
        inc(xPtr(q),xValueSize*(k-j+3));
        q^:=FieldValues[i+1];
       end
      else
        raise Exception.CreateFmt('%.7x: field index not found %d',
          [cardinal(NodeType),cardinal(FieldValues[i])]) {$IFDEF DEBUG}at src{$ENDIF};
      inc(i,2);
     end;
   end;
end;

function TStratoSphere.Add(ListOwner:xItem;ListField:xTypeNr;NodeType:xTypeNr;
  Parent:xItem;SrcPos:xSrcPos;const FieldValues:array of xValue):xItem;
var
  p,q:PxValue;
begin
  Result:=Add(NodeType,Parent,SrcPos,FieldValues);
  //append: n(ListOwner,ListField) points to last element in list
  //        last element's fNext point to top of list
  p:=n(ListOwner,ListField);
  if p^=0 then
    n(Result,fNext)^:=Result
  else
   begin
    q:=n(p^,fNext);
    n(Result,fNext)^:=q^;
    q^:=Result;
   end;
  p^:=Result;
end;

function TStratoSphere.Add(ListOwner:xItem;ListField:xTypeNr;NodeType:xTypeNr;Name:xName;
  Parent:xItem;SrcPos:xSrcPos;const FieldValues:array of xValue;
  var NewItem:xItem):boolean;
var
  x:xItem;
  p,q:PxValue;
begin
  p:=n(ListOwner,ListField);
  if p^=0 then
   begin
    NewItem:=Add(NodeType,Parent,SrcPos,FieldValues);
    n(NewItem,fNext)^:=NewItem;
    n(NewItem,vName)^:=Name;
    p^:=NewItem;
    Result:=true;
   end
  else
   begin
    x:=n(p^,fNext)^;
    while (x<>p^) and (n(x,vName)^<>Name) do x:=n(x,fNext)^;
    if n(x,vName)^=Name then
     begin
      //NewItem:=0;//?
      NewItem:=Add(NodeType,Parent,SrcPos,FieldValues);//create placeholder entry to avoid further errors
      Result:=false;
     end
    else
     begin
      q:=n(p^,fNext);
      NewItem:=Add(NodeType,Parent,SrcPos,FieldValues);
      n(NewItem,fNext)^:=q^;
      n(NewItem,vName)^:=Name;
      q^:=NewItem;
      p^:=NewItem;
      Result:=true;
     end;
   end;
end;

procedure TStratoSphere.Append(ListOwner:xItem;ListField:xTypeNr;ListItem:xItem);
var
  p,q,r:PxValue;
begin
  p:=n(ListOwner,ListField);
  q:=n(ListItem,fNext);
  if q^<>0 then
    raise Exception.CreateFmt('Append: broken list detected, item can''t be on two lists $%.8x',[cardinal(ListItem)]);
  if p^=0 then
   begin
    q^:=ListItem;
    p^:=ListItem;
   end
  else
   begin
    r:=n(p^,fNext);
    q^:=r^;
    r^:=ListItem;
    p^:=ListItem;
   end;
end;

procedure TStratoSphere.Prepend(ListOwner:xItem;ListField:xTypeNr;ListItem:xItem);
var
  p,q,r:PxValue;
begin
  p:=n(ListOwner,ListField);
  q:=n(ListItem,fNext);
  if q^<>0 then
    raise Exception.CreateFmt('Append: broken list detected, item can''t be on two lists $%.8x',[cardinal(ListItem)]);
  if p^=0 then
   begin
    q^:=ListItem;
    p^:=ListItem;
   end
  else
   begin
    r:=n(p^,fNext);
    q^:=r^;
    r^:=ListItem;
   end;
end;

function TStratoSphere.Lookup(Parent:xItem;Name:xName):xItem;
var
  p,p0:xItem;
begin
  case n(Parent,vTypeNr)^ of
    nSourceFile:
      p0:=n(Parent,fSourceFile_NameSpaces)^;//Globals?
    nNameSpace,nRecord,nEnumeration,nClass,nInterface:
      p0:=n(Parent,fItems)^;
    nCodeBlock:
      p0:=n(Parent,fVarDecls)^;
    else
      p0:=0;
  end;
  p:=n(p0,fNext)^;
  while (p<>0) and not(((n(p,vTypeNr)^ and $F00)>=$300) and (n(p,vName)^=Name)) do
    if p=p0 then p:=0 else p:=n(p,fNext)^;
  Result:=p;
end;

procedure TStratoSphere.First(ListOwner:xItem;ListField:xTypeNr;var ListItem,ListDelim:xItem);
begin
  ListDelim:=n(ListOwner,ListField)^;//list owner points to last element in list (for easy append)
  ListItem:=n(ListDelim,fNext)^;//last element points 'next' to first element
end;

procedure TStratoSphere.Next(var ListItem:xItem;ListDelim:xItem);
begin
  if ListItem=ListDelim then //did last element?
    ListItem:=0 //then we're done
  else
    ListItem:=n(ListItem,fNext)^; //else advance on the list
end;

procedure TStratoSphere.None(var ListItem,ListDelim:xItem);
begin
  ListDelim:=0;
  ListItem:=0;
end;

function TStratoSphere.FQN(p: xItem): UTF8String;
var
  q:PxValue;
  i,j:cardinal;
begin
  Result:='';
  while p<>0 do
   begin
    q:=n(p,vTypeNr);
    case q^ of
      nCodeBlock,nOverload:;//don't include in name path
      else
       begin
        if q^<n_Item_Low then i:=0 else i:=xTypeDefX[q^];
        if i<>0 then
         begin
          j:=i;
          while (xTypeDef[j]<f_FieldsMax) and (xTypeDef[j]<>vName) do inc(j);
          if xTypeDef[j]<f_FieldsMax then
           begin
            inc(xPtr(q),xValueSize*(j-i+3));
            i:=q^;
           end
          else
            i:=0;//no vName
         end;
        if i=0 then
          Result:=Format('$%.8x.%s',[cardinal(p),Result])
        else
          Result:=FStore.Dict.Str[i]+'.'+Result;
       end;
    end;
    p:=n(p,fParent)^;
   end;
  if Result<>'' then SetLength(Result,Length(Result)-1);//trailing "."
end;

const
  //ttBinaryData
  tf_BinaryData_Length =1;
  tf_BinaryData_Start  =2;

function TStratoSphere.AddBinaryData(const x:UTF8String):xItem;
var
  l:cardinal;
  p:PxValue;
begin
  l:=Length(x);
  FStore.AddNode(FBlockIndex,n_BinaryData,l,Result,p);
  inc(xPtr(p),xValueSize);
  p^:=l;
  inc(xPtr(p),xValueSize);
  Move(x[1],p^,l);
end;

function TStratoSphere.GetBinaryData(p:xItem):UTF8String;
var
  q:PxValue;
  l:cardinal;
begin
  q:=n(p,vTypeNr);
  if q^<>n_BinaryData then
    raise Exception.CreateFmt('GetBinaryData: item is not binary data $%.8x: %.7x',[cardinal(p),cardinal(q^)]);
  inc(xPtr(q),xValueSize);
  if q^=0 then Result:='' else
   begin
    l:=q^;
    inc(xPtr(q),xValueSize);
    SetLength(Result,l);
    Move(q^,Result[1],l);
   end;
end;

function TStratoSphere.DebugInfo(p:xItem):string;
var
  i,k,l:cardinal;
  t:xTypeNr;
  q:PxValue;
begin
  q:=n(p,vTypeNr);
  t:=q^;
  Result:=Format('%.7x',[t]);
  l:=(t shr 24)+2;
  i:=1;
  k:=$1000000;
  while i<l do
   begin
    k:=k shr 1;
    inc(xPtr(q),xValueSize);
    if (t and k)=0 then
      Result:=Format('%s, %d',[Result,q^])
    else
      Result:=Format('%s, $%.8x',[Result,q^]);
    inc(i);
   end;
  Result[8]:=':';//was ','
end;

procedure TStratoSphere.InlineError(Sender: TObject; Line, LPos: cardinal;
  const ErrorMsg: string);
begin
  {$IFDEF DEBUG}
  Store.SaveToFile('test.xsu');
  {$ENDIF}
  AddBinaryData(Format('### %s(%d:%d): %s',
    [(Sender as TStratoSource).FilePath,Line,LPos,ErrorMsg]));
end;

{ StratoGetSourceFile }

function StratoGetSourceFile(Sphere:TStratoSphere;p:xItem;
  var srcLine,srcColumn:cardinal;var p1,p2:xItem):boolean;
var
  i:cardinal;
  s:PxSourceFile;
  q:PxValue;
  t:xTypeNr;
begin
  s:=Sphere.FStore.SourceFile(p);
  if s=nil then
    Result:=false
  else
   begin
    Result:=true;
    q:=Sphere.n(p,vTypeNr);
    t:=q^;

    //SrcPos:=r^;
    inc(xPtr(q),xValueSize);
    i:=s.SrcPosLineIndex;
    if i=0 then i:=1;
    srcLine  :=q^ div i;
    srcColumn:=q^ mod i;

    //fParent
    inc(xPtr(q),xValueSize);
    p1:=q^;

    //fNext?
    if (t>=n_Item_Low) and (t<=n_Item_High) and (xTypeDef[xTypeDefX[t]]=fNext) then
     begin
      inc(xPtr(q),xValueSize);
      p2:=q^;//fParent
     end
    else
      p2:=0;

   end;
end;

function DefaultTypeDisplay(s:TStratoSphere;p:xItem):string;
begin
  Result:=Format('%.7x',[s.n(p,vTypeNr)^]);
end;

initialization
  xDisplay:=DefaultTypeDisplay;
end.
