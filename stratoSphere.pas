unit stratoSphere;

interface

{$D-}
{$L-}

uses stratoDecl, Classes, SysUtils;

const
  StratoSphereBlockSize = $10000;

type
  TStratoSphere=class;//forward

  xKeyValue=record
    k:xKey;   //key
    n:xRef;   //next
    i:xValue; //sphere index (0 for local sphere)
    v:xValue; //value
    procedure s(kx:xKey;ix,vx:xValue); inline;
    function BinaryData:UTF8String;
  end;
  PxKeyValue=^xKeyValue;

  xNode=record
    sphere:TStratoSphere;
    index:xRef;
    {$IFDEF X64}
    _padding:cardinal; //TODO: xValue=int64?
    {$ENDIF}

    function Key:xKey; inline;
    function AsString:string;
    function IsNone:boolean; inline;
    function IsSame(Node:xNode):boolean; inline;

    function i(Key:xKey):xValue; //index
    function v(Key:xKey):xValue; //value
    function q(Key:xKey;var Value:xValue):boolean;//query
    function r(Key:xKey):xNode; //reference
    function rl(Key:xKey):xNode; //local

    procedure s(ASphere:TStratoSphere;AIndex:xRef); inline;
    procedure ss(ASphere:TStratoSphere;ASphereIndex:xValue;AIndex:xRef); inline;
    procedure none; inline;

    function Lookup(const Name:UTF8String):xNode;

    procedure Start(Subject:xNode;List:xKey);
    function Next(var Item:xNode):boolean;
    function Done:boolean;
  end;

  TStratoSphereBlock=array[0..StratoSphereBlockSize-1] of xKeyValue;
  PStratoSphereBlock=^TStratoSphereBlock;

  TStratoSphere=class(TObject)
  private
    NextIndex:cardinal;

    BlocksCount,BlocksSize:cardinal;
    Blocks:array of PStratoSphereBlock;

    //DependsCount,DependsSize:cardinal;
    //Depends:array of cardinal;

    function GetIndex(Index:xRef):PxKeyValue; inline;

    function AddRaw(Length:cardinal;var NewItem:xRef):PxKeyValue; inline;
    function SetKey(Item:xRef;Key:xKey):PxKeyValue;

  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromStream(f:TStream);
    procedure SaveToStream(f:TStream);
    property kCount:cardinal read NextIndex;

    property k[Index:xRef]:PxKeyValue read GetIndex; default;
    function v(Index:xRef;Key:xKey):PxKeyValue;
    function r(Index:xRef;Key:xKey):xNode;
    function n(Index:xRef):xName; inline;
    function a(Index:xRef;Key:xKey;Value:cardinal):cardinal;

    function Add(Key:xKey;ReserveKeys:cardinal):xRef;
    procedure SetVal(Item:xRef;Key:xKey;Index,Value:xValue);
    procedure SetRef(Item:xRef;Key:xKey;Node:xNode);
    function AddName(const x:UTF8String):xName;
    function AddBinaryData(const x:UTF8String):xRef;

    function GetName(DictionaryEntry:xRef):UTF8String;
    function IsName(Node:xNode;Name:xName):boolean;
    function FQN(Item:xRef):UTF8String;
    function BinaryData(p:xRef):UTF8String;

    function Lookup(Parent:xRef;Name:xName;List:xKey):xNode;

    function Prepend(Parent:xRef;List:xKey;Item:xRef):xRef;
    function Append(Parent:xRef;List:xKey;Item:xRef):xRef;

    //function ByteSize(p:xRef):cardinal;
  end;

function kv(Key:xKey;Index,Value:xValue):xKeyValue; inline;
//function kn(Key:xKey;Node:xNode):xKeyValue; inline;

function none:xNode; inline;

var
  KnownPaths:array of record
    Key,Path:string;
  end;
  KnownPathsCount,KnownPathsSize:cardinal;

  Spheres:array of TStratoSphere;
  SpheresCount,SpheresSize:cardinal;

  SystemWordSize:cardinal;//TODO: sphere property?
  IntrinsicTypes:array[TStratoIntrinsicType] of xNode;

const
  SpheresGrowStep=32;
  DependsGrowStep=32;
  BlocksGrowStep=32;

  InvalidIndex=cardinal(-1);//$FFFFFFFF;

function KeyToStr(Key:xKey):string;

function IsIntrinsicNumeric(i:xNode):boolean;

procedure AddSphere(Sphere:TStratoSphere);
function SphereIndex(Sphere:TStratoSphere):cardinal;

//tried consts, didn't work, so inline functions:
function SphereIndexPrefix:UTF8String;
function SphereIndexSuffix:UTF8String;

implementation

uses Windows, stratoTools;

procedure BlockFree(x:pointer);
begin
  if not VirtualFree(x,0,MEM_RELEASE) then RaiseLastOSError;
end;

function KeyToStr(Key:xKey):string;
begin
  case Key of

  nNameSpace   :Result:='NameSpace';
  nType        :Result:='Type';
  nLiteral     :Result:='Literal';
  nConstant    :Result:='Constant';
  nArray       :Result:='Array';
  nEnum        :Result:='Enum';
  nRecord      :Result:='Record';
  nPointer     :Result:='Pointer';
  nTypeAlias   :Result:='TypeAlias';
  nSignature   :Result:='Signature';
  nSigArg      :Result:='SigArg';
  nSigArgByRef :Result:='SigArgByRef';
  nOverload    :Result:='Overload';
  nClass       :Result:='Class';
  nClassRef    :Result:='ClassRef';
  nCtor        :Result:='Constructor';
  nDtor        :Result:='Destructor';
  nPropGet     :Result:='PropertyGet';
  nPropSet     :Result:='PropertySet';
  nInterface   :Result:='Interface';

  nCodeBlock   :Result:='CodeBlock';
  nVar         :Result:='Variable';
  nVarByRef    :Result:='VarByRef';
  nVarReadOnly :Result:='VarReadOnly';
  nThis        :Result:='This';

  nSCall       :Result:='SysCall';
  nFCall       :Result:='FnCall';
  nVCall       :Result:='VirtualCall';
  nICall       :Result:='IntfCall';
  nCallArg     :Result:='CallArg';

  nCast        :Result:='Cast';
  nAddressOf   :Result:='AddressOf';
  nDereference :Result:='Dereference';
  nArrayIndex  :Result:='ArrayIndex';
  nField       :Result:='Field';

  nAssign      :Result:='Assign';
  nUnaryOp     :Result:='UnaryOp';
  nBinaryOp    :Result:='BinaryOp';
  nSelection   :Result:='Selection';
  nIteration   :Result:='Iteration';
  nIterPostEval:Result:='IterPostEval';
  nRange       :Result:='Range';
  nRangeIndex  :Result:='RangeIndex';

  nTry         :Result:='Try';
  nThrow       :Result:='Throw';
  nDefer       :Result:='Defer';
  nCatch       :Result:='Catch';

  //nImport      :Result:='Import';
  nSphere      :Result:='Sphere';
  //TODO
  else Result:=Format('?%d',[cardinal(Key)]);
  end;
end;

function IsIntrinsicNumeric(i:xNode):boolean; inline;
begin
  //assert IntrinsicTypes[itNumber]<>0
  //assert IntrinsicTypes[itString]<>0
  //assert IntrinsicTypes[].source=0 of those inbetween
  Result:=(SpheresCount<>0) and (i.sphere=Spheres[0]) and
    (i.index>=IntrinsicTypes[itNumber].index) and (i.index<IntrinsicTypes[itString].index);
end;

procedure AddSphere(Sphere:TStratoSphere);
begin
  if SpheresCount=SpheresSize then
   begin
    inc(SpheresSize,SpheresGrowStep);
    SetLength(Spheres,SpheresSize);
   end;
  Spheres[SpheresCount]:=Sphere;
  inc(SpheresCount);

  //TODO: mark Sphere read-only
end;

function SphereIndex(Sphere:TStratoSphere):cardinal;
begin
  Result:=0;
  while (Result<SpheresCount) and (Spheres[Result]<>Sphere) do inc(Result);
  if Result=SpheresCount then
    raise Exception.Create('Sphere not fully parsed yet.');
  inc(Result);
end;

function SphereIndexPrefix:UTF8String; inline;
begin
  SetLength(Result,2);
  Result[1]:=#$C2;
  Result[2]:=#$AB;//&laquo;
end;

function SphereIndexSuffix:UTF8String; inline;
begin
  SetLength(Result,2);
  Result[1]:=#$C2;
  Result[2]:=#$BB;//&raquo;
end;

{ ... }

function kv(Key:xKey;Index,Value:xValue):xKeyValue;
begin
  Result.k:=Key;
  Result.n:=xRef(0);//handled by sphere
  Result.i:=Index;
  Result.v:=Value;
end;

{
function kn(Key:xKey;Node:xNode):xKeyValue;
begin
  Result.k:=Key;
  Result.n:=xRef(0);//handled by sphere
  Result.i:=SphereIndex(Node.sphere);
  Result.v:=Node.index;
end;
}

function none:xNode;
begin
  Result.none;
end;

{ xKeyValue }

procedure xKeyValue.s(kx:xKey;ix,vx:xValue);
begin
  //assert owner sourcefile not marked complete
  k:=kx;
  i:=ix;
  v:=vx;
end;

function xKeyValue.BinaryData:UTF8String;
type
  x2Value=record a,b:xValue end;
  Px2Value=^x2Value;
begin
  {$IFDEF DEBUG}
  if k<>xBinaryData then
    raise Exception.Create('Key is not xBinaryData ('+IntToStr(cardinal(k))+')');
  {$ENDIF}
  SetLength(Result,i);
  Move(Px2Value(@v)^.b,Result[1],i);
end;

{ TStratoSphere }

constructor TStratoSphere.Create;
begin
  inherited Create;
  //DependsCount:=0;
  //DependsSize:=0;
  BlocksCount:=0;
  BlocksSize:=0;
  NextIndex:=0;
  Add(nSphere,8);
end;

destructor TStratoSphere.Destroy;
begin
  //?
  inherited Destroy;
end;

procedure TStratoSphere.LoadFromStream(f: TStream);
var
  s:TStratoSphereHeader;
  i,l:integer;
begin
  l:=SizeOf(TStratoSphereHeader);
  if f.Read(s,l)<>l then RaiseLastOSError;
  NextIndex:=0;
  Add(xUnassigned,s.Items);//allocate memory
  i:=0;
  while s.Items<>0 do
   begin
    if s.Items>StratoSphereBlockSize then
      l:=StratoSphereBlockSize
    else
      l:=s.Items;
    dec(s.Items,l);
    l:=l*SizeOf(xKeyValue);
    if f.Read(Blocks[i][0],l)<>l then
      RaiseLastOSError;
    inc(i);
   end;
end;

procedure TStratoSphere.SaveToStream(f: TStream);
var
  s:TStratoSphereHeader;
  i,l:cardinal;
begin
  s.Items:=NextIndex;
  s.Reserved_1:=0;
  s.Reserved_2:=0;
  s.Reserved_3:=0;
  l:=SizeOf(TStratoSphereHeader);
  if cardinal(f.Write(s,l))<>l then RaiseLastOSError;
  i:=0;
  while s.Items<>0 do
   begin
    if s.Items>StratoSphereBlockSize then
      l:=StratoSphereBlockSize
    else
      l:=s.Items;
    dec(s.Items,l);
    l:=l*SizeOf(xKeyValue);
    if cardinal(f.Write(Blocks[i][0],l))<>l then
      RaiseLastOSError;
    inc(i);
   end;
end;

function TStratoSphere.GetIndex(Index:xRef):PxKeyValue;
begin
  {$IFDEF DEBUG}
  if Index>=NextIndex then
    raise Exception.CreateFmt('Item index out of range %d (%d)',[index,NextIndex]);
  {$ENDIF}
  Result:=@Blocks
    [Index div StratoSphereBlockSize]
    [Index mod StratoSphereBlockSize];
end;

function TStratoSphere.v(Index:xRef;Key:xKey):PxKeyValue;
var
  i:xRef;
begin
  if k[Index].k<n_Max then
   begin
    //TODO: check Field valid member for k[i].k
    i:=k[Index].n;
    while (i<>0) and (k[i].k<>Key) do i:=k[i].n;
    if i=0 then Result:=nil else Result:=k[i];
   end
  else
    Result:=nil;
end;

function TStratoSphere.r(Index:xRef;Key:xKey):xNode;
var
  p:PxKeyValue;
begin
  p:=v(Index,Key);
  if p=nil then
    Result.none
  else
    Result.ss(Self,p.i,p.v);
end;

function TStratoSphere.n(Index:xRef):xName;
var
  p:PxKeyValue;
begin
  p:=v(Index,dName);
  if p=nil then
    Result:=0
  else
    Result:=p.v;
end;

function TStratoSphere.a(Index:xRef;Key:xKey;Value:cardinal):cardinal;
var
  p:PxKeyValue;
begin
  p:=v(Index,Key);
  if p=nil then
   begin
    Result:=0;
    SetVal(Index,Key,0,Value);
   end
  else
   begin
    Result:=p.v;
    p.v:=p.v+Value;
   end;
end;

function TStratoSphere.AddRaw(Length:cardinal;var NewItem:xRef):PxKeyValue;
var
  p:pointer;
begin
  //TODO: deny when switched to read-only/complete

  //assert Length>0
  NewItem:=NextIndex;

  while NextIndex+Length>=BlocksCount*StratoSphereBlockSize do
   begin
    if BlocksCount=BlocksSize then
     begin
      inc(BlocksSize,BlocksGrowStep);
      SetLength(Blocks,BlocksSize);
     end;

    //allocate new block
    p:=VirtualAlloc(nil,SizeOf(TStratoSphereBlock),
      MEM_RESERVE or MEM_COMMIT,PAGE_READWRITE);//MEM_LARGE_PAGES?
    if p=nil then RaiseLastOSError;

    Blocks[BlocksCount]:=p;
    inc(BlocksCount);
   end;

  inc(NextIndex,Length);
  Result:=k[NewItem];
end;

function TStratoSphere.Add(Key:xKey;ReserveKeys:cardinal):xRef;
var
  p:xRef;
  q:PxKeyValue;
begin
  //assert ReserveKeys>0
  q:=AddRaw(ReserveKeys,Result);
  q.k:=Key;
  q.n:=0;
  q.i:=0;
  q.v:=0;
  p:=Result;
  dec(ReserveKeys);
  while ReserveKeys<>0 do
   begin
    inc(p);
    q.n:=p;
    q:=k[p];
    q.k:=xUnassigned;
    q.n:=0;
    q.i:=0;
    q.v:=0;
    dec(ReserveKeys);
   end;
end;

function TStratoSphere.SetKey(Item:xRef;Key:xKey):PxKeyValue;
var
  i,p:xRef;
begin
  Result:=nil;//default
  if Item>=NextIndex then
    raise Exception.Create('Item index out of range '+IntToStr(Item)+
      ' ('+IntToStr(NextIndex)+')');
  //assert k[Item].k<n_Max
  //TODO: check Field valid member for k[Item].k
  p:=Item;
  i:=k[p].n;
  while i<>0 do
   begin
    if i>NextIndex then
      raise Exception.Create('Broken chain detected '+IntToStr(i));
    Result:=k[i];
    if Result.k=xUnassigned then
     begin
      i:=0;
      Result.k:=Key;
     end
    else
      if Result.k=Key then
        i:=0
      else
       begin
        p:=i;
        i:=k[p].n;
        Result:=nil;
       end;
   end;
  if Result=nil then
   begin
    Result:=AddRaw(1,k[p].n);
    Result.k:=Key;
   end;
end;

procedure TStratoSphere.SetVal(Item:xRef;Key:xKey;Index,Value:xValue);
var
  p:PxKeyValue;
begin
  p:=SetKey(Item,Key);
  p.i:=Index;
  p.v:=Value;
end;

procedure TStratoSphere.SetRef(Item:xRef;Key:xKey;Node:xNode);
var
  p:PxKeyValue;
begin
  if not(Node.IsNone) then
   begin
    p:=SetKey(Item,Key);
    if Node.sphere=Self then
      p.i:=0
    else
      p.i:=SphereIndex(Node.sphere);
    p.v:=Node.index;
   end;
end;

function TStratoSphere.AddName(const x:UTF8String):xName;
var
  i,i0,j,l,nf:cardinal;
  p0,p1,p2,p3:xRef;
  q0,q1,q2,q3:PxKeyValue;
begin
  //TODO: disallow when marked read-only!
  i:=0;
  l:=Length(x);
  p0:=0;
  q0:=SetKey(0,lSphere_Dictionary);
  while i<l do
   begin
    //4 bytes name fragment
    nf:=0;
    i0:=i;
    for j:=0 to 3 do
     begin
      nf:=nf shl 8;
      if i<l then
       begin
        inc(i);
        nf:=nf or byte(x[i]);
       end;
     end;
    //lookup
    p1:=q0.v;
    if p1=0 then
     begin
      //start this level
      q2:=AddRaw(2,p2);
      q2.k:=xDictionary_Entry;
      q2.n:=p2+1;
      q2.i:=nf;
      q2.v:=0;
      q3:=k[p2+1];
      q3.k:=xDictionary_Tail;
      q3.i:=i0;
      q3.v:=p0;
      //
      q0.v:=p2;
      p0:=p2;
      q0:=q2;
     end
    else
     begin
      q1:=k[p1];
      p2:=0;
      q2:=nil;
      while p1<>0 do
        case q1.k of
        xDictionary_Entry:
          if q1.i<nf then
           begin
            //keep looking
            p2:=p1;
            q2:=q1;
            p1:=q1.n;
            q1:=k[p1];
           end
          else
          if q1.i=nf then
           begin
            //found
            p0:=p1;
            q0:=q1;
            p1:=0;//end loop
           end
          else
          //if q1.i>nf then
           begin
            //insert here
            q3:=AddRaw(1,p3);
            q3.k:=xDictionary_Entry;
            q3.n:=p1;
            q3.i:=nf;
            q3.v:=0;
            //
            if p2=0 then q0.v:=p3 else q2.n:=p3;
            p0:=p3;
            q0:=q3;
            p1:=0;//end loop
           end;
        xDictionary_Tail:
         begin
          //insert here
          q3:=AddRaw(1,p3);
          q3.k:=xDictionary_Entry;
          q3.n:=p1;
          q3.i:=nf;
          q3.v:=0;
          //
          if p2=0 then q0.v:=p3 else q2.n:=p3;
          p0:=p3;
          q0:=q3;
          p1:=0;//end loop
         end;
        else
          raise Exception.CreateFmt('Dictionary entry expected %d:%d',[p1,cardinal(q1.k)]);
        end;
     end;
   end;
  Result:=p0;
end;

function TStratoSphere.GetName(DictionaryEntry:xRef):UTF8String;
var
  nf,j,l,ll:cardinal;
  p:xRef;
begin
  p:=DictionaryEntry;
  if p=0 then Result:='';
  l:=0;
  ll:=0;
  while p<>0 do
   begin
    //get fragment
    if (p=0) or (k[p].k<>xDictionary_Entry) then
      raise Exception.CreateFmt('Dictionary entry expected %d:%d',[p,cardinal(k[p].k)]);
    nf:=k[p].i;
    //proceed to trailing parent reference
    while (p<>0) and (k[p].k<>xDictionary_Tail) do p:=k[p].n;
    if (p=0) or (k[p].k<>xDictionary_Tail) then
      raise Exception.CreateFmt('Dictionary entry expected %d:%d',[p,cardinal(k[p].k)]);
    //determine total length
    if l=0 then
     begin
      ll:=k[p].i+4;
      l:=ll;
      SetLength(Result,l);
     end;
    //unpack fragment
    for j:=0 to 3 do
     begin
      Result[l]:=AnsiChar(byte(nf and $FF));
      dec(l);
      nf:=nf shr 8;
     end;
    //repeat with parent
    p:=k[p].v;
   end;
  while (ll<>0) and (Result[ll]=#0) do dec(ll);
  SetLength(Result,ll);
end;

function TStratoSphere.IsName(Node:xNode;Name:xName):boolean;
begin
  if Node.sphere=nil then
    Result:=false
  else
  if Node.sphere=Self then
    Result:=n(Node.index)=Name
  else
    Result:=Node.sphere.GetName(Node.sphere.n(Node.index))=GetName(Name);//case sensitive?
end;

function TStratoSphere.FQN(Item:xRef):UTF8String;
var
  p:PxKeyValue;
begin
  Result:='';
  while Item<>0 do
   begin
    p:=v(Item,dName);
    if p=nil then
      Result:=SphereIndexPrefix+IntToStr8(Item)+SphereIndexSuffix+'.'+Result
    else
      try
        Result:=GetName(p.v)+'.'+Result;
      except
        Result:='?'+SphereIndexPrefix+IntToStr8(Item)+SphereIndexSuffix+'.'+Result;
      end;
    p:=v(Item,iParent);
    if p=nil then
      Item:=0
    else
      Item:=p.v;
   end;
  if Result<>'' then SetLength(Result,Length(Result)-1);//trailing "."
end;

function TStratoSphere.AddBinaryData(const x:UTF8String):xRef;
var
  l,n:cardinal;
  p:xRef;
  q:PxKeyValue;
begin
  l:=Length(x);
  n:=(l+SizeOf(xKeyValue)-1) div SizeOf(xKeyValue);
  q:=AddRaw(n+1,p);
  q.k:=xBinaryData;
  q.n:=0;
  q.i:=l;//?
  q.v:=0;//?
  //clear trialer
  q:=k[p+n];
  q.k:=xKey(0);
  q.n:=0;
  q.i:=0;
  q.v:=0;
  //copy data
  Move(x[1],k[p+1]^,l);
  Result:=p;
end;

function TStratoSphere.BinaryData(p:xRef):UTF8String;
var
  l:cardinal;
begin
  if (Self=nil) or (k[p].k<>xBinaryData) then
    raise Exception.Create('Key is not xBinaryData ('+IntToStr(p)+')');
  l:=k[p].i;
  SetLength(Result,l);
  Move(k[p+1].k,Result[1],l);
end;

function TStratoSphere.Lookup(Parent:xRef;Name:xName;List:xKey{=lChildren}):xNode;
var
  p:xRef;
  q:PxKeyValue;
  n:xNode;
begin
  Result.none;//default
  if Name<>0 then
   begin
    q:=v(Parent,List);
    if q<>nil then
     begin
      p:=q.v;
      while p<>0 do
       begin
        q:=k[p];
        if q.k<>xListEntry then
          raise Exception.CreateFmt('List entry expected %d:%d',[p,cardinal(q.k)]);
        n.ss(Self,q.i,q.v);
        if IsName(n,Name) then
         begin
          Result:=n;
          p:=0;//end loop
         end
        else
          p:=q.n;//next
       end;
     end;
   end;
end;

function TStratoSphere.Prepend(Parent:xRef;List:xKey;Item:xRef):xRef;
var
  p,q:PxKeyValue;
begin
  p:=AddRaw(1,Result);
  p.k:=xListEntry;
  //p.n:=//see below
  p.i:=0;//local sphere
  p.v:=Item;
  q:=SetKey(Parent,List);
  if q.v=0 then
   begin
    p.n:=0;
    q.i:=Item;//end of list
    q.v:=Item;//one entry
   end
  else
   begin
    if q.k<>xListEntry then
      raise Exception.CreateFmt('List entry expected %d:%d',[Parent,cardinal(q.k)]);
    p.n:=q.v;
    q.v:=Result;
   end;
end;

function TStratoSphere.Append(Parent:xRef;List:xKey;Item:xRef):xRef;
var
  p,q,r:PxKeyValue;
begin
  p:=AddRaw(1,Result);
  p.k:=xListEntry;
  p.n:=0;
  p.i:=0;//local sphere
  p.v:=Item;
  if List=xStageList then
   begin
    q:=k[Parent];
    if (q=nil) or (q.k<>xStageList) then
      raise Exception.CreateFmt('Stage list expected %d:%d',[Parent,cardinal(q.k)]);
   end
  else
    q:=SetKey(Parent,List);
  if q.v=0 then
   begin
    q.i:=Result;//end of list
    q.v:=Result;//one entry
   end
  else
   begin
    r:=k[q.i];
    if r.k<>xListEntry then
      raise Exception.CreateFmt('List entry expected %d:%d',[q.i,cardinal(r.k)]);
    r.n:=Result;//assert was 0
    q.i:=Result;//end of list
   end;
end;

{
function TStratoSphere.ByteSize(p:xRef):cardinal;
var
  q:PxValue;
begin
  //TODO: move to stratoLogic.pas
  if p=0 then
    raise Exception.Create('request for byte size of nothing')//Result:=0
  else
   begin
    case k[p].k of
      nVar,nVarByRef,nVarReadOnly,nThis,nLiteral:
        p:=r(p,iType);
    end;
    while k[p].k=nTypeAlias do
      p:=r(p,iTarget);
    case k[p].k of
      nEnum,nSignature,nPointer,nClass,nInterface,nClassRef:
        Result:=SystemWordSize;
      nType,nRecord,nArray:
       begin
        q:=v(p,vByteSize);
        if q=nil then
          raise Exception.CreateFmt(
            'byte size not declared for item %d:%s',
            [p,KeyToStr(k[p].k)])
        else
          Result:=q^;
       end;
      else
        raise Exception.CreateFmt(
          'request for byte size of unsupported item %d:%s',
          [p,KeyToStr(k[p].k)]);
      //else raise?Sphere.Error?
    end;
   end;
end;
}

{ xNode }

function xNode.Key:xKey;
begin
  if sphere=nil then
    Result:=xUnassigned //assert index=0
  else
    Result:=sphere[index].k;
  //assert Result<n_Max;
end;

function xNode.AsString:string;
var
  i:cardinal;
begin
  if sphere=nil then
    Result:='0#0'
  else
   begin
    i:=0;
    while (i<SpheresCount) and (Spheres[i]<>sphere) do inc(i);
    if i=SpheresCount then
      Result:=Format('?#%d',[index])
    else
      Result:=Format('%d#%d',[i+1,index]);
   end;
end;

function xNode.IsNone:boolean;
begin
  Result:=sphere=nil;//and/or index=0?
end;

function xNode.IsSame(Node:xNode):boolean;
begin
  Result:=(Node.sphere=sphere) and (Node.index=index);
end;

function xNode.i(Key:xKey):xValue;
var
  p:PxKeyValue;
begin
  if sphere=nil then
    Result:=0 //assert index=0 //TODO: null-ref exception?
  else
   begin
    p:=sphere.v(index,Key);
    if p=nil then
      Result:=0//TODO: null-ref exception?
    else
      Result:=p.i;
   end;
end;

function xNode.v(Key:xKey):xValue;
var
  p:PxKeyValue;
begin
  if sphere=nil then
    Result:=0 //assert index=0 //TODO: null-ref exception?
  else
   begin
    p:=sphere.v(index,Key);
    if p=nil then
      Result:=0//TODO: null-ref exception?
    else
      Result:=p.v;
   end;
end;

function xNode.q(Key:xKey;var Value:xValue):boolean;
var
  p:PxKeyValue;
begin
  if sphere=nil then
    Result:=false //assert index=0
  else
   begin
    p:=sphere.v(index,Key);
    if p=nil then
      Result:=false
    else
     begin
      Value:=p.v;
      Result:=true;
     end;
   end;
end;

function xNode.r(Key:xKey):xNode;
var
  p:PxKeyValue;
begin
  if sphere=nil then p:=nil else p:=sphere.v(index,Key);
  if (p=nil) or (p.v=0) then Result.none else Result.ss(Sphere,p.i,p.v);
end;

function xNode.rl(Key:xKey):xNode;
var
  i:xRef;
begin
  //assert Key in [l*]
  i:=index;
  while (i<>0) and (sphere.k[i].k<>Key) do i:=sphere.k[i].n;
  if i=0 then Result.none else
   begin
    Result.sphere:=sphere;
    Result.index:=i;
   end;
end;

procedure xNode.s(ASphere:TStratoSphere;AIndex:xRef);
begin
  //assert sphere<>nil
  sphere:=ASphere;
  index:=AIndex;
end;

procedure xNode.ss(ASphere:TStratoSphere;ASphereIndex:xValue;AIndex:xRef);
var
  i:cardinal;
begin
  if ASphereIndex=0 then
   begin
    //assert sphere<>nil
    sphere:=ASphere;
   end
  else
   begin
    //TODO: check/register dependency
    i:=ASphereIndex-1;
    if i>=SpheresCount then
      raise Exception.Create('Invalid sphere index detected ('+
        IntToStr(ASphereIndex)+')');
    sphere:=Spheres[i];
   end;
  index:=AIndex;
end;

procedure xNode.none;
begin
  sphere:=nil;
  index:=0;
end;

function xNode.Lookup(const Name:UTF8String):xNode;
var
  i,j,l,nf:cardinal;
  p,p1:xRef;
  q:PxKeyValue;
begin
  i:=0;
  l:=Length(Name);
  p1:=0;
  if sphere=nil then q:=nil else q:=sphere.v(0,lSphere_Dictionary);
  while (q<>nil) and (i<l) do
   begin
    //4 bytes name fragment
    nf:=0;
    for j:=0 to 3 do
     begin
      nf:=nf shl 8;
      if i<l then
       begin
        inc(i);
        nf:=nf or byte(Name[i]);
       end;
     end;
    //lookup
    p:=q.v;
    while p<>0 do
     begin
      q:=sphere.k[p];
      case q.k of
      xDictionary_Entry:
        if q.i<nf then
         begin
          //not yet, keep looking
          p:=q.n;
         end
        else
        if q.i=nf then
         begin
          //found!
          p1:=p;
          p:=0;
         end
        else
         begin
          q:=nil;//past already
          p:=0;//end loop
         end;
      xDictionary_Tail:
       begin
        q:=nil;//at end nothing found
        p:=0;//end loop
       end
      else
        raise Exception.CreateFmt('Dictionary entry expected %d:%d',[p,cardinal(q.k)]);
      end;
     end;
   end;
  if (q=nil) or (p1=0) then
    Result.none
  else
    Result:=sphere.Lookup(Index,p1,lChildren);
end;

procedure xNode.Start(Subject:xNode;List:xKey);
var
  i:xRef;
begin
  sphere:=Subject.sphere;
  index:=0;//default
  if sphere<>nil then
   begin
    i:=Subject.index;
    if sphere.k[i].k<n_Max then
     begin
      //TODO: check Field valid member for k[i].k
      i:=sphere.k[i].n;
      while (i<>0) and (sphere.k[i].k<>List) do i:=sphere.k[i].n;
      if i<>0 then index:=i;
     end;
   end;
end;

function xNode.Next(var Item:xNode):boolean;
var
  p:PxKeyValue;
begin
  if sphere=nil then
   begin
    Item.none;
    Result:=false //raise? 'Call Start first'?
   end
  else
   begin
    p:=sphere[index];
    if p.k<>xListEntry then //assert p.k in [l...]
      index:=p.v //first call
    else
      index:=p.n;
    if index=0 then p:=nil else p:=sphere[index];
    if p=nil then
     begin
      Item.none;
      Result:=false;
     end
    else
     begin
      if p.k<>xListEntry then
        raise Exception.CreateFmt('List entry expected %d:%d',[index,cardinal(p.k)]);
      Item.ss(sphere,p.i,p.v);
      Result:=true;
     end;
   end;
end;

function xNode.Done:boolean;
begin
  Result:=index=0;
end;

initialization
  KnownPathsCount:=0;
  KnownPathsSize:=0;
  SpheresCount:=0;
  SpheresSize:=0;
  SystemWordSize:=4;//default
  //ZeroMemory(IntrinsicTypes,?);
finalization
  //TODO: clean-up
end.
