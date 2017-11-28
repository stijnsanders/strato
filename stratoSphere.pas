unit stratoSphere;

interface

{$D-}
{$L-}

uses stratoDecl, SysUtils;

const
  {$IFDEF DEBUG}
  StratoSphereBlockBase = 1000000;
  {$ELSE}
  StratoSphereBlockBase = $100000;
  {$ENDIF}

  BlocksGrowStep=32;

type
  TStratoSphereBlock=array[0..StratoSphereBlockBase-1] of xValue;
  PStratoSphereBlock=^TStratoSphereBlock;

var
  KnownPaths:array of record
    Key,Path:string;
  end;
  KnownPathsCount,KnownPathsSize:cardinal;

  SourceFiles:array of xSourceFile;
  SourceFilesCount,SourceFilesSize:cardinal;

  Blocks:array of PStratoSphereBlock;
  BlocksCount,BlocksSize:cardinal;

  SystemWordSize:cardinal;
  IntrinsicTypes:array[TStratoIntrinsicType] of xItem;

function SourceFile(x:cardinal):PxSourceFile;

type
  rItem=object
    x:xItem;
    function r(Field:xTypeNr):rItem; //reference
    function v(Field:xTypeNr):xValue; //value
    procedure s(Field:xTypeNr;Item:rItem); overload; //set
    procedure s(Field:xTypeNr;Value:xValue); overload; //set
    function a(Field:xTypeNr;Value:cardinal):cardinal; //add
    function NodeType:xTypeNr;

    //cdadadadr?
    function rr(Field1,Field2:xTypeNr):rItem;
    function rrr(Field1,Field2,Field3:xTypeNr):rItem;
    function rrrr(Field1,Field2,Field3,Field4:xTypeNr):rItem;
  end;

  EStratoFieldIndexNotFound=class(Exception);

function xxr(x:xItem):rItem;
function xxv(p:rItem;f:xTypeNr):xValue;
function xxt(x:rItem):xTypeNr;
function rSrc(p:rItem):cardinal;
function ItemToStr(p:rItem):string;
function BinaryData(p:rItem):UTF8String;

function Lookup(Parent:rItem;Name:xName):rItem;
function GetName(p:xName):UTF8String;
function FQN(p:rItem):UTF8String;

procedure ListFirst(ListOwner:rItem;ListField:xTypeNr;
  var ListItem,ListDelim:rItem);
procedure ListNext(var ListItem:rItem;ListDelim:rItem);
procedure ListNone(var ListItem,ListDelim:rItem);

function NodeTypeToStr(f:xTypeNr):string;
function NodeFieldToStr(f:xTypeNr):string;

implementation

uses Classes;

function SourceFile(x:cardinal):PxSourceFile;
begin
  if x<SourceFilesCount then
    Result:=@SourceFiles[x]
  else
    raise ERangeError.CreateFmt('SourceFile index out of range: %d,%d',
      [x,SourceFilesCount]);
end;

function xxr(x:xItem):rItem;
begin
  Result.x:=x;
end;

//sometimes required to call methods on function returning rItem:
function xxv(p:rItem;f:xTypeNr):xValue;
begin
  Result:=p.v(f);
end;

function xxt(x:rItem):xTypeNr;
begin
  Result:=x.NodeType;
end;

function rSrc(p:rItem):cardinal;
var
  i:cardinal;
begin
  i:=p.x div StratoSphereBlockBase;
  if i<BlocksCount then
    Result:=Blocks[i][0]
  else
    raise ERangeError.CreateFmt('Block index out of range: %d,%d',
      [i,BlocksCount]);
end;

const
  ItemToStrMask='%d#%d';

function ItemToStr(p:rItem):string;
begin
  Result:=Format(ItemToStrMask,
    [p.x div StratoSphereBlockBase
    ,p.x mod StratoSphereBlockBase
    ]);
end;

function BinaryData(p:rItem):UTF8String;
var
  i,j,l:cardinal;
begin
  if p.x=0 then
    Result:=''
  else
   begin
    i:=p.x div StratoSphereBlockBase;
    j:=p.x mod StratoSphereBlockBase;
    if not((i<BlocksCount) and (Blocks[i][j]=n_BinaryData)) then
      raise Exception.Create('BinaryData: node is not binary data '+
        ItemToStr(p));
    l:=Blocks[i][j+1];
    SetLength(Result,l);
    Move(Blocks[i][j+2],Result[1],l);
   end;
end;

function Lookup(Parent:rItem;Name:xName):rItem;
var
  i,j,l,v:cardinal;
  x:UTF8String;
  p,p0,q:rItem;
  qf:xTypeNr;
begin
  i:=rSrc(Parent);
  if i<>rSrc(xxr(Name)) then
   begin
    q.x:=i * StratoSphereBlockBase;
    qf:=lSourceFile_Dictionary;
    x:=GetName(Name);
    i:=0;
    l:=Length(x);
    while i<l do
     begin
      //4 bytes
      v:=0;
      for j:=0 to 3 do
        if i=l then
          v:=v shl 8
        else
         begin
          inc(i);
          v:=(v shl 8) or byte(x[i]);
         end;
      //lookup
      ListFirst(q,qf,p,p0);
      while (p.x<>0) and (p.v(vKey)<v) do
        ListNext(p,p0);
      if (p.x=0) or (p.v(vKey)>v) then
       begin
        q.x:=0;
        i:=l;//not found
       end
      else
       begin
        q:=p;
        qf:=lItems;
       end;
     end;
    Name:=q.x;
   end;
  if Name=0 then
     Result.x:=0//?
  else
   begin
    //ListFirst(Parent,?,p,p0);
    case Parent.NodeType of
      0:
        if Parent.x=0 then //Globals?
          p0.x:=SourceFile(Parent.x div StratoSphereBlockBase).NameSpaces
        else
          p0.x:=0;
      nNameSpace,
      nRecord,nEnum,nClass,nInterface:
        p0:=Parent.r(lItems);
      nCodeBlock:
        p0:=Parent.r(lLocals);
      else
        p0.x:=0;
    end;
    p:=p0.r(iNext);
    while (p.x<>0) and (p.v(iName)<>Name) do
      ListNext(p,p0);
    Result:=p;
   end;
end;

function GetName(p:xName):UTF8String;
var
  v,j:cardinal;
  q:rItem;
begin
  //TODO: build then reverse?
  Result:='';
  q.x:=p;
  while (q.x mod StratoSphereBlockBase)<>0 do
   begin
    v:=q.v(vKey);
    for j:=0 to 3 do
     begin
      if (v and $FF)<>0 then Result:=AnsiChar(v and $FF)+Result;
      v:=v shr 8;
     end;
    q:=q.r(iParent);
   end;
end;

function FQN(p:rItem):UTF8String;
var
  nx:xName;
begin
  Result:='';
  while (p.x mod StratoSphereBlockBase)<>0 do
   begin
    case p.NodeType of
      nCodeBlock,nOverload:;//don't include in name path
      else
       begin
        nx:=p.v(iName);
        if nx=0 then
          Result:=#$AB+ItemToStr(p)+#$BB'.'+Result
        else
          Result:=GetName(nx)+'.'+Result;
       end;
    end;
    p:=p.r(iParent);
   end;
  if Result<>'' then SetLength(Result,Length(Result)-1);//trailing "."
end;


procedure ListFirst(ListOwner:rItem;ListField:xTypeNr;
  var ListItem,ListDelim:rItem);
begin
  ListDelim:=ListOwner.r(ListField);
  ListItem:=ListDelim.r(iNext);
end;

procedure ListNext(var ListItem:rItem;ListDelim:rItem);
begin
  if ListItem.x=ListDelim.x then //did last element?
    ListItem.x:=0 //then we're done
  else
    ListItem:=ListItem.r(iNext); //else advance on the list
end;

procedure ListNone(var ListItem,ListDelim:rItem);
begin
  ListItem.x:=0;
  ListDelim.x:=0;
end;

{ rItem }

function rItem.r(Field: xTypeNr): rItem;
var
  i,j,k,l:cardinal;
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
  i:=x div StratoSphereBlockBase;
  j:=x mod StratoSphereBlockBase;
  if i<BlocksCount then
    if j=0 then //get info from SourceFile?
      if (Field div 100)=3 then
        Result.x:=PStratoSphereBlock(
          @SourceFiles[Blocks[i][0]])[Field mod 100]
      else
        Result.x:=0//raise??? 'null reference'?
    else
      if Field=vTypeNr then
        Result.x:=Blocks[i][j]
      else
       begin
        t:=xTypeNr(Blocks[i][j]);
        {$IFDEF DEBUG}
        if t<n_TypeNr_Low then k:=0 else k:=xTypeDefX[t];
        if k=0 then
          raise EStratoFieldIndexNotFound.CreateFmt(
            ItemToStrMask+'(%s): fields not defined',[i,j,NodeTypeToStr(t)]) at src;
        {$ELSE}
        k:=xTypeDefX[t];
        {$ENDIF}
        l:=k;
        while (xTypeDef[l]<>Field) and (xTypeDef[l]<f_FieldsMax) do inc(l);
        if xTypeDef[l]<f_FieldsMax then
          Result.x:=Blocks[i][j+l-k+1]
        else
          raise EStratoFieldIndexNotFound.CreateFmt(
            ItemToStrMask+'(%s): field index not found: %s',
            [i,j,NodeTypeToStr(t),NodeFieldToStr(Field)])
            {$IFDEF DEBUG}at src{$ENDIF};
       end
  else
    raise ERangeError.CreateFmt('Block index out of range: %d,%d',
      [i,BlocksCount]);
end;

function rItem.v(Field: xTypeNr): xValue;
var
  i,j,k,l:cardinal;
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
  i:=x div StratoSphereBlockBase;
  j:=x mod StratoSphereBlockBase;
  if i<BlocksCount then
    if j=0 then //get info from SourceFile?
      raise EStratoFieldIndexNotFound.CreateFmt(
        'Value %s requested from 0',[NodeFieldToStr(Field)])
    else
      if Field=vTypeNr then //see also function NodeType
        Result:=Blocks[i][j]
      else
       begin
        t:=xTypeNr(Blocks[i][j]);
        {$IFDEF DEBUG}
        if t<n_TypeNr_Low then k:=0 else k:=xTypeDefX[t];
        if k=0 then
          raise EStratoFieldIndexNotFound.CreateFmt(
            ItemToStrMask+'(%s): fields not defined',[i,j,NodeTypeToStr(t)]) at src;
        {$ELSE}
        k:=xTypeDefX[t];
        {$ENDIF}
        l:=k;
        while (xTypeDef[l]<>Field) and (xTypeDef[l]<f_FieldsMax) do inc(l);
        if xTypeDef[l]<f_FieldsMax then
          Result:=Blocks[i][j+l-k+1]
        else
          if Field=iName then //see Lookup(
            Result:=0
          else
            raise EStratoFieldIndexNotFound.CreateFmt(
              ItemToStrMask+'(%s): field index not found: %s',
              [i,j,NodeTypeToStr(t),NodeFieldToStr(Field)])
              {$IFDEF DEBUG}at src{$ENDIF};
       end
  else
    raise ERangeError.CreateFmt('Block index out of range: %d,%d',
      [i,BlocksCount]);
end;

procedure rItem.s(Field: xTypeNr; Item: rItem);
begin
  s(Field,Item.x);
end;

procedure rItem.s(Field: xTypeNr; Value: xValue);
var
  i,j,k,l:cardinal;
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
  i:=x div StratoSphereBlockBase;
  j:=x mod StratoSphereBlockBase;
  if i<BlocksCount then
    if j=0 then //get info from SourceFile?
      if (Field div 100)=3 then
        PStratoSphereBlock(
          @SourceFiles[Blocks[i][0]])[Field mod 100]:=Value
      else
        raise EStratoFieldIndexNotFound.CreateFmt(
          'Value %s update on 0',[NodeFieldToStr(Field)])
    else
      if Field=vTypeNr then
        if (Blocks[i][j] div n_TypeNr_Base)<>(Value div n_TypeNr_Base) then
          raise Exception.CreateFmt(
            'Size mismatch on note type update %s:%s',
            [NodeTypeToStr(Blocks[i][j]),NodeTypeToStr(Value)])
        else
          Blocks[i][j]:=Value
      else
       begin
        t:=xTypeNr(Blocks[i][j]);
        {$IFDEF DEBUG}
        if t<n_TypeNr_Low then k:=0 else k:=xTypeDefX[t];
        if k=0 then
          raise EStratoFieldIndexNotFound.CreateFmt(
            ItemToStrMask+'(%s): fields not defined',[i,j,NodeTypeToStr(t)]) at src;
        {$ELSE}
        k:=xTypeDefX[t];
        {$ENDIF}
        l:=k;
        while (xTypeDef[l]<>Field) and (xTypeDef[l]<f_FieldsMax) do inc(l);
        if xTypeDef[l]<f_FieldsMax then
          Blocks[i][j+l-k+1]:=Value
        else
          raise EStratoFieldIndexNotFound.CreateFmt(
            ItemToStrMask+'(%s): field index not found: %s',
            [i,j,NodeTypeToStr(t),NodeFieldToStr(Field)])
            {$IFDEF DEBUG}at src{$ENDIF};
       end
  else
    raise ERangeError.CreateFmt('Block index out of range: %d,%d',
      [i,BlocksCount]);
end;

function rItem.a(Field: xTypeNr; Value: cardinal): cardinal;
var
  i,j,k,l:cardinal;
  t:xTypeNr;
  p:PxValue;
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
  i:=x div StratoSphereBlockBase;
  j:=x mod StratoSphereBlockBase;
  if i<BlocksCount then
    if j=0 then //get info from SourceFile?
      raise EStratoFieldIndexNotFound.CreateFmt(
        'Value %s increment on 0',[NodeFieldToStr(Field)])
    else
     begin
      t:=xTypeNr(Blocks[i][j]);
      {$IFDEF DEBUG}
      if t<n_TypeNr_Low then k:=0 else k:=xTypeDefX[t];
      if k=0 then
        raise EStratoFieldIndexNotFound.CreateFmt(
          ItemToStrMask+'(%s): fields not defined',[i,j,NodeTypeToStr(t)]) at src;
      {$ELSE}
      k:=xTypeDefX[t];
      {$ENDIF}
      l:=k;
      while (xTypeDef[l]<>Field) and (xTypeDef[l]<f_FieldsMax) do inc(l);
      if xTypeDef[l]<f_FieldsMax then
       begin
        p:=@Blocks[i][j+l-k+1];
        Result:=p^;
        p^:=p^+Value;
       end
      else
        raise EStratoFieldIndexNotFound.CreateFmt(
          ItemToStrMask+'(%s): field index not found: %s',
          [i,j,NodeTypeToStr(t),NodeFieldToStr(Field)])
          {$IFDEF DEBUG}at src{$ENDIF};
     end
  else
    raise ERangeError.CreateFmt('Block index out of range: %d,%d',
      [i,BlocksCount]);
end;

function rItem.NodeType: xTypeNr;
//Result:=Self.v(vTypeNr);
var
  i,j:cardinal;
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
  i:=x div StratoSphereBlockBase;
  j:=x mod StratoSphereBlockBase;
  if i<BlocksCount then
    if j=0 then
      Result:=xTypeNr(0)//raise??? 'null reference'?
    else
      Result:=xTypeNr(Blocks[i][j])
  else
    raise ERangeError.CreateFmt('Block index out of range: %d,%d',
      [i,BlocksCount]);
end;

function rItem.rr(Field1, Field2: xTypeNr): rItem;
var
  p:rItem;
begin
  p:=r(Field1);
  Result:=p.r(Field2);
end;

function rItem.rrr(Field1, Field2, Field3: xTypeNr): rItem;
var
  p:rItem;
begin
  p:=r(Field1);
  p:=p.r(Field2);
  Result:=p.r(Field3);
end;

function rItem.rrrr(Field1, Field2, Field3, Field4: xTypeNr): rItem;
var
  p:rItem;
begin
  p:=r(Field1);
  p:=p.r(Field2);
  p:=p.r(Field3);
  Result:=p.r(Field4);
end;

function NodeTypeToStr(f:xTypeNr):string;
begin
  case f of
    nNameSpace   :Result:='NameSpace';
    nType        :Result:='Type';
    nLiteral     :Result:='Literal';
    nConstant    :Result:='Constant';
    nRecord      :Result:='Record';
    nArray       :Result:='Array';
    nEnum        :Result:='Enum';
    nSignature   :Result:='Signature';
    nSigArg      :Result:='SigArg';
    nSigArgByRef :Result:='SigArgByRef';
    nMember      :Result:='Member';
    nOverload    :Result:='Overload';
    nPointer     :Result:='Pointer';
    nTypeAlias   :Result:='TypeAlias';

    nGlobal      :Result:='Global';

    nClass       :Result:='Class';
    nClassRef    :Result:='ClassRef';
    nCtors       :Result:='Ctors';
    nCtor        :Result:='Ctor';
    nDtor        :Result:='Dtor';
    nPropGet     :Result:='PropGet';
    nPropSet     :Result:='PropSet';
    nInterface   :Result:='Interface';

    nCodeBlock   :Result:='CodeBlock';
    nVar         :Result:='Var';
    nVarByRef    :Result:='VarByRef';
    nVarReadOnly :Result:='VarReadOnly';
    nThis        :Result:='This';

    nSCall       :Result:='SCall';
    nFCall       :Result:='FCall';
    nVCall       :Result:='VCall';
    nICall       :Result:='ICall';
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

    else Result:=Format('%d',[f]);
  end;
end;

function NodeFieldToStr(f:xTypeNr):string;
begin
  case f of
    iName            :Result:='iName';
    iParent          :Result:='iParent';
    iNext            :Result:='iNext';
    iType            :Result:='iType';
    iSubject         :Result:='iSubject';
    iTarget          :Result:='iTarget';
    iSignature       :Result:='iSignature';
    iValue           :Result:='iValue';
    iReturnType      :Result:='iReturnType';
    iInheritsFrom    :Result:='iInheritsFrom';

    iBody            :Result:='iBody';
    iLeft            :Result:='iLeft';
    iRight           :Result:='iRight';
    iFirstArgVar     :Result:='iFirstArgVar';
    iPredicate       :Result:='iPredicate';
    iDoTrue          :Result:='iDoTrue';
    iDoFalse         :Result:='iDoFalse';

    lItems           :Result:='lItems';
    lArguments       :Result:='lArguments';
    lTypes           :Result:='lTypes';
    lLocals          :Result:='lLocals';

    vTypeNr          :Result:='vTypeNr';
    vSrcPos          :Result:='vSrcPos';
    vByteSize        :Result:='vByteSize';
    vOffset          :Result:='vOffset';
    vOperator        :Result:='vOperator';
    vKey             :Result:='vKey';

    else Result:=Format('%.3x',[f]);
  end;
end;

initialization
  KnownPathsCount:=0;
  KnownPathsSize:=0;
  SourceFilesCount:=0;
  SourceFilesSize:=0;
  BlocksCount:=0;
  BlocksSize:=0;
  SystemWordSize:=4;//default
  //ZeroMemory(IntrinsicTypes,?);
finalization
  //TODO: clean-up
end.
