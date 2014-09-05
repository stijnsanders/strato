unit stratoLogic;

interface

{$D-}

uses stratoDecl, stratoSphere;

const
  OffsetUseDefault=cardinal(-1);

function SameType(Sphere:TStratoSphere;s1,s2:TStratoIndex):boolean;
function StratoRecordAddField(Sphere:TStratoSphere;Struct:TStratoIndex;
  const FieldName:UTF8String;FieldType:TStratoIndex;Offset:cardinal):TStratoIndex;
procedure StratoSelectionCheckType(Sphere:TStratoSphere;pp:TStratoIndex);
procedure StratoOperatorCheckType(Sphere:TStratoSphere;pp:TStratoIndex);

procedure MoveChain(Sphere:TStratoSphere;var FirstItem:TStratoIndex;
  MergeOnto:TStratoIndex);

implementation

function SameType(Sphere:TStratoSphere;s1,s2:TStratoIndex):boolean;
var
  ptr1,ptr2:integer;
begin
  //TODO: auto-cast?
  //TODO: stAlias?
  //TODO: stSignature!!
  //TODO: stInterface!!!
  ptr1:=0;
  while (s1<>0) and (Sphere[s1].ThingType=ttPointer) do
   begin
    inc(ptr1);
    s1:=Sphere[s1].EvaluatesTo;
   end;
  ptr2:=0;
  while (s2<>0) and (Sphere[s2].ThingType=ttPointer) do
   begin
    inc(ptr2);
    s2:=Sphere[s2].EvaluatesTo;
   end;
  if (s1=0) or (s2=0) then Result:=false else
    Result:=(s1=s2) and (ptr1=ptr2);
  //TODO: ptr to TypeDecl_number
end;

function StratoRecordAddField(Sphere:TStratoSphere;Struct:TStratoIndex;
  const FieldName:UTF8String;FieldType:TStratoIndex;Offset:cardinal):TStratoIndex;
var
  p,q:PStratoThing;
  s,i:cardinal;
begin
  p:=Sphere[Struct];
  Result:=Sphere.AddTo(p.FirstItem,ttVar,FieldName);
  if Result<>0 then
   begin
    q:=Sphere[Result];
    q.Parent:=Struct;
    q.EvaluatesTo:=FieldType;
    if FieldType=0 then s:=0 else s:=Sphere[FieldType].ByteSize;
    if Offset=OffsetUseDefault then
     begin
      q.Offset:=p.ByteSize;
      inc(p.ByteSize,s);
     end
    else
     begin
      q.Offset:=Offset;
      i:=Offset+s;
      if i>p.ByteSize then p.ByteSize:=i;
     end;
   end;
end;

procedure StratoSelectionCheckType(Sphere:TStratoSphere;pp:TStratoIndex);
var
  p,s1,s2:PStratoThing;
begin
  p:=Sphere[pp];
  if p.DoThen=0 then s1:=nil else s1:=Sphere[p.DoThen];
  if p.DoElse=0 then s2:=nil else s2:=Sphere[p.DoElse];
  if s1=nil then
    if s2=nil then
      p.EvaluatesTo:=0
    else
      p.EvaluatesTo:=s2.EvaluatesTo
  else
    if s2=nil then
      p.EvaluatesTo:=s1.EvaluatesTo
    else
      if s1.EvaluatesTo=s2.EvaluatesTo then
        p.EvaluatesTo:=s1.EvaluatesTo
      else
        if (s1.EvaluatesTo<>0) and (SameType(Sphere,s1.EvaluatesTo,s2.EvaluatesTo)) then
          p.EvaluatesTo:=s2.EvaluatesTo
        else
        if (s2.EvaluatesTo<>0) and (SameType(Sphere,s2.EvaluatesTo,s1.EvaluatesTo)) then
          p.EvaluatesTo:=s1.EvaluatesTo
        else
          ;//TODO: auto-expand on numerics?
end;

procedure StratoOperatorCheckType(Sphere:TStratoSphere;pp:TStratoIndex);
var
  p:PStratoThing;
begin
  p:=Sphere[pp];
  if (p.Left<>0) and (p.Right<>0) then
    if (Sphere[p.Left].EvaluatesTo=Sphere[p.Right].EvaluatesTo) then
      p.EvaluatesTo:=Sphere[p.Left].EvaluatesTo
    else
    if SameType(Sphere,Sphere[p.Right].EvaluatesTo,Sphere[p.Left].EvaluatesTo) then
      p.EvaluatesTo:=Sphere[p.Right].EvaluatesTo;
    //TODO: auto-expand on numerics?
end;

procedure MoveChain(Sphere:TStratoSphere;var FirstItem:TStratoIndex;
  MergeOnto:TStratoIndex);
var
  p,q,r:TStratoIndex;
  n:TStratoName;
begin
  if FirstItem<>0 then
   begin
    q:=Sphere[MergeOnto].FirstItem;
    if q=0 then
      Sphere[MergeOnto].FirstItem:=FirstItem
    else
     begin
      if q<>0 then
        while Sphere[q].Next<>0 do q:=Sphere[q].Next;
      Sphere[q].Next:=FirstItem;
     end;
    p:=FirstItem;
    while p<>0 do
     begin
      Sphere[p].Parent:=MergeOnto;
      n:=Sphere[p].Name;
      r:=Sphere[MergeOnto].FirstItem;
      while (r<>FirstItem) and (Sphere[r].Name<>n) do r:=Sphere[r].Next;
      if r<>FirstItem then Sphere.Error(p,'duplicate identifier');
      p:=Sphere[p].Next;
     end;
   end;
end;

end.
