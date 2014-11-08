unit stratoLogic;

interface

uses stratoDecl, stratoSphere;

const
  OffsetUseDefault=cardinal(-1);

function ResType(Sphere:TStratoSphere;p:TStratoIndex):TStratoIndex;
function ByteSize(Sphere:TSTratoSphere;p:TStratoIndex):cardinal;
function SameType(Sphere:TStratoSphere;s1,s2:TStratoIndex):boolean;
function StratoRecordAddField(Sphere:TStratoSphere;Struct:TStratoIndex;
  const FieldName:UTF8String;FieldType:TStratoIndex;Offset:cardinal):TStratoIndex;
procedure StratoSelectionCheckType(Sphere:TStratoSphere;pp:TStratoIndex);
procedure StratoOperatorCheckType(Sphere:TStratoSphere;pp:TStratoIndex);

procedure MoveChain(Sphere:TStratoSphere;var FirstItem:TStratoIndex;
  MergeOnto:TStratoIndex);

implementation

uses SysUtils, stratoRunTime;

function ResType(Sphere:TStratoSphere;p:TStratoIndex):TStratoIndex;
var
  px:PStratoThing;
begin
  Result:=0;//default
  if p<>0 then
   begin
    px:=Sphere[p];
    if (px.ThingType and tt__Typed)<>0 then
      Result:=px.EvaluatesTo
    else
      case px.ThingType of
        ttFnCall:
          if px.Signature<>0 then
            Result:=Sphere[px.Signature].EvaluatesTo;
        //TODO: ttAlias?
        //TODO: ttFunction: px.Signature?
        ttFunction:
          Result:=px.Signature;//TODO:  stAssignment check
        //else Result:=0;//see default
      end;
   end;
end;

function ByteSize(Sphere:TSTratoSphere;p:TStratoIndex):cardinal;
begin
  //TODO: if? ResType?
  if p=0 then
    raise Exception.Create('request for byte size of nothing')//Result:=0
  else
    case Sphere[p].ThingType of
      ttEnumeration,ttSignature,ttPointer,ttClass,ttInterface:
        Result:=SystemWordSize;
      ttTypeDecl,ttRecord,ttArray:
        Result:=Sphere[p].ByteSize;
      else
        raise Exception.Create('request for byte size of unsupported item '+
          IntToHex(Sphere[p].ThingType,4));
      //else raise?Sphere.Error?
    end;
end;

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
  if (s1=0) or (s2=0) then
    Result:=(s1=0) and (s2=0) and (ptr1=ptr2)
  else
  if s1=s2 then
    Result:=ptr1=ptr2
  else
  if (Sphere[s1].ThingType=ttClass) and (Sphere[s2].ThingType=ttClass) then
   begin
    while (s2<>0) and (s1<>s2) do s2:=Sphere[s2].InheritsFrom;
    Result:=s1=s2;
   end
  else
    Result:=false;
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
    if FieldType=0 then s:=0 else s:=ByteSize(Sphere,FieldType);
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
  p:PStratoThing;
  p1,p2:TStratoIndex;
begin
  p:=Sphere[pp];
  if p.DoThen=0 then p1:=0 else p1:=ResType(Sphere,p.DoThen);
  if p.DoElse=0 then p2:=0 else p2:=ResType(Sphere,p.DoElse);
  if p1=0 then
    p.EvaluatesTo:=p2
  else
    if p2=0 then
      p.EvaluatesTo:=p1
    else
      if p1=p2 then
        p.EvaluatesTo:=p1
      else
        if (p1<>0) and SameType(Sphere,p1,p2) then
          p.EvaluatesTo:=p2
        else
        if (p2<>0) and SameType(Sphere,p2,p1) then //?
          p.EvaluatesTo:=p1
        else
          ;//TODO: auto-expand on numerics?
end;

procedure StratoOperatorCheckType(Sphere:TStratoSphere;pp:TStratoIndex);
var
  p:PStratoThing;
  p1,p2:TStratoIndex;
begin
  p:=Sphere[pp];
  if (p.Left<>0) and (p.Right<>0) then
   begin
    p1:=ResType(Sphere,p.Left);
    p2:=ResType(Sphere,p.Right);
    if p1=p2 then
      p.EvaluatesTo:=p1
    else
    if SameType(Sphere,p1,p2) then
      p.EvaluatesTo:=p2
    else
    if SameType(Sphere,p2,p1) then //?
      p.EvaluatesTo:=p1
    //TODO: auto-expand on numerics?
   end;
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
