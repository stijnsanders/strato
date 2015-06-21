unit stratoLogic;

interface

uses stratoDecl, stratoSphere;

const
  OffsetUseDefault=cardinal(-1);

function ResType(Sphere:TStratoSphere;p:TStratoIndex):TStratoIndex;
function ByteSize(Sphere:TSTratoSphere;p:TStratoIndex):cardinal;
function SameType(Sphere:TStratoSphere;s1,s2:TStratoIndex):boolean;
function IsAssignable(Sphere:TStratoSphere;p:TStratoIndex):boolean;
function IsAddressable(Sphere:TStratoSphere;p:TStratoIndex):boolean;
procedure StratoSelectionCheckType(Sphere:TStratoSphere;pp:TStratoIndex);
function StratoOperatorCheckType(Sphere:TStratoSphere;pp:TStratoIndex):boolean;

procedure MoveChain(Sphere:TStratoSphere;var FirstItem:TStratoIndex;
  MergeOnto:TStratoIndex);

const
{$IFDEF DEBUG}
  IndexStep1=999999901;
  IndexStep2=999999902;
  IndexStep3=999999903;
  IndexStep4=999999904;
  IndexStep5=999999905;
  IndexStep6=999999906;
  IndexStep7=999999907;
{$ELSE}
  IndexStep1={TStratoIndex}cardinal(-$E);
  IndexStep2={TStratoIndex}cardinal(-$D);
  IndexStep3={TStratoIndex}cardinal(-$C);
  IndexStep4={TStratoIndex}cardinal(-$B);
  IndexStep5={TStratoIndex}cardinal(-$A);
  IndexStep6={TStratoIndex}cardinal(-$9);
  IndexStep7={TStratoIndex}cardinal(-$8);
{$ENDIF}

implementation

uses SysUtils, stratoRunTime, stratoFn;

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
         begin
          if (px.Target<>0) and (Sphere[px.Target].ThingType=ttConstructor)
            and (px.EvaluatesTo<>0) then
           begin
            //constructor called may be of base class when class doesn't
            //have a constructor of its own that matches
            Result:=px.EvaluatesTo;
           end
          else
           begin
            px:=FnSignature(Sphere,px);
            if px=nil then Result:=0 else Result:=px.EvaluatesTo;
           end;
         end;
        //TODO: ttAlias?
        //TODO: ttFunction?
        ttOverload:
          Result:=px.Target;//ttSignature
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
  tt:TStratoThingType;
  x1,x2:PStratoThing;
begin
  //TODO: auto-cast?
  //TODO: ttAlias?
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
   begin
    tt:=Sphere[s1].ThingType;
    if tt=Sphere[s2].ThingType then
      case tt of
        ttClass:
         begin
          while (s1<>0) and (s1<>s2) do s1:=Sphere[s1].InheritsFrom;
          Result:=(s1=s2) and (ptr1=ptr2);
         end;
        ttSignature:
         begin
          x1:=Sphere[s1];
          x2:=Sphere[s2];
          if SameType(Sphere,x1.EvaluatesTo,x2.EvaluatesTo) and
            SameType(Sphere,x1.Target,x2.Target) then
           begin
            s1:=x1.FirstArgument;
            s2:=x2.FirstArgument;
            x1:=Sphere[s1];
            x2:=Sphere[s2];
            while (s1<>0) and (s2<>0) and //assert ttArgument
              SameType(Sphere,x1.EvaluatesTo,x2.EvaluatesTo) do
             begin
              s1:=x1.Next;
              s2:=x2.Next;
              x1:=Sphere[s1];
              x2:=Sphere[s2];
             end;
            Result:=((s1=0) and (s2=0)) and (ptr1=ptr2);
           end
          else
            Result:=false;
         end;
        //ttInterface
        else Result:=false;
      end
    else
      Result:=false;
   end;
end;

function IsAssignable(Sphere:TStratoSphere;p:TStratoIndex):boolean;
var
  px:PStratoThing;
  b:boolean;
begin
  Result:=false;//default
  if p<>0 then
   begin
    px:=Sphere[p];
    b:=true;
    while b do
     begin
      b:=false;
      case px.ThingType of
        ttVar,
        ttCast,//TODO: check about dirty casts
        ttThis:
          Result:=true;
        ttVarIndex:
          if px.Target<>0 then
           begin
            px:=Sphere[px.Target];
            b:=true;
           end;
        ttProperty:
          Result:=px.AssignTo<>0;//has a setter (thus isn't read-only)
        //TODO: more?
      end;
     end;
   end;
end;

function IsAddressable(Sphere:TStratoSphere;p:TStratoIndex):boolean;
var
  px:PStratoThing;
  b:boolean;
begin
  Result:=false;//default
  if p<>0 then
   begin
    px:=Sphere[p];
    b:=true;
    while b do
     begin
      b:=false;
      case px.ThingType of
        ttVar,
        ttThis:
          Result:=true;//TODO: always?
        ttVarIndex:
          if px.Target<>0 then
           begin
            px:=Sphere[px.Target];
            b:=true;
           end;
      end;
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

function StratoOperatorCheckType(Sphere:TStratoSphere;pp:TStratoIndex):boolean;
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
  Result:=p.EvaluatesTo<>0;
end;

procedure MoveChain(Sphere:TStratoSphere;var FirstItem:TStratoIndex;
  MergeOnto:TStratoIndex);
var
  p,q:TStratoIndex;
  n:TStratoName;
begin
  if FirstItem<>0 then
   begin
    p:=FirstItem;
    Sphere.Append(Sphere[MergeOnto].FirstItem,p);
    while p<>0 do
     begin
      Sphere[p].Parent:=MergeOnto;
      n:=Sphere[p].Name;
      q:=Sphere[MergeOnto].FirstItem;
      while (q<>FirstItem) and (Sphere[q].Name<>n) do q:=Sphere[q].Next;
      if q<>FirstItem then Sphere.Error(p,'duplicate identifier');
      p:=Sphere[p].Next;
     end;
   end;
end;

end.
