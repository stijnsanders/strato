unit stratoLogic;

interface

uses stratoDecl, stratoSphere;

const
  OffsetUseDefault=cardinal(-1);

function ResType(Sphere:TStratoSphere;p:TStratoIndex):TStratoIndex;
function ByteSize(Sphere:TSTratoSphere;p:TStratoIndex):cardinal;
function SameType(Sphere:TStratoSphere;s1,s2:TStratoIndex):boolean;
function IsAddressable(Sphere:TStratoSphere;p:TStratoIndex):boolean;
procedure StratoSelectionCheckType(Sphere:TStratoSphere;p:TStratoIndex);
function StratoOperatorCheckType(Sphere:TStratoSphere;p:TStratoIndex):boolean;
function StratoComparativeCheckType(Sphere:TStratoSphere;p:TStratoIndex):boolean;

procedure MoveChain(Sphere:TStratoSphere;Parent,MergeOnto:TStratoIndex);
procedure ReplaceNode(Sphere:TStratoSphere;Parent,Subject,ReplaceWith:TStratoIndex);

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

uses SysUtils, stratoRunTime, stratoFn, stratoTokenizer;

function ResType(Sphere:TStratoSphere;p:TStratoIndex):TStratoIndex;
var
  q:TStratoIndex;
begin
  Result:=0;//default
  if p<>0 then
   begin
    if (Sphere.t(p) and tt__Typed)<>0 then
      Result:=Sphere.r(p,tfEvaluatesTo)
    else
      case Sphere.t(p) of
        ttFnCall:
          Result:=Sphere.r(p,tfEvaluatesTo);
        //TODO: ttAlias?
        ttClass:
          Result:=Sphere.Add(ttClassRef,
            [tfParent,Sphere.r(p,tfParent)
            ,tfByteSize,SystemWordSize
            ,tfEvaluatesTo,p
            ]);
        //TODO: ttMember:
        ttOverload,ttConstructor:
          Result:=Sphere.r(p,tfSignature);//ttSignature
        ttPropertyGet:
          Result:=Sphere.r(Sphere.r(p,tfSignature),tfEvaluatesTo);//type from ttSignature
        ttPropCall:
          if Sphere.r(p,tfTarget,q) then
           begin
            if Sphere.t(q)=ttField then q:=Sphere.r(q,tfTarget);
            Result:=Sphere.r(Sphere.r(q,tfSignature),tfEvaluatesTo);//type from ttSignature
           end;
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
    case Sphere.t(p) of
      ttEnumeration,ttSignature,ttPointer,
      ttClass,ttInterface,ttClassRef:
        Result:=SystemWordSize;
      ttTypeDecl,ttRecord,ttArray:
        Result:=Sphere.v(p,tfByteSize);
      else
        raise Exception.CreateFmt(
          'request for byte size of unsupported item %d:%.4x',
          [p,Sphere.t(p)]);
      //else raise?Sphere.Error?
    end;
end;

function SameType(Sphere:TStratoSphere;s1,s2:TStratoIndex):boolean;
var
  ptr1,ptr2:integer;
  tt:TStratoThingType;
begin
  //TODO: auto-cast?
  //TODO: ttAlias?
  ptr1:=0;
  while Sphere.t(s1)=ttPointer do
   begin
    inc(ptr1);
    s1:=Sphere.r(s1,tfEvaluatesTo);
   end;
  ptr2:=0;
  while Sphere.t(s2)=ttPointer do
   begin
    inc(ptr2);
    s2:=Sphere.r(s2,tfEvaluatesTo);
   end;
  if (s1=0) or (s2=0) then
    Result:=(s1=0) and (s2=0) and (ptr1=ptr2)
  else
  if s1=s2 then
    Result:=ptr1=ptr2
  else
   begin
    tt:=Sphere.t(s1);
    if tt=Sphere.t(s2) then
      case tt of
        ttClass:
         begin
          while (s1<>0) and (s1<>s2) do s1:=Sphere.r(s1,tfInheritsFrom);
          Result:=(s1=s2) and (ptr1=ptr2);
         end;
        ttClassRef:
         begin
          s1:=Sphere.r(s1,tfEvaluatesTo);
          s2:=Sphere.r(s2,tfEvaluatesTo);
          while (s1<>0) and (s1<>s2) do s1:=Sphere.r(s1,tfInheritsFrom);
          Result:=(s1=s2) and (ptr1=ptr2);
         end;
        ttSignature:
         begin
          //TODO: local stack array
          if SameType(Sphere,Sphere.r(s1,tfEvaluatesTo),Sphere.r(s2,tfEvaluatesTo)) and
            SameType(Sphere,Sphere.r(s1,tfTarget),Sphere.r(s2,tfTarget)) then
           begin
            s1:=Sphere.r(s1,tfFirstArgument);
            s2:=Sphere.r(s2,tfFirstArgument);
            while (s1<>0) and (s2<>0) and //assert ttArgument
              SameType(Sphere,Sphere.r(s1,tfEvaluatesTo),Sphere.r(s2,tfEvaluatesTo)) do
             begin
              s1:=Sphere.r(s1,tfNext);
              s2:=Sphere.r(s2,tfNext);
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
    //if (tt=ttClassRef) and (s2=TypeDecl_type) then
    if (tt=ttTypeDecl) and (s1=TypeDecl_type) and
      (Sphere.t(s2)=ttClassRef) then
      Result:=true
    else
      Result:=false;
   end;
end;

function IsAddressable(Sphere:TStratoSphere;p:TStratoIndex):boolean;
var
  b:boolean;
begin
  Result:=false;//default
  b:=true;
  while b do
   begin
    b:=false;
    case Sphere.t(p) of
      ttVar,ttThis:
        Result:=true;//TODO: always? (not read-only?)
      ttArrayIndex,ttField:
        b:=Sphere.r(p,tfTarget,p);
    end;
   end;
end;

procedure StratoSelectionCheckType(Sphere:TStratoSphere;p:TStratoIndex);
var
  q,p1,p2:TStratoIndex;
begin
  if Sphere.r(p,tfDoThen,q) then p1:=ResType(Sphere,q) else p1:=0;
  if Sphere.r(p,tfDoElse,q) then p2:=ResType(Sphere,q) else p2:=0;
  if p1=0 then
    Sphere.s(p,tfEvaluatesTo,p2)
  else
    if p2=0 then
      Sphere.s(p,tfEvaluatesTo,p1)
    else
      if p1=p2 then
        Sphere.s(p,tfEvaluatesTo,p1)
      else
        if (p1<>0) and SameType(Sphere,p1,p2) then
          Sphere.s(p,tfEvaluatesTo,p2)
        else
        if (p2<>0) and SameType(Sphere,p2,p1) then //?
          Sphere.s(p,tfEvaluatesTo,p1)
        else
          ;//TODO: auto-expand on numerics?
end;

function StratoOperatorCheckType(Sphere:TStratoSphere;p:TStratoIndex):boolean;
var
  p1,p2:TStratoIndex;
begin
  if Sphere.r(p,tfLeft,p1) and Sphere.r(p,tfRight,p2) then
   begin
    p1:=ResType(Sphere,p1);
    p2:=ResType(Sphere,p2);
    if p1=p2 then
      Sphere.s(p,tfEvaluatesTo,p1)
    else
    if SameType(Sphere,p1,p2) then
      Sphere.s(p,tfEvaluatesTo,p2)
    else
    if SameType(Sphere,p2,p1) then //?
      Sphere.s(p,tfEvaluatesTo,p1)
    //TODO: auto-expand on numerics?
   end;
  Result:=Sphere.r(p,tfEvaluatesTo)<>0;
end;

function StratoComparativeCheckType(Sphere:TStratoSphere;p:TStratoIndex):boolean;
var
  p1,p2:TStratoIndex;
begin
  Result:=false;//default
  if Sphere.r(p,tfLeft,p1) and Sphere.r(p,tfRight,p2) then
   begin
    p1:=ResType(Sphere,p1);
    p2:=ResType(Sphere,p2);
    if TStratoToken(Sphere.v(p,tfOperator))=stOpTypeIs then
      Result:=(p1<>0) and (p2<>0)
        and ((Sphere.t(p1) and tt__IsType)<>0)
        and ((Sphere.t(p2) and tt__IsType)<>0)
    else
      Result:=(p1=p2)
        or (SameType(Sphere,p1,p2))
        or (SameType(Sphere,p2,p1));
    //TODO: if p1=TypeDecl_bool then support 'x<y<z'
   end;
end;

procedure MoveChain(Sphere:TStratoSphere;Parent,MergeOnto:TStratoIndex);
var
  p,p1,q:TStratoIndex;
  n:TStratoName;
begin
  p1:=Sphere.r(Parent,tfFirstItem);
  if p1<>0 then
   begin
    Sphere.Append(MergeOnto,tfFirstItem,p1);
    p:=p1;
    while p<>0 do
     begin
      Sphere.s(p,tfParent,MergeOnto);
      n:=Sphere.v(p,tfName);
      q:=Sphere.r(MergeOnto,tfFirstItem);
      while (q<>p1) and (Sphere.v(q,tfName)<>n) do q:=Sphere.r(q,tfNext);
      if q<>p1 then Sphere.Error(p,'duplicate identifier');
      p:=Sphere.r(p,tfNext);
     end;
   end;
end;

procedure ReplaceNode(Sphere:TStratoSphere;Parent,Subject,ReplaceWith:TStratoIndex);
var
  p,q:TStratoIndex;
begin
  //assert Subject<>0
  //assert ReplaceWith not pointed to
  //assert ReplaceWith.Parent=Subject.Parent
  {$IFDEF DEBUG}
  if Sphere.r(ReplaceWith,tfNext)<>0 then
    raise Exception.Create('broken chain detected');
  {$ENDIF}
  p:=Sphere.r(Parent,tfFirstItem);
  if p=0 then
    Sphere.s(Parent,tfFirstItem,ReplaceWith)
  else
   begin
    q:=0;
    while (p<>0) and (p<>Subject) do
     begin
      q:=p;
      p:=Sphere.r(p,tfNext);
     end;
    if p=0 then
      raise Exception.CreateFmt(
        'ReplaceNode called with subject %d not on chain %d',
        [Subject,Parent])
    else
     begin
      Sphere.s(ReplaceWith,tfNext,Sphere.r(Subject,tfNext));
      Sphere.s(Subject,tfNext,0);//?
      if q=0 then
        Sphere.s(Parent,tfFirstItem,ReplaceWith) //assert FirstItem was Subject
      else
        Sphere.s(q,tfNext,ReplaceWith);
     end;
   end;
end;

end.
