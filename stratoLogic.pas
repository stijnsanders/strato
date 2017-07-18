unit stratoLogic;

interface

uses stratoDecl, stratoSphere;

const
  OffsetUseDefault=cardinal(-1);

function ResType(Sphere:TStratoSphere;p:xItem):xItem;
function ByteSize(Sphere:TStratoSphere;p:xItem):cardinal;
function SameType(Sphere:TStratoSphere;s1,s2:xItem):boolean;
function IsAddressable(Sphere:TStratoSphere;p:xItem):boolean;
procedure StratoSelectionCheckType(Sphere:TStratoSphere;p:xItem);
function StratoOperatorCheckType(Sphere:TStratoSphere;p:xItem):boolean;
function StratoComparativeCheckType(Sphere:TStratoSphere;p:xItem):boolean;
function StratoAssignmentCheckType(Sphere:TStratoSphere;p,q:xItem):cardinal;

procedure MoveChain(Sphere:TStratoSphere;Parent,MergeOnto:xItem);
procedure ReplaceNode(Sphere:TStratoSphere;Parent,Subject,ReplaceWith:xItem);

implementation

uses SysUtils, stratoRunTime, stratoFn, stratoTokenizer;

function ResType(Sphere:TStratoSphere;p:xItem):xItem;
var
  q:xItem;
begin
  Result:=0;//default
  if p<>0 then
   begin
    while Sphere.n(p,vTypeNr)^=nField do p:=Sphere.n(p,fTarget)^;//?
    case Sphere.n(p,vTypeNr)^ of
      nArgument,nVarDecl,nThis,nLiteral,nConstant: //nGlobal?
        Result:=Sphere.n(p,fTypeDecl)^;

      nTypeDecl,nSignature,nArray,nRecord,nEnumeration,
      nPointer,nClassRef,nInterface,nTypeAlias:
        Result:=TypeDecl_type;

      nClass:
        Result:=Sphere.Add(nClassRef,0,0,[fSubject,p]);
      nOverload,nConstructor:
        Result:=Sphere.n(p,fSignature)^;
      nPropertyGet:
        Result:=Sphere.n(Sphere.n(p,fSignature)^,fReturnType)^;
      //nMember: find overload without arguments? //TODO

      nUnaryOp,nBinaryOp,nAddressOf,nDereference:
        Result:=Sphere.n(p,fReturnType)^;
      nCast:
        Result:=Sphere.n(p,fTypeDecl)^;
      nArrayIndex:
       begin
        q:=ResType(Sphere,Sphere.n(p,fSubject)^);
        if Sphere.n(q,vTypeNr)^=nArray then
          Result:=Sphere.n(q,fTypeDecl)^//element type
        else
          raise Exception.Create('ArrayIndex: Unexpected subject: '+
            xDisplay(Sphere,q));
       end;

      nFCall,nSCall,nVCall:
       begin
        q:=Sphere.n(p,fTarget)^;
        while q<>0 do
          case Sphere.n(q,vTypeNr)^ of
            nField:
              q:=Sphere.n(q,fTarget)^;
            nOverload,nPropertyGet,nConstructor:
             begin
              Result:=Sphere.n(Sphere.n(q,fSignature)^,fReturnType)^;
              q:=0;
             end;
            nArrayIndex:
             begin
              q:=ResType(Sphere,Sphere.n(q,fSubject)^);
              if Sphere.n(q,vTypeNr)^=nArray then
                q:=Sphere.n(q,fTypeDecl)^
              else
                raise Exception.Create('ArrayIndex: Unexpected subject: '+xDisplay(Sphere,q));
             end;
            nVarDecl:
              q:=Sphere.n(q,fTypeDecl)^;
            nClassRef:
             begin
              //assert constructor searched at run-time
              Result:=Sphere.n(q,fSubject)^;
              q:=0;
             end;
            //more?

            else
              raise Exception.Create('Call: Unexpected target: '+xDisplay(Sphere,q));
          end;
       end;
      nSelection,nCodeBlock:
        Result:=Sphere.n(p,fReturnType)^;

      //else?
    end;
   end;
end;

function ByteSize(Sphere:TStratoSphere;p:xItem):cardinal;
begin
  if p=0 then
    raise Exception.Create('request for byte size of nothing')//Result:=0
  else
   begin
    if Sphere.n(p,vTypeNr)^=nVarDecl then
      p:=Sphere.n(p,fTypeDecl)^;
    case Sphere.n(p,vTypeNr)^ of
      nEnumeration,nSignature,nPointer,nClass,nInterface,nClassRef:
        Result:=SystemWordSize;
      nTypeDecl,nRecord,nArray:
        Result:=Sphere.n(p,vByteSize)^;
      else
        raise Exception.CreateFmt(
          'request for byte size of unsupported item $%.8x:%.7x',
          [cardinal(p),cardinal(Sphere.n(p,vTypeNr)^)]);
      //else raise?Sphere.Error?
    end;
   end;
end;

function SameType(Sphere:TStratoSphere;s1,s2:xItem):boolean;
var
  ptr1,ptr2:integer;
  nn:xTypeNr;
  r1,r2:xItem;
begin
  //TODO: auto-cast?
  //TODO: nAlias?
  ptr1:=0;
  while Sphere.n(s1,vTypeNr)^=nPointer do
   begin
    inc(ptr1);
    s1:=Sphere.n(s1,fSubject)^;
   end;
  ptr2:=0;
  while Sphere.n(s2,vTypeNr)^=nPointer do
   begin
    inc(ptr2);
    s2:=Sphere.n(s2,fSubject)^;
   end;
  if (s1=0) or (s2=0) then
    Result:=(s1=0) and (s2=0) and (ptr1=ptr2)
  else
  if s1=s2 then
    Result:=ptr1=ptr2
  else
   begin
    nn:=Sphere.n(s1,vTypeNr)^;
    if nn=Sphere.n(s2,vTypeNr)^ then
      case nn of
        nClass:
         begin
          while (s1<>0) and (s1<>s2) do s1:=Sphere.n(s1,fInheritsFrom)^;
          Result:=(s1=s2) and (ptr1=ptr2);
         end;
        nClassRef:
         begin
          s1:=Sphere.n(s1,fSubject)^;
          s2:=Sphere.n(s2,fSubject)^;
          while (s1<>0) and (s1<>s2) do s1:=Sphere.n(s1,fInheritsFrom)^;
          Result:=(s1=s2) and (ptr1=ptr2);
         end;
        nSignature:
         begin
          //TODO: local stack array
          if (ptr1=ptr2) and
            SameType(Sphere,Sphere.n(s1,fReturnType)^,Sphere.n(s2,fReturnType)^) and
            SameType(Sphere,Sphere.n(s1,fSubject)^,Sphere.n(s2,fSubject)^) then
           begin
            Sphere.First(s1,fArguments,s1,r1);
            Sphere.First(s2,fArguments,s2,r2);
            while (s1<>0) and (s2<>0) and
              SameType(Sphere,Sphere.n(s1,fTypeDecl)^,Sphere.n(s2,fTypeDecl)^) do
             begin
              Sphere.Next(s1,r1);
              Sphere.Next(s2,r2);
             end;
            Result:=(s1=0) and (s2=0);
           end
          else
            Result:=false;
         end;
        //nInterface //TODO exact same members?
        else Result:=false;
      end
    else
    //if (nn=nClassRef) and (s2=TypeDecl_type) then
    if (nn=nTypeDecl) and (s1=TypeDecl_type) and (Sphere.n(s2,vTypeNr)^=nClassRef) then
      Result:=true
    else
      Result:=false;
   end;
end;

function IsAddressable(Sphere:TStratoSphere;p:xItem):boolean;
begin
  Result:=false;//default
  while p<>0 do
    case Sphere.n(p,vTypeNr)^ of
      nVarDecl,nThis:
       begin
        Result:=true;//TODO: always? (not read-only?)
        p:=0;//end loop
       end;
      nArrayIndex:
        p:=Sphere.n(p,fSubject)^;
      nField:
        p:=Sphere.n(p,fTarget)^;
      else
        p:=0;//end loop
    end;
end;

procedure StratoSelectionCheckType(Sphere:TStratoSphere;p:xItem);
var
  p1,p2:xItem;
  q:PxValue;
begin
  q:=Sphere.n(p,fDoThen);
  if q^=0 then p1:=0 else p1:=ResType(Sphere,q^);
  q:=Sphere.n(p,fDoElse);
  if q^=0 then p2:=0 else p2:=ResType(Sphere,q^);
  {
  if p1=0 then
    Sphere.n(p,fReturnType)^:=p2
  else
    if p2=0 then
      Sphere.n(p,fReturnType) ^:=p1
    else
  }
      if p1=p2 then
        Sphere.n(p,fReturnType)^:=p1
      else
        if (p1<>0) and SameType(Sphere,p1,p2) then
          Sphere.n(p,fReturnType)^:=p2
        else
        if (p2<>0) and SameType(Sphere,p2,p1) then //?
          Sphere.n(p,fReturnType)^:=p1
        else
          ;//TODO: auto-expand on numerics?
end;

function StratoOperatorCheckType(Sphere:TStratoSphere;p:xItem):boolean;
var
  p1,p2:xItem;
  q,r:PxValue;
begin
  q:=Sphere.n(p,fLeft);
  if q^=0 then p1:=0 else p1:=ResType(Sphere,q^);
  q:=Sphere.n(p,fRight);
  if q^=0 then p2:=0 else p2:=ResType(Sphere,q^);
  r:=Sphere.n(p,fReturnType);
  if (p1<>0) and (p2<>0) then
   begin
    if p1=p2 then
      r^:=p1
    else
    if SameType(Sphere,p1,p2) then
      r^:=p2
    else
    if SameType(Sphere,p2,p1) then //?
      r^:=p1
    else
      ;//TODO: auto-expand on numerics?
   end;
  Result:=r^<>0;
end;

function StratoComparativeCheckType(Sphere:TStratoSphere;p:xItem):boolean;
var
  p1,p2:xItem;
  q:PxValue;
begin
  Result:=false;//default
  q:=Sphere.n(p,fLeft);
  if q^=0 then p1:=0 else p1:=ResType(Sphere,q^);
  q:=Sphere.n(p,fRight);
  if q^=0 then p2:=0 else p2:=ResType(Sphere,q^);
  if (p1<>0) and (p2<>0) then
   begin
    if TStratoToken(Sphere.n(p,vOperator)^)=stOpTypeIs then
      Result:=true
    else
      Result:=(p1=p2)
        or (SameType(Sphere,p1,p2))
        or (SameType(Sphere,p2,p1))//?
    ;
    //TODO: if p1=TypeDecl_bool then support 'x<y<z'
   end;
end;

function StratoAssignmentCheckType(Sphere:TStratoSphere;p,q:xItem):cardinal;
var
  r:xItem;
begin
  //Result:=0;//default
  if Sphere.n(p,vTypeNr)^=nCast then //allow dirty cast
   begin
    if IsAddressable(Sphere,Sphere.n(p,fSubject)^) then
     begin
      r:=Sphere.n(p,fTypeDecl)^;
      //TODO: if nArray
      if SameType(Sphere,q,r) then
        Result:=0
      else
        Result:=2;
     end
    else
      Result:=1;
    //TODO: check ByteSize's equal?
    //TODO: switch to enable pointer arith (default off!)
   end
  else
   begin
    if IsAddressable(Sphere,p) then
     begin
      //TODO: merge IsAddressable, ResType and ArrayIndex (check indexes)
      r:=ResType(Sphere,p);
      //while Sphere.n(r,vTypeNr)^=nArray do r:=Sphere.n(r,fTypeDecl)^;
      if SameType(Sphere,q,r) then
        Result:=0
      else
        Result:=2;
     end
    else
      Result:=1;
    //TODO: local lookup member ":=" (use TStratoToken(p2)!)
   end;
end;

procedure MoveChain(Sphere:TStratoSphere;Parent,MergeOnto:xItem);
var
  p0,p1,q0,q1:PxValue;
  r,r0,s,s0:xItem;
  nn:xName;
begin
  //assert Sphere.n(Parent,vTypeNr)^=nCodeBlock
  //assert Sphere.n(MergeOnto,vTypeNr)^=nCodeBlock
  //check duplicates
  Sphere.First(Parent,fVarDecls,r,r0);
  while r<>0 do
   begin
    nn:=Sphere.n(r,vName)^;//TODO: check xNamed?
    Sphere.First(MergeOnto,fVarDecls,s,s0);
    while (s<>0) and (Sphere.n(s,vName)^<>nn) do Sphere.Next(s,s0);
    if s=0 then
      Sphere.Next(r,r0)
    else
     begin
      Sphere.Error(r,'duplicate identifier');
      Exit;
     end;
   end;
  //do merge
  p0:=Sphere.n(Parent,fVarDecls);
  if p0^<>0 then
   begin
    p1:=Sphere.n(p0^,fNext);
    q0:=Sphere.n(MergeOnto,fVarDecls);
    if q0^=0 then
      q0^:=p0^ //? assert p1^=p0^
    else
     begin
      r:=p1^;
      q1:=Sphere.n(q0^,fNext);
      q0^:=p0^;
      p1^:=q1^;
      q1^:=r;
     end;
   end;
  //fix locals size
  q0:=Sphere.n(Parent,vByteSize);
  q1:=Sphere.n(MergeOnto,vByteSize);
  inc(q1^,q0^);
  q0^:=0;
end;

procedure ReplaceNode(Sphere:TStratoSphere;Parent,Subject,ReplaceWith:xItem);
var
  p,p0,q:xItem;
  r:PxValue;
begin
  //assert Subject<>nil
  //assert ReplaceWith not pointed to
  //assert ReplaceWith.Parent=Subject.Parent
  {$IFDEF DEBUG}
  if Sphere.n(ReplaceWith,fNext)^<>0 then
    raise Exception.Create('broken chain detected');
  {$ENDIF}
  Sphere.First(Parent,fItems,p,p0);
  if p=0 then
   begin
    Sphere.n(Parent,fItems)^:=ReplaceWith;
    Sphere.n(ReplaceWith,fNext)^:=ReplaceWith;
   end
  else
   begin
    q:=0;
    while (p<>0) and (p<>Subject) do
     begin
      q:=p;
      Sphere.Next(p,p0);
     end;
    if p=0 then
      raise Exception.CreateFmt(
        'ReplaceNode called with subject $%.8x not on chain $%.8x',
        [cardinal(Subject),cardinal(Parent)])
    else
     begin
      r:=Sphere.n(Subject,fNext);
      Sphere.n(ReplaceWith,fNext)^:=r^;
      r^:=0;
      if q=0 then q:=p0;
      Sphere.n(q,fNext)^:=ReplaceWith;
      if Subject=p0 then Sphere.n(Parent,fItems)^:=ReplaceWith;
     end;
   end;
end;

end.
