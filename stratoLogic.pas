unit stratoLogic;

interface

{xx$D-}
{xx$L-}

uses stratoDecl, stratoSphere;

function ByteSize(p:xNode):cardinal;
function SameNode(p1,p2:xNode):boolean;
function SameType(p1,p2:xNode):boolean;
function IsAddressable(p:xNode):boolean;

implementation

uses SysUtils;

function ByteSize(p:xNode):cardinal;
begin
  if p.IsNone then
    raise Exception.Create('request for byte size of nothing')//Result:=0
  else
   begin
    case p.Key of
      nVar,nVarByRef,nVarReadOnly,nThis:
        p:=p.r(iType);
    end;
    while p.Key=nTypeAlias do
      p:=p.r(iTarget);
    case p.Key of
      nEnum,nSignature,nPointer,nClass,nInterface,nClassRef:
        Result:=SystemWordSize;
      nType,nRecord,nArray:
        Result:=p.v(vByteSize);
      else
        raise Exception.CreateFmt(
          'request for byte size of unsupported item %s:%s',
          [p.AsString,KeyToStr(p.Key)]);
      //else raise?Sphere.Error?
    end;
   end;
end;

function SameNode(p1,p2:xNode):boolean;
begin
  Result:=(p1.sphere=p2.sphere) and (p1.index=p2.index);
  //TODO: nImport?
end;

function SameType(p1,p2:xNode):boolean;
var
  ptr1,ptr2:integer;
  k:xKey;
  q1,q2,r1,r2:xNode;
  b:boolean;
begin
  //TODO: resolve here?

  //pointer count 1
  ptr1:=0;
  b:=true;
  while b do
    case p1.Key of
      nTypeAlias:
        p1:=p1.r(iTarget);
      nPointer:
       begin
        inc(ptr1);
        p1:=p1.r(iTarget);
       end;
      else
        b:=false;
    end;
  //pointer count 2
  ptr2:=0;
  b:=true;
  while b do
    case p2.Key of
      nTypeAlias:
        p2:=p2.r(iTarget);
      nPointer:
       begin
        inc(ptr2);
        p2:=p2.r(iTarget);
       end;
      else
        b:=false;
    end;
  //equal already?
  if p1.IsNone or p2.IsNone then
    //Result:=p1.IsNone and p2.IsNone and (ptr1=ptr2)
    Result:=false//error?!
  else
  if SameNode(p1,p2) then
    Result:=ptr1=ptr2
  else
   begin
    k:=p1.Key;
    if k=p2.Key then
      case k of
        nClass:
         begin
          while not(p1.IsNone) and not(SameNode(p1,p2)) do
            p1:=p1.r(iInheritsFrom);
          Result:=SameNode(p1,p2) and (ptr1=ptr2);
         end;
        nClassRef:
         begin
          p1:=p1.r(iTarget);
          p2:=p2.r(iTarget);
          while not(p1.IsNone) and not(SameNode(p1,p2)) do
            p1:=p1.r(iInheritsFrom);
          Result:=SameNode(p1,p2) and (ptr1=ptr2);
         end;
        nSignature:
         begin
          //TODO: local stack array
          if (ptr1=ptr2) and
            SameType(p1.r(iReturnType),p2.r(iReturnType)) and
            SameType(p1.r(iSubject),   p2.r(iSubject)) then
           begin
            r1.Start(p1,lArguments);
            r2.Start(p2,lArguments);
            Result:=true;
            while Result and r1.Next(q1) and r2.Next(q2) do
              Result:=SameType(q1,q2);
            Result:=Result and r1.Done and r2.Done;
           end
          else
            Result:=false;
         end;
        //nInterface //TODO exact same members?
        else Result:=false;
      end
    else
    if (k=nType)
      and SameNode(p1,IntrinsicTypes[itType])
      and (p2.Key=nClassRef)
      then
      Result:=true
    else
      Result:=false;
   end;
end;

function IsAddressable(p:xNode):boolean;
begin
  Result:=false;//default
  while not p.IsNone do
    case p.Key of
      nVar,nVarByRef,nThis://nVarReadOnly?
       begin
        Result:=true;//TODO: always? (not read-only?)
        p.none;//end loop
       end;
      nArrayIndex:
        p:=p.r(iSubject);
      nField:
        p:=p.r(iTarget);
      else
        p.none;//end loop
    end;
end;

end.
