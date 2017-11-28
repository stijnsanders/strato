unit stratoLogic;

interface

{$D-}
{$L-}

uses stratoDecl, stratoSphere;

procedure RunError(p: rItem; const Msg: string);

function ByteSize(p: rItem): cardinal;
function SameType(p1, p2: rItem): boolean;
function IsAddressable(p: rItem): boolean;

implementation

uses SysUtils;

procedure RunError(p: rItem; const Msg: string);
var
  s:PxSourceFile;
  i:cardinal;
  SrcPos:xSrcPos;
begin
  //s:=SourceFile(p)
  i:=p.x div StratoSphereBlockBase;
  if i<SourceFilesCount then s:=SourceFile(i) else s:=nil;
  if (s<>nil) or (s.FileName=0) then
    Writeln(ErrOutput,Msg)
  else
   begin
    SrcPos:=p.v(vSrcPos);
    i:=s.SrcPosLineIndex;
    if i=0 then i:=1;
    Writeln(ErrOutput,Format('%s(%d:%d): %s',
      [BinaryData(xxr(s.FileName))
      ,SrcPos div i
      ,SrcPos mod i
      ,Msg
      ]));
   end;
  ExitCode:=1;
end;

function ByteSize(p: rItem): cardinal;
  {$IFDEF DEBUG}
var
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
  if p.x=0 then
    raise Exception.Create('request for byte size of nothing')//Result:=0
    {$IFDEF DEBUG}at src{$ENDIF}
  else
   begin
    case p.NodeType of
      nVar,nVarByRef,nVarReadOnly,nThis:
        p:=p.r(iType);
    end;
    while p.NodeType=nTypeAlias do
      p:=p.r(iTarget);
    case p.NodeType of
      nEnum,nSignature,nPointer,nClass,nInterface,nClassRef:
        Result:=SystemWordSize;
      nType,nRecord,nArray:
        Result:=p.v(vByteSize);
      else
        raise Exception.CreateFmt(
          'request for byte size of unsupported item %s:%s',
          [ItemToStr(p),NodeTypeToStr(p.NodeType)])
          {$IFDEF DEBUG}at src{$ENDIF};
      //else raise?Sphere.Error?
    end;
   end;
end;

function SameType(p1, p2: rItem): boolean;
var
  ptr1,ptr2:integer;
  tt:xTypeNr;
  r1,r2:rItem;
  b:boolean;
begin
  //pointer count 1
  ptr1:=0;
  b:=true;
  while b do
    case p1.NodeType of
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
    case p2.NodeType of
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
  if (p1.x=0) or (p2.x=0) then
    //Result:=(p1.x=0) and (p2.x=0) and (ptr1=ptr2)
    Result:=false//error?!
  else
  if p1.x=p2.x then
    Result:=ptr1=ptr2
  else
   begin
    tt:=p1.NodeType;
    if tt=p2.NodeType then
      case tt of
        nClass:
         begin
          while (p1.x<>0) and (p1.x<>p2.x) do
            p1:=p1.r(iInheritsFrom);
          Result:=(p1.x=p2.x) and (ptr1=ptr2);
         end;
        nClassRef:
         begin
          p1:=p1.r(iTarget);
          p2:=p2.r(iTarget);
          while (p1.x<>0) and (p1.x<>p2.x) do
            p1:=p1.r(iInheritsFrom);
          Result:=(p1.x=p2.x) and (ptr1=ptr2);
         end;
        nSignature:
         begin
          //TODO: local stack array
          if (ptr1=ptr2) and
            SameType(p1.r(iReturnType),p2.r(iReturnType)) and
            SameType(p1.r(iSubject),   p2.r(iSubject)) then
           begin
            ListFirst(p1,lArguments,p1,r1);
            ListFirst(p2,lArguments,p2,r2);
            while (p1.x<>0) and (p2.x<>0) and
              SameType(p1.r(iType),p2.r(iType)) do
             begin
              ListNext(p1,r1);
              ListNext(p2,r2);
             end;
            Result:=(p1.x=0) and (p2.x=0);
           end
          else
            Result:=false;
         end;
        //nInterface //TODO exact same members?
        else Result:=false;
      end
    else
    if (tt=nType)
      and (p1.x=IntrinsicTypes[itType])
      and (p2.NodeType=nClassRef)
      then
      Result:=true
    else
      Result:=false;
   end;
end;

function IsAddressable(p: rItem): boolean;
begin
  Result:=false;//default
  while p.x<>0 do
    case p.NodeType of
      nVar,nVarByRef,nThis://nVarReadOnly?
       begin
        Result:=true;//TODO: always? (not read-only?)
        p.x:=0;//end loop
       end;
      nArrayIndex:
        p:=p.r(iSubject);
      nField:
        p:=p.r(iTarget);
      else
        p.x:=0;//end loop
    end;
end;

end.
