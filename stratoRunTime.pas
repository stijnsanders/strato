unit stratoRunTime;

interface

uses stratoSphere, stratoDecl, stratoLogic, stratoFn;

const
  SystemWordSize=4;//32-bits? 64-bits?

  stratoSysCall_writeln=1;
  stratoSysCall_malloc=2;
  stratoSysCall_realloc=3;
  stratoSysCall_mfree=4;
  //TODO: malloc, free

procedure DefaultTypes(Sphere:TStratoSphere;ns:TStratoIndex);

var
  TypeDecl_void,TypeDecl_type,TypeDecl_bool,TypeDecl_string,
  TypeDecl_number,TypeDecl_variant,TypeDecl_pointer:TStratoIndex;

implementation

uses SysUtils;

procedure DefaultTypes(Sphere:TStratoSphere;ns:TStratoIndex);
var
  p:PStratoThing;
  function A(const Name:UTF8String;s:integer): TStratoIndex;
  var
    q:PStratoThing;
  begin
    Result:=Sphere.Add(ttTypeDecl,Name);
    q:=Sphere[Result];
    q.Parent:=ns;
    q.ByteSize:=s;
    if p=nil then Sphere[ns].FirstItem:=Result else p.Next:=Result;
    p:=q;
  end;
  procedure C(const FunctionName,SignatureName:UTF8String;Op:cardinal;
    const Arguments:array of TStratoIndex;ReturnType:TStratoIndex);
  var
    i:integer;
    n,p1,p2:TStratoIndex;
    q,r:PStratoThing;
  begin
    //n:=Sphere.Lookup(ns.FirstItem,FunctionName);
    //if n=0 then
     begin
      n:=Sphere.Add(ttFunction,FunctionName);
      q:=Sphere[n];
      q.Parent:=ns;
     end;

    q.Signature:=Sphere.Add(ttSignature,SignatureName);
    r:=Sphere[q.Signature];
    r.Parent:=ns;
    r.EvaluatesTo:=ReturnType;

    p2:=0;
    for i:=0 to Length(Arguments)-1 do
     begin
      p1:=Sphere.Add(ttArgument,'');
      if p2=0 then r.FirstArgument:=p1 else Sphere[p2].Next:=p1;
      p2:=p1;
      r:=Sphere[p1];
      r.Parent:=q.Signature;
      r.EvaluatesTo:=Arguments[i];
     end;

    q.Body:=Sphere.Add(ttCodeBlock,'');
    r:=Sphere[q.Body];
    r.Parent:=n;
    r.FirstStatement:= Sphere.Add(ttSysCall,'');
    r:=Sphere[r.FirstStatement];
    r.Parent:=n;
    r.Op:=Op;

    if ReturnType<>0 then
     begin
      p1:=Sphere.Add(ttVar,FunctionName);
      r:=Sphere[p1];
      r.Parent:=q.Body;
      r.EvaluatesTo:=ReturnType;
      r.Offset:=Sphere[q.Body].ByteSize;
      inc(Sphere[q.Body].ByteSize,Sphere[ReturnType].ByteSize);
     end;

    p2:=0;
    for i:=0 to Length(Arguments)-1 do
     begin
      p1:=Sphere.Add(ttVar,'');
      if p2=0 then
       begin
        q.FirstArgument:=p1;
        Sphere[q.Body].FirstItem:=p1;
       end
      else Sphere[p2].Next:=p1;
      p2:=p1;
      r:=Sphere[p1];
      r.Parent:=q.Body;
      r.EvaluatesTo:=Arguments[i];
      r.Offset:=Sphere[q.Body].ByteSize;
      inc(Sphere[q.Body].ByteSize,Sphere[Arguments[i]].ByteSize);
     end;

    p.Next:=n;
    p:=q;
  end;
var
  n:TStratoIndex;
  q:PStratoThing;
begin
  p:=nil;
  TypeDecl_void:=A('void',0);
  TypeDecl_type:=A('type',SystemWordSize);
  TypeDecl_bool:=A('bool',SystemWordSize);
  TypeDecl_string:=A('string',SystemWordSize);
  TypeDecl_number:=A('number',SystemWordSize);//TODO: other numerics inherit, (auto)casting
  TypeDecl_variant:=A('variant',16);//TODO: OLE compatible variants
  A('int8',1);
  A('int16',2);
  A('int32',4);
  A('int64',8);
  A('uint8',1);
  A('uint16',2);
  A('uint32',4);
  A('uint64',8);
  A('float',4);
  A('double',8);
  A('hash',SystemWordSize);
  //TODO: auto-casting

  n:=Sphere.Add(ttPointer,'pointer');
  q:=Sphere[n];
  q.Parent:=ns;
  q.ByteSize:=SystemWordSize;
  p.Next:=n;
  p:=q;
  TypeDecl_pointer:=n;

  C('__writeln','__writeln(string)',
    stratoSysCall_writeln,[TypeDecl_string],0);
  C('__malloc','__malloc(number)',
    stratoSysCall_malloc,[TypeDecl_number],TypeDecl_pointer);

  //TODO: exitcode (and all system things)
  //TODO: objects with reference counting
  //TODO: strings with reference counting, copy-on-write, etc
end;

end.
