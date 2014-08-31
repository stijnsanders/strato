unit stratoRunTime;

interface

{$D-}

uses stratoSphere, stratoDecl, stratoLogic, stratoFn;

const
  SystemWordSize=4;//32-bits? 64-bits?

  stratoSysCall_writeln=1;
  //TODO: malloc, free

procedure DefaultTypes(Sphere:TStratoSphere;ns:TStratoIndex);
procedure PerformSysCall(Sphere:TStratoSphere;Fn:TStratoIndex;
  const Value:UTF8String);

var
  TypeDecl_void,TypeDecl_type,TypeDecl_bool,TypeDecl_string,
  TypeDecl_number,TypeDecl_variant:TStratoIndex;

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
var
  n,m:TStratoIndex;
  q,r:PStratoThing;
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

  //for ...
   begin
    n:=Sphere.Add(ttFunction,'__writeln');
    q:=Sphere[n];
    q.Parent:=ns;
    q.Op:=stratoSysCall_writeln;

    q.Signature:=Sphere.Add(ttSignature,'__writeln(string)');
    r:=Sphere[q.Signature];
    r.Parent:=ns;
    r.EvaluatesTo:=0;//void

      r.FirstArgument:=Sphere.Add(ttArgument,'');
      r:=Sphere[r.FirstArgument];
      r.Parent:=q.Signature;
      r.EvaluatesTo:=TypeDecl_string;

    q.Body:=Sphere.Add(ttCodeBlock,'');
    r:=Sphere[q.Body];
    r.Parent:=n;
    r.FirstStatement:=n;//!

      m:=Sphere.Add(ttVar,'');
      r:=Sphere[m];
      r.Parent:=q.Body;
      //r.Offset:=0;
      r.EvaluatesTo:=TypeDecl_string;
      q.FirstArgument:=m;
      Sphere[q.Body].FirstItem:=m;

      r.Offset:=Sphere[q.Body].ByteSize;
      inc(Sphere[q.Body].ByteSize,SystemWordSize);

    p.Next:=n;
    p:=q;
   end;

  //TODO: exitcode (and all system things)
  //TODO: malloc, free  
  //TODO: objects with reference counting
  //TODO: strings with reference counting, copy-on-write, etc
end;

procedure PerformSysCall(Sphere:TStratoSphere;Fn:TStratoIndex;
  const Value:UTF8String);
begin
  case Sphere[Fn].Op of

    stratoSysCall_writeln:
      Writeln(Value);

    else Sphere.Error(Fn,'unknown system call '+IntToStr(Sphere[Fn].Op));
  end;
end;

end.
