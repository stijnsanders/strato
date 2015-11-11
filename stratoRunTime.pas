unit stratoRunTime;

{

stratoRunTime

declare procedure DefaultTypes that adds basic run-time declarations
into a TStratoSphere

}

interface

uses stratoSphere, stratoDecl, stratoLogic, stratoFn;

const
  SystemWordSize=4;//32-bits? 64-bits?

  stratoSysCall_xinc=1;
  stratoSysCall_xdec=2;
  stratoSysCall_malloc=$100;
  stratoSysCall_realloc=$101;
  stratoSysCall_mfree=$102;
  stratoSysCall_writeln=$200;

procedure DefaultTypes(Sphere:TStratoSphere);

//TODO: move these into TStratoSpere!
var
  TypeDecl_void,TypeDecl_type,TypeDecl_bool,TypeDecl_string,
  TypeDecl_number,TypeDecl_intLast,TypeDecl_variant,TypeDecl_pointer,
  TypeDecl_hash,TypeDecl_object:TStratoIndex;
  Name_Inherited:TStratoName;

implementation

uses SysUtils;

procedure DefaultTypes(Sphere:TStratoSphere);
var
  ns:TStratoIndex;
  p:PStratoThing;

  function A(const Name:UTF8String;s:integer): TStratoIndex;
  var
    q:PStratoThing;
  begin
    Result:=Sphere.Add(ttTypeDecl,q);
    q.Name:=Sphere.Dict.StrIdx(Name);
    q.Parent:=ns;
    q.ByteSize:=s;
    if p=nil then Sphere[ns].FirstItem:=Result else p.Next:=Result;
    p:=q;
  end;

  const
    C_ByRef=$10000;

  procedure C(const FunctionName,SignatureName:UTF8String;Op:cardinal;
    const Arguments:array of TStratoIndex;ReturnType:TStratoIndex);
  var
    i:integer;
    n,n1,p1,p2:TStratoIndex;
    q,r,s,cx:PStratoThing;
  begin
    //n:=Sphere.Lookup(ns.FirstItem,FunctionName);
    //if n=0 then
     begin
      n1:=Sphere.Add(ttMember,q);
      q.Name:=Sphere.Dict.StrIdx(FunctionName);
      q.Parent:=ns;
      n:=Sphere.Add(ttOverload,q);
      q.Parent:=n1;

      p.Next:=n1;
      p:=Sphere[n1];
      p.FirstItem:=n;
     end;

    q.Target:=Sphere.Add(ttSignature,r);
    r.Name:=Sphere.Dict.StrIdx(SignatureName);
    r.Parent:=n;
    r.EvaluatesTo:=ReturnType;

    p2:=0;
    for i:=0 to Length(Arguments)-1 do
     begin
      p1:=Sphere.Add(ttArgument,s);
      if p2=0 then r.FirstArgument:=p1 else Sphere[p2].Next:=p1;
      p2:=p1;
      s.Parent:=q.Target;
      if (Arguments[i] and C_ByRef)=0 then
        s.EvaluatesTo:=Arguments[i]
      else
       begin
        s.ThingType:=ttArgByRef;
        s.EvaluatesTo:=(Arguments[i] xor C_ByRef);
       end;
     end;

    q.Body:=Sphere.Add(ttCodeBlock,cx);
    cx.Parent:=n;
    cx.FirstStatement:=Sphere.Add(ttSysCall,s);
    s.Parent:=n;
    s.Op:=Op;

    if ReturnType<>0 then
     begin
      p1:=Sphere.Add(ttVar,r);
      r.Name:=Sphere.Dict.StrIdx(FunctionName);
      r.Parent:=q.Body;
      r.EvaluatesTo:=ReturnType;
      r.Offset:=cx.ByteSize;
      inc(cx.ByteSize,ByteSize(Sphere,ReturnType));
      cx.FirstItem:=p1;
     end
    else
      p1:=0;

    p2:=p1;
    for i:=0 to Length(Arguments)-1 do
     begin
      p1:=Sphere.Add(ttVar,r);
      if p2=0 then
       begin
        if cx.FirstItem=0 then
          cx.FirstItem:=p1;
       end
      else
        Sphere[p2].Next:=p1;
      p2:=p1;
      if i=0 then q.FirstArgument:=p1;
      r.Parent:=q.Body;
      r.Offset:=cx.ByteSize;
      if (Arguments[i] and C_ByRef)=0 then
       begin
        r.EvaluatesTo:=Arguments[i];
        inc(cx.ByteSize,Sphere[Arguments[i]].ByteSize);
       end
      else
       begin
        r.ThingType:=ttVarByRef;
        r.EvaluatesTo:=Arguments[i] xor C_ByRef;
        inc(cx.ByteSize,SystemWordSize);
       end;
     end;

  end;
  
begin
  //assert Sphere.Header.FirstNameSpace=0
  //Sphere.MarkIndex($10000);
  Sphere.MarkIndex(10000);
  ns:=Sphere.Add(ttNameSpace,p);
  p.Name:=Sphere.Dict.StrIdx('Strato');
  Sphere.Header.FirstNameSpace:=ns;

  p:=nil;
  TypeDecl_void:=A('void',0);
  TypeDecl_type:=A('type',SystemWordSize);
  TypeDecl_bool:=A('bool',SystemWordSize);
  TypeDecl_string:=A('string',SystemWordSize);
  TypeDecl_variant:=A('variant',16);//TODO: OLE compatible variants
  TypeDecl_number:=A('number',SystemWordSize);//TODO: other numerics inherit, (auto)casting
  A('i8',1);
  A('i16',2);
  A('i32',4);
  A('i64',8);
  A('u8',1);
  A('u16',2);
  A('u32',4);
  TypeDecl_intLast:=A('u64',8);
  A('f32',4);//TODO: floating-point support
  A('f64',8);
  TypeDecl_object:=0;//see StratoParseSource: allow only one object()={}
  TypeDecl_hash:=A('hash',SystemWordSize);//TODO:
  TypeDecl_pointer:=A('pointer',SystemWordSize);

  C('__xinc','__xinc(^number)',
    stratoSysCall_xinc,[TypeDecl_number or C_ByRef],TypeDecl_number);
  C('__xdec','__xdec(^number)',
    stratoSysCall_xdec,[TypeDecl_number or C_ByRef],TypeDecl_number);
  C('__writeln','__writeln(string)',
    stratoSysCall_writeln,[TypeDecl_string],0);
  C('__malloc','__malloc(number)',
    stratoSysCall_malloc,[TypeDecl_number],TypeDecl_pointer);
  C('__realloc','__realloc(pointer,number)',
    stratoSysCall_realloc,[TypeDecl_pointer,TypeDecl_number],TypeDecl_pointer);
  C('__mfree','__mfree(pointer)',
    stratoSysCall_mfree,[TypeDecl_pointer],0);

  //TODO: exitcode (and all system things)
  //TODO: objects with reference counting
  //TODO: strings with reference counting, copy-on-write, etc

  Sphere.MarkIndex(0);
end;

end.
