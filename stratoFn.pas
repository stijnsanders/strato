unit stratoFn;

interface

{$D-}

uses stratoDecl, stratoSphere, stratoSource;

function StratoSignatureAddArgument(Sphere:TStratoSphere;
  Signature,ArgType,Defaultvalue:TStratoIndex;const ArgName:UTF8String):TStratoIndex;
procedure StratoSignatureRetype(Sphere:TStratoSphere;
  Signature,ArgType:TStratoIndex;Count:cardinal);
function StratoFunctionAddOverload(Sphere:TStratoSphere;Source:TStratoSource;
  Fn,Signature,Codeblock:TStratoIndex;const Name:UTF8String):TStratoIndex;
function StratoFnCallAddArgument(Sphere:TStratoSphere;
  FnCall,Value:TStratoIndex):TStratoIndex;
procedure StratoFnCallFindSignature(Sphere:TStratoSphere;FnCall:TStratoIndex);

implementation

uses stratoLogic, stratoRunTime;

function StratoSignatureAddArgument(Sphere:TStratoSphere;
  Signature,ArgType,Defaultvalue:TStratoIndex;const ArgName:UTF8String):TStratoIndex;
var
  p:PStratoThing;
begin
  Result:=Sphere.AddTo(Sphere[Signature].FirstArgument,ttArgument,ArgName);
  if Result<>0 then
   begin
    p:=Sphere[Result];
    p.Parent:=Signature;
    //p.Offset:=??? see strFnCall
    p.EvaluatesTo:=ArgType;
    p.InitialValue:=Defaultvalue;
   end;
end;

procedure StratoSignatureRetype(Sphere:TStratoSphere;
  Signature,ArgType:TStratoIndex;Count:cardinal);
var
  x:cardinal;
  p:PStratoThing;
begin
  x:=Count;
  //assert arguments to retype at the end!
  while x<>0 do
   begin
    dec(x);
    p:=Sphere[Sphere[Signature].FirstArgument];
    while (p<>nil) and (p.Next<>0) do p:=Sphere[p.Next];
    if (p<>nil) and (p.EvaluatesTo=0) then p.EvaluatesTo:=ArgType;
    //p.Offset? see strFnCall
   end;
end;

function StratoFunctionAddOverload(Sphere:TStratoSphere;Source:TStratoSource;
  Fn,Signature,Codeblock:TStratoIndex;const Name:UTF8String):TStratoIndex;
var
  bs:integer;
  p,q:TStratoIndex;
  px,qx,sx,cx:PStratoThing;
  b:boolean;
begin
  //TODO: detect duplicates, ambigiousness
  if Sphere[Fn].Signature=0 then
    p:=Fn
  else
   begin
    p:=Sphere.Add(ttFunction,Name);
    q:=Fn;
    while Sphere[q].Next<>0 do q:=Sphere[q].Next;
    Sphere[q].Next:=p;
   end;
  Result:=p;
  px:=Sphere[p];
  px.Signature:=Signature;
  px.Body:=CodeBlock;

  //populate code block
  sx:=Sphere[Signature];
  cx:=Sphere[CodeBlock];
  bs:=cx.ByteSize;
  //this "@@"
  if sx.Subject<>0 then
   begin
    q:=Sphere.AddTo(cx.FirstItem,ttThis,'@@');
    if q=0 then
      Source.Error('duplicate identifier ''@@''')
    else
     begin
      qx:=Sphere[q];
      qx.Parent:=CodeBlock;
      qx.Offset:=bs;
      qx.EvaluatesTo:=sx.Subject;
      inc(bs,SystemWordSize);
     end;
   end;
  //return value
  if sx.EvaluatesTo<>0 then
   begin
    q:=Sphere.AddTo(cx.FirstItem,ttVar,Name);
    if q=0 then
      Source.Error('duplicate identifier '''+string(Name)+'''')
    else
     begin
      qx:=Sphere[q];
      qx.Parent:=CodeBlock;
      qx.Offset:=bs;
      qx.EvaluatesTo:=sx.EvaluatesTo;
      if qx.EvaluatesTo<>0 then
        inc(bs,Sphere[qx.EvaluatesTo].ByteSize);
     end;
   end;
  //arguments
  p:=sx.FirstArgument;
  b:=true;
  while p<>0 do
   begin
    px:=Sphere[p];
    q:=Sphere.AddTo(cx.FirstItem,ttVar,Sphere.Dict.Str[px.Name]);
    if q=0 then
      Source.Error('duplicate identifier '''+string(Sphere.Dict.Str[px.Name])+'''')
    else
     begin
      qx:=Sphere[q];
      qx.Parent:=CodeBlock;
      qx.Offset:=bs;
      qx.EvaluatesTo:=px.EvaluatesTo;
      if qx.EvaluatesTo<>0 then
        inc(bs,Sphere[qx.EvaluatesTo].ByteSize);
      if b then //store first arg value on function overload index
       begin
        Sphere[Fn].FirstArgument:=q;
        b:=false;
       end;
     end;
    p:=px.Next;
   end;
  cx.ByteSize:=bs;
end;

function StratoFnCallAddArgument(Sphere:TStratoSphere;
  FnCall,Value:TStratoIndex):TStratoIndex;
var
  p,q:TStratoIndex;
  px:PStratoThing;
begin
  p:=Sphere.Add(ttArgument,'');
  px:=Sphere[p];
  px.Name:=Sphere[Value].Name;
  px.Parent:=FnCall;
  px.Subject:=Value;
  if Value=0 then
    px.EvaluatesTo:=0
  else
    px.EvaluatesTo:=Sphere[Value].EvaluatesTo;
  q:=Sphere[FnCall].FirstArgument;
  if q=0 then
    Sphere[FnCall].FirstArgument:=p
  else
   begin
    while (Sphere[q].Next<>0) do q:=Sphere[q].Next;
    Sphere[q].Next:=p;
   end;
  Result:=p;
end;

procedure StratoFnCallFindSignature(Sphere:TStratoSphere;FnCall:TStratoIndex);
var
  p,q:PStratoThing;
  fn,p0,p1:TStratoIndex;
begin
  //assert all Arguments added
  p:=Sphere[FnCall];
  p.Signature:=0;//default
  p.Body:=0;//default
  //check overloads
  fn:=p.Subject;
  if (fn<>0) and (Sphere[fn].ThingType=ttVarIndex) then
    fn:=Sphere[fn].Subject;
  while fn<>0 do
   begin
    p0:=Sphere[Sphere[fn].Signature].FirstArgument;
    p1:=p.FirstArgument;
    //TODO: default argument values
    //TODO: arg by reference!
    while (p0<>0) and (p1<>0)
      and SameType(Sphere,Sphere[p0].EvaluatesTo,Sphere[p1].EvaluatesTo) do
     begin
      p0:=Sphere[p0].Next;
      p1:=Sphere[p1].Next;
     end;
    if (p0=0) and (p1=0) then
     begin
      //found! set signature,body from matching overload
      q:=Sphere[fn];
      p.Signature:=q.Signature;
      p.Body:=q.Body;
      fn:=0;//end loop
     end
    else
      fn:=Sphere[fn].Next;
   end;
end;

end.
