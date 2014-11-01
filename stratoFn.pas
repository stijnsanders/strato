unit stratoFn;

interface

uses stratoDecl, stratoSphere, stratoSource;

function StratoFunctionAddOverload(Sphere:TStratoSphere;Source:TStratoSource;
  Fn,Signature,Codeblock:TStratoIndex;const Name:UTF8String):TStratoIndex;
function StratoFnCallAddArgument(Sphere:TStratoSphere;
  FnCall,Value:TStratoIndex):TStratoIndex;
procedure StratoFnCallFindSignature(Sphere:TStratoSphere;FnCall:TStratoIndex);

implementation

uses stratoLogic, stratoRunTime;

function StratoFunctionAddOverload(Sphere:TStratoSphere;Source:TStratoSource;
  Fn,Signature,Codeblock:TStratoIndex;const Name:UTF8String):TStratoIndex;
var
  bs:integer;
  p,q:TStratoIndex;
  px,qx,sx,cx:PStratoThing;
  b:boolean;
  tt:cardinal;
begin
  //TODO: detect duplicates, ambigiousness
  case Sphere[Fn].ThingType of
    ttFunction:
      if Sphere[Fn].Signature=0 then
        p:=Fn
      else
       begin
        p:=Sphere.Add(ttFunction,Name);
        q:=Fn;
        while Sphere[q].Next<>0 do q:=Sphere[q].Next;
        Sphere[q].Next:=p;
       end;
    ttClass:
     begin
      //assert Sphere[Signature].EvaluatesTo=Fn
      p:=Sphere.Add(ttConstructor,Name);
      Sphere[p].Parent:=Fn;
      q:=Sphere[Fn].FirstConstructor;
      if q=0 then
        Sphere[Fn].FirstConstructor:=p
      else
       begin
        while Sphere[q].Next<>0 do q:=Sphere[q].Next;
        Sphere[q].Next:=p;
       end;
     end;
    else
     begin
      Source.Error('unexpected overload subject');
      p:=Sphere.Add(ttFunction,Name);//counter warning
     end;
  end;
  Result:=p;
  px:=Sphere[p];
  px.Signature:=Signature;
  px.Body:=CodeBlock;

  if CodeBlock<>0 then
   begin
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
          inc(bs,ByteSize(Sphere,qx.EvaluatesTo));
       end;
     end;
    //arguments
    p:=sx.FirstArgument;
    b:=true;
    while p<>0 do
     begin
      px:=Sphere[p];
      if px.ThingType=ttArgByRef then tt:=ttVarByRef else tt:=ttVar;
      q:=Sphere.AddTo(cx.FirstItem,tt,Sphere.Dict.Str[px.Name]);
      if q=0 then
        Source.Error('duplicate identifier '''+string(Sphere.Dict.Str[px.Name])+'''')
      else
       begin
        qx:=Sphere[q];
        qx.Parent:=CodeBlock;
        qx.Offset:=bs;
        qx.EvaluatesTo:=px.EvaluatesTo;
        if tt=ttVarByRef then
          inc(bs,SystemWordSize)
        else
        if qx.EvaluatesTo<>0 then
          inc(bs,ByteSize(Sphere,qx.EvaluatesTo));
        if b then //store first arg value on function overload index
         begin
          Sphere[Result].FirstArgument:=q;
          b:=false;
         end;
       end;
      p:=px.Next;
     end;
    cx.ByteSize:=bs;
   end;
end;

function StratoFnCallAddArgument(Sphere:TStratoSphere;
  FnCall,Value:TStratoIndex):TStratoIndex;
var
  p,q:TStratoIndex;
  px,vx:PStratoThing;
begin
  p:=Sphere.Add(ttArgument,'');
  px:=Sphere[p];
  vx:=Sphere[Value];
  px.Name:=vx.Name;
  px.Parent:=FnCall;
  px.Subject:=Value;
  if Value=0 then
    px.EvaluatesTo:=0
  else
  if vx.ThingType=ttFnCall then
    px.EvaluatesTo:=Sphere[vx.Signature].EvaluatesTo
  else
  if (vx.ThingType and tt__Typed)<>0 then
    px.EvaluatesTo:=vx.EvaluatesTo
  else
    px.EvaluatesTo:=0;//Source.Error?
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
  p,q,x0,x1:PStratoThing;
  fn,p0,p1,r:TStratoIndex;
begin
  //assert all Arguments added
  p:=Sphere[FnCall];
  p.Signature:=0;//default
  p.Body:=0;//default
  //check overloads
  fn:=p.Subject;
  q:=Sphere[fn];
  if (q<>nil) and (q.ThingType=ttVarIndex) then
    fn:=q.Subject;
  if (q<>nil) and (q.ThingType=ttVar) then
    fn:=q.EvaluatesTo;
  //if (q<>nil) and (q.ThingType=ttInterface) then
  if (q<>nil) and (q.ThingType=ttClass) then
    fn:=q.FirstConstructor;
  while fn<>0 do
   begin

//TODO: debug more! raise?
if Sphere[fn].Signature=0 then
p0:=0
else

    p0:=Sphere[Sphere[fn].Signature].FirstArgument;
    p1:=p.FirstArgument;
    //TODO: default argument values
    while (p0<>0) and (p1<>0) do
     begin
      x0:=Sphere[p0];
      x1:=Sphere[p1];
      if SameType(Sphere,x0.EvaluatesTo,x1.EvaluatesTo) then
       begin
        if x0.ThingType=ttArgByRef then
         begin
          r:=x1.Subject;
          while (r<>0) and (Sphere[r].ThingType=ttAssign) do
            r:=Sphere[r].AssignTo;
          if (r<>0) and (Sphere[r].ThingType<>ttVar) then //=ttLiteral then Error?
            p0:=0;//TODO: warning like 'unsuitable argument for byref'?
         end;
       end
      else
        p0:=0;//not OK, break loop
      if p0<>0 then
       begin
        p0:=x0.Next;
        p1:=x1.Next;
       end;
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
