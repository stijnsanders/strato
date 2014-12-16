unit stratoFn;

interface

uses stratoDecl, stratoSphere, stratoSource;

function StratoFunctionAddOverload(Sphere:TStratoSphere;Source:TStratoSource;
  Fn,Signature,CodeBlock:TStratoIndex;const Name:UTF8String):TStratoIndex;
function StratoFnCallAddArgument(Sphere:TStratoSphere;
  FnCall,Value:TStratoIndex):TStratoIndex;
procedure StratoFnCallFindSignature(Sphere:TStratoSphere;FnCall:TStratoIndex);
function StratoFnCallFindInherited(Sphere:TStratoSphere;
  Method:PStratoThing;Name:TStratoName):TStratoIndex;
procedure StratoFnArgByValues(Sphere:TStratoSphere;
  FnCall,FirstArg,FirstValue:TStratoIndex);

implementation

uses stratoLogic, stratoRunTime;

function StratoFunctionAddOverload(Sphere:TStratoSphere;Source:TStratoSource;
  Fn,Signature,CodeBlock:TStratoIndex;const Name:UTF8String):TStratoIndex;
var
  bs:integer;
  p,q,r:TStratoIndex;
  px,qx,sx,cx:PStratoThing;
  b:boolean;
  tt:cardinal;
begin
  case Sphere[Fn].ThingType of
    ttFunction:
     begin
      //TODO: detect duplicates, ambigiousness
      p:=0;
      r:=0;
      q:=Sphere[Fn].FirstItem;
      while (q<>0) do
       begin
        if SameType(Sphere,Sphere[q].Signature,Signature) then
          if Sphere[q].Body=0 then //forward! fill in CB
           begin
            p:=q;
            q:=0;
            //TODO: delete/avoid superfluous Signature?
           end
          else
           begin
            q:=0;
            Source.Error('Duplicate overload or equivalent signature');
           end;
        if q<>0 then
         begin
          r:=q;
          q:=Sphere[q].Next;
         end;
       end;
      if p=0 then
       begin
        p:=Sphere.Add(ttOverload,'');
        if r=0 then
          Sphere[Fn].FirstItem:=p
        else
          Sphere[r].Next:=p;
       end;
     end;
    ttClass:
     begin
      //assert Sphere[Signature].EvaluatesTo=0
      Sphere[Signature].EvaluatesTo:=Fn;
      p:=Sphere.Add(ttConstructor,'');
      Sphere[p].Parent:=Fn;
      Sphere.Append(Sphere[Fn].FirstConstructor,p);
      if CodeBlock<>0 then Sphere[CodeBlock].Parent:=p;
     end;
    else
     begin
      Source.Error('unexpected overload subject');
      p:=Sphere.Add(ttOverload,'');//counter warning
     end;
  end;
  Result:=p;
  px:=Sphere[p];
  px.Parent:=Fn;
  px.Signature:=Signature;
  px.Body:=CodeBlock;

  if CodeBlock<>0 then
   begin
    //populate code block
    sx:=Sphere[Signature];
    cx:=Sphere[CodeBlock];
    cx.Parent:=p;
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
  px.EvaluatesTo:=ResType(Sphere,Value);
  Sphere.Append(Sphere[FnCall].FirstArgument,p);
  Result:=p;
end;

function StratoFnArgListsMatch(Sphere:TStratoSphere;
  Arg0,Arg1:TStratoIndex):boolean;
var
  p0,p1,q:TStratoIndex;
  x0,x1:PStratoThing;
begin
  p0:=Arg0;//Sphere[Arg0].FirstArgument?
  p1:=Arg1;//Sphere[Arg1].FirstArgument?
  //TODO: default argument values
  while (p0<>0) and (p1<>0) do
   begin
    x0:=Sphere[p0];
    x1:=Sphere[p1];
    if SameType(Sphere,x0.EvaluatesTo,x1.EvaluatesTo) then
     begin
      if x0.ThingType=ttArgByRef then
       begin
        q:=x1.Subject;
        while (q<>0) and (Sphere[q].ThingType=ttAssign) do
          q:=Sphere[q].AssignTo;
        while (q<>0) and (Sphere[q].ThingType=ttVarIndex) do
          q:=Sphere[q].Subject;
        if (q<>0) and (Sphere[q].ThingType<>ttVar) then //=ttLiteral then Error?
          p0:=0;//keep searching
        //TODO: warning like 'unsuitable argument for byref'?
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
  Result:=(p0=0) and (p1=0);//found or not?
end;

procedure StratoFnCallFindSignature(Sphere:TStratoSphere;FnCall:TStratoIndex);
var
  p,fx:PStratoThing;
  fn,q:TStratoIndex;
begin
  //assert all Arguments added
  p:=Sphere[FnCall];
  p.Signature:=0;//default
  p.Body:=0;//default
  //check overloads
  fn:=p.Subject;
  if (fn<>0) and (Sphere[fn].ThingType=ttVarIndex) then
    fn:=Sphere[fn].Subject;
  if (fn<>0) and (Sphere[fn].ThingType=ttVar) then
    fn:=Sphere[fn].EvaluatesTo;
  //if (fn<>0) and (Sphere[fn].ThingType=ttInterface) then?

  if fn<>0 then
   begin
    fx:=Sphere[fn];
    case fx.ThingType of
      ttClass:
       begin
        //see also StratoFnCallFindInherited!
        q:=fn;
        fn:=fx.FirstConstructor;
        while not((fn<>0) and StratoFnArgListsMatch(Sphere,
          Sphere[fn].FirstArgument,p.FirstArgument)) do
          if fn=0 then
           begin
            q:=Sphere[q].InheritsFrom;
            if q=0 then fn:=0 else fn:=Sphere[q].FirstConstructor;
           end
          else
            fn:=Sphere[fn].Next;
       end;
      ttInherited:
       begin
        //q:=fn;
        fn:=Sphere[fn].Parent;//ttCodeBlock
        if fn<>0 then
          fn:=StratoFnCallFindInherited(Sphere,Sphere[Sphere[fn].Parent],p.Name);
          //Sphere[FnCall].Subject:=fn?
          //xxx  ttVarIndex ???
       end;
      ttFunction:
       begin
        fn:=Sphere[fn].FirstItem;//ttOverload
        while (fn<>0) and not(StratoFnArgListsMatch(Sphere,
          Sphere[Sphere[fn].Signature].FirstArgument,
          p.FirstArgument)) do
          fn:=Sphere[fn].Next;
       end;
      else
        fn:=0;//error?
    end;
    if fn<>0 then
     begin
      //found! set signature,body from matching overload
      p.Signature:=Sphere[fn].Signature;
      p.Body:=Sphere[fn].Body;
     end;
   end;
end;

function StratoFnCallFindInherited(Sphere:TStratoSphere;
  Method:PStratoThing;Name:TStratoName):TStratoIndex;
var
  p,q:TStratoIndex;
  px:PStratoThing;
begin
  if (Method=nil) or (Method.Parent=0) then Result:=0 else
   begin
    case Method.ThingType of
      ttConstructor,ttDestructor:
        px:=Sphere[Method.Parent];
      ttOverload:
        px:=Sphere[Sphere[Method.Parent].Parent];
      else px:=nil;//error?
    end;
    p:=0;
    if px=nil then q:=0 else q:=px.InheritsFrom;
    while (p=0) and (q<>0) do
     begin
      case Method.ThingType of
        ttConstructor:
          p:=Sphere[q].FirstConstructor;
        ttDestructor:
         begin
          p:=Sphere[q].FirstItem;
          while (p<>0) and (Sphere[p].ThingType<>ttDestructor) do
            p:=Sphere[p].Next;
         end;
        ttOverload:
         begin
          p:=Sphere.Lookup(Sphere[q].FirstItem,Name);
          if (p<>0) and (Sphere[p].ThingType=ttFunction) then
            p:=Sphere[p].FirstItem
          else
            p:=0;//error?
         end;
        //ttProperty://TODO!
        else p:=0;//error?
      end;
      while (p<>0) and not(StratoFnArgListsMatch(Sphere,
        Method.FirstArgument,Sphere[p].FirstArgument)) do
        p:=Sphere[p].Next;
      if (p=0) and (q<>0) then
        q:=Sphere[q].InheritsFrom;
     end;
    //default to the constructor without arguments
    if (p=0) and (Method.ThingType=ttConstructor) then
     begin
      q:=px.InheritsFrom;
      while (p=0) and (q<>0) do
       begin
        p:=Sphere[q].FirstConstructor;
        while (p<>0) and (Sphere[p].FirstArgument<>0) do
          p:=Sphere[p].Next;
        if (p=0) and (q<>0) then
          q:=Sphere[q].InheritsFrom;
       end;
     end;
    Result:=p;
   end;
end;

procedure StratoFnArgByValues(Sphere:TStratoSphere;
  FnCall,FirstArg,FirstValue:TStratoIndex);
var
  p,q,r:TStratoIndex;
  rx:PStratoThing;
begin
  rx:=nil;
  //Assert Sphere[FnCall].FirstArgument=0;
  p:=FirstArg;
  q:=FirstValue;
  //TODO: default argument values
  while p<>0 do
   begin
    r:=Sphere.Add(ttArgument,'');
    //SetSrc?
    if rx=nil then Sphere[FnCall].FirstArgument:=r else rx.Next:=r;
    rx:=Sphere[r];
    rx.Name:=Sphere[q].Name;
    rx.Parent:=FnCall;
    rx.Subject:=q;
    rx.EvaluatesTo:=ResType(Sphere,q);
    p:=Sphere[p].Next;
    if q<>0 then q:=Sphere[q].Next; //else error?
   end;
end;

end.
