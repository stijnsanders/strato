unit stratoFn;

interface

{$D-}
{$L-}

uses stratoDecl, stratoSphere, stratoSource;

function StratoFnAddOverload(Sphere:TStratoSphere;Source:TStratoSource;
  Fn,Signature,SourceFile:TStratoIndex):TStratoIndex;
function StratoFnOverloadCodeBlock(Sphere:TStratoSphere;Source:TStratoSource;
  Fn,Signature,SourceFile:TStratoIndex):TStratoIndex;
function StratoFnCallAddArgument(Sphere:TStratoSphere;
  FnCall,Value:TStratoIndex;var Info:PStratoThing):TStratoIndex;
function StratoFnCallFindSignature(Sphere:TStratoSphere;
  FnCall:TStratoIndex):boolean;
function StratoFnCallFindInherited(Sphere:TStratoSphere;
  Method:PStratoThing;Name:TStratoName):TStratoIndex;
function StratoFnCallFindVirtual(Sphere:TStratoSphere;
  ImplClass,Target:PStratoThing):TStratoIndex;
function StratoFnCodeBlock(Sphere:TStratoSphere;
  Parent,ThisType,ValueType:TStratoIndex;
  ValueName:TStratoName;SrcPos:cardinal):TStratoIndex;
procedure StratoFnArgByValues(Sphere:TStratoSphere;
  FnCall,FirstArg,FirstValue:TStratoIndex);
function FnSignature(Sphere:TStratoSphere;Fn:PStratoThing):PStratoThing;

implementation

uses stratoLogic, stratoRunTime;

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
        q:=x1.Target;
        while (q<>0) and (Sphere[q].ThingType=ttAssign) do
          q:=Sphere[q].AssignTo;
        while (q<>0) and (Sphere[q].ThingType=ttVarIndex) do
          q:=Sphere[q].Target;
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
  //found or not?
  Result:=(p0=0) and (p1=0);
end;

function StratoFnAddOverload(Sphere:TStratoSphere;Source:TStratoSource;
  Fn,Signature,SourceFile:TStratoIndex):TStratoIndex;
var
  p,q:TStratoIndex;
  px:PStratoThing;
begin
  case Sphere[Fn].ThingType of
    ttFunction:
     begin
      p:=0;
      px:=nil;
      q:=Sphere[Fn].FirstItem;
      while (q<>0) do
        if SameType(Sphere,Sphere[q].Target,Signature) then
         begin
          q:=0;
          if Sphere[q].Body=0 then //forward! fill in CB
           begin
            p:=q;
            q:=0;
            //TODO: delete/avoid superfluous Signature?
           end
          else
           begin
            q:=0;
            Source.Error('duplicate overload or equivalent signature');
           end;
         end
        else
        if StratoFnArgListsMatch(Sphere,
          Sphere[q].FirstArgument,Sphere[Signature].FirstArgument) then
         begin
          q:=0;
          Source.Error('duplicate overload with equivalent arguments');
         end
        else
          q:=Sphere[q].Next;
      if p=0 then
       begin
        p:=Sphere.Add(ttOverload,px);
        Sphere.Append(Sphere[Fn].FirstItem,p);
       end
      else
        px:=Sphere[p];
     end;
    ttClass:
     begin
      //assert Sphere[Signature].EvaluatesTo=0
      Sphere[Signature].EvaluatesTo:=Fn;
      p:=Sphere.Add(ttConstructor,px);
      Sphere.Append(Sphere[Fn].FirstItem,p);
     end;
    else
     begin
      Source.Error('unexpected overload subject');
      p:=Sphere.Add(ttOverload,px);//
     end;
  end;
  Result:=p;
  px.Parent:=Fn;
  px.Target:=Signature;
  //px.Body:=CodeBlock;
  px.SourceFile:=SourceFile;
  px.SrcPos:=Source.SrcPos;
end;

function StratoFnOverloadCodeBlock(Sphere:TStratoSphere;Source:TStratoSource;
  Fn,Signature,SourceFile:TStratoIndex):TStratoIndex;
var
  p,q,ovl:TStratoIndex;
  px,qx:PStratoThing;
  bs:cardinal;
  tt:TStratoThingType;
begin
  ovl:=StratoFnAddOverload(Sphere,Source,Fn,Signature,SourceFile);
  Result:=Sphere.Add(ttCodeBlock,qx);
  qx.Parent:=ovl;
  qx.SrcPos:=Source.SrcPos;
  Sphere[ovl].Body:=Result;
  //populate code block
  bs:=0;//bs:=cx.ByteSize;
  //this "@@"
  if Sphere[Signature].Target<>0 then
   begin
    q:=Sphere.Add(ttThis,qx);
    qx.Name:=Sphere.Dict.StrIdx('@@');
    qx.Parent:=Result;//CodeBlock;
    qx.SrcPos:=Source.SrcPos;
    qx.Offset:=bs;
    qx.EvaluatesTo:=Sphere[Signature].Target;
    inc(bs,SystemWordSize);
    Sphere.Append(Sphere[Result].FirstItem,q);
   end;
  //return value
  if Sphere[Signature].EvaluatesTo<>0 then
    if Sphere[Fn].ThingType=ttConstructor then//if Sphere[Fn].ThingType=ttClass then
     begin
      //with a constructor, store the effective class type here
      q:=Sphere.Add(ttVar,qx);
      qx.Name:=Sphere.Dict.StrIdx('?@@');
      qx.Parent:=Result;//CodeBlock;
      qx.SrcPos:=Source.SrcPos;
      qx.Offset:=bs;
      qx.EvaluatesTo:=TypeDecl_type;
      inc(bs,SystemWordSize);
      Sphere.Append(Sphere[Result].FirstItem,q);
     end
    else
     begin
      q:=Sphere.Add(ttVar,qx);
      qx.Name:=Sphere[Fn].Name;
      qx.Parent:=Result;//CodeBlock;
      qx.SrcPos:=Source.SrcPos;
      qx.Offset:=bs;
      qx.EvaluatesTo:=Sphere[Signature].EvaluatesTo;
      if qx.EvaluatesTo<>0 then
        inc(bs,ByteSize(Sphere,qx.EvaluatesTo));
      Sphere.Append(Sphere[Result].FirstItem,q);
     end;
  //arguments
  p:=Sphere[Signature].FirstArgument;
  while p<>0 do
   begin
    px:=Sphere[p];
    if px.ThingType=ttArgByRef then tt:=ttVarByRef else tt:=ttVar;
    q:=Sphere.AddTo(Sphere[Result].FirstItem,tt,px.Name,qx);
    if q=0 then
      Source.Error('duplicate identifier "'+string(Sphere.Dict.Str[px.Name])+'"')
    else
     begin
      qx.Parent:=Result;//CodeBlock
      qx.SrcPos:=Source.SrcPos;
      qx.Offset:=bs;
      qx.EvaluatesTo:=px.EvaluatesTo;
      if tt=ttVarByRef then
        inc(bs,SystemWordSize)
      else
      if qx.EvaluatesTo<>0 then
        inc(bs,ByteSize(Sphere,qx.EvaluatesTo));
      //store first arg value on function overload index
      if Sphere[ovl].FirstArgument=0 then Sphere[ovl].FirstArgument:=q;
     end;
    p:=px.Next;
   end;
  Sphere[Result].ByteSize:=bs;
end;

function StratoFnCallAddArgument(Sphere:TStratoSphere;
  FnCall,Value:TStratoIndex;var Info:PStratoThing):TStratoIndex;
begin
  Result:=Sphere.Add(ttArgument,Info);
  //Info.Name:=Sphere[Value].Name;//TODO: if tt__Named?
  Info.Parent:=FnCall;
  Info.Target:=Value;
  Info.EvaluatesTo:=ResType(Sphere,Value);
  Sphere.Append(Sphere[FnCall].FirstArgument,Result);
end;

function StratoFnCallFindSignature(Sphere:TStratoSphere;
  FnCall:TStratoIndex):boolean;
var
  p,q,p1:TStratoIndex;
  px,qx,rx:PStratoThing;
begin
  //assert all Arguments added
  rx:=Sphere[FnCall];
  p:=rx.Target;
  px:=Sphere[p];
  //TODO: remove use of fnCall.Body
  //rx.Target:=0;//default
  rx.Body:=0;//default
  if (p<>0) and (px.ThingType=ttVarIndex) and (px.EvaluatesTo=0) then
   begin
    p1:=p;
    p:=px.Target;
    px:=Sphere[p];
   end
  else
    p1:=FnCall;
  if p<>0 then
    case px.ThingType of
      ttVar,ttVarIndex:
        p:=px.EvaluatesTo;
      //ttInterface? //TODO:
    end;
  if p=0 then Result:=false else
   begin
    px:=Sphere[p];
    case px.ThingType of
      ttClass:
       begin
        //see also StratoFnCallFindInherited!
        Sphere[p1].EvaluatesTo:=p;
        q:=p;
        p:=px.FirstItem;
        while (p<>0) and (Sphere[p].ThingType<>ttConstructor) do
          p:=Sphere[p].Next;
        while (q<>0) and (p=0) or not(StratoFnArgListsMatch(Sphere,
          Sphere[p].FirstArgument,Sphere[FnCall].FirstArgument)) do
         begin
          if p=0 then
           begin
            if q<>0 then q:=Sphere[q].InheritsFrom;
            if q=0 then p:=0 else p:=Sphere[q].FirstItem;
           end
          else
            p:=Sphere[p].Next;
          while (p<>0) and (Sphere[p].ThingType<>ttConstructor) do
            p:=Sphere[p].Next;
         end;
       end;
      ttInherited:
       begin
        p:=Sphere[p].Parent;//ttCodeBlock
        if p<>0 then
          p:=StratoFnCallFindInherited(Sphere,
            Sphere[Sphere[p].Parent],Sphere[FnCall].Name);
        if (p<>0) and (Sphere[p].ThingType=ttConstructor) then
         begin
          q:=p;
          qx:=Sphere[q];
          p:=Sphere.Add(ttVarIndex,px);
          rx:=Sphere[p1];
          px.Parent:=Sphere[rx.Parent].FirstItem;//assert ttThis
          px.SrcPos:=rx.SrcPos;
          px.Target:=q;
          rx.EvaluatesTo:=qx.Parent;
         end;
       end;
      ttFunction:
       begin
        p:=Sphere[p].FirstItem;//ttOverload
        while (p<>0) and not(StratoFnArgListsMatch(Sphere,
          Sphere[Sphere[p].Target].FirstArgument,
          Sphere[FnCall].FirstArgument)) do
          p:=Sphere[p].Next;
       end;
      else
        p:=0;//error?
    end;
    if p=0 then Result:=false else
     begin
      //found! set body from matching overload
      Result:=true;
      rx:=Sphere[p1];
      if Sphere[p].ThingType=ttConstructor then
        rx.EvaluatesTo:=rx.Target;//Sphere[p].Parent;
      rx.Target:=p;
      //Sphere[FnCall].Body:=Sphere[p].Body;
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
      ttConstructor,ttDestructor,ttProperty:
        px:=Sphere[Method.Parent];
      ttOverload:
       begin
        px:=Sphere[Sphere[Method.Parent].Parent];
        if px.ThingType<>ttClass then px:=nil;
       end;
      else px:=nil;//error?
    end;
    p:=0;
    if px=nil then q:=0 else q:=px.InheritsFrom;
    while (p=0) and (q<>0) do
     begin
      case Method.ThingType of
        ttConstructor:
         begin
          p:=Sphere[q].FirstItem;
          while (p<>0) and (Sphere[p].ThingType<>ttConstructor) do
            p:=Sphere[p].Next;
         end;
        ttDestructor:
         begin
          p:=Sphere[q].FirstItem;
          while (p<>0) and (Sphere[p].ThingType<>ttDestructor) do
            p:=Sphere[p].Next;
         end;
        ttOverload,ttClass:
         begin
          p:=Sphere.Lookup(Sphere[q].FirstItem,Name);
          if (p<>0) and (Sphere[p].ThingType=ttFunction) then
            p:=Sphere[p].FirstItem
          else
            p:=0;//error?
         end;
        ttProperty:
         begin
          p:=Sphere.Lookup(Sphere[q].FirstItem,Name);
          if (p<>0) and (Sphere[p].ThingType<>ttProperty) then
            p:=0;//error?
         end;
        else p:=0;//error?
      end;
      if Method.ThingType<>ttProperty then
        while (p<>0) and not(StratoFnArgListsMatch(Sphere,
          Method.FirstArgument,Sphere[p].FirstArgument)) do
         begin
          p:=Sphere[p].Next;
          if Method.ThingType=ttConstructor then
            while (p<>0) and (Sphere[p].ThingType<>ttConstructor) do
              p:=Sphere[p].Next;
         end;
      if (p=0) and (q<>0) then
        q:=Sphere[q].InheritsFrom;
     end;
    //default to the constructor without arguments
    if (p=0) and (Method.ThingType=ttConstructor) then
     begin
      q:=px.InheritsFrom;
      while (p=0) and (q<>0) do
       begin
        p:=Sphere[q].FirstItem;
        while (p<>0) and not(
          (Sphere[p].ThingType=ttConstructor) and (Sphere[p].FirstArgument=0)) do
          p:=Sphere[p].Next;
        if (p=0) and (q<>0) then
          q:=Sphere[q].InheritsFrom;
       end;
     end;
    Result:=p;
   end;
end;

function StratoFnCallFindVirtual(Sphere:TStratoSphere;
  ImplClass,Target:PStratoThing):TStratoIndex;
var
  n:TStratoName;
  p:TStratoIndex;
  qx:PStratoThing;
begin
  //assert ImplClass<>nil and ImplClass.ThingType=ttClass
  //assert Target.ThingType=ttOverload
  qx:=Target;
  while (qx<>nil) and (qx.ThingType<>ttFunction) do //TODO: in [] like Sphere.FQN?
    qx:=Sphere[qx.Parent];
  if qx=nil then Result:=0 else
   begin
    n:=qx.Name;
    qx:=ImplClass;
    p:=0;
    while (p=0) and (qx<>nil) do
     begin
      p:=Sphere.Lookup(qx.FirstItem,n);
      if (p<>0) and (Sphere[p].ThingType=ttFunction) then
        p:=Sphere[p].FirstItem
      else
        p:=0;//error?
      while (p<>0) and not(StratoFnArgListsMatch(Sphere,
        Target.FirstArgument,Sphere[p].FirstArgument)) do
        p:=Sphere[p].Next;
      if p=0 then qx:=Sphere[qx.InheritsFrom];
     end;
    Result:=p;
   end;
end;

function StratoFnCodeBlock(Sphere:TStratoSphere;
  Parent,ThisType,ValueType:TStratoIndex;
  ValueName:TStratoName;SrcPos:cardinal):TStratoIndex;
var
  cx,px:PStratoThing;
begin
  Result:=Sphere.Add(ttCodeBlock,cx);
  cx.Parent:=Parent;
  cx.SrcPos:=SrcPos;
  //'this' inside of code block
  if ThisType<>0 then
   begin
    Sphere.AddTo(cx.FirstItem,ttThis,Sphere.Dict.StrIdx('@@'),px);
    px.Parent:=Result;
    px.SrcPos:=SrcPos;
    px.Offset:=cx.ByteSize;
    px.EvaluatesTo:=ThisType;
    inc(cx.ByteSize,SystemWordSize);
   end;
  //'value' inside of code block
  if ValueType<>0 then
   begin
    Sphere.AddTo(px.Next,ttVar,ValueName,px);
    px.Parent:=Result;
    px.SrcPos:=SrcPos;
    px.EvaluatesTo:=ValueType;
    px.Offset:=cx.ByteSize;
    inc(cx.ByteSize,ByteSize(Sphere,ValueType));
   end;
end;

procedure StratoFnArgByValues(Sphere:TStratoSphere;
  FnCall,FirstArg,FirstValue:TStratoIndex);
var
  p,q,r:TStratoIndex;
  fx,qx,rx,lx:PStratoThing;
begin
  fx:=Sphere[FnCall];
  lx:=nil;
  //assert fx.FirstArgument=0;
  p:=FirstArg;
  q:=FirstValue;
  //TODO: default argument values
  while (p<>0) and (q<>0) do
   begin
    qx:=Sphere[q];
    r:=Sphere.Add(ttArgument,rx);
    rx.Name:=qx.Name;
    rx.Parent:=FnCall;
    rx.SrcPos:=fx.SrcPos;
    rx.Target:=q;
    rx.EvaluatesTo:=ResType(Sphere,q);
    if lx=nil then fx.FirstArgument:=r else lx.Next:=r;
    lx:=rx;
    p:=Sphere[p].Next;
    q:=qx.Next; //else error?
   end;
  //if (q=0) and (p<>0) then raise?error?
end;

function FnSignature(Sphere:TStratoSphere;Fn:PStratoThing):PStratoThing;
var
  p:TStratoIndex;
begin
  p:=Fn.Target;
  while (p<>0) and (Sphere[p].ThingType=ttVarIndex) do
    p:=Sphere[p].Target;
  if (p<>0) and (Sphere[p].ThingType=ttVar) then
   begin
    p:=Sphere[p].EvaluatesTo;
    if Sphere[p].ThingType<>ttSignature then p:=0; 
   end;
  //assert Sphere[p].ThingType=ttOverload
  if p<>0 then p:=Sphere[p].Target;
  if p=0 then Result:=nil else Result:=Sphere[p];
end;

end.
