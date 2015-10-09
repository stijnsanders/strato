unit stratoFn;

interface

{xx$D-}
{xx$L-}

uses stratoDecl, stratoSphere, stratoSource;

function StratoFnAdd(Sphere:TStratoSphere;Source:TStratoSource;
  MethodType:TStratoThingType;Fn,Signature,SourceFile:TStratoIndex;
  SrcPos:cardinal):TStratoIndex;
function StratoFnOvlCodeBlock(Sphere:TStratoSphere;Source:TStratoSource;
  FnOvl:TStratoIndex):TStratoIndex;
function StratoFnCallAddArgument(Sphere:TStratoSphere;
  FnCall,Value:TStratoIndex;var Info:PStratoThing):TStratoIndex;
function StratoFnCallFindSignature(Sphere:TStratoSphere;
  FnCall:TStratoIndex;ThingType:TStratoThingType):boolean;
function StratoFnCallFindInherited(Sphere:TStratoSphere;
  MethodType:TStratoThingType;MethodParent,FirstArg:TStratoIndex;
  Name:TStratoName):TStratoIndex;
function StratoFnCallFindVirtual(Sphere:TStratoSphere;
  ImplClass,Target:TStratoIndex):TStratoIndex;
function StratoFnCodeBlock(Sphere:TStratoSphere;
  Parent,ThisType,ValueType:TStratoIndex;
  ValueName:TStratoName;SrcPos:cardinal):TStratoIndex;
procedure StratoFnArgByValues(Sphere:TStratoSphere;
  FnCall,FirstArg,FirstValue:TStratoIndex);
function StratoFindPropertySet(Sphere:TStratoSphere;
  AssignTo,PropCall:TStratoIndex;Op,SrcPos:cardinal):boolean;
function StratoCheckMemberNoArguments(Sphere:TStratoSphere;
  Field,Target:TStratoIndex):TStratoIndex;

implementation

uses stratoLogic, stratoRunTime, stratoTokenizer;

function StratoFnArgListsMatch(Sphere:TStratoSphere;
  Arg0,Arg1:TStratoIndex):boolean;
var
  p0,p1:TStratoIndex;
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
        IsAddressable(Sphere,x1.Target);//TODO: check not read-only
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

function StratoFnAdd(Sphere:TStratoSphere;Source:TStratoSource;
  MethodType:TStratoThingType;Fn,Signature,SourceFile:TStratoIndex;
  SrcPos:cardinal):TStratoIndex;
var
  p,q,fn1:TStratoIndex;
  px,qx,sx:PStratoThing;
begin
  //assert Signature<>0
  //assert Fn<>0
  sx:=Sphere[Signature];
  case MethodType of
    ttOverload,ttPropertySet,ttPropertyGet:q:=Fn;
    ttConstructor:
     begin
      //assert sx.EvaluatesTo=0
      sx.EvaluatesTo:=Fn;
      q:=Sphere[Fn].FirstItem;
      while (q<>0) and (Sphere[q].ThingType<>ttConstructors) do
        q:=Sphere[q].Next;
      if q=0 then
       begin
        q:=Sphere.AddTo(Sphere[Fn].FirstItem,ttConstructors,0,px);
        px.Parent:=Fn;
       end;
     end;
    //ttDestructor? doesn't do overloads, just add (unique)
    else q:=0;
  end;
  if q=0 then
   begin
    Source.Error('unexpected overload subject');
    p:=Sphere.Add(MethodType,px);//
    fn1:=Fn;//?
   end
  else
   begin
    p:=0;
    fn1:=q;
    q:=Sphere[q].FirstItem;
    while (q<>0) do
     begin
      qx:=Sphere[q];
      if (qx.ThingType=MethodType) and
        SameType(Sphere,qx.Target,Signature) then
       begin
        if qx.Body=0 then //forward! fill in CB
          p:=q //TODO: delete/avoid superfluous Signature?
        else
          Source.Error('duplicate overload');
        q:=0;
       end
      else
      if (qx.ThingType=MethodType) and StratoFnArgListsMatch(Sphere,
          qx.FirstArgument,sx.FirstArgument) then
       begin
        q:=0;
        Source.Error('duplicate overload with equivalent arguments');
       end
      else
        q:=qx.Next;
     end;
    if p=0 then
     begin
      p:=Sphere.Add(MethodType,px);
      Sphere.Append(Sphere[fn1].FirstItem,p);
     end
    else
      px:=Sphere[p];
   end;
  Result:=p;
  px.Parent:=fn1;
  px.Target:=Signature;
  px.SourceFile:=SourceFile;
  px.SrcPos:=SrcPos;
end;

function StratoFnOvlCodeBlock(Sphere:TStratoSphere;Source:TStratoSource;
  FnOvl:TStratoIndex):TStratoIndex;
var
  p,q:TStratoIndex;
  px,qx,rx,ox,sx:PStratoThing;
  bs:cardinal;
  tt:TStratoThingType;
begin
  ox:=Sphere[FnOvl];
  sx:=Sphere[ox.Target];//ttSignature
  Result:=Sphere.Add(ttCodeBlock,rx);
  rx.Parent:=FnOvl;
  rx.SrcPos:=Source.SrcPos;
  ox.Body:=Result;
  //populate code block
  bs:=0;//bs:=cx.ByteSize;
  //this "@@"
  if sx.Target<>0 then
   begin
    q:=Sphere.Add(ttThis,qx);
    qx.Name:=Sphere.Dict.StrIdx('@@');
    qx.Parent:=Result;//CodeBlock;
    qx.SrcPos:=Source.SrcPos;
    qx.Offset:=bs;
    qx.EvaluatesTo:=sx.Target;
    inc(bs,SystemWordSize);
    Sphere.Append(rx.FirstItem,q);
   end;
  //return value
  if sx.EvaluatesTo<>0 then
    if Sphere[ox.Parent].ThingType=ttClass then //constructor
     begin
      //with a constructor, store the effective class type here
      q:=Sphere.Add(ttVar,qx);
      qx.Name:=Sphere.Dict.StrIdx('?@@');
      qx.Parent:=Result;//CodeBlock;
      qx.SrcPos:=Source.SrcPos;
      qx.Offset:=bs;
      qx.EvaluatesTo:=TypeDecl_type;//TODO:TypeDecl_ClassRef to TypeDecl_obj
      inc(bs,SystemWordSize);
      Sphere.Append(rx.FirstItem,q);
     end
    else
     begin
      q:=Sphere.Add(ttVar,qx);
      qx.Name:=Sphere[ox.Parent].Name;
      qx.Parent:=Result;//CodeBlock;
      qx.SrcPos:=ox.SrcPos;
      qx.Offset:=bs;
      qx.EvaluatesTo:=sx.EvaluatesTo;
      if qx.EvaluatesTo<>0 then
        inc(bs,ByteSize(Sphere,qx.EvaluatesTo));
      Sphere.Append(rx.FirstItem,q);
     end;
  //arguments
  p:=sx.FirstArgument;
  while p<>0 do
   begin
    px:=Sphere[p];
    if px.ThingType=ttArgByRef then tt:=ttVarByRef else tt:=ttVar;
    q:=Sphere.AddTo(rx.FirstItem,tt,px.Name,qx);
    if q=0 then
      Source.Error('duplicate identifier "'+string(Sphere.Dict.Str[px.Name])+'"')
    else
     begin
      qx.Parent:=Result;//CodeBlock
      qx.SrcPos:=px.SrcPos;//Source.SrcPos;
      qx.Offset:=bs;
      qx.EvaluatesTo:=px.EvaluatesTo;
      if tt=ttVarByRef then
        inc(bs,SystemWordSize)
      else
      if qx.EvaluatesTo<>0 then
        inc(bs,ByteSize(Sphere,qx.EvaluatesTo));
      //store first arg value on function overload index
      if ox.FirstArgument=0 then ox.FirstArgument:=q;
     end;
    p:=px.Next;
   end;
  rx.ByteSize:=bs;
end;

function StratoFnCallAddArgument(Sphere:TStratoSphere;
  FnCall,Value:TStratoIndex;var Info:PStratoThing):TStratoIndex;
begin
  Result:=Sphere.Add(ttArgument,Info);
  //Info.Name:=Sphere[Value].Name;//TODO: if tt__Named?
  Info.Parent:=FnCall;
  Info.Target:=Value;
  Info.EvaluatesTo:=ResType(Sphere,Value);
  if Value<>0 then Info.SrcPos:=Sphere[Value].SrcPos;
  Sphere.Append(Sphere[FnCall].FirstArgument,Result);
end;

function StratoFnCallFindSignature(Sphere:TStratoSphere;
  FnCall:TStratoIndex;ThingType:TStratoThingType):boolean;
var
  p,p1,q,r,rt:TStratoIndex;
  px,qx,fx:PStratoThing;
begin
  //see also TStratoParser.Combine: pArgList
  //assert all Arguments added
  fx:=Sphere[FnCall];
  p:=fx.Target;
  if Sphere[p].ThingType=ttField then p:=Sphere[p].Target;
  r:=0;//default
  rt:=0;//default
  while (p<>0) and (Sphere[p].ThingType in
    [ttVar,ttCast,ttField,ttArrayIndex,ttClassRef]) do
    p:=Sphere[p].EvaluatesTo;
  px:=Sphere[p];
  case px.ThingType of
    ttMember:
     begin
      r:=Sphere[p].FirstItem;
      while (r<>0) and not((Sphere[r].ThingType=ThingType) and
        StratoFnArgListsMatch(Sphere,
          Sphere[Sphere[r].Target].FirstArgument,fx.FirstArgument)) do
        r:=Sphere[r].Next;
      if r<>0 then rt:=Sphere[Sphere[r].Target].EvaluatesTo;
     end;

    ttOverload:
     begin
      r:=StratoFnCallFindInherited(Sphere,ttOverload,Sphere[p].Parent,
        fx.FirstArgument,Sphere[Sphere[p].Parent].Name);
      if r<>0 then rt:=Sphere[Sphere[r].Target].EvaluatesTo;
     end;

    //ttInterface? //TODO:

    ttClass:
     begin
      //see also StratoFnCallFindInherited!
      r:=0;
      case ThingType of
        ttOverload:
         begin
          ThingType:=ttConstructors;
          rt:=p;
         end;
        ttDestructor:;//
        else ThingType:=0;//Source.Error(...
      end;
      repeat
        if r=0 then
         begin
          r:=Sphere[p].FirstItem;
          while (r<>0) and (Sphere[r].ThingType<>ThingType) do
            r:=Sphere[r].Next;
          if ThingType=ttConstructors then
            if r<>0 then r:=Sphere[r].FirstItem;//ttConstructor
         end
        else
          if ThingType=ttConstructors then r:=Sphere[r].Next else r:=0;
        if r=0 then p:=Sphere[p].InheritsFrom;
      until (p=0) or ((r<>0) and StratoFnArgListsMatch(Sphere,
        Sphere[Sphere[r].Target].FirstArgument,fx.FirstArgument));
     end;

    ttConstructor:
     begin
      //constructor calling inherited constructor
      r:=StratoFnCallFindInherited(Sphere,ttConstructor,Sphere[p].Parent,
        fx.FirstArgument,0);
      if r<>0 then
       begin
        p:=fx.Parent;
        while (p<>0) and (Sphere[p].ThingType=ttCodeBlock) do
          p:=Sphere[p].Parent;
        rt:=p;
       end;
     end;

    ttDestructor:
      r:=StratoFnCallFindInherited(Sphere,ttDestructor,Sphere[p].Parent,0,0);

    //else error?
  end;
  if r=0 then Result:=false else
   begin
    p:=fx.Target;
    if (p<>0) and (px.ThingType=ttField) then
     begin
      p1:=p;
      p:=Sphere[p].Target;
     end
    else
      p1:=FnCall;
    while (p<>0) and (Sphere[p].ThingType in
      [ttVar,ttCast,ttField,ttArrayIndex,ttClassRef]) do
     begin
      px:=Sphere[p];
      if Sphere[p].ThingType in [ttVar,ttCast] then
       begin
        q:=Sphere.Add(ttField,qx);
        qx.Parent:=fx.Parent;
        qx.Subject:=p;
        qx.SrcPos:=fx.SrcPos;
        //px.Target:=//see p1:= below
        //qx.EvaluatesTo:=?
        Sphere[p1].Target:=q;
        p1:=q;
       end;
      p:=px.EvaluatesTo;
     end;
    Sphere[p1].Target:=r;
    if ThingType=ttPropertyGet then
      fx.ThingType:=ttPropCall
    else
      if rt<>0 then fx.EvaluatesTo:=rt;
    Result:=true;
   end;
end;

function StratoFnCallFindInherited(Sphere:TStratoSphere;
  MethodType:TStratoThingType;MethodParent,FirstArg:TStratoIndex;
  Name:TStratoName):TStratoIndex;
var
  p,q:TStratoIndex;
  px:PStratoThing;
begin
  if MethodParent=0 then Result:=0 else
   begin
    case MethodType of
      ttOverload,ttPropertyGet,ttPropertySet:
       begin
        px:=Sphere[Sphere[MethodParent].Parent];
        if px.ThingType<>ttClass then px:=nil;
       end;
      ttConstructor:
        px:=Sphere[Sphere[MethodParent].Parent];//ttClass
      ttDestructor:
        px:=Sphere[MethodParent];
      else
        px:=nil;//error?
    end;
    p:=0;
    if (px=nil) or (px.ThingType=0) then q:=0 else q:=px.InheritsFrom;
    while (p=0) and (q<>0) do
     begin
      case MethodType of
        ttOverload,ttPropertyGet,ttPropertySet:
         begin
          p:=Sphere.Lookup(Sphere[q].FirstItem,Name);
          if (p<>0) and (Sphere[p].ThingType=ttMember) then
            p:=Sphere[p].FirstItem
          else
            p:=0;//error?
         end;
        ttConstructor:
         begin
          p:=Sphere[q].FirstItem;
          while (p<>0) and (Sphere[p].ThingType<>ttConstructors) do
            p:=Sphere[p].Next;
          if p<>0 then p:=Sphere[p].FirstItem;
         end;
        ttDestructor:
         begin
          p:=Sphere[q].FirstItem;
          while (p<>0) and (Sphere[p].ThingType<>ttDestructor) do
            p:=Sphere[p].Next;
         end;
        else p:=0;//error?
      end;
      if MethodType<>ttDestructor then
        while (p<>0) and not((Sphere[p].ThingType=MethodType) and
          StratoFnArgListsMatch(Sphere,
            FirstArg,Sphere[p].FirstArgument)) do
          p:=Sphere[p].Next;
      if (p=0) and (q<>0) then
        q:=Sphere[q].InheritsFrom;
     end;
    Result:=p;
   end;
end;

function StratoFnCallFindVirtual(Sphere:TStratoSphere;
  ImplClass,Target:TStratoIndex):TStratoIndex;
var
  n:TStratoName;
  p,q:TStratoIndex;
  tx:PStratoThing;
  tt:TStratoThingType;
begin
  //assert ImplClass<>nil and ImplClass.ThingType=ttClass
  if Target=0 then Result:=0 else
   begin
    tx:=Sphere[Target];
    tt:=tx.ThingType;
    case tt of
      ttOverload,ttPropertyGet,ttPropertySet:
       begin
        n:=Sphere[tx.Parent].Name;//ttMember
        p:=0;
        q:=ImplClass;
        while (p=0) and (q<>0) do
         begin
          p:=Sphere.Lookup(Sphere[q].FirstItem,n);
          if (p<>0) and (Sphere[p].ThingType=ttMember) then
            p:=Sphere[p].FirstItem
          else
            p:=0;//error?
          while (p<>0) and not((Sphere[p].ThingType=tt) and
            StratoFnArgListsMatch(Sphere,
              tx.FirstArgument,Sphere[p].FirstArgument)) do
            p:=Sphere[p].Next;
          if p=0 then q:=Sphere[q].InheritsFrom;
         end;
        Result:=p;
       end;
      ttConstructor:
       begin
        q:=ImplClass;
        p:=0;
        while (p=0) and (q<>0) do
         begin
          p:=Sphere[q].FirstItem;
          while (p<>0) and (Sphere[p].ThingType<>ttConstructors) do
            p:=Sphere[p].Next;
          if p<>0 then p:=Sphere[p].FirstItem;
          while (p<>0) and not(StratoFnArgListsMatch(Sphere,
            Sphere[Target].FirstArgument,Sphere[p].FirstArgument)) do
            p:=Sphere[p].Next;
          if p=0 then q:=Sphere[q].InheritsFrom;
         end;
        Result:=p;
       end;
      ttDestructor:
       begin
        q:=ImplClass;
        p:=0;
        while (p=0) and (q<>0) do
         begin
          p:=Sphere[q].FirstItem;
          while (p<>0) and (Sphere[p].ThingType<>ttDestructor) do
            p:=Sphere[p].Next;
          if p=0 then q:=Sphere[q].InheritsFrom;
         end;
        Result:=p;
       end;
      else
        Result:=0;//raise?
    end;
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
  if (ThisType<>0) and (Sphere[ThisType].ThingType<>ttNameSpace) then
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
    Sphere.AddTo(cx.FirstItem,ttVar,ValueName,px);
    px.Parent:=Result;
    px.SrcPos:=Sphere[Parent].SrcPos;
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

function StratoFindPropertySet(Sphere:TStratoSphere;
  AssignTo,PropCall:TStratoIndex;Op,SrcPos:cardinal):boolean;
var
  p,q,f,s:TStratoIndex;
  px,qx,rx,qx1,rx1:PStratoThing;
  n:TStratoName;
  st:TStratoToken;
  p1:^TStratoIndex;
begin
  //assert Sphere[PropColl].ThingType=ttPropCall
  Result:=false;//default
  px:=Sphere[PropCall];
  //find virtual property setter
  qx:=Sphere[px.Target];//ttPropertyGet
  if qx.ThingType=ttField then qx:=Sphere[qx.Target];
  if qx.ThingType=ttPropertyGet then
   begin
    s:=qx.Target;//ttSignature
    f:=qx.Parent;//ttMember
    rx:=Sphere[f];
    n:=rx.Name;
    q:=rx.Parent;//ttClass
    repeat
      p:=Sphere[f].FirstItem;
      while (p<>0) and not((Sphere[p].ThingType=ttPropertySet)
        and (SameType(Sphere,Sphere[p].Target,s))) do
        p:=Sphere[p].Next;
      if p=0 then
       begin
        f:=0;
        while (q<>0) and (f=0) do
         begin
          q:=Sphere[q].InheritsFrom;
          if q<>0 then
            f:=Sphere.Lookup(Sphere[q].FirstItem,n);
         end;
       end;
    until (p<>0) or (q=0);
    if p<>0 then
     begin
      Result:=true;
      //px:=Sphere[PropCall];//see above
      st:=st_Unknown;
      case TStratoToken(Op) of
        stOpAssign:;//st:=st_Unknown;
        stOpAssignAdd:st:=stOpAdd;
        stOpAssignSub:st:=stOpSub;
        stOpAssignMul:st:=stOpMul;
        stOpAssignDiv:st:=stOpDiv;
        stOpAssignMod:st:=stOpMod;
        stOpAssignOr :st:=stOpOr;
        stOpAssignAnd:st:=stOpAnd;
        //else error?
      end;
      if st<>st_Unknown then
       begin
        //duplicate ttPropCall, insert ttBinaryOp
        q:=AssignTo;
        px.EvaluatesTo:=Sphere.Add(ttBinaryOp,rx);
        rx.Parent:=px.Parent;//cb
        rx.SrcPos:=SrcPos;
        rx.Op:=cardinal(st);
        //rx.Right: see Combine pAssignment
        p1:=@rx.Left;
        while (q<>0) and (q<>PropCall) do
         begin
          qx:=Sphere[q];
          case qx.ThingType of
            ttField:
             begin
              p1^:=Sphere.Add(ttField,rx);
              rx.Parent:=qx.Parent;
              rx.Subject:=qx.Subject;
              rx.EvaluatesTo:=qx.EvaluatesTo;
              rx.SrcPos:=qx.SrcPos;
              p1:=@rx.Target;
              q:=qx.Target;
             end;
            //TODO: more?
            else
              q:=0;//Source.Error('unsupported property header');
          end;
         end;
        if q<>0 then //if q=PropCall then
         begin
          qx:=Sphere[q];
          p1^:=Sphere.Add(ttPropCall,rx);
          rx.Parent:=qx.Parent;
          if (qx.Target<>0) and (Sphere[qx.Target].ThingType=ttField) then
           begin
            qx1:=Sphere[qx.Target];
            rx.Target:=Sphere.Add(ttField,rx1);
            rx1.Parent:=qx1.Parent;
            rx1.Subject:=qx1.Subject;
            rx1.Target:=qx1.Target;
            rx1.EvaluatesTo:=qx1.EvaluatesTo;
            rx1.SrcPos:=qx1.SrcPos;
           end
          else
            rx.Target:=qx.Target;
          rx.FirstArgument:=qx.FirstArgument;
          rx.SrcPos:=qx.SrcPos;
         end
        else
          Result:=false;
       end;
      //update ttPropCall
      px.Op:=Op;
      qx:=Sphere[px.Target];
      if qx.ThingType=ttField then qx.Target:=p else px.Target:=p;
     end;
   end;
  //else Source.Error?
end;

function StratoCheckMemberNoArguments(Sphere:TStratoSphere;
  Field,Target:TStratoIndex):TStratoIndex;
var
  p,q:TStratoIndex;
  px:PStratoThing;
begin
  Result:=Target;//default;
  if Target<>0 then
   begin
    p:=0;//default
    px:=Sphere[Target];
    //TODO: while?
    case px.ThingType of
      ttMember:p:=Target;
      ttField:
        if Sphere[px.Target].ThingType=ttMember then p:=px.Target;
    end;
    if p<>0 then
     begin
      q:=Sphere[p].FirstItem;
      while (q<>0) and not((Sphere[q].ThingType=ttOverload) and
        (Sphere[Sphere[q].Target].FirstArgument=0)) do
        q:=Sphere[q].Next;
      if q<>0 then
       begin
        px:=Sphere[Field];
        px.ThingType:=ttFnCall;
        px.Name:=Sphere[p].Name;//ttMember
        px.Target:=q;
        px.EvaluatesTo:=Sphere[Sphere[q].Target].EvaluatesTo;//ttSignature
        Result:=Field;
       end
      else
       begin
        q:=Sphere[p].FirstItem;
        while (q<>0) and not((Sphere[q].ThingType=ttPropertyGet) and
          (Sphere[Sphere[q].Target].FirstArgument=0)) do
          q:=Sphere[q].Next;
        if q<>0 then
         begin
          px:=Sphere[Field];
          px.ThingType:=ttPropCall;
          px.Target:=q;
          px.EvaluatesTo:=Sphere[Sphere[q].Target].EvaluatesTo;//ttSignature
          Result:=Field;
         end;
       end;
     end;
   end;
end;

end.
