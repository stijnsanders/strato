unit stratoFn;

interface

{$D-}
{$L-}

uses stratoDecl, stratoSphere, stratoSource;

function StratoFnAdd(Sphere:TStratoSphere;Source:TStratoSource;
  MethodType:TStratoThingType;Fn,Signature:TStratoIndex;
  SrcPos:cardinal):TStratoIndex;
function StratoFnOvlCodeBlock(Sphere:TStratoSphere;Source:TStratoSource;
  FnOvl:TStratoIndex):TStratoIndex;
function StratoFnCallAddArgument(Sphere:TStratoSphere;
  FnCall,Value:TStratoIndex):TStratoIndex;
function StratoFnCallFindSignature(Sphere:TStratoSphere;
  FnCall:TStratoIndex;ThingType:TStratoThingType):boolean;
function StratoFnCallFindInherited(Sphere:TStratoSphere;
  MethodType:TStratoThingType;MethodParent,Signature:TStratoIndex;
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
  x0,x1:TStratoIndex):boolean;
var
  p0,p1:TStratoIndex;
begin
  p0:=Sphere.r(x0,tfFirstArgument);
  p1:=Sphere.r(x1,tfFirstArgument);
  //TODO: default argument values
  while (p0<>0) and (p1<>0) do
   begin
    if SameType(Sphere,Sphere.r(p0,tfEvaluatesTo),Sphere.r(p1,tfEvaluatesTo)) then
     begin
      if Sphere.t(p0)=ttArgByRef then
        IsAddressable(Sphere,Sphere.r(p1,tfTarget));//TODO: check not read-only
     end
    else
      p0:=0;//not OK, break loop
    if p0<>0 then
     begin
      p0:=Sphere.r(p0,tfNext);
      p1:=Sphere.r(p1,tfNext);
     end;
   end;
  //found or not?
  Result:=(p0=0) and (p1=0);
end;

function StratoFnAdd(Sphere:TStratoSphere;Source:TStratoSource;
  MethodType:TStratoThingType;Fn,Signature:TStratoIndex;
  SrcPos:cardinal):TStratoIndex;
var
  p,q,fn1:TStratoIndex;
begin
  //assert Signature<>0
  //assert Fn<>0
  case MethodType of
    ttOverload,ttPropertySet,ttPropertyGet:
      q:=Fn;
    ttConstructor:
     begin
      //assert sx.EvaluatesTo=0
      Sphere.s(Signature,tfEvaluatesTo,Fn);
      q:=Sphere.r(Fn,tfFirstItem);
      while (q<>0) and (Sphere.t(q)<>ttConstructors) do
        q:=Sphere.r(q,tfNext);
      if q=0 then
        q:=Sphere.Prepend(Fn,tfFirstItem,Sphere.Add(ttConstructors,
          [tfName,0
          ,tfParent,Fn
          ]));
     end;
    //ttDestructor? doesn't do overloads, just add (unique)
    else q:=0;
  end;
  if q=0 then
   begin
    Source.Error('unexpected overload subject');
    p:=Sphere.Add(MethodType,[]);
    fn1:=Fn;//?
   end
  else
   begin
    p:=0;
    fn1:=q;
    q:=Sphere.r(q,tfFirstItem);
    while q<>0 do
     begin
      if (Sphere.t(q)=MethodType) and
        SameType(Sphere,Sphere.r(q,tfSignature),Signature) then
       begin
        if Sphere.r(q,tfBody)=0 then //forward! fill in CB
          p:=q //TODO: delete/avoid superfluous Signature?
        else
          Source.Error('duplicate overload');
        q:=0;
       end
      else
      if (Sphere.t(q)=MethodType) and
        StratoFnArgListsMatch(Sphere,q,Signature) then
       begin
        q:=0;
        Source.Error('duplicate overload with equivalent arguments');
       end
      else
        q:=Sphere.r(q,tfNext);
     end;
    if p=0 then
     begin
      p:=Sphere.Add(MethodType,[]);
      Sphere.Append(fn1,tfFirstItem,p);
     end;
   end;
  Result:=p;
  Sphere.s(p,
    [tfParent,fn1
    ,tfSignature,Signature
    ,tfSrcPos,SrcPos
    ]);
end;

function StratoFnOvlCodeBlock(Sphere:TStratoSphere;Source:TStratoSource;
  FnOvl:TStratoIndex):TStratoIndex;
var
  p,q,r,Signature:TStratoIndex;
  bs:cardinal;
  tt:TStratoThingType;
begin
  Signature:=Sphere.r(FnOvl,tfSignature);
  Result:=Sphere.Add(ttCodeBlock,
    [tfParent,FnOvl
    ,tfSrcPos,Source.SrcPos
    ]);
  Sphere.s(FnOvl,tfBody,Result);
  //populate code block
  bs:=0;
  //this "@@"
  if Sphere.r(Signature,tfTarget,p) then
   begin
    q:=Sphere.Add(ttThis,
      [tfName,Sphere.Store.Dict.StrIdx('@@')
      ,tfParent,Result//CodeBlock
      ,tfSrcPos,Source.SrcPos
      ,tfOffset,bs
      ,tfEvaluatesTo,p
      ]);
    inc(bs,SystemWordSize);
    Sphere.Append(Result,tfFirstItem,q);
   end;
  //return value
  if Sphere.r(Signature,tfEvaluatesTo,p) then
   begin
    if Sphere.t(Sphere.r(FnOvl,tfParent))=ttClass then //constructor
     begin
      //with a constructor, store the effective class type here
      q:=Sphere.Add(ttVar,
        [tfName,Sphere.Store.Dict.StrIdx('?@@')
        ,tfParent,Result//CodeBlock
        ,tfSrcPos,Source.SrcPos
        ,tfOffset,bs
        ,tfEvaluatesTo,TypeDecl_type//TODO:TypeDecl_ClassRef to TypeDecl_obj
        ]);
      inc(bs,SystemWordSize);
     end
    else
     begin
      q:=Sphere.Add(ttVar,
        [tfName,Sphere.v(Sphere.r(FnOvl,tfParent),tfName)
        ,tfParent,Result//CodeBlock
        ,tfSrcPos,Sphere.v(FnOvl,tfSrcPos)
        ,tfOffset,bs
        ,tfEvaluatesTo,p
        ]);
      inc(bs,ByteSize(Sphere,p));
     end;
    Sphere.Append(Result,tfFirstItem,q);
   end;
  //arguments
  p:=Sphere.r(Signature,tfFirstArgument);
  while p<>0 do
   begin
    if Sphere.t(p)=ttArgByRef then tt:=ttVarByRef else tt:=ttVar;
    r:=Sphere.r(p,tfEvaluatesTo);
    if not Sphere.Add(Result,tfFirstItem,tt,
      [tfName,Sphere.v(p,tfName)
      ,tfParent,Result//CodeBlock
      ,tfSrcPos,Sphere.v(p,tfSrcPos)//Source.SrcPos;
      ,tfOffset,bs
      ,tfEvaluatesTo,r
      ],q) then
      Source.Error('duplicate identifier "'+string(
        Sphere.Store.Dict.Str[Sphere.v(p,tfName)])+'"');
    if tt=ttVarByRef then
      inc(bs,SystemWordSize)
    else
    if r<>0 then
      inc(bs,ByteSize(Sphere,r));
    //store first arg value on function overload index
    if Sphere.r(FnOvl,tfFirstArgument)=0 then
      Sphere.s(FnOvl,tfFirstArgument,q);
    p:=Sphere.r(p,tfNext);
   end;
  Sphere.s(Result,tfByteSize,bs);
end;

function StratoFnCallAddArgument(Sphere:TStratoSphere;
  FnCall,Value:TStratoIndex):TStratoIndex;
begin
  Result:=Sphere.Add(ttArgument,
    //tfName,Sphere[Value].Name;//TODO: if tt__Named?
    [tfParent,FnCall
    ,tfTarget,Value
    ,tfEvaluatesTo,ResType(Sphere,Value)
    ]);
  if Value<>0 then Sphere.s(Result,tfSrcPos,Sphere.v(Value,tfSrcPos));
  Sphere.Append(FnCall,tfFirstArgument,Result);
end;

function StratoFnCallFindSignature(Sphere:TStratoSphere;
  FnCall:TStratoIndex;ThingType:TStratoThingType):boolean;
var
  p,p1,q,r,rt:TStratoIndex;
begin
  //see also TStratoParser.Combine: pArgList
  //assert all Arguments added
  p:=Sphere.r(FnCall,tfTarget);
  if Sphere.t(p)=ttField then p:=Sphere.r(p,tfTarget);
  r:=0;//default
  rt:=0;//default
  while Sphere.t(p) in [ttVar,ttCast,ttField,ttArrayIndex,ttClassRef] do
    p:=Sphere.r(p,tfEvaluatesTo);
  case Sphere.t(p) of
    ttMember:
     begin
      r:=Sphere.r(p,tfFirstItem);
      while (r<>0) and not((Sphere.t(r)=ThingType) and
        StratoFnArgListsMatch(Sphere,Sphere.r(r,tfSignature),FnCall)) do
        r:=Sphere.r(r,tfNext);
      if r<>0 then rt:=Sphere.r(Sphere.r(r,tfSignature),tfEvaluatesTo);
     end;

    ttOverload:
     begin
      r:=StratoFnCallFindInherited(Sphere,ttOverload,Sphere.r(p,tfParent),
        FnCall,Sphere.v(Sphere.r(p,tfParent),tfName));
      if r<>0 then rt:=Sphere.r(Sphere.r(r,tfSignature),tfEvaluatesTo);
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
          r:=Sphere.r(p,tfFirstItem);
          while (r<>0) and (Sphere.t(r)<>ThingType) do
            r:=Sphere.r(r,tfNext);
          if ThingType=ttConstructors then
            if r<>0 then r:=Sphere.r(r,tfFirstItem);//ttConstructor
         end
        else
          if ThingType=ttConstructors then r:=Sphere.r(r,tfNext) else r:=0;
        if r=0 then p:=Sphere.r(p,tfInheritsFrom);
      until (p=0) or ((r<>0) and
        StratoFnArgListsMatch(Sphere,Sphere.r(r,tfSignature),FnCall));
     end;

    ttConstructor:
     begin
      //constructor calling inherited constructor
      r:=StratoFnCallFindInherited(Sphere,ttConstructor,Sphere.r(p,tfParent),
        FnCall,0);
      if r<>0 then
       begin
        p:=Sphere.r(FnCall,tfParent);
        while Sphere.t(p)=ttCodeBlock do p:=Sphere.r(p,tfParent);
        rt:=Sphere.rr(p,[tfParent,tfParent]);
       end;
     end;

    ttDestructor:
      r:=StratoFnCallFindInherited(Sphere,ttDestructor,Sphere.r(p,tfParent),0,0);

    //else error?
  end;
  if r=0 then Result:=false else
   begin
    p:=Sphere.r(FnCall,tfTarget);
    if Sphere.t(p)=ttField then
     begin
      p1:=p;
      p:=Sphere.r(p,tfTarget);
     end
    else
      p1:=FnCall;
    while Sphere.t(p) in [ttVar,ttCast,ttField,ttArrayIndex,ttClassRef] do
     begin
      if Sphere.t(p) in [ttVar,ttCast] then
       begin
        q:=Sphere.Add(ttField,
          [tfParent,Sphere.r(FnCall,tfParent)
          ,tfSubject,p
          ,tfSrcPos,Sphere.v(FnCall,tfSrcPos)
          //,tfEvaluatesTo?
          ]);
        //p,tfTarget//see p1:= below
        Sphere.s(p1,tfTarget,q);
        p1:=q;
       end;
      p:=Sphere.r(p,tfEvaluatesTo);
     end;
    Sphere.s(p1,tfTarget,r);
    if ThingType=ttPropertyGet then
      Sphere.s(FnCall,tfThingType,ttPropCall)
    else
      if rt<>0 then Sphere.s(FnCall,tfEvaluatesTo,rt);
    Result:=true;
   end;
end;

function StratoFnCallFindInherited(Sphere:TStratoSphere;
  MethodType:TStratoThingType;MethodParent,Signature:TStratoIndex;
  Name:TStratoName):TStratoIndex;
var
  p,q:TStratoIndex;
begin
  if MethodParent=0 then Result:=0 else
   begin
    case MethodType of
      ttOverload,ttConstructor,ttPropertyGet,ttPropertySet:
        q:=Sphere.r(MethodParent,tfParent);
      ttDestructor:
        q:=MethodParent;
      else
        q:=0;//error?
    end;
    if (q<>0) and (Sphere.t(q)<>ttClass) then q:=0;
    if q<>0 then q:=Sphere.r(q,tfInheritsFrom);
    p:=0;
    while (p=0) and (q<>0) do
     begin
      case MethodType of
        ttOverload,ttPropertyGet,ttPropertySet:
         begin
          p:=Sphere.Lookup(q,tfFirstItem,Name);
          if Sphere.t(p)=ttMember then
            p:=Sphere.r(p,tfFirstItem)
          else
            p:=0;//error?
         end;
        ttConstructor:
         begin
          p:=Sphere.r(q,tfFirstItem);
          while (p<>0) and (Sphere.t(p)<>ttConstructors) do
            p:=Sphere.r(p,tfNext);
          if p<>0 then p:=Sphere.r(p,tfFirstItem);
         end;
        ttDestructor:
         begin
          p:=Sphere.r(q,tfFirstItem);
          while (p<>0) and (Sphere.t(p)<>ttDestructor) do
            p:=Sphere.r(p,tfNext);
         end;
        else p:=0;//error?
      end;
      if MethodType<>ttDestructor then
        while (p<>0) and not((Sphere.t(p)=MethodType) and
          StratoFnArgListsMatch(Sphere,Signature,p)) do
          p:=Sphere.r(p,tfNext);
      if (p=0) and (q<>0) then q:=Sphere.r(q,tfInheritsFrom);
     end;
    Result:=p;
   end;
end;

function StratoFnCallFindVirtual(Sphere:TStratoSphere;
  ImplClass,Target:TStratoIndex):TStratoIndex;
var
  n:TStratoName;
  p,q:TStratoIndex;
  tt:TStratoThingType;
begin
  //assert ImplClass<>nil and ImplClass.ThingType=ttClass
  if Target=0 then Result:=0 else
   begin
    tt:=Sphere.t(Target);
    case tt of
      ttOverload,ttPropertyGet,ttPropertySet:
       begin
        n:=Sphere.v(Sphere.r(Target,tfParent),tfName);//ttMember
        p:=0;
        q:=ImplClass;
        while (p=0) and (q<>0) do
         begin
          p:=Sphere.Lookup(q,tfFirstItem,n);
          if (p<>0) and (Sphere.t(p)=ttMember) then
            p:=Sphere.r(p,tfFirstItem)
          else
            p:=0;//error?
          while (p<>0) and not((Sphere.t(p)=tt) and
            StratoFnArgListsMatch(Sphere,Target,p)) do
            p:=Sphere.r(p,tfNext);
          if p=0 then q:=Sphere.r(q,tfInheritsFrom);
         end;
        Result:=p;
       end;
      ttConstructor:
       begin
        p:=0;
        q:=ImplClass;
        while (p=0) and (q<>0) do
         begin
          p:=Sphere.r(q,tfFirstItem);
          while (p<>0) and (Sphere.t(p)<>ttConstructors) do
            p:=Sphere.r(p,tfNext);
          if p<>0 then p:=Sphere.r(p,tfFirstItem);
          while (p<>0) and not(StratoFnArgListsMatch(Sphere,Target,p)) do
            p:=Sphere.r(p,tfNext);
          if p=0 then q:=Sphere.r(q,tfInheritsFrom);
         end;
        Result:=p;
       end;
      ttDestructor:
       begin
        p:=0;
        q:=ImplClass;
        while (p=0) and (q<>0) do
         begin
          p:=Sphere.r(q,tfFirstItem);
          while (p<>0) and (Sphere.t(p)<>ttDestructor) do
            p:=Sphere.r(p,tfNext);
          if p=0 then q:=Sphere.r(q,tfInheritsFrom);
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
  bs:cardinal;
begin
  Result:=Sphere.Add(ttCodeBlock,
    [tfParent,Parent
    ,tfSrcPos,SrcPos
    ]);
  bs:=0;
  //'this' inside of code block
  if (ThisType<>0) and (Sphere.t(ThisType)<>ttNameSpace) then
   begin
    Sphere.Add(Result,tfFirstItem,ttThis,
      [tfName,Sphere.Store.Dict.StrIdx('@@')
      ,tfParent,Result
      ,tfSrcPos,SrcPos
      ,tfOffset,bs
      ,tfEvaluatesTo,ThisType
      ]);
    inc(bs,SystemWordSize);
   end;
  //'value' inside of code block
  if ValueType<>0 then
   begin
    Sphere.Add(Result,tfFirstItem,ttVar,
      [tfName,ValueName
      ,tfParent,Result
      ,tfSrcPos,Sphere.v(Parent,tfSrcPos)
      ,tfEvaluatesTo,ValueType
      ,tfOffset,bs
      ]);
    inc(bs,ByteSize(Sphere,ValueType));
   end;
  Sphere.s(Result,tfByteSize,bs);
end;

procedure StratoFnArgByValues(Sphere:TStratoSphere;
  FnCall,FirstArg,FirstValue:TStratoIndex);
var
  p,q,r,n:TStratoIndex;
begin
  //assert Sphere.r(FnCall,tfFirstArgument)=0;
  n:=0;
  p:=FirstArg;
  q:=FirstValue;
  //TODO: default argument values
  while (p<>0) and (q<>0) do
   begin
    r:=Sphere.Add(ttArgument,
      [tfName,Sphere.v(q,tfName)
      ,tfParent,FnCall
      ,tfSrcPos,Sphere.v(FnCall,tfSrcPos)
      ,tfTarget,q
      ,tfEvaluatesTo,ResType(Sphere,q)
      ]);
    if n=0 then Sphere.s(FnCall,tfFirstArgument,r) else Sphere.s(n,tfNext,r);
    n:=r;
    p:=Sphere.r(p,tfNext);
    q:=Sphere.r(q,tfNext);//else error?
   end;
  //if (q=0) and (p<>0) then raise?error?
end;

function StratoFindPropertySet(Sphere:TStratoSphere;
  AssignTo,PropCall:TStratoIndex;Op,SrcPos:cardinal):boolean;
var
  Getter,SetCall,Member,Signature,ParentClass,SetOp:TStratoIndex;
  p,q:TStratoIndex;
  qf:TStratoField;
  Name:TStratoName;
  st:TStratoToken;
begin
  //assert Sphere.t(PropColl)=ttPropCall
  Result:=false;//default
  //find virtual property setter
  Getter:=Sphere.r(PropCall,tfTarget);//ttPropertyGet
  if Sphere.t(Getter)=ttField then Getter:=Sphere.r(Getter,tfTarget);
  if Sphere.t(Getter)=ttPropertyGet then
   begin
    Signature:=Sphere.r(Getter,tfSignature);
    Member:=Sphere.r(Getter,tfParent);//ttMember
    Name:=Sphere.v(Member,tfName);
    ParentClass:=Sphere.r(Member,tfParent);//ttClass
    repeat
      Getter:=Sphere.r(Member,tfFirstItem);
      while (Getter<>0) and not((Sphere.t(Getter)=ttPropertySet)
        and (SameType(Sphere,Sphere.r(Getter,tfSignature),Signature))) do
        Getter:=Sphere.r(Getter,tfNext);
      if Getter=0 then
       begin
        Member:=0;
        while (ParentClass<>0) and (Member=0) do
         begin
          ParentClass:=Sphere.r(ParentClass,tfInheritsFrom);
          if ParentClass<>0 then
            Member:=Sphere.Lookup(ParentClass,tfFirstItem,Name);
         end;
       end;
    until (Getter<>0) or (ParentClass=0);
    if Getter<>0 then
     begin
      Result:=true;
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
        SetOp:=Sphere.Add(ttBinaryOp,
          [tfParent,Sphere.r(PropCall,tfParent)//cb
          ,tfSrcPos,SrcPos
          ,tfOperator,cardinal(st)
          //tfRight: see Combine pAssignment
          ]);
        Sphere.s(PropCall,tfEvaluatesTo,SetOp);
        q:=SetOp;
        qf:=tfLeft;
        p:=AssignTo;
        while (p<>0) and (p<>PropCall) do
         begin
          case Sphere.t(p) of
            ttField:
             begin
              p:=Sphere.Add(ttField,
                [tfParent,Sphere.r(p,tfParent)
                ,tfSubject,Sphere.r(p,tfSubject)
                ,tfEvaluatesTo,Sphere.r(p,tfEvaluatesTo)
                ,tfSrcPos,Sphere.v(p,tfSrcPos)
                ]);
              Sphere.s(q,qf,p);
              q:=p;
              qf:=tfTarget;
              p:=Sphere.r(p,tfTarget);
             end;
            //TODO: more?
            else
              p:=0;//Source.Error('unsupported property header');
          end;
         end;
        if p<>0 then //if p=PropCall then
         begin
          SetCall:=Sphere.Add(ttPropCall,
            [tfParent,Sphere.r(p,tfParent)
            ,tfFirstArgument,Sphere.r(p,tfFirstArgument)
            ,tfSrcPos,Sphere.v(p,tfSrcPos)
            ]);
          Sphere.s(q,qf,SetCall);
          q:=Sphere.r(p,tfTarget);
          if Sphere.t(q)=ttField then
            p:=Sphere.Add(ttField,
              [tfParent,Sphere.r(q,tfParent)
              ,tfSubject,Sphere.r(q,tfSubject)
              ,tfTarget,Sphere.r(q,tfTarget)
              ,tfEvaluatesTo,Sphere.r(q,tfEvaluatesTo)
              ,tfSrcPos,Sphere.v(q,tfSrcPos)
              ]);
          Sphere.s(SetCall,tfTarget,p);
         end
        else
          Result:=false;
       end;
      //update ttPropCall
      Sphere.s(PropCall,tfOperator,Op);
      q:=Sphere.r(PropCall,tfTarget);
      if Sphere.t(q)=ttField then
        Sphere.s(q,tfTarget,Getter)
      else
        Sphere.s(PropCall,tfTarget,Getter);
     end;
   end;
  //else Source.Error?
end;

function StratoCheckMemberNoArguments(Sphere:TStratoSphere;
  Field,Target:TStratoIndex):TStratoIndex;
var
  p,q:TStratoIndex;
begin
  Result:=Target;//default;
  if Target<>0 then
   begin
    p:=0;//default
    //TODO: while?
    case Sphere.t(Target) of
      ttMember:
        p:=Target;
      ttField:
       begin
        q:=Sphere.r(Target,tfTarget);
        if Sphere.t(q)=ttMember then p:=q;
       end;
    end;
    if p<>0 then
     begin
      q:=Sphere.r(p,tfFirstItem);
      while (q<>0) and not((Sphere.t(q)=ttOverload) and
        (Sphere.r(Sphere.r(q,tfTarget),tfFirstArgument)=0)) do
        q:=Sphere.r(q,tfNext);
      if q<>0 then
       begin
        Sphere.s(Field,
          [tfThingType,ttFnCall
          ,tfName,Sphere.v(p,tfName)//ttMember
          ,tfTarget,q
          ,tfEvaluatesTo,Sphere.r(Sphere.r(q,tfTarget),tfEvaluatesTo)//ttSignature
          ]);
        Result:=Field;
       end
      else
       begin
        q:=Sphere.r(p,tfFirstItem);
        while (q<>0) and not((Sphere.t(q)=ttPropertyGet) and
          (Sphere.r(Sphere.r(q,tfTarget),tfFirstArgument)=0)) do
          q:=Sphere.r(q,tfNext);
        if q<>0 then
         begin
          Sphere.s(Field,
            [tfThingType,ttPropCall
            ,tfTarget,q
            ,tfEvaluatesTo,Sphere.r(Sphere.r(q,tfTarget),tfEvaluatesTo)//ttSignature
            ]);
          Result:=Field;
         end;
       end;
     end;
   end;
end;

end.
