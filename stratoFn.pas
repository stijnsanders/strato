unit stratoFn;

interface

{xx$D-}
{xx$L-}

uses stratoDecl, stratoSphere, stratoSource;

function StratoFnAdd(Sphere:TStratoSphere;Source:TStratoSource;
  MethodType:xTypeNr;Member,Signature:xItem;SrcPos:xSrcPos):xItem;
function StratoFnOvlCodeBlock(Sphere:TStratoSphere;Source:TStratoSource;
  FnOvl:xItem):xItem;
procedure StratoFnCallAddArgument(Sphere:TStratoSphere;
  var ListTail:xItem;Value:xItem;SrcPos:xSrcPos);
function StratoFnCallBySignature(Sphere:TStratoSphere;
  MethodType:xTypeNr;Subject,Arguments,Parent:xItem;SrcPos:xSrcPos):xItem;
function StratoFnCallFindVirtual(Sphere:TStratoSphere;
  FnCall,SubjectType:xItem;SubjectData:PxValue):xItem;
function StratoFnCallDestructor(Sphere:TStratoSphere;Source:TStratoSource;
  CodeBlock:xItem):xItem;
function StratoFnCodeBlock(Sphere:TStratoSphere;
  Parent,ThisType,ValueType:xItem;ValueName:xName;SrcPos:xSrcPos):xItem;
procedure StratoFnArgByValues(Sphere:TStratoSphere;
  FnCall,ArgsFrom,ValuesFrom:xItem);
//function StratoFindPropertySet(Sphere:TStratoSphere;
//  AssignTo,PropCall:xItem;Op:cardinal;SrcPos:xSrcPos):boolean;
function StratoCheckMemberNoArguments(Sphere:TStratoSphere;
  Field,Target,Parent:xItem;SrcPos:xSrcPos):xItem;

implementation

uses stratoLogic, stratoRunTime, stratoTokenizer;

function StratoFnArgListsMatch(Sphere:TStratoSphere;Signature,Arguments:xItem):boolean;
var
  p,p0,q,q0:xItem;
begin
  Sphere.First(Signature,fArguments,p,p0);
  //Sphere.First(,fArguments,q,q0);
  q0:=Arguments;//assert pointer to last in cyclic list
  q:=Sphere.n(q0,fNext)^;
  Result:=true;//default
  while not((p=0) and (q=0)) do
   begin
    if SameType(Sphere,Sphere.n(p,fTypeDecl)^,Sphere.n(q,fTypeDecl)^) then
     begin
      //TODO: default argument values
      if Sphere.n(p,vTypeNr)^=nArgByRef then
        IsAddressable(Sphere,Sphere.n(q,fValue)^);//TODO: check not read-only

      //next
      Sphere.Next(p,p0);
      Sphere.Next(q,q0);
     end
    else
     begin
      p:=0;//not OK, break loop
      q:=0;
      Result:=false;
     end;
   end;
end;

function StratoFnAdd(Sphere:TStratoSphere;Source:TStratoSource;
  MethodType:xTypeNr;Member,Signature:xItem;SrcPos:xSrcPos):xItem;
var
  p,q,q0,m:xItem;
begin
  //assert Signature<>0
  //assert Fn<>0
  case MethodType of
    nOverload,nPropertyGet,nPropertySet:
      m:=Member;
    nConstructor:
     begin
      //assert Signature.Returns=nil
      Sphere.n(Signature,fReturnType)^:=Member;//n(Member,vTypeNr)=nClass
      Sphere.First(Member,fItems,q,q0);
      while (q<>0) and (Sphere.n(q,vTypeNr)^<>nConstructors) do
        Sphere.Next(q,q0);
      if q=0 then
       begin
        //prepend
        m:=Sphere.Add(nConstructors,Member,Source.SrcPos,[]);
        Sphere.Prepend(Member,fItems,m);
       end
      else
        m:=q;
     end;
    //nDestructor? doesn't do overloads, just add (unique)
    else
      m:=0;
  end;
  if m=0 then
   begin
    Source.Error('unexpected overload subject');
    Result:=Sphere.Add(MethodType,Member,0,[]);//pro-forma
   end
  else
   begin
    p:=0;
    Sphere.First(Member,fItems,q,q0);
    while q<>0 do
      if (Sphere.n(q,vTypeNr)^=MethodType) and SameType(Sphere,Sphere.n(q,fSignature)^,Signature) then
       begin
        if Sphere.n(q,fBody)^=0 then //forward! fill in CB
          p:=q //TODO: delete/avoid superfluous Signature?
        else
          Source.Error('duplicate overload');
        q:=0;
       end
      else
      if (Sphere.n(q,vTypeNr)^=MethodType) and
        StratoFnArgListsMatch(Sphere,Signature,Sphere.n(q,fArguments)^) then
       begin
        q:=0;
        Source.Error('duplicate overload with equivalent arguments');
       end
      else
        Sphere.Next(q,q0);
    if p=0 then
      p:=Sphere.Add(m,fItems,MethodType,m,SrcPos,[fSignature,Signature])
    else
     begin
      Sphere.n(p,vSrcPos)^:=SrcPos;
      Sphere.n(p,fParent)^:=m;
      Sphere.n(p,fSignature)^:=Signature;
     end;
    Result:=p;
   end;
end;

function StratoFnOvlCodeBlock(Sphere:TStratoSphere;Source:TStratoSource;
  FnOvl:xItem):xItem;
var
  cb,p,v,a,a0:xItem;
  bs,q:PxValue;
  t:xTypeNr;
begin
  cb:=Sphere.Add(nCodeBlock,FnOvl,Source.SrcPos,[]);
  bs:=Sphere.n(cb,vByteSize);
  //assert FnOvl.Body=nil
  Sphere.n(FnOvl,fBody)^:=cb;
  //populate code block
  //this "@@"
  p:=Sphere.n(Sphere.n(FnOvl,fSignature)^,fSubject)^;
  if p<>0 then
   begin
    Sphere.Add(cb,fVarDecls,nThis,cb,Source.SrcPos,
      [vName,Sphere.Store.Dict.StrIdx('@@')
      ,vOffset,bs^
      ,fTypeDecl,p
      ]);
    inc(bs^,SystemWordSize);
   end;
  //return value
  p:=Sphere.n(Sphere.n(FnOvl,fSignature)^,fReturnType)^;
  if p<>0 then
    if Sphere.n(FnOvl,vTypeNr)^=nConstructor then
     begin
      //with a constructor, store the effective class type here
      Sphere.Add(cb,fVarDecls,nVarDecl,cb,Source.SrcPos,
        [vName,Sphere.Store.Dict.StrIdx('?@@')
        ,vOffset,bs^
        ,fTypeDecl,TypeDecl_type//TODO:TypeDecl_ClassRef to TypeDecl_obj
        ]);
      inc(bs^,SystemWordSize);
     end
    else
     begin
      Sphere.Add(cb,fVarDecls,nVarDecl,cb,Sphere.n(FnOvl,vSrcPos)^,
        [vName,Sphere.n(Sphere.n(FnOvl,fParent)^,vName)^
        ,vOffset,bs^
        ,fTypeDecl,p
        ]);
      inc(bs^,ByteSize(Sphere,p));
     end;
  //arguments
  Sphere.First(Sphere.n(FnOvl,fSignature)^,fArguments,a,a0);
  while a<>0 do
   begin
    if Sphere.Lookup(cb,Sphere.n(a,vName)^)<>0 then
      Source.Error('duplicate identifier "'+string(
        Sphere.Store.Dict.Str[Sphere.n(a,vName)^])+'"');
    //TODO: force read-only
    p:=Sphere.n(a,fTypeDecl)^;
    if Sphere.n(a,vTypeNr)^=nArgByRef then t:=nVarByRef else t:=nVarDecl;
    v:=Sphere.Add(cb,fVarDecls,t,cb,Sphere.n(a,vSrcPos)^,
      [vName,Sphere.n(a,vName)^
      ,vOffset,bs^
      ,fTypeDecl,p
      ]);
    if t=nVarByRef then
      inc(bs^,SystemWordSize)
    else
    if p<>0 then
      inc(bs^,ByteSize(Sphere,p));
    //store first arg value on function overload index
    q:=Sphere.n(FnOvl,fFirstArgVar);
    if q^=0 then q^:=v;
    //next argument
    Sphere.Next(a,a0);
   end;
  Result:=cb;
end;

procedure StratoFnCallAddArgument(Sphere:TStratoSphere;
  var ListTail:xItem;Value:xItem;SrcPos:xSrcPos);
var
  p,q:xItem;
begin
  //TODO: parent from earlier Push(pArgList? (determine F/S/V-call then?)
  p:=Sphere.Add(nArgument,0,SrcPos,
    [fValue,Value
    ,fTypeDecl,ResType(Sphere,Value)
    ]);
  if ListTail=0 then //see also Sphere.Append
    Sphere.n(p,fNext)^:=p
  else
   begin
    q:=Sphere.n(ListTail,fNext)^;
    Sphere.n(ListTail,fNext)^:=p;
    Sphere.n(p,fNext)^:=q;
   end;
  ListTail:=p;
end;

function StratoFnCallBySignature(Sphere:TStratoSphere;
  MethodType:xTypeNr;Subject,Arguments,Parent:xItem;SrcPos:xSrcPos):xItem;
var
  p,p0,q,q0:xItem;
  tt:xTypeNr;
  nn:xName;
  v:boolean;
begin
  //assert Arguments 0 or last in a cyclic list (ready for fArguments)
  p:=0;
  q:=Subject;
  v:=false;//default
  while p=0 do
    case Sphere.n(q,vTypeNr)^ of
      nVarDecl,nCast:
       begin
        q:=Sphere.n(q,fTypeDecl)^;
        v:=true;
       end;
      nField:
       begin
        Subject:=Sphere.n(q,fSubject)^;//TODO: only once!!!
        q:=Sphere.n(q,fTarget)^;
        v:=true;
       end;
      nClassRef:
       begin
        q:=Sphere.n(q,fSubject)^;
        v:=true;
        if MethodType=nOverload then MethodType:=nConstructor;
       end;
      nArrayIndex:
       begin
        q:=ResType(Sphere,Sphere.n(q,fSubject)^);
        v:=true;
       end;
      nFCall,nSCall,nVCall:
       begin
        q:=ResType(Sphere,Sphere.n(q,fTarget)^);//?
        v:=true;
       end;
      nThis://assert called by combine pArgList "@@@(...": find inherited
        p:=Sphere.n(Sphere.n(q,fParent)^,fParent)^; //see also below (initial fInheritsFrom)
      else
        p:=q;
    end;
  Result:=0;//default
  q:=0;//default
  case Sphere.n(p,vTypeNr)^ of
    nMember:
     begin
      Sphere.First(p,fItems,q,q0);
      while (q<>0) and not((Sphere.n(q,vTypeNr)^=MethodType) and
        StratoFnArgListsMatch(Sphere,Sphere.n(q,fSignature)^,Arguments)) do
        Sphere.Next(q,q0);
      if q<>0 then
        case Sphere.n(Sphere.n(p,fParent)^,vTypeNr)^ of
          nClass,nInterface:
            Result:=Sphere.Add(nVCall,Parent,SrcPos,
              [fSubject,Subject
              ,fTarget,q
              ,fArguments,Arguments
              ]);
          nRecord:
            Result:=Sphere.Add(nSCall,Parent,SrcPos,
              [fSubject,Subject
              ,fTarget,q
              ,fArguments,Arguments
              ]);
          else
            Result:=Sphere.Add(nFCall,Parent,SrcPos,
              [fTarget,q
              ,fArguments,Arguments
              ]);
        end;
     end;

    //nInterface? //TODO:
    //nPropertyGet //TODO:

    nClass:
     begin
      if MethodType=nConstructor then MethodType:=nConstructors;
      repeat
        if q=0 then
         begin
          Sphere.First(p,fItems,q,q0);
          while (q<>0) and (Sphere.n(q,vTypeNr)^<>MethodType) do
            Sphere.Next(q,q0);
          if (MethodType=nConstructors) and (q<>0) then
            Sphere.First(q,fItems,q,q0);//assert Sphere.n(q,vTypeNr)=nConstructor
         end
        else
          if MethodType=nConstructors then
            Sphere.Next(q,q0)
          else
            q:=0;
        if q=0 then p:=Sphere.n(p,fInheritsFrom)^;
      until (p=0) or ((q<>0) and
        StratoFnArgListsMatch(Sphere,Sphere.n(q,fSignature)^,Arguments));
      if q<>0 then
        if v then
          Result:=Sphere.Add(nVCall,Parent,SrcPos,
            [fSubject,Subject
            ,fTarget,q
            ,fArguments,Arguments
            ])
        else
          Result:=Sphere.Add(nSCall,Parent,SrcPos,
            [fSubject,Subject
            ,fTarget,q
            ,fArguments,Arguments
            ]);
     end;

    nOverload,nConstructor,nDestructor:
     begin
      //assert p.fBody.fVarDecls.fNext.vTypeNr=nThis
      q:=Sphere.n(p,fParent)^;
      tt:=Sphere.n(p,vTypeNr)^;
      nn:=0;//default
      case tt of
        nOverload,nPropertyGet,nPropertySet:
          nn:=Sphere.n(q,vName)^;
      end;
      if tt<>nDestructor then q:=Sphere.n(q,fParent)^;
      if Sphere.n(Subject,vTypeNr)^=nThis then q:=Sphere.n(q,fInheritsFrom)^;//find inherited
      if Sphere.n(q,vTypeNr)^<>nClass then q:=0;
      p:=0;
      while (p=0) and (q<>0) do
       begin
        case tt of
          nOverload,nPropertyGet,nPropertySet:
           begin
            p:=Sphere.Lookup(q,nn);
            if Sphere.n(p,vTypeNr)^=nMember then
              Sphere.First(p,fItems,p,p0)
            else
              Sphere.None(p,p0);//error?
           end;
          nConstructor:
           begin
            Sphere.First(q,fItems,p,p0);
            while (p<>0) and (Sphere.n(p,vTypeNr)^<>nConstructors) do
              Sphere.Next(p,p0);
            if p<>0 then Sphere.First(p,fItems,p,p0);
           end;
          nDestructor:
           begin
            Sphere.First(q,fItems,p,p0);
            while (p<>0) and (Sphere.n(p,vTypeNr)^<>nDestructor) do
              Sphere.Next(p,p0);
           end;
          else
            Sphere.None(p,p0);//error?
        end;
        if tt<>nDestructor then
          while (p<>0) and not((Sphere.n(p,vTypeNr)^=tt) and
            StratoFnArgListsMatch(Sphere,Sphere.n(p,fSignature)^,Arguments)) do
            Sphere.Next(p,p0);
        if (p=0) and (q<>0) then q:=Sphere.n(q,fInheritsFrom)^;
       end;
      if p=0 then
        Result:=0
      else
        if v then
          Result:=Sphere.Add(nVCall,Parent,SrcPos,
            [fSubject,Subject
            ,fTarget,p
            ,fArguments,Arguments
            ])
        else
          Result:=Sphere.Add(nSCall,Parent,SrcPos,
            [fSubject,Subject
            ,fTarget,p
            ,fArguments,Arguments
            ]);
     end;

    nArray:
      if MethodType=nPropertyGet then //Combine:pBrackets
       begin
        //TODO: multidimensional arrays, array of array
        //if ResType(Sphere,Arguments)<>TypeDecl_number then
        //  Source.Error('argument index not a number');
        Result:=Sphere.Add(nArrayIndex,Parent,SrcPos,
          [fSubject,Subject
          ,fItems,Arguments
          ]);
       end;

    //else error?
  end;
end;

function StratoFnCallFindVirtual(Sphere:TStratoSphere;
  FnCall,SubjectType:xItem;SubjectData:PxValue):xItem;
var
  p,p0,q,Arguments:xItem;
  xp:PxValue;
  tt:xTypeNr;
  nn:xName;
begin
  xp:=nil;//default
  case Sphere.n(SubjectType,vTypeNr)^ of
    nClassRef,nRecord,nInterface:
      xp:=SubjectData;
    nClass:
     begin
      //instance? get instance class
      xp:=pointer(SubjectData^);
      //inc(xPtr(xp),Sphere.n(Sphere.Lookup(TypeDecl_object,Sphere.Store.Dict['_baseclass']),vOffset)^);
      dec(xPtr(xp),SystemWordSize);//assert object._baseclass offset -4
     end;
  end;
  if xp=nil then
    Result:=0
  else
   begin
    //lookup
    p:=Sphere.n(FnCall,fTarget)^;
    Arguments:=Sphere.n(FnCall,fArguments)^;
    tt:=Sphere.n(p,vTypeNr)^;
    nn:=0;//default
    case tt of
      nOverload,nPropertyGet,nPropertySet:
        nn:=Sphere.n(Sphere.n(p,fParent)^,vName)^;
    end;
    p:=0;
    q:=PxValue(xp)^;
    while (p=0) and (q<>0) do
     begin
      if tt=nConstructor then
       begin
        Sphere.First(q,fItems,p,p0);
        while (p<>0) and (Sphere.n(p,vTypeNr)^<>nConstructors) do
          Sphere.Next(p,p0);
        if p<>0 then Sphere.First(p,fItems,p,p0);
       end
      else
       begin
        p:=Sphere.Lookup(q,nn);
        if Sphere.n(p,vTypeNr)^=nMember then
          Sphere.First(p,fItems,p,p0)
        else
          Sphere.None(p,p0);//error?
       end;
      while (p<>0) and not((Sphere.n(p,vTypeNr)^=tt) and
        StratoFnArgListsMatch(Sphere,Sphere.n(p,fSignature)^,Arguments)) do
        Sphere.Next(p,p0);
      if p=0 then q:=Sphere.n(q,fInheritsFrom)^;
     end;
    Result:=p;
   end;
end;

function StratoFnCallDestructor(Sphere:TStratoSphere;Source:TStratoSource;
  CodeBlock:xItem):xItem;
var
  p,pThis,q,q0:xItem;
begin
  q:=CodeBlock;
  p:=0;
  while q<>0 do
   begin
    //p:=xxLookup(q,Store.Dict.StrIdx('@@'));
    Sphere.First(q,fVarDecls,p,q0);
    if Sphere.n(p,vTypeNr)^=nThis then
      q:=0
    else
     begin
      p:=0;
      q:=Sphere.n(q,fParent)^;
      if Sphere.n(q,vTypeNr)^<>nCodeBlock then q:=0;
     end;
   end;
  if p=0 then
   begin
    Source.Error('"@@" undefined');
    Result:=0;//?Sphere.Add(nFnCall?
   end
  else
   begin
    //find destructor (see also StratoFnCallFindInherited
    pThis:=p;//see Add(nVCall below)
    p:=Sphere.n(p,fTypeDecl)^;
    q:=0;
    while (p<>0) and (q=0) do
     begin
      Sphere.First(p,fItems,q,q0);
      while (q<>0) and (Sphere.n(q,vTypeNr)^<>nDestructor) do Sphere.Next(q,q0);
      if q=0 then p:=Sphere.n(p,fInheritsFrom)^;
     end;
    if q=0 then
      Source.Error('unable to determine destructor');
    //then call it
    Result:=Sphere.Add(nVCall,CodeBlock,Source.SrcPos,[fSubject,pThis,fTarget,q]);
    Source.Skip(stPOpen);
    Source.Skip(stPClose);
   end;
end;

function StratoFnCodeBlock(Sphere:TStratoSphere;
  Parent,ThisType,ValueType:xItem;
  ValueName:xName;SrcPos:xSrcPos):xItem;
var
  cb:xItem;
  bs:PxValue;
begin
  cb:=Sphere.Add(nCodeBlock,Parent,SrcPos,[]);
  bs:=Sphere.n(cb,vByteSize);
  //'this' inside of code block
  if (ThisType<>0) and (Sphere.n(ThisType,vTypeNr)^<>nNameSpace) then
   begin
    Sphere.Add(cb,fVarDecls,nThis,cb,SrcPos,
      [vName,Sphere.Store.Dict.StrIdx('@@')
      ,vOffset,bs^
      ,fTypeDecl,ThisType
      ]);
    inc(bs^,SystemWordSize);
   end;
  //'value' inside of code block
  if ValueType<>0 then
   begin
    Sphere.Add(cb,fVarDecls,nVarDecl,cb,Sphere.n(Parent,vSrcPos)^,
      [vName,ValueName
      ,fTypeDecl,ValueType
      ,vOffset,bs^
      ]);
    inc(bs^,ByteSize(Sphere,ValueType));
   end;
  Result:=cb;
end;

procedure StratoFnArgByValues(Sphere:TStratoSphere;
  FnCall,ArgsFrom,ValuesFrom:xItem);
var
  p,p0,q:xItem;
begin
  //assert FnCall.FirstArg=nil
  Sphere.First(Sphere.n(ArgsFrom,fSignature)^,fArguments,p,p0);
  q:=Sphere.n(ValuesFrom,fFirstArgVar)^;
  //TODO: default argument values
  while (p<>0) and (q<>0) do
   begin
    Sphere.Add(FnCall,fArguments,nArgument,FnCall,Sphere.n(FnCall,vSrcPos)^,
      [vName,Sphere.n(q,vName)^
      ,fValue,q
      ,fTypeDecl,ResType(Sphere,q)
      ]);
    Sphere.Next(p,p0);
    q:=Sphere.n(q,fNext)^;
   end;
  //if (p<>0) and (q=0) then raise?error?
end;

{
function StratoFindPropertySet(Sphere:TStratoSphere;
  AssignTo,PropCall:xItem;Op:cardinal;SrcPos:xSrcPos):boolean;
var
  Getter,Getter0,Member,Signature,ParentClass,SetCall,SetOp,p,q,r,r0:xItem;
  Name:xName;
  st:TStratoToken;
begin
  //assert Sphere.n(PropCall,vTypeNr)^ in [nPropGetCall,nPropSetCall]
  Result:=false;//default
  //find virtual property setter
  Getter:=Sphere.n(PropCall,fParent)^;
  if Sphere.n(Getter,vTypeNr)^=nField then Getter:=Sphere.n(Getter,fTarget)^;
  if Sphere.n(Getter,vTypeNr)^=nPropertyGet then
   begin
    Signature:=Sphere.n(Getter,fSignature)^;
    Member:=Sphere.n(Getter,fParent)^;
    Name:=Sphere.n(Member,vName)^;
    ParentClass:=Sphere.n(Member,fParent)^;
    repeat
      Sphere.First(Member,fItems,Getter,Getter0);
      while (Getter<>0) and not((Sphere.n(Getter,vTypeNr)^=nPropertySet)
        and (SameType(Sphere,Sphere.n(Getter,fSignature)^,Signature))) do
        Sphere.Next(Getter,Getter0);
      if Getter=0 then
       begin
        Member:=0;
        while (ParentClass<>0) and (Member=0) do
         begin
          ParentClass:=Sphere.n(ParentClass,fInheritsFrom)^;
          if ParentClass<>0 then Member:=Sphere.Lookup(ParentClass,Name);
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
        //duplicate nPropCall, insert nBinaryOp
        SetOp:=Sphere.Add(nBinaryOp,Sphere.n(PropCall,fParent)^,SrcPos,
          [vOperator,xValue(st)
          //fRight: see Combine pAssignment
          ]);
        q:=0;
        p:=AssignTo;
        while (p<>0) and (p<>PropCall) do
         begin
          case Sphere.n(p,vTypeNr)^ of
            nField:
             begin
              r:=Sphere.Add(nField,Sphere.n(p,fParent)^,Sphere.n(p,vSrcPos)^,
                [fSubject,Sphere.n(p,fSubject)^
                ]);
              if q=0 then Sphere.n(SetOp,fLeft)^:=r else Sphere.n(q,fTarget)^:=r;
              q:=r;
              p:=Sphere.n(p,fTarget)^;
             end;
            //TODO: more?
            else
              p:=0;//Source.Error('unsupported property header');
          end;
         end;
        if p<>0 then //if p is PropCall then
         begin
          SetCall:=Sphere.Add(nFnCall,Sphere.n(p,fParent)^,Sphere.n(p,vSrcPos)^,[]);
          Sphere.First(p,fArguments,r,r0);
          Sphere.Append(SetCall,fArguments,r);
          if q=0 then Sphere.n(SetOp,fLeft)^:=SetCall else Sphere.n(q,fTarget)^:=SetCall;
          q:=Sphere.n(p,fSubject)^;
          if Sphere.n(q,vTypeNr)^=nField then
            p:=Sphere.Add(nField,Sphere.n(q,fParent)^,Sphere.n(q,vSrcPos)^,
              [fSubject,Sphere.n(q,fSubject)^
              ,fTarget,Sphere.n(q,fTarget)^
              ]);
          Sphere.n(SetCall,fTarget)^:=p;
         end
        else
          Result:=false;
       end;
      //update nPropCall
      q:=Sphere.n(PropCall,fSubject)^;
      if Sphere.n(q,vTypeNr)^=nField then
        Sphere.n(q,fSubject)^:=Getter
      else
        Sphere.n(PropCall,fSubject)^:=Getter;
     end;
   end;
  //else Source.Error?
end;
}

function StratoCheckMemberNoArguments(Sphere:TStratoSphere;
  Field,Target,Parent:xItem;SrcPos:xSrcPos):xItem;
var
  q,q0:xItem;
begin
  Result:=Target;//default
  if Target<>0 then
    case Sphere.n(Target,vTypeNr)^ of
      nMember:
       begin
        Sphere.First(Target,fItems,q,q0);
        while (q<>0) and not(
          ((Sphere.n(q,vTypeNr)^=nOverload) or (Sphere.n(q,vTypeNr)^=nPropertyGet)) and
          //(Sphere.n(q,fFirstArgVar)^=0)) do
          (Sphere.n(Sphere.n(q,fSignature)^,fArguments)^=0)
        ) do
          Sphere.Next(q,q0);
        if q<>0 then
          Result:=Sphere.Add(nFCall,Parent,SrcPos,[fTarget,q]);

        //else nPGetCall?
       end;

      //else//more?
    end;
end;

end.
