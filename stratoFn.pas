unit stratoFn;

interface

{$D-}
{$L-}

uses stratoDecl, stratoSphere, stratoParse;

function StratoFnAdd(Parser:TStratoParser;MethodType:xTypeNr;
  Member,Signature:rItem;SrcPos:xSrcPos):rItem;
procedure StratoFnCallAddArgument(Parser:TStratoParser;
  var ListTail:rItem;Value,ValueType:rItem;SrcPos:xSrcPos);
procedure StratoFnCallBySignature(Parser:TStratoParser;
  MethodType:xTypeNr;Subject,Arguments,Parent:rItem;SrcPos:xSrcPos;
  var FnCall:rItem;var FnResType:rItem;ICall:boolean);
function StratoFnCallFindVirtual(FnCall,SubjectType:rItem;
  SubjectData:PxValue):rItem;
function StratoFnCallDestructor(Parser:TStratoParser;CodeBlock:rItem):rItem;
procedure StratoFnArgByValues(Parser:TStratoParser;FnCall,ValuesFrom:rItem);
//function StratoFindPropertySet(Sphere:TStratoSphere;
//  AssignTo,PropCall:xItem;Op:cardinal;SrcPos:xSrcPos):boolean;
procedure StratoCheckMemberNoArguments(Parser:TStratoParser;
  var Target:rItem;var TargetType:rItem;Parent:rItem;SrcPos:xSrcPos);

implementation

uses stratoTokenizer, stratoLogic;

function StratoFnArgListsMatch(Signature:rItem;Arguments:rItem):boolean;
var
  p,p0,q,q0:rItem;
begin
  ListFirst(Signature,lArguments,p,p0);
  //ListFirst(!!!,lArguments,q,q0);
  q0:=Arguments;//assert pointer to last in cyclic list
  q:=q0.r(iNext);

  Result:=true;//default
  while Result and not((p.x=0) and (q.x=0)) do
   begin
    if SameType(p.r(iType),q.r(iType)) then
     begin
      if p.NodeType=nSigArgByRef then
        if not IsAddressable(q.r(iValue)) then
          Result:=false;

      //next
      ListNext(p,p0);
      ListNext(q,q0);
     end
    else
      Result:=false;
   end;
  //remaining signature arguments with default value?
  if Result then
    if q.x=0 then
      while Result and (p.x<>0) do
        if (p.NodeType<>nSigArg) and (p.r(iValue).x<>0) then
          Result:=false
        else
          ListNext(p,p0)
    else
      Result:=false;
end;

function StratoFnAdd(Parser:TStratoParser;MethodType:xTypeNr;
  Member,Signature:rItem;SrcPos:xSrcPos):rItem;
var
  p,q,q0,m:rItem;
begin
  //assert Signature.nz
  //assert Fn.nz
  case MethodType of
    nOverload,nPropGet,nPropSet:
      m:=Member;
    nCtor:
     begin
      //assert Signature.Returns=nil
      Signature.s(iReturnType,Member);//Member is nClass
      ListFirst(Member,lItems,q,q0);
      while (q.x<>0) and (q.NodeType<>nCtors) do
        ListNext(q,q0);
      if q.x=0 then
       begin
        //prepend
        m:=Parser.Add(nCtors,[iParent,Member.x]);
        Parser.Prepend(Member,lItems,m);
       end
      else
        m:=q;
     end;
    //nDtor? doesn't do overloads, just add (unique)
    else
      m.x:=0;
  end;
  if m.x=0 then
   begin
    Parser.Source.Error('unexpected overload subject');
    Result:=Parser.Add(MethodType,[]);//pro-forma
   end
  else
   begin
    p.x:=0;
    ListFirst(Member,lItems,q,q0);
    while q.x<>0 do
      if (q.NodeType=MethodType) and SameType(q.r(iSignature),Signature) then
       begin
        if q.r(iBody).x=0 then //forward! fill in CB
          p:=q //TODO: delete/avoid superfluous Signature?
        else
          Parser.Source.Error('duplicate overload');
        q.x:=0;
       end
      else
      if (q.NodeType=MethodType) and
        StratoFnArgListsMatch(Signature,q.r(iFirstArgVar)) then
       begin
        q.x:=0;
        Parser.Source.Error('duplicate overload with equivalent arguments');
       end
      else
        ListNext(q,q0);
    if p.x=0 then
      p:=Parser.Add(m,lItems,MethodType,
        [iParent,m.x
        ,vSrcPos,SrcPos
        ,iSignature,Signature.x
        ])
    else
     begin
      p.s(iParent,m);
      p.s(vSrcPos,SrcPos);
      p.s(iSignature,Signature);
     end;
    Result:=p;
   end;
end;

procedure StratoFnCallAddArgument(Parser:TStratoParser;
  var ListTail:rItem;Value,ValueType:rItem;SrcPos:xSrcPos);
var
  p,q:rItem;
begin
  if Value.x<>0 then
   begin
    p:=Parser.Add(nCallArg, //iParent: see StratoFnCallBySignature
      [vSrcPos,SrcPos
      ,iValue,Value.x
      ,iType,ValueType.x
      ]);
    if ListTail.x=0 then //see also Sphere.Append
      p.s(iNext,p)
    else
     begin
      q:=ListTail.r(iNext);
      ListTail.s(iNext,p);
      p.s(iNext,q);
     end;
    ListTail:=p;
   end;
end;

procedure StratoFnCallBySignature(Parser:TStratoParser;
  MethodType:xTypeNr;Subject,Arguments,Parent:rItem;SrcPos:xSrcPos;
  var FnCall:rItem;var FnResType:rItem;ICall:boolean);
var
  p,p0,q,q0:rItem;
  tt:xTypeNr;
  nn:xName;
  v:boolean;
  nvt:xTypeNr;
begin
  //assert Arguments 0 or last in a cyclic list (ready for fArguments)
  p.x:=0;
  q:=Subject;
  v:=false;//default
  if ICall then nvt:=nICall else nvt:=nVCall;
  while (p.x=0) and (q.x<>0) do
    case q.NodeType of
      nVar,nVarReadOnly,nVarByRef,nCast:
       begin
        q:=q.r(iType);
        v:=true;
       end;
      nField:
       begin
        if v then Parser.Source.Error('unexpected cascading virtual');
        Subject:=q.r(iSubject);
        q:=q.r(iTarget);
        v:=true;
       end;
      nClassRef:
       begin
        q:=q.r(iTarget);
        v:=true;
        if MethodType=nOverload then MethodType:=nCtor;
       end;
      nArrayIndex:
       begin
        q:=q.r(iSubject);
        v:=true;
       end;
      nFCall,nSCall,nVCall,nICall:
       begin
        q:=q.r(iTarget);
        v:=true;
       end;
      nThis://assert called by combine pArgList "@@@(...": find inherited
       begin
        p:=q.rr(iParent,iParent);//see also below (for initial iInheritsFrom)
        q.x:=0;
        v:=true;
       end;
      nClass:
       begin
        p:=q;
        if ICall then v:=true;
       end;
      else
        p:=q;
    end;
  FnCall.x:=0;//default
  FnResType.x:=0;
  q.x:=0;
  case p.NodeType of
    nMember:
     begin
      ListFirst(p,lItems,q,q0);
      while (q.x<>0) and not((q.NodeType=MethodType) and
        StratoFnArgListsMatch(q.r(iSignature),Arguments)) do
        ListNext(q,q0);
      if q.x<>0 then
       begin
        q0:=q.r(iParent);
        case q0.NodeType of
          nClass,nInterface,nRecord://TODO: more?
            FnCall:=Parser.Add(nVCall,
              [iParent,Parent.x
              ,vSrcPos,SrcPos
              ,iSubject,Subject.x
              ,iTarget,q.x
              ,lArguments,Arguments.x
              ]);
          else
            if v then
              FnCall:=Parser.Add(nvt,
                [iParent,Parent.x
                ,vSrcPos,SrcPos
                ,iSubject,Subject.x
                ,iTarget,q.x
                ,lArguments,Arguments.x
                ])
            else
              FnCall:=Parser.Add(nFCall,
                [iParent,Parent.x
                ,vSrcPos,SrcPos
                ,iTarget,q.x
                ,lArguments,Arguments.x
                ]);
        end;
        if MethodType=nDtor then
          FnResType.x:=0
        else
          FnResType:=q.rr(iSignature,iReturnType);
       end;
     end;

    //nInterface? //TODO:
    //nPropertyGet //TODO:

    nClass:
     begin
      if MethodType=nCtor then MethodType:=nCtors;
      repeat
        if q.x=0 then
         begin
          ListFirst(p,lItems,q,q0);
          while (q.x<>0) and (q.NodeType<>MethodType) do
            ListNext(q,q0);
          if (MethodType=nCtors) and (q.x<>0) then
            ListFirst(q,lItems,q,q0);//assert q is nCtor
         end
        else
          if MethodType=nCtors then
            ListNext(q,q0)
          else
            q.x:=0;
        if q.x=0 then
         begin
          q:=p;
          q:=q.r(iInheritsFrom);
          p:=q;
          q.x:=0;
         end;
      until (p.x=0) or ((q.x<>0)
        and (((MethodType=nDtor) and (Arguments.x=0))
        or StratoFnArgListsMatch(q.r(iSignature),Arguments)));
      if q.x<>0 then
       begin
        if v then
          FnCall:=Parser.Add(nvt,
            [iParent,Parent.x
            ,vSrcPos,SrcPos
            ,iSubject,Subject.x
            ,iTarget,q.x
            ,lArguments,Arguments.x
            ])
        else
          FnCall:=Parser.Add(nFCall,
            [iParent,Parent.x
            ,vSrcPos,SrcPos
            ,iTarget,q.x
            ,lArguments,Arguments.x
            ]);
        if MethodType=nDtor then
          FnResType.x:=0
        else
          FnResType:=q.rr(iSignature,iReturnType);
       end;
     end;

    nOverload,nCtor,nDtor:
     begin
      tt:=p.NodeType;
      q:=p.r(iParent);
      nn:=q.v(iName);
      if tt<>nDtor then q:=q.r(iParent);
      if q.NodeType<>nClass then q.x:=0;
      if ICall then q:=q.r(iInheritsFrom);
      p.x:=0;
      while (p.x=0) and (q.x<>0) do
       begin
        case tt of
          nOverload,nPropGet,nPropSet:
           begin
            p:=Lookup(q,nn);
            if p.NodeType=nMember then
              ListFirst(p,lItems,p,p0)
            else
              ListNone(p,p0);//error?
           end;
          nCtor:
           begin
            ListFirst(q,lItems,p,p0);
            while (p.x<>0) and (p.NodeType<>nCtors) do
              ListNext(p,p0);
            if p.x<>0 then ListFirst(p,lItems,p,p0);
           end;
          nDtor:
           begin
            ListFirst(q,lItems,p,p0);
            while (p.x<>0) and (p.NodeType<>nDtor) do
              ListNext(p,p0);
           end;
          else
            ListNone(p,p0);//error?
        end;
        if tt<>nDtor then
          while (p.x<>0) and not((p.NodeType=tt) and
            StratoFnArgListsMatch(p.r(iSignature),Arguments)) do
            ListNext(p,p0);
        if (p.x=0) and (q.x<>0) then
          q:=q.r(iInheritsFrom);
       end;
      if p.x=0 then
       begin
        FnCall.x:=0;
        FnResType.x:=0;
       end
      else
       begin
        if v then
          FnCall:=Parser.Add(nvt,
            [iParent,Parent.x
            ,vSrcPos,SrcPos
            ,iSubject,Subject.x
            ,iTarget,p.x
            ,lArguments,Arguments.x
            ])
        else
          FnCall:=Parser.Add(nFCall,
            [iParent,Parent.x
            ,vSrcPos,SrcPos
            ,iTarget,p.x
            ,lArguments,Arguments.x
            ]);
        if p.NodeType=nDtor then
          FnResType.x:=0
        else
          FnResType:=p.rr(iSignature,iReturnType);
       end;
     end;

    nArray:
      if MethodType=nPropGet then //Combine:pBrackets
       begin
        //TODO: multidimensional arrays, array of array
        //if Argument.iType<>IntrinsicTypes[itNumber] then
        //  Source.Error('argument index not a number');
        FnResType:=p.r(iType);
        FnCall:=Parser.Add(nArrayIndex,
          [iParent,Parent.x
          ,vSrcPos,SrcPos
          ,iSubject,Subject.x
          ,lItems,Arguments.x
          ,iType,FnResType.x
          ]);
       end;

    //else error? (caller should check Result=0 !!!)
  end;
  //set arguments parent
  ListFirst(FnCall,lArguments,p,p0);
  while p.x<>0 do
   begin
    p.s(iParent,FnCall);//assert was 0
    ListNext(p,p0);
   end;
end;

function StratoFnCallFindVirtual(FnCall,SubjectType:rItem;
  SubjectData:PxValue):rItem;
var
  p,p0,q,Arguments:rItem;
  xp:PxValue;
  tt:xTypeNr;
  nx:xName;
begin
  q.x:=0;
  case SubjectType.NodeType of
    nClassRef,nInterface:
      q.x:=PxValue(SubjectData)^;
    nRecord:
      q:=SubjectType;
    nClass:
     begin
      //instance? get instance class
      xp:=pointer(SubjectData^);
      dec(cardinal(xp),SystemWordSize);//assert object._baseclass offset -4
      q.x:=PxValue(xp)^;
     end;
  end;
  if q.x=0 then
    Result.x:=0
  else
   begin
    //lookup
    p:=FnCall.r(iTarget);
    Arguments:=FnCall.r(lArguments);
    tt:=p.NodeType;
    nx:=0;//default
    case tt of
      nOverload,nPropGet,nPropSet:
       begin
        p0:=p.r(iParent);
        nx:=p0.v(iName);
       end;
    end;
    p.x:=0;
    while (p.x=0) and (q.x<>0) do
     begin
      if tt=nCtor then
       begin
        ListFirst(q,lItems,p,p0);
        while (p.x<>0) and (p.NodeType<>nCtors) do
          ListNext(p,p0);
        if p.x<>0 then ListFirst(p,lItems,p,p0);
       end
      else
       begin
        p:=Lookup(q,nx);
        if p.NodeType=nMember then
          ListFirst(p,lItems,p,p0)
        else
          ListNone(p,p0);
       end;
      while (p.x<>0) and not((p.NodeType=tt) and
        StratoFnArgListsMatch(p.r(iSignature),Arguments)) do
        ListNext(p,p0);
      if p.x=0 then q:=q.r(iInheritsFrom);
     end;
    Result:=p;
   end;
end;

function StratoFnCallDestructor(Parser:TStratoParser;CodeBlock:rItem):rItem;
var
  p,pThis,q,q0:rItem;
begin
  q:=CodeBlock;
  pThis.x:=0;
  while q.x<>0 do
   begin
    ListFirst(q,lLocals,p,q0);
    if p.NodeType=nThis then
     begin
      pThis:=p;
      q.x:=0; //found, end loop
     end
    else
     begin
      q:=q.r(iParent);
      if q.NodeType<>nCodeBlock then q.x:=0;
     end;
   end;
  if pThis.x=0 then
   begin
    Parser.Source.Error('"@@" undefined');
    Result.x:=0;//?Sphere.Add(nFnCall?
   end
  else
   begin
    //find destructor
    p:=pThis.r(iType);
    q.x:=0;
    while (p.x<>0) and (q.x=0) do
     begin
      ListFirst(p,lItems,q,q0);
      while (q.x<>0) and (q.NodeType<>nDtor) do ListNext(q,q0);
      if q.x=0 then p:=p.r(iInheritsFrom);
     end;
    if q.x=0 then
      Parser.Source.Error('unable to determine destructor');
    //then call it
    Result:=Parser.Add(nVCall,
      [iParent,CodeBlock.x
      ,vSrcPos,Parser.Source.SrcPos
      ,iSubject,pThis.x
      ,iTarget,q.x
      ]);
    Parser.Source.Skip(stPOpen);
    Parser.Source.Skip(stPClose);
   end;
end;

procedure StratoFnArgByValues(Parser:TStratoParser;FnCall,ValuesFrom:rItem);
var
  p,p0,q:rItem;
  SrcPos:xSrcPos;
begin
  SrcPos:=FnCall.v(vSrcPos);//?
  //ArgsFrom
  p:=FnCall.r(iTarget);
  //assert FnCall.FirstArg=nil
  p:=p.r(iSignature);
  ListFirst(p,lArguments,p,p0);
  q:=ValuesFrom.r(iFirstArgVar);
  //TODO: default argument values
  while (p.x<>0) and (q.x<>0) do
   begin
    Parser.Add(FnCall,nCallArg,lArguments,
      [iParent,FnCall.x
      ,vSrcPos,SrcPos
      ,iValue,q.x
      ,iType,p.r(iType).x
      ]);
    //assert SameType(p.r(iType),q.r(iType))
    ListNext(p,p0);
    ListNext(q,xxr(0));
   end;
  //if (p.x<>0) and (q.x=0) then raise?error?
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
        SetOp:=Sphere.Add(nBinaryOp,
          [iParent,Sphere.n(PropCall,fParent)^
          ,vSrcPos,SrcPos,
          ,vOperator,xValue(st)
          //fRight: see Combine pAssignment
          ]);
        q:=0;
        p:=AssignTo;
        while (p<>0) and (p<>PropCall) do
         begin
          case Sphere.n(p,vTypeNr)^ of
            nField:
             begin
              r:=Sphere.Add(nField,
                [iParent,Sphere.n(p,fParent)^
                ,vSrcPos,Sphere.n(p,vSrcPos)^
                ,fSubject,Sphere.n(p,fSubject)^
                ]);
              if q=0 then Sphere.n(SetOp,fLeft)^:=r else Sphere.n(q,fTarget)^:=r;
              q:=r;
              p:=Sphere.n(p,fTarget)^;
             end;
            //TODO: more?
            else
              p:=0;//Source.RunError(ip,'unsupported property header');
          end;
         end;
        if p<>0 then //if p is PropCall then
         begin
          SetCall:=Sphere.Add(nFnCall,
            [iParent,Sphere.n(p,fParent)^
            ,vSrcPos,Sphere.n(p,vSrcPos)^
            ]);
          Sphere.First(p,fArguments,r,r0);
          Sphere.Append(SetCall,fArguments,r);
          if q=0 then Sphere.n(SetOp,fLeft)^:=SetCall else Sphere.n(q,fTarget)^:=SetCall;
          q:=Sphere.n(p,fSubject)^;
          if Sphere.n(q,vTypeNr)^=nField then
            p:=Sphere.Add(nField,
              [iParent,Sphere.n(q,fParent)^
              ,vSrcPos,Sphere.n(q,vSrcPos)^
              ,fSubject,Sphere.n(q,fSubject)^
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

procedure StratoCheckMemberNoArguments(Parser:TStratoParser;
  var Target:rItem;var TargetType:rItem;Parent:rItem;SrcPos:xSrcPos);
var
  q,q0:rItem;
begin
  case Target.NodeType of
    nMember:
     begin
      ListFirst(Target,lItems,q,q0);
      while (q.x<>0) and not(
        ((q.NodeType=nOverload)
        or (q.NodeType=nPropGet))
        and (q.rr(iSignature,lArguments).x=0)
      ) do
        ListNext(q,q0);
      if q.x<>0 then
       begin
        Target:=Parser.Add(nFCall,
          [iParent,Parent.x
          ,vSrcPos,SrcPos
          ,iTarget,q.x
          ]);
        TargetType:=q.rr(iSignature,iReturnType);
       end;

      //else nPGetCall?
     end;

    //TODO: nClass: constructor without arguments?

    //else//more?
  end;
end;

end.
