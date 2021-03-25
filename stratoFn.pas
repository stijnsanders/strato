unit stratoFn;

interface

{xxx$D-}
{xxx$L-}

uses stratoDecl, stratoSphere, stratoParseDecl, stratoParseLogic;

function StratoFnAdd(Parser:TStratoParserDecl;MethodType:xKey;
  Member,Signature:xNode;SrcPos:xSrcPos):xNode;
procedure StratoFnCallBySignature(Parser:TStratoParserLogic;
  MethodType:xKey;Subject,Arguments:xNode;Parent:xRef;SrcPos:xSrcPos;
  ICall:boolean;var FnCall,FnResType:xNode);
function StratoFnCallFindVirtual(FnCall,SubjectType:xNode;
  SubjectData:PxKeyValue):xNode;
function StratoFnCallDestructor(Parser:TStratoParserLogic;CodeBlock:xRef):xNode;
procedure StratoFnArgByValues(Parser:TStratoParserLogic;FnCall:xRef);
//function StratoFindPropertySet(Sphere:TStratoSphere;
//  AssignTo,PropCall:xKey;Op:cardinal;SrcPos:xSrcPos):boolean;
procedure StratoCheckMemberNoArguments(Parser:TStratoParserLogic;
  var Target,TargetType:xNode;Parent:xRef;SrcPos:xSrcPos);

implementation

uses SysUtils, stratoTokenizer, stratoLogic;

function kl(Key:xKey;List:xNode):xKeyValue;
var
  p:PxKeyValue;
begin
  p:=List.sphere.k[List.index];
  //assert p.k=xStageList
  Result.k:=Key;
  Result.n:=0;//handled by Sphere.Add
  Result.i:=p.i;
  Result.v:=p.v;
  //clear
  //p.k:=xUnassigned;//?
  p.i:=0;
  p.v:=0;
end;

function StratoFnArgListsMatch(Signature,Arguments:xNode):boolean;
var
  p,q,p0,q0:xNode;
begin
  p.Start(Signature,lArguments);
  p.Next(p0);

  //if Arguments.Key in [lArguments,xStageList] then ?
  q:=Arguments;//q.Start(Arguments,Arguments.Key);
  q.Next(q0);

  Result:=true;//default
  while Result and not(p.Done and q.Done) do
    if SameType(p0.r(iType),q0.r(iType)) then
     begin
      if p0.Key=nSigArgByRef then
        if not IsAddressable(q0.r(iValue)) then
          Result:=false;
      p.Next(p0);
      q.Next(q0);
     end
    else
      Result:=false;
  //remaining signature arguments with default value?
  if Result then
    if q.Done then
      while Result and not(p.Done) do
        if (p0.Key<>nSigArg) and not(p0.r(iValue).IsNone) then
          Result:=false
        else
          p.Next(p0)
    else
      Result:=false;
end;

function kn(Key:xKey;Parser:TStratoParserDecl;Node:xNode):xKeyValue;
begin
  Result.k:=Key;
  Result.n:=xRef(0);//handled by sphere
  if (Node.sphere=nil) or (Node.sphere=Parser.Sphere) then
    Result.i:=0
  else
    Result.i:=SphereIndex(Node.sphere);
  Result.v:=Node.index;
end;

function StratoFnAdd(Parser:TStratoParserDecl;MethodType:xKey;
  Member,Signature:xNode;SrcPos:xSrcPos):xNode;
var
  p,q,q0,r:xNode;
begin
  //assert Signature.nz
  //assert Fn.nz
  if MethodType=nCtor then
   begin
    //assert Signature.Returns=nil
    if Signature.sphere=Parser.Sphere then
      Parser.Sphere.SetRef(Signature.index,iReturnType,Member)//Member is nClass
    else
      Parser.Source.Error('can''t alter constructor signature in other sphere');
   end;
  p.none;
  q.Start(Member,lChildren);
  q.Next(q0);
  while not(q.Done) do
    if (q0.Key=MethodType) and SameType(q0.r(iSignature),Signature) then
     begin
      if (r.sphere=Parser.Sphere) and (Parser.Sphere.v(r.index,iBody)=nil) then //forward! fill in CB
        p:=r //TODO: delete/avoid superfluous Signature?
      else
        Parser.Source.Error('duplicate overload');
      q.none;//end loop
     end
    else
    if (q0.Key=MethodType) and
      StratoFnArgListsMatch(Signature,q0.rl(lArguments)) then
     begin
      Parser.Source.Error('duplicate overload with equivalent arguments');
      q.none;//end loop
     end
    else
      q.Next(q0);
  if p.IsNone then
    if Member.sphere=Parser.Sphere then
      p:=Parser.Add(Member.index,lChildren,MethodType,8,
        [kn(iParent,Parser,Member)
        ,kv(vSrcPos,0,SrcPos)
        ,kn(iSignature,Parser,Signature)
        ])
    else
      Parser.Source.Error('function member unexpectedly in other sphere')
  else
   begin
    if p.sphere=Parser.Sphere then
     begin
      Parser.Sphere.SetRef(p.index,iParent,Member);
      Parser.Sphere.SetVal(p.index,vSrcPos,0,SrcPos);
      Parser.Sphere.SetRef(p.index,iSignature,Signature);
     end
    else
      Parser.Source.Error('can''t add function to other sphere');
   end;
  Result:=p;
end;

procedure StratoFnCallBySignature(Parser:TStratoParserLogic;
  MethodType:xKey;Subject,Arguments:xNode;Parent:xRef;SrcPos:xSrcPos;
  ICall:boolean;var FnCall,FnResType:xNode);
var
  p,p1,q,r,p0,q0:xNode;
  k:xKey;
  nn:xName;
  v:boolean;
  nvt:xKey;
begin
  //assert Arguments IsNone or xListEntry (ready for lArguments)
  p.none;
  q:=Subject;
  v:=false;//default
  if ICall then nvt:=nICall else nvt:=nVCall;
  while p.IsNone and not(q.IsNone) do
    case q.Key of
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
        p:=q.r(iParent).r(iParent);//see also below (for initial iInheritsFrom)
        q.none;
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
  FnCall.none;//default
  r.none;
  q.none;
  case p.Key of
    nNameSpace://member
     begin
      q0.Start(p,lChildren);
      while q0.Next(q) and not((q.Key=MethodType) and
        StratoFnArgListsMatch(q.r(iSignature),Arguments)) do ;
      if not q0.Done then
       begin
        case q.r(iParent).Key of
          nClass,nInterface,nRecord://TODO: more?
            FnCall:=Parser.Add(nVCall,8,
              [kv(iParent,0,Parent)
              ,kv(vSrcPos,0,SrcPos)
              ,kn(iSubject,Parser,Subject)
              ,kn(iTarget,Parser,q)
              ,kl(lArguments,Arguments)
              ]);
          else
            if v then
              FnCall:=Parser.Add(nvt,8,
                [kv(iParent,0,Parent)
                ,kv(vSrcPos,0,SrcPos)
                ,kn(iSubject,Parser,Subject)
                ,kn(iTarget,Parser,q)
                ,kl(lArguments,Arguments)
                ])
            else
              FnCall:=Parser.Add(nFCall,8,
                [kv(iParent,0,Parent)
                ,kv(vSrcPos,0,SrcPos)
                ,kn(iTarget,Parser,q)
                ,kl(lArguments,Arguments)
                ]);
        end;
        if MethodType=nDtor then
          r.none
        else
          r:=q.r(iSignature).r(iReturnType);
       end;
     end;

    //nInterface? //TODO:
    //nPropertyGet //TODO:

    nClass:
     begin
      p1:=Arguments;
      repeat
        if q.IsNone then
         begin
          q0.Start(p,lChildren);
          while q0.Next(q) and (q.Key<>MethodType) do ;
          p:=p.r(iInheritsFrom);
         end;
      until (p.IsNone) or (not(q.IsNone)
        and (((MethodType=nDtor) and (Arguments.IsNone))
        or StratoFnArgListsMatch(q.r(iSignature),Arguments)));
      if not q.IsNone then
       begin
        if v then
          FnCall:=Parser.Add(nvt,8,
            [kv(iParent,0,Parent)
            ,kv(vSrcPos,0,SrcPos)
            ,kn(iSubject,Parser,Subject)
            ,kn(iTarget,Parser,q)
            ,kl(lArguments,Arguments)
            ])
        else
          FnCall:=Parser.Add(nFCall,8,
            [kv(iParent,0,Parent)
            ,kv(vSrcPos,0,SrcPos)
            ,kn(iTarget,Parser,q)
            ,kl(lArguments,Arguments)
            ]);
        if MethodType=nDtor then
          r.none
        else
          r:=q.r(iSignature).r(iReturnType);
       end;
     end;

    nOverload,nCtor,nDtor:
     begin
      k:=p.Key;
      q:=p.r(iParent);
      nn:=q.v(dName);
      if k in [nCtor,nPropGet,nPropSet] then q:=q.r(iParent);
      //if q.Key<>nClass then q.none;
      if ICall then q:=q.r(iInheritsFrom);
      p.none;
      while (p.IsNone) and not(q.IsNone) do
       begin
        case k of
          nOverload,nPropGet,nPropSet:
           begin
            p:=q.sphere.Lookup(q.index,nn,lChildren);
            if p.Key=nNameSpace then
              p0.Start(p,lChildren)
            else
              p0.None;//error?
           end;
          nCtor:
            p0.Start(q,lChildren);
          nDtor:
           begin
            p0.Start(q,lChildren);
            while p0.Next(r) and (r.Key<>nDtor) do ;
           end;
          else
            p0.None;//error?
        end;
        if k<>nDtor then
          while p0.Next(r) and not((r.Key=k) and
            StratoFnArgListsMatch(r.r(iSignature),Arguments)) do ;
        if (p.IsNone) and not(q.IsNone) then //and (q.Key=nClass)?
          q:=q.r(iInheritsFrom);
       end;
      if p.IsNone then
       begin
        FnCall.none;
        r.none;
       end
      else
       begin
        if v then
          FnCall:=Parser.Add(nvt,8,
            [kv(iParent,0,Parent)
            ,kv(vSrcPos,0,SrcPos)
            ,kn(iSubject,Parser,Subject)
            ,kn(iTarget,Parser,p)
            ,kl(lArguments,Arguments)
            ])
        else
          FnCall:=Parser.Add(nFCall,8,
            [kv(iParent,0,Parent)
            ,kv(vSrcPos,0,SrcPos)
            ,kn(iTarget,Parser,p)
            ,kl(lArguments,Arguments)
            ]);
        if p.Key=nDtor then
          r.none
        else
          r:=q.r(iSignature).r(iReturnType);
       end;
     end;

    nArray:
      if MethodType=nPropGet then //Combine:pBrackets
       begin
        //TODO: multidimensional arrays, array of array
        //if Argument.iType<>IntrinsicTypes[itNumber] then
        //  Source.Error('argument index not a number');
        r:=p.r(iType);
        FnCall:=Parser.Add(nArrayIndex,8,
          [kv(iParent,0,Parent)
          ,kv(vSrcPos,0,SrcPos)
          ,kn(iSubject,Parser,Subject)
          ,kl(lArguments,Arguments)
          ,kn(iType,Parser,r)
          ]);
       end;

    //else error? (caller should check Result=0 !!!)
  end;

  FnResType:=r;

  //set arguments parent
  p0.Start(FnCall,lArguments);
  p0.Next(p);
  while not p0.Done do
    if p.Key=nCallArg then
     begin
      Parser.Sphere.SetRef(p.index,iParent,FnCall);//assert was not set
      p0.Next(p);
     end
    else
      p0.none;//Source.Error?
end;

function StratoFnCallFindVirtual(FnCall,SubjectType:xNode;
  SubjectData:PxKeyValue):xNode;
var
  p,p0,p1,q,Arguments:xNode;
  pv:PxKeyValue;
  k:xKey;
  nn:UTF8String;
begin
  q.none;
  case SubjectType.Key of
    nClassRef,nInterface:
      q.ss(nil,SubjectData.i,SubjectData.v);
    nRecord:
      q:=SubjectType;
    nClass:
     begin
      //instance? get instance class
      //pv:=pointer(SubjectData^);//!!!!
      pv:=SubjectData;
      dec(xValue(pv),SystemWordSize);//assert object._baseclass offset -4
      q.ss(nil,pv.i,pv.v);
     end;
  end;
  if q.IsNone then
    Result.none
  else
   begin
    //lookup
    p:=FnCall.r(iTarget);
    Arguments:=FnCall.rl(lArguments);
    k:=p.Key;
    nn:='';//default
    case k of
      nOverload,nPropGet,nPropSet:
       begin
        p1:=p.r(iParent);
        nn:=p1.sphere.GetName(p1.v(dName));
       end;
    end;
    p.none;
    while p.IsNone and not(q.IsNone) do
     begin
      if k=nCtor then
        p0.Start(q,lChildren)
      else
       begin
        p:=q.Lookup(nn);
        if p.Key=nNameSpace then
          p0.Start(p,lChildren)
        else
          p0.None;
       end;
      while p0.Next(p1) and not((p1.Key=k) and
        StratoFnArgListsMatch(p1.r(iSignature),Arguments)) do ;
      if p.IsNone then q:=q.r(iInheritsFrom);
     end;
    Result:=p;
   end;
end;

function StratoFnCallDestructor(Parser:TStratoParserLogic;CodeBlock:xRef):xNode;
var
  p,pThis,q,q0:xNode;
begin
  q.s(Parser.Sphere,CodeBlock);
  pThis.none;
  while not q.IsNone do
   begin
    q0.Start(q,lCodeBlock_Locals);
    q0.Next(p);
    if p.Key=nThis then
     begin
      pThis:=p;
      q.none; //found, end loop
     end
    else
     begin
      q:=q.r(iParent);
      if q.Key<>nCodeBlock then q.none;
     end;
   end;
  if pThis.IsNone then
   begin
    Parser.Source.Error('"@@" undefined');
    Result.none;//?Sphere.Add(nFnCall?
   end
  else
   begin
    //find destructor
    p:=pThis.r(iType);
    q.none;
    while not(p.IsNone) and (q.IsNone) do
     begin
      q0.Start(p,lChildren);
      while q0.Next(q) and (q.Key<>nDtor) do ;
      if q.IsNone then p:=p.r(iInheritsFrom);
     end;
    if q.IsNone then
      Parser.Source.Error('unable to determine destructor');
    //then call it
    Result:=Parser.Add(nVCall,8,
      [kv(iParent,0,CodeBlock)
      ,kv(vSrcPos,0,Parser.Source.SrcPos)
      ,kn(iSubject,Parser,pThis)
      ,kn(iTarget,Parser,q)
      ]);
    Parser.Source.Skip(stPOpen);
    Parser.Source.Skip(stPClose);
   end;
end;

procedure StratoFnArgByValues(Parser:TStratoParserLogic;FnCall:xRef);
var
  p,q,p0,q0:xNode;
  SrcPos:xSrcPos;
begin
  p.s(Parser.Sphere,FnCall);
  SrcPos:=p.v(vSrcPos);//?
  p0.Start(p.r(iSignature),lArguments);
  q0.Start(p.r(iTarget),lArguments);
  //TODO: default argument values
  while p0.Next(p) and q0.Next(q) do
   begin
    Parser.Add(FnCall,nCallArg,lArguments,8,
      [kv(iParent,0,FnCall)
      ,kv(vSrcPos,0,SrcPos)
      ,kn(iValue,Parser,q)//?????
      ,kn(iType,Parser,p.r(iType))//??!!!!
      ]);
    //assert SameType(p.r(iType),q.r(iType))
   end;
  //if not(p0.Done and q0.Done) then rasise?error?
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

procedure StratoCheckMemberNoArguments(Parser:TStratoParserLogic;
  var Target,TargetType:xNode;Parent:xRef;SrcPos:xSrcPos);
var
  q0,q1:xNode;
begin
  case Target.Key of
    nNameSpace:
     begin
      q0.Start(Target,lChildren);
      while q0.Next(q1) and not(
        ((q1.Key=nOverload) or (q1.Key=nPropGet))
        and (q1.r(iSignature).v(lArguments)=0)
      ) do ;
      if not q1.IsNone then
       begin
        Target:=Parser.Add(nFCall,8,
          [kv(iParent,0,Parent)
          ,kv(vSrcPos,0,SrcPos)
          ,kn(iTarget,Parser,q1)
          ]);
        TargetType:=q1.r(iSignature).r(iReturnType);
       end;

      //else nPGetCall?
     end;

    //TODO: nClass: constructor without arguments?

    //else//more?
  end;
end;

end.
