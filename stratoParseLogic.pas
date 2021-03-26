unit stratoParseLogic;

interface

uses stratoParseDecl, stratoDecl, stratoPred, stratoTokenizer, stratoSphere;

type
  TStratoParserLogic=class(TStratoParserDecl)
  private
    cb:xRef;

    procedure LookUpLogic(nx:xName;var p,pt:xNode;SrcPos:xSrcPos);

    function CombineOne(var p,pt:xNode):xSrcPos;
    function Combine(zz:TPrecedence;var p,pt:xNode):xSrcPos;
    procedure Juxta(var p,pt:xNode);
    procedure PushUnary(st:TStratoToken;var p,pt:xNode);
    procedure PushBinary(pr:TPrecedence;st:TStratoToken;var p,pt:xNode);

    procedure CheckPassed(p:xNode);
    function IsType(p:xNode):boolean;

    procedure CheckType_Selection(p,pt:xNode);
    procedure CheckType_Operator(p,pt:xNode);
    procedure CheckType_Comparison(p,pt:xNode);
    procedure CheckType_Assignment(p,pt,qt:xNode);
    procedure CheckType_Range(p1,p2:xNode);

  protected
    procedure ParseLogic;

  end;

implementation

uses SysUtils, stratoParseBase, stratoSource, stratoFn, stratoLogic;

{ TStratoParserLogic }

procedure TStratoParserLogic.ParseLogic;
var
  StartStackIndex:cardinal;
  nx:xName;
  nn,fqn:UTF8String;
  p,pt,p0,p1,p2,p3,q,r,r0,r1:xNode;
  k:xKey;
  st:TStratoToken;
  SrcPos,SrcPos1:xSrcPos;
  z:TPrecedence;
  i:cardinal;
begin
  StartStackIndex:=stackIndex;
  //TODO: store stackIndex here and check equal afterwards?
  p.none;
  pt.none;
  cb:=Local(Peek1);//assert Peek=pCodeBlock
  while (SyntaxClass=scImperative) and Source.NextToken(st) do
  case st of

  stIdentifier:
   begin
    Juxta(p,pt);
    ID(nx,nn,SrcPos);
    LookUpLogic(nx,p,pt,SrcPos);

    if p.IsNone then
      Source.ErrorN('undeclared identifier',nn)
    else
    if (Peek=pCast) and not(pt.IsNone) then
      CombineOne(p,pt)
    else
      StratoCheckMemberNoArguments(Self,p,pt,cb,SrcPos);
   end;

  stPeriod://"."
    if p.IsNone then
      Source.Error('unexpected "."')
    else
    if Source.IsNext([stIdentifier]) then
     begin
      //Combine?
      ID(nx,nn,SrcPos);
      LookUpLogic(nx,p,pt,SrcPos);

      if p.IsNone then
        Source.ErrorN('undeclared identifier',nn)//TODO: FQN?
      else
      if (Peek=pCast) and IsType(pt) then
        CombineOne(p,pt)
      else
        StratoCheckMemberNoArguments(Self,p,pt,cb,SrcPos);

     end
    else
      Source.Error('unexpected "."');

  stStringLiteral:
   begin
    Juxta(p,pt);
    pt:=IntrinsicTypes[itString];
    p:=Add(nLiteral,4,
      [kv(vSrcPos,0,Source.SrcPos)
      ,kn(iType,pt)
      ]);
    if Peek in [pIfThen,pIfElse] then
      Sphere.SetVal(p.index,iValue,0,Sphere.AddBinaryData(Source.GetStr))
    else
      Sphere.SetVal(p.index,iValue,0,Sphere.AddBinaryData(Source.GetStrs));
   end;

  stNumericLiteral:
   begin
    Juxta(p,pt);
    pt:=IntrinsicTypes[itNumber];//default
    p:=Add(nLiteral,4,
      [kv(vSrcPos,0,Source.SrcPos)
      ,kn(iType,pt)
      ,kv(iValue,0,Sphere.AddBinaryData(Source.GetID(SrcPos)))
      ]);
    if Source.IsNext([stColon]) then
     begin
      pt:=LookUpDecl_Type('literal type');
      if pt.IsNone then Source.Error('unknown literal type');
      Sphere.SetRef(p.index,iType,pt);
     end;
   end;

  stColon://":"
   begin
    if Peek=pIfThen then Combine(pUntypedVar,p,pt);//TODO: un-hack this, revise pIfThen?
    if p.IsNone then
      if not(Source.IsNext([stIdentifier,stPeriod]))
        and Source.IsNext([stIdentifier]) then
       begin
        //new local variable(s)
        ID(nx,nn,SrcPos);
        if not Add(cb,lCodeBlock_Locals,nVar,6,nx,
          [kv(iParent,0,cb)
          ,kv(vSrcPos,0,SrcPos)
          //,vOffset,//see pUnTypedVar
          //,iType,//see pUnTypedVar
          ],p) then
          Source.ErrorN('duplicate identifier',nn);
        Push(pUnTypedVar,p,p,SrcPos);//see also stComma
        pt.none;
       end
      else
        Source.Error('no value to cast')
    else
      //local variable(s) type decl?
      if Peek=pUnTypedVar then
       begin
        //TODO: tuples...
        pt:=LookUpDecl_Type('type');
        i:=ByteSize(pt);
        Pop(z,p1,p2,SrcPos);

        r0.s(Sphere,cb);;
        r.Start(r0,lCodeBlock_Locals);
        while r.Next(r1) and not(r1.IsSame(p1)) do ;
        repeat
          Sphere.SetVal(Local(r1),vOffset,0,Sphere.a(cb,vByteSize,i));
          Sphere.SetRef(Local(r1),iType,pt);//assert was 0
          if r1.IsSame(p2) then r.none;
        until r.IsNone or not(r.Next(r1));

        if Source.IsNext([stSemiColon]) then
         begin
          p.none;//don't add as statement
          pt.none;
         end;
       end
      else
       begin
        //cast
        Push(pCast,Add(nCast,4,
          [kv(iParent,0,cb)
          ,kv(vSrcPos,0,Source.SrcPos)
          ,kn(iSubject,p)
          //,iType,//see Combine pCast
          ]),pt,Source.SrcPos);
        p.none;
        pt.none;
       end;
   end;

  stPOpen://"("
   begin
    SrcPos:=Combine(p_POpen,p,pt);
    if Peek<>pIterationZ then
      if SameType(pt,IntrinsicTypes[itBoolean]) then
       begin
        Push(pIfThen,p,pt,SrcPos);
        p.none;
        pt.none;
       end;
    Push(pParentheses,p,none,SrcPos);
    p.none;
    pt.none;
   end;

  stPClose://")"
   begin
    Combine(pParentheses,p,pt);
    if Peek=pParentheses then
     begin
      Pop(z,p1,p2,SrcPos);
      if p1.IsNone then
       begin
        if not(Peek in [pIterationX,pIterationY]) then
          if SameType(pt,IntrinsicTypes[itBoolean]) then
           begin
            //See also Juxta()
            Push(pIfThen,p,pt,SrcPos);
            p.none;
            pt.none;
           end
          else
            if Peek in [pIfThen,pIfElse] then
              CombineOne(p,pt);
       end
      else
       begin
        Add(cb,lArguments,nCallArg,4,
          [kn(iValue,p)
          ,kn(iType,pt)
          ,kv(vSrcPos,0,Source.SrcPos)
          ]);
        //calling constructor or destructor?
        p.none;
        pt.none;
        k:=nOverload;//default
        case p1.Key of
          nClass://calling destructor? (detect prefix '-' or '~')
            if (Peek=pUnary) and (TStratoToken(Peek1.v(vOperator))
              in [stOpSub,stTilde]) and not(IntrinsicTypes[itObject].IsNone) then
             begin
              Pop(z,p0,p0,SrcPos);
              k:=nDtor;
             end
            else
              k:=nCtor;
          nSCall:
           begin
            //assert p0=p1 and ps=Sphere
            //Sphere.SetVal(p1,lArguments,p2 ??
            p:=p1;
            pt.none;//pt:=p1.rr(iSignature,iReturnType);
           end;
          nThis://"@@@(...)": call inherited
            k:=nThis;
        end;
        if p.IsNone then
         begin
          p2.s(Sphere,cb);
          p2:=p2.rl(lArguments);
          StratoFnCallBySignature(Self,k,p1,p2,cb,SrcPos,k=nThis,p,pt);
         end;
        if p.IsNone then
          Source.Error('no function overload found with these arguments');
       end;
     end
    else
      Source.Error('unexpected ")"');
   end;

  stComma://","
   begin
    Combine(pUnTypedVar,p,pt);
    case Peek of
      pParentheses,pBrackets:
        if Peek1.IsNone or p.IsNone then
          Source.Error('unexpected ","')
        else
          Add(Local(Peek2),xStageList,nCallArg,4,
            [kn(iValue,p)
            ,kn(iType,pt)
            ,kv(vSrcPos,0,Source.SrcPos)
            ]);
      pUnTypedVar:
        if p.IsSame(Peek1) then
          if not(Source.IsNext([stIdentifier,stPeriod]))
            and Source.IsNext([stIdentifier]) then
           begin
            Pop(z,p1,p2,SrcPos1);
            //new local variable(s)
            ID(nx,nn,SrcPos);
            if not Add(cb,lCodeBlock_Locals,nVar,6,nx,
              [kv(iParent,0,cb)
              ,kv(vSrcPos,0,SrcPos)
              ,kv(vOffset,0,Sphere.v(cb,vByteSize).v)
              ],p2) then
              Source.ErrorN('duplicate identifier',nn);
            Push(pUnTypedVar,p1,p2,SrcPos);
            //assert p1 before p2 on list lCodeBlock_Locals
           end
          else
            Source.Error('unexpected ","')
        else
          if (p.Key=nAssign) and
            (TStratoToken(p.v(vOperator))=stOpAssign) then
            Sphere.Append(cb,lCodeBlock_Statements,Local(p))
          else
            Source.Error('unexpected syntax in local variable declaration');
      else
        Source.Error('unexpected ","');
    end;
   end;

  stCOpen://"{"
    if Source.IsNext([stIdentifier,stColon]) or
      Source.IsNext([stStringLiteral,stColon]) or
      Source.IsNext([stNumericLiteral,stColon]) then
     begin
      //TODO: JSON
      Source.Error('//TODO:JSON');
     end
    else
     begin
      Juxta(p,pt);
      //start code block
      SrcPos:=Source.SrcPos;
      p1.s(Sphere,cb);
      Push(pCodeBlock,p1,none,SrcPos);
      cb:=Add(nCodeBlock,8,
        [kv(iParent,0,cb)
        ,kv(vSrcPos,0,SrcPos)
        ]).index;
      //CurrentSyntaxClass:=scImperative;
     end;

  stCClose://"}"
   begin
    Combine(p_Statement,p,pt);
    if not p.IsNone then
     begin
      //resulting value
      p1:=Sphere.r(cb,iParent);
      case p1.Key of
        nOverload,nPropGet:
         begin
          q:=p1.r(iSignature).r(iReturnType);
          if q.IsNone then
           begin
            if not p.IsNone then
              Source.Error('missing closing semicolon');
           end
          else
           begin
            r0:=pt;
            r1:=q;
            if not SameType(r0,r1) then
              Source.Error('result value type mismatch');
            //assign to result value
            nx:=Sphere.n(Local(p1));
            p:=Add(nAssign,6,
              [kv(iParent,0,cb)
              ,kv(vSrcPos,0,Source.SrcPos)
              ,kv(vOperator,0,xValue(stOpAssign))
              ,kn(iTarget,Sphere.Lookup(Local(p),nx,lChildren))//nMember
              ,kn(iValue,p)
              ]);
            pt.none;//by language design!
           end;
         end
        else
         begin
          CheckPassed(p);
          //??? p.iType:=ResType(Sphere,p);
         end;
      end;
      Sphere.Append(cb,lCodeBlock_Statements,Local(p));
     end;
    while not(Peek in [p___,pCodeBlock]) do
     begin
      Pop(z,p,p0,SrcPos);
      if z=pIfElse then
        Source.Error('if-then without else')
      else
        Source.Error('unexpected "}"');//+xDisplay(p)?
     end;
    p.s(Sphere,cb);//keep a copy
    if stackIndex=StartStackIndex then //and (Peek=pCodeBlock)
     begin
//TODO: move to Combine
//TODO: rework as a 'return code pointer'?

      Pop(z,p,p0,SrcPos);

      //return to declarations
      cb:=0;
      SyntaxClass:=scDeclarative;
      //code block done: checks
      p1:=p.r(iParent);
      case p1.Key of
        nNameSpace:;//initialization/finalization block
        nCtor://constructor block done? check inherited called
          if not cbInheritedCalled then
           begin
            p2:=p1.r(iParent);//nClass
            if not(p2.IsSame(IntrinsicTypes[itObject])) then
             begin
              StratoFnCallBySignature(Self,nCtor,
                p2.r(iInheritsFrom),
                p1.r(iSignature).rl(lArguments),//!!
                Local(p),p.v(vSrcPos),true,p3,pt);
              if p3.IsNone and not(p1.v(lArguments)=0) then
                //try again for inherited constructor without arguments
                StratoFnCallBySignature(Self,nCtor,
                  p2.r(iInheritsFrom),none,
                  Local(p),p.v(vSrcPos),true,p3,pt);
              if p3.IsNone then
                Source.Error('unable to find base constructor')
              else
               begin
                //arguments
                StratoFnArgByValues(Self,Local(p3));//,Sphere.r(p,iParent));
                //insert first into code block
                Sphere.Prepend(Local(p),lCodeBlock_Statements,Local(p3));
               end;
             end;
           end;
        nDtor://destructor block done? check inherited called
          if not cbInheritedCalled then
           begin
            p2:=p1.r(iParent);//nClass
            if not(p2.IsSame(IntrinsicTypes[itObject])) then
             begin
              StratoFnCallBySignature(Self,nDtor,
                p2.r(iInheritsFrom),none,
                Local(p),Source.SrcPos,true,p3,pt);
              if p3.IsNone then
                Source.Error('unable to find base destructor')
              else
                Sphere.Append(Local(p),lCodeBlock_Statements,Local(p3));
             end;
           end;
        nOverload:;
          //TODO: check inherited called?
        {
        nPropGet://property getter done? parse setter
         begin
          //TODO: if not(cbInheritedCalled)
          if Source.IsNext([stCOpen]) then
           begin
            //TODO: construct setter signature? (use the same for now)
            xxxxxxxxxxxxxxxxx
           end
          else
            Source.Skip(stSemiColon);//closing property declaration
         end;
        }

        //TODO: nPropSet and not cbInheritedCalled
        //else
        else
          raise Exception.Create('//TODO: call something unknown '+p1.AsString);

      end;
     end
    else
     begin
      //pop from stack
      Pop(z,p1,p0,SrcPos);
      cb:=Local(p1);
      //assert z=pCodeBlock (see above)
      Combine(p_Statement,p,pt);//?
      //add to parent chain
      if not(p.IsNone) and (pt.IsNone) then
        Sphere.Append(cb,lCodeBlock_Statements,Local(p));
     end;
    p.none;
    pt.none;
   end;

  stSemiColon:
   begin
    {$IFDEF DEBUG}
    if Source.IsNext([stSemiColon,stSemiColon]) then
     begin
      Source.Skip(stSemiColon);
      Source.Skip(stSemiColon);
      asm int 3 end;//for debugging the parser
     end;
    {$ENDIF}
    Combine(p_Statement,p,pt);
    if not p.IsNone then
     begin
      CheckPassed(p);
      Sphere.Append(cb,lCodeBlock_Statements,Local(p));
     end;
    p.none;
    pt.none;
   end;

  stDefine:
   begin
    Source.Error('use either ":=" or "=="');
    p.none;
    pt.none;
   end;

  stOpAssign,
  stOpAssignAdd,stOpAssignSub,
  stOpAssignMul,stOpAssignDiv,stOpAssignMod,
  stOpAssignOr,stOpAssignAnd:
   begin
    //Combine(pAssign,p,pt);
    if Peek=pCast then CombineOne(p,pt);//allow only casts (dirty assignment)
    if p.IsNone then
      Source.Error('no left side defined for assignment')
    else
     begin
      Push(pAssign,Add(nAssign,6,
        [kv(iParent,0,cb)
        ,kv(vSrcPos,0,Source.SrcPos)
        ,kv(vOperator,0,xValue(st))
        ,kn(iTarget,p)
        //,iValue,see Combine pAssign
        ]),pt,Source.SrcPos);
      p.none;
      pt.none;
     end;
   end;
  stOpEQ,stOpNEQ:
    PushBinary(pEqual,st,p,pt);
  stOpLT,stOpLTE,stOpGT,stOpGTE,stOpWhatIs:
    PushBinary(pComparative,st,p,pt);
  //TODO: stOpLT: if not Combine(pComparative,p) then support inline HTML?
  stOpAdd:
   begin
    Combine(pAddSub,p,pt);
    if p.IsNone then //unary operator
      PushUnary(st,p,pt)
    else
      PushBinary(pAddSub,st,p,pt);
   end;
  stOpSub:
   begin
    Combine(pAddSub,p,pt);
    if p.IsNone then
      if Source.IsNext([stAtAt,stPOpen,stPClose]) then
       begin
        p:=StratoFnCallDestructor(Self,cb);
        pt.none;//since destructor never returns a value
       end
      else
        PushUnary(st,p,pt)
    else
      PushBinary(pAddSub,st,p,pt);
   end;
  stOpMul,stOpDiv,stOpMod:
    PushBinary(pMulDiv,st,p,pt);
  stOpShl,stOpShr,stThreeLT://stThreeGT: see below
    PushBinary(pShift,st,p,pt);
  stOpAnd:
    if SameType(pt,IntrinsicTypes[itBoolean]) then
      PushBinary(pLogicalAnd,st,p,pt)
    else
      PushBinary(pBitwiseAnd,st,p,pt);
  stOpOr:
    if SameType(pt,IntrinsicTypes[itBoolean]) then
      PushBinary(pLogicalOr,st,p,pt)
    else
      PushBinary(pBitwiseOr,st,p,pt);
  stOpXor:
    if SameType(pt,IntrinsicTypes[itBoolean]) then
      PushBinary(pLogicalXor,st,p,pt)
    else
      PushBinary(pBitwiseXor,st,p,pt);
  stOpNot:
    if p.IsNone then
      PushUnary(st,p,pt)
    else
      Source.Error('Unexpected left operand to unary NOT');
  stTilde:
    if p.IsNone then
      if Source.IsNext([stAtAt,stPOpen,stPClose]) then
        p:=StratoFnCallDestructor(Self,cb)
      else
        PushUnary(st,p,pt)
    else
      Source.Error('Unexpected left operand to unary XOR');

  stOpInc,stOpDec:
    if p.IsNone then
      Source.Error('increment/decrement operators only allowed as suffix')
    else
      p:=Add(nUnaryOp,8,
        [kv(iParent,0,cb)
        ,kv(vSrcPos,0,Source.SrcPos)
        ,kv(vOperator,0,xValue(st))
        ,kn(iReturnType,pt)
        ,kn(iRight,p)  //should be 'iLeft', but most nUnaryOp uses iRight...
        ]);

  stAtAt://"@@": this
   begin
    Juxta(p,pt);
    //see also StratoFnAddOverload
    p1.s(Sphere,cb);
    while not(p1.IsNone) do
     begin
      //assert nThis first of lLocals
      r0:=p1;
      r.Start(p1,lCodeBlock_Locals);
      r.Next(p);
      if not(p.IsNone) and (p.Key=nThis) then
        p1.none//end loop
      else
       begin
        p.none;
        p1:=p1.r(iParent);
        if p1.Key<>nCodeBlock then p1.none;
       end;
     end;
    if p.IsNone then
     begin
      Source.Error('"@@" undefined');
      p:=Add(nThis,4,//add anyway to avoid further errors
        [kv(iParent,0,cb)
        //,iType,?
        //,vOffset,?
        ]);
     end;
    pt:=p.r(iType);
   end;
  stWhatWhat://"??": result value
   begin
    Juxta(p,pt);
    p0.none;
    p1.s(Sphere,cb);
    while not(p1.IsNone) and (p1.Key=nCodeBlock) do
     begin
      p0:=p1;
      p1:=p1.r(iParent);
     end;
    if p1.IsNone then p.none else
      case p1.Key of
        nOverload,nPropGet,nPropSet:
          p:=Sphere.Lookup(Local(p0),Sphere.n(Local(p1.r(iParent))),lCodeBlock_Locals);//nMember
        else
          p.none;
      end;
    if p.IsNone then
     begin
      Source.Error('"??" undefined');
      p:=Add(nVar,6,//add anyway to avoid further errors
        [kv(iParent,0,cb)
        ,kv(vSrcPos,0,Source.SrcPos)
        ,kv(dName,0,Sphere.AddName('??'))
        ]);
     end;
    pt:=p.r(iType);
   end;

  //stAmpersand:?

  stHashHash://iteration "##"
   begin
    Combine(p_Juxta,p,pt);//?
    Push(pIterationX,p,pt,Source.SrcPos);
    p.none;
    pt.none;
   end;
  stOpRange:
   begin
    Combine(pRange,p,pt);
    //TODO: if p.x=0 then create literal "0"?
    p:=Add(nRange,6,
      [kv(iParent,0,cb)
      ,kv(vSrcPos,0,Source.SrcPos)
      ,kn(iLeft,p)
      //,iRight,//see Combine
      ,kn(iReturnType,pt)
      ]);
    Push(pRange,p,pt,SrcPos);
    p.none;
    pt.none;
   end;

  stThreeColons://":::"
   begin
    Combine(p_Statement,p,pt);
    Add(cb,lCodeBlock_Statements,nTry,4,
      [kv(iParent,0,cb)
      ,kv(vSrcPos,0,Source.SrcPos)
      ]);
   end;
  stThreeGT://">>>"
   begin
    Combine(pShift,p,pt);
    if p.IsNone then //defer
     begin
      Combine(p_Statement,p,pt);
      Push(pDefer,none,none,Source.SrcPos);
     end
    else
      PushBinary(pShift,st,p,pt);//roll right
   end;
  stThreeWhats://"???"
   begin
    Combine(p_Statement,p,pt);
    //"???(e:ExceptionType)"
    SrcPos:=Source.SrcPos;
    if Source.IsNext([stPOpen,stIdentifier,stColon,stIdentifier]) then
     begin
      p1:=Add(nCatch,4,
        [kv(iParent,0,cb)
        ,kv(vSrcPos,0,SrcPos)
        ]);
      Source.Token;//stIdentifier
      ID(nx,nn,SrcPos);
      Source.Token;//stColon
      Source.Token;//stIdentifier
      Sphere.SetRef(p1.index,iTarget,Add(cb,lCodeBlock_Locals,nVarReadOnly,8,
        [kv(iParent,0,cb)
        ,kv(vSrcPos,0,SrcPos)
        ,kv(dName,0,nx)
        ,kn(iType,LookUpDecl_Type('catch filter'))
        ,kv(vOffset,0,Sphere.a(cb,vByteSize,SystemWordSize))//??!!
        ]));
      Source.Skip(stPClose);//TODO: enforce
     end
    else
    if Source.IsNext([stPOpen]) then
     begin
      p1:=Add(nCatch,4,
        [kv(iParent,0,cb)
        ,kv(vSrcPos,0,SrcPos)
        ]);
      repeat
        Add(p1.index,lCatch_Types,nTypeAlias,4,
          [kn(iParent,p1)
          ,kv(vSrcPos,0,Source.SrcPos)
          ,kn(iTarget,LookUpDecl_Type('catch filter'))
          ]);
      until not Source.IsNext([stComma]);
      Source.Skip(stPClose);
     end
    else
      p1:=Add(nCatch,4,
        [kv(iParent,0,cb)
        ,kv(vSrcPos,0,SrcPos)
        ]);
    Push(pCatch,p1,none,SrcPos);
   end;
  stThreeBangs://"!!!"
   begin
    Juxta(p,pt);
    Push(pThrow,none,none,Source.SrcPos);
   end;

  stBOpen://"["
    if p.IsNone then
      Source.Error('unexpected "["')//TODO: lambda
    else
     begin
      //TODO: check p is of type array?
      Push(pBrackets,p,none,Source.SrcPos);
      p.none;
      pt.none;
     end;
  stBClose://"]"
   begin
    Combine(pBrackets,p,pt);
    if Peek=pBrackets then
     begin
      Pop(z,p1,p2,SrcPos);
      if p.IsNone then
       begin
        //TODO:
        Source.Error('unsupported "[]" without subject');//TODO: lambda
       end
      else
       begin
        p2.s(Sphere,Sphere.Add(xStageList,1));
        Add(p2.index,xStageList,nCallArg,6,
          [kn(iValue,p)
          ,kn(iType,pt)
          ,kv(vSrcPos,0,Source.SrcPos)
          ]);
        StratoFnCallBySignature(Self,nPropGet,p1,p2,cb,SrcPos,false,p,pt);
        if p.IsNone then
          Source.Error('no property overload found with these arguments');
       end;
     end
    else
      Source.Error('unexpected "]"');
   end;

  stAt://"@"
   begin
    Juxta(p,pt);
    //TODO: if p<>0 then ?
    Push(pAddressOf,none,none,Source.SrcPos);
   end;

  stQuestionMark://"?"
   begin
    if not p.IsNone then
      Source.Error('unexpected "?"');//TODO: trinary "x?y:z"?
    p.none;
    pt.none;
    //type-of this in constructor?
    if Source.IsNext([stAtAt]) then
     begin
      p0.s(Sphere,cb);
      p1:=p0.r(iParent);
      while p1.Key=nCodeBlock do
       begin
        p0:=p1;
        p1:=p1.r(iParent);
       end;
      if p1.Key=nCtor then
       begin
        nx:=Sphere.AddName('?@@');
        r.Start(p0,lCodeBlock_Locals);
        while r.Next(p) and (Sphere.n(Local(p))<>nx) do ;
        pt:=p.r(iType);//IntrinsicType(itType)?
        if p.IsNone then
          Source.Error('constructor class reference not found');
       end;
     end;
    if p.IsNone then
     begin
      p1.s(nil,xValue(st));
      Push(pTypeOf,p1,none,Source.SrcPos);
     end;
   end;
  stOpSizeOf://"@?"
   begin
    Juxta(p,pt);
    if not(p.IsNone) then Source.Error('unexpected "?"');
    p1.s(nil,xValue(st));
    Push(pSizeOf,p1,none,Source.SrcPos);
   end;

  stCaret://"^"
    if p.IsNone then
      Source.Error('unexpected "^"')
    else
     begin
      Juxta(p,pt);
      case pt.Key of
        nPointer://TODO: xSignature? xOverload?
         begin
          pt:=pt.r(iTarget);
          p:=Add(nDereference,6,
            [kv(iParent,0,cb)
            ,kv(vSrcPos,0,Source.SrcPos)
            ,kn(iSubject,p)
            ,kn(iReturnType,pt)
            ]);
         end;
        else
          Source.Error('dereference expected on pointer');
      end;
     end;

  stAtAtAt://"@@@": inherited
   begin
    Juxta(p,pt);
    SrcPos:=Source.SrcPos;
    if Source.IsNext([stPOpen]) then
     begin
      //see also StratoFnAddOverload
      p1.s(Sphere,cb);
      while not p1.IsNone do
       begin
        //assert nThis first of lLocals
        r.Start(p1,lCodeBlock_Locals);
        r.Next(p);
        if not(p.IsNone) and (p.Key=nThis) then
          p1.none //end loop
        else
         begin
          p.none;
          p1:=p1.r(iParent);
          if p1.Key<>nCodeBlock then p1.none;
         end;
       end;
      if p.IsNone then
       begin
        Source.Error('"@@" undefined');
        p:=Add(nThis,4,//add anyway to avoid further errors
          [kv(iParent,0,cb)
          //,iType,?
          //,vOffset,?
          ]);
       end;
      Push(pParentheses,p,none,SrcPos);//see also stPClose
      cbInheritedCalled:=true;
      p.none;
      pt.none;
     end
    else
      Source.Error('manipulating inherited pointer not allowed');
   end;

  stDollar://"$":abstract system call
   begin
    Juxta(p,pt);
    SrcPos1:=Source.SrcPos;
    if Source.IsNextID([stPOpen]) then
     begin
      ID(nx,nn,SrcPos);
      fqn:=nn;
      while Source.IsNext([stPeriod,stIdentifier]) do
       begin
        Source.Token;//stIdentifier
        ID(nx,nn,SrcPos);
        fqn:=fqn+'.'+nn;
       end;
      Source.Token;//stPOpen
      //TODO: checks?
      Push(pParentheses,Add(nSCall,4,
        [kv(iParent,0,cb)
        ,kv(vSrcPos,0,SrcPos1)
        ,kv(iTarget,0,Sphere.AddBinaryData(fqn))
        ]),none,SrcPos1);
      p.none;
      pt.none;
     end
    else
    if Source.IsNext([stStringLiteral]) then
     begin
      p:=Add(nSCall,4,
        [kv(iParent,0,cb)
        ,kv(vSrcPos,0,SrcPos1)
        ,kv(iTarget,0,Sphere.AddBinaryData(Source.GetStr))
        ]);
      pt.none;
     end
    else
    if Source.IsNext([stDollar,stNumericLiteral]) then
     begin
      p:=Add(nSCall,4,
        [kv(iParent,0,cb)
        ,kv(vSrcPos,0,SrcPos1)
        ,kn(iTarget,ParseLiteral(Source.Token,nil))
        ]);
      pt.none;
     end
    else
      Source.Error('unexpected "$"');
   end;

  else
    Source.Error('unsupported syntax');//'unexpected token');
  end;

  if stackIndex>=StartStackIndex then Source.Error('unexpected end of source');
end;

procedure TStratoParserLogic.LookUpLogic(nx:xName;var p,pt:xNode;SrcPos:xSrcPos);
var
  i:cardinal;
  p0,q,q0,q1,r,rt:xNode;
begin
  if not p.IsNone then
   begin
    //see below: search by type
    r:=p;
    rt:=pt;
    p.none;
   end
  else
   begin
    //check code block, then up stack
    r.none;
    rt.none;
    p:=Sphere.Lookup(cb,nx,lCodeBlock_Locals);
    if p.IsNone then
     begin
      i:=stackIndex;
      while (i<>0) and p.IsNone do
       begin
        dec(i);
        case stack[i].pr of
          pCodeBlock:
            p:=Sphere.Lookup(Local(stack[i].p1),nx,lCodeBlock_Locals);
          pCatch:
           begin
            p:=stack[i].p1;
            p:=p.r(iTarget);
            if not((p.Key=nVarReadOnly) and (Sphere.n(Local(p))=nx)) then
              p.none;
           end;
        end;
       end;
     end;
    //not found? check under 'this'
    if p.IsNone then
     begin
      q.s(Sphere,cb);
      while r.IsNone and (q.Key=nCodeBlock) do
       begin
        q0.Start(q,lCodeBlock_Locals);
        q0.Next(q1);
        if q1.Key=nThis then
         begin
          //see below: search by type
          r:=q1;
          rt:=r.r(iType);
         end
        else
          q:=q.r(iParent);
       end;
     end;
   end;
  //search under something?
  if not(r.IsNone) then
   begin
    //assert Parent=0
    //first try direct
    case r.Key of
      nClass:
        while p.IsNone and not(r.IsNone) do
         begin
          p:=Sphere.Lookup(Local(r),nx,lChildren);
          if p.IsNone then
            r:=r.r(iInheritsFrom);
         end;
      nRecord:
        p:=Sphere.Lookup(Local(r),nx,lChildren);
      //TODO: more?
    end;
    //nothing yet, is it typed? search type
    if p.IsNone then //) and (ParentType.p<>0) then
     begin
      case rt.Key of
        nClass:
          while p.IsNone and not(rt.IsNone) do
           begin
            p:=Sphere.Lookup(Local(rt),nx,lChildren);
            if p.IsNone then
              rt:=rt.r(iInheritsFrom);
           end;
        nRecord:
          p:=Sphere.Lookup(Local(rt),nx,lChildren);
        //TODO: more?
      end;
     end
    else
      r.none;
   end;
  //nothing yet? check locals
  if p.IsNone then
   begin
    r.none;
    LookUpDecl_First(nx);
    //no "while Source.IsNext([stPeriod,stIdentifier])" here!
    p:=LookUpDecl_Any;
   end;
  //resolve type
  p0:=p;
  case p0.Key of
    xUnassigned:
      pt.none;
    nType,nArray,nEnum,nRecord,nPointer,nTypeAlias,nSignature:
      pt:=IntrinsicTypes[itType];
    nClass:
      pt:=IntrinsicTypes[itType];//add(nClassRef?
    nNameSpace:
      pt.none;//assert resolved later

    else
      pt:=p.r(iType);
  end;
  //field
  if not(p.IsNone) and not(r.IsNone) then
    p:=Add(nField,8,
      [kv(iParent,0,cb)
      ,kv(vSrcPos,0,SrcPos)
      ,kn(iSubject,r)
      ,kn(iTarget,p)
      //,kn(iType,pt)
      ]);
end;

function TStratoParserLogic.Combine(zz:TPrecedence;var p,pt:xNode):xSrcPos;
begin
  Result:=Source.SrcPos;//default
  stackPushed:=false;
  while not(stackPushed) and (Peek>zz) do
    Result:=CombineOne(p,pt);
end;

function TStratoParserLogic.CombineOne(var p,pt:xNode):xSrcPos;
var
  z:TPrecedence;
  p1,p2,q1,q2:xNode;
  SrcPos,SrcPos1:xSrcPos;
begin
  //IMPORTANT: don't call Push from within CombineOne!
  Pop(z,p1,p2,SrcPos);
  Result:=SrcPos;
  case z of

    pCodeBlock:
      Source.Error('missing expected "}"');
    pParentheses:
      Source.Error('missing expected ")"');
    pBrackets:
      Source.Error('missing expected "]"');

    //nSelection
    pIfThen:
      if p.IsNone then
       begin
        //just a floating boolean value, no selection
        p:=p1;
        pt:=p2;
       end
      else
       begin
        //assert Peek=pIfElse
        Push(pIfElse,Add(nSelection,8,
          [kv(iParent,0,cb)
          ,kv(vSrcPos,0,SrcPos)
          ,kn(iPredicate,p1)
          ,kn(iDoTrue,p)
          ,kn(iReturnType,pt)
          ]),none,SrcPos);
        p.none;
        pt.none;
       end;
    pIfElse:
     begin
      if p.IsNone then
        //selection without false section
      else
       begin
        Sphere.SetRef(Local(p1),iDoFalse,p);
        CheckType_Selection(p1,pt);
       end;
      p:=p1;
      //pt:=p1.r(iReturnType);
     end;

    pRange:
     begin
      Sphere.SetRef(Local(p1),iRight,p);
      CheckType_Range(p2,pt);
      p:=p1;
      //pt:=pt;//?
     end;

    //nIteration,nIterPostEval
    pIterationX:
     begin
      //assert Peek=pIterationZ
      if SameType(pt,IntrinsicTypes[itBoolean]) then
       begin
        if not(p1.IsNone) then
          Source.Error('unexpected iterator for iteration with boolean predicate');
        Push(pIterationZ,Add(nIteration,6,
          [kv(iParent,0,cb)
          ,kv(vSrcPos,0,SrcPos)
          ,kn(iPredicate,p)
          //,iBody,//see pIterationZ
          //,iReturnType,//see pIterationZ
          ]),none,SrcPos);
       end
      else if p.Key=nRange then
       begin
        if Peek=pUnTypedVar then
         begin
          Pop(z,q1,q2,SrcPos1);
          if not(q1.IsSame(q2)) then
            Source.Error('unexpected multiple iterators');//TODO: ?
          //assert q1=p1
          Sphere.SetVal(Local(q1),vOffset,0,Sphere.a(cb,vByteSize,ByteSize(pt)));
          Sphere.SetRef(Local(q1),iType,pt);
         end;
        Push(pIterationZ,Add(nIteration,6,
          [kv(iParent,0,cb)
          ,kv(vSrcPos,0,SrcPos)
          ,kn(iPredicate,Add(nRangeIndex,6,
            [kv(iParent,0,cb)
            ,kv(vSrcPos,0,SrcPos)
            ,kn(iLeft,p1) //iterator
            ,kn(iRight,p) //range
            ]))
          ]),none,SrcPos);
        //TODO: more checks on nRangeIndex?
       end
      else if p.IsNone then
       begin
        if not(p1.IsNone) then
          Source.Error('unexpected iterator for iteration without predicate');
        Push(pIterationY,none,none,SrcPos);
       end
      else
        Push(pIterationZ,Add(nIterPostEval,8,
          [kv(iParent,0,cb)
          ,kv(vSrcPos,0,SrcPos)
          //,iPredicate,//see pIterationZ
          ,kn(iBody,p)
          ,kn(iReturnType,pt)
          ]),none,SrcPos);
      p.none;
      pt.none;
     end;
    pIterationY:
     begin
      if not(p1.IsNone) then
        Source.Error('unexpected iterator for iteration with boolean predicate');
      Push(pIterationZ,Add(nIteration,6,
        [kv(iParent,0,cb)
        ,kv(vSrcPos,0,SrcPos)
        ,kn(iPredicate,p)
        //,iBody,//see pIterationZ
        //,iReturnType,//see pIterationZ
        ]),none,SrcPos);
      p.none;
      pt.none;
     end;
    pIterationZ:
      if p1.Key=nIterPostEval then
       begin
        if SameType(pt,IntrinsicTypes[itBoolean]) then
          Sphere.SetRef(Local(p1),iPredicate,p)
        else
          Source.Error('boolean expression expected for iteration predicate');
        p:=p1;
        pt:=p1.r(iReturnType);
       end
      else
       begin
        Sphere.SetRef(Local(p1),iBody,p);
        Sphere.SetRef(Local(p1),iReturnType,pt);
        p:=p1;
        //pt:=pt;//pt:=p1.s(iReturnT
       end;

    //nUnaryOp
    pUnary:
     begin
      if p.IsNone then Source.Error('unary operand missing');
      //TODO: lookup
      p:=Add(nUnaryOp,8,
        [kv(iParent,0,cb)
        ,kv(vSrcPos,0,SrcPos)
        ,kn(vOperator,p1)//xValue(st)
        ,kn(iRight,p)
        ,kn(iReturnType,pt)
        ]);
      //pt:=pt;
     end;
    pSizeOf:
     begin
      if p.IsNone then Source.Error('size-of operand missing');
      q1:=IntrinsicTypes[itNumber];
      p:=Add(nUnaryOp,8,
        [kv(iParent,0,cb)
        ,kv(vSrcPos,0,SrcPos)
        ,kn(vOperator,p1)//xValue(st)
        ,kn(iRight,p)
        ,kn(iReturnType,q1)
        ]);
      pt:=IntrinsicTypes[itNumber];
     end;
    pTypeOf:
     begin
      if p.IsNone then Source.Error('type-of operand missing');
      if pt.Key=nClass then
        q1:=Add(nClassRef,4,
          [kv(iParent,0,cb)
          ,kv(vSrcPos,0,SrcPos)
          ,kn(iTarget,pt)
          ])
      else
        q1:=IntrinsicTypes[itType];
      p:=Add(nUnaryOp,8,
        [kv(iParent,0,cb)
        ,kv(vSrcPos,0,SrcPos)
        ,kn(vOperator,p1)//xValue(st)
        ,kn(iRight,p)
        ,kn(iReturnType,q1)
        ]);
      pt:=q1;
     end;
    pAddressOf:
     begin
      //is addressable?
      if IsAddressable(p) then
       begin
        while pt.Key=nArray do
          pt:=pt.r(iType);
        q1:=Add(nPointer,4,
          [kv(iParent,0,cb)
          ,kv(vSrcPos,0,SrcPos)
          ,kn(iTarget,pt)
          ]);
       end
      else
       begin
        Source.Error('invalid address-of subject');
        q1.none;
       end;
      p:=Add(nAddressOf,6,
        [kv(iParent,0,cb)
        ,kv(vSrcPos,0,SrcPos)
        ,kn(iSubject,p)
        ,kn(iReturnType,q1)
        ]);
      pt:=q1;
     end;

    //nBinaryOp
    pMulDiv,pAddSub,pShift,
    pLogicalOr,pLogicalXor,pLogicalAnd,
    pBitwiseOr,pBitwiseXor,pBitwiseAnd:
     begin
      if p.IsNone then Source.Error('binary operand missing');
      Sphere.SetRef(Local(p1),iRight,p);
      CheckType_Operator(p1,pt);
      p:=p1;
      //pt:=pt?
     end;
    pEqual,pComparative:
     begin
      if p.IsNone then Source.Error('comparison operand missing');
      Sphere.SetRef(Local(p1),iRight,p);
      CheckType_Comparison(p1,pt);
      p:=p1;
      pt:=IntrinsicTypes[itBoolean];
     end;

    //nAssign
    pAssign:
      if p.IsNone then
        Source.Error('invalid assignment value')
      else
       begin
        if Peek=pUnTypedVar then
         begin
          Pop(z,q1,q2,SrcPos1);
          if not(q1.IsSame(q2)) then
            Source.Error('unexpected multiple assignees');//TODO: deconstruction (or tuples?)
          Sphere.SetVal(Local(q1),vOffset,0,Sphere.a(cb,vByteSize,ByteSize(pt)));
          Sphere.SetRef(Local(q1),iType,pt);
          if q1.IsSame(p1.r(iTarget)) then p2:=pt;//assert p2 was 0
         end;

        Sphere.SetRef(Local(p1),iValue,p);
        CheckType_Assignment(p1,pt,p2);

        //assigning an object reference? reference counting!
        if pt.Key=nClass then
         begin
          if TStratoToken(p1.v(vOperator))<>stOpAssign then
            Source.Error('invalid assignment type for object reference');
          if IntrinsicTypes[itObject].IsNone then
            Source.Error('base class for reference counting not defined')
          else
           begin
            //TODO: check not zero then release
            //TODO: call _addref (with StratoFnCallFindSignature ?)
            //TODO: defer release refcount
           end;
         end;

        p:=p1;
        pt.none;//by language design!
       end;

    pUnTypedVar:
     begin
      //see also pAssignment above and stColon below
      //assert p1.NodeType=nVar
      if p1.r(iType).IsNone then
        Source.ErrorN('no type for local var',Sphere.FQN(Local(p1)));
     end;

    pCast:
     begin
      if pt.IsNone then //if pt<>IntrinsicType(itType)?
        Source.Error('invalid cast')
      else
       begin
        if (ByteSize(p)<>ByteSize(p2)) and not(IsIntrinsicNumeric(p) and IsIntrinsicNumeric(p2)) then
          Source.Error('cast type size mismatch');
       end;
      Sphere.SetRef(Local(p1),iType,p);
      pt:=p;
      p:=p1;
     end;

    pDefer:
     begin
      p:=Add(nDefer,4,
        [kv(iParent,0,cb)
        ,kv(vSrcPos,0,SrcPos)
        ]);
      pt.none;
     end;
    pThrow:
     begin
      p:=Add(nThrow,4,
        [kv(iParent,0,cb)
        ,kv(vSrcPos,0,SrcPos)
        ,kn(iSubject,p)
        ]);
      pt.none;
     end;
    pCatch:
     begin
      Sphere.SetRef(Local(p1),iBody,p);//TODO: check? here or with stCClose??
      p.none;
      pt.none;
     end;

    //else ?

  end;
end;

procedure TStratoParserLogic.Juxta(var p,pt:xNode);
var
  SrcPos:xSrcPos;
begin
  if not p.IsNone then
   begin
    SrcPos:=Combine(p_Juxta,p,pt);
    case Peek of
      pIfThen,pIterationX:
        CombineOne(p,pt);
      pIterationZ:
        CombineOne(p,pt);//????
      else
        if SameType(pt,IntrinsicTypes[itBoolean]) then
          Push(pIfThen,p,pt,SrcPos)
        else
          Source.Error('missing operator or semicolon');
    end;
    p.none;
    pt.none;
   end;
end;

procedure TStratoParserLogic.PushUnary(st:TStratoToken;var p,pt:xNode);
var
  q:xNode;
begin
  q.sphere:=nil;
  q.index:=xRef(st);
  Push(pUnary,q,none,Source.SrcPos);
  p.none;
  pt.none;
end;

procedure TStratoParserLogic.PushBinary(pr:TPrecedence;st:TStratoToken;
  var p,pt:xNode);
var
  SrcPos:xSrcPos;
begin
  Combine(pr,p,pt);
  if p.IsNone then
    Source.Error('no left side defined for binary operator')
  else
   begin
    SrcPos:=Source.SrcPos;
    p:=Add(nBinaryOp,8,
      [kv(iParent,0,cb)
      ,kv(vSrcPos,0,SrcPos)
      ,kv(vOperator,0,xValue(st))
      ,kn(iLeft,p)
      //,iRight,//see Combine
      ,kn(iReturnType,pt)
      ]);
    Push(pr,p,none,SrcPos);
   end;
  p.none;
  pt.none;
end;

function TStratoParserLogic.IsType(p:xNode):boolean;
begin
  Result:=false;//default
  while p.Key=nField do p:=p.r(iTarget);
  case p.Key of
    nType,nSignature,nArray,nRecord,nEnum,nPointer,
    nClass,nClassRef,nInterface:
      Result:=true;
    //nOverload,nCtor://TODO
    //  Result:=true;//signature!!!
    //TODO: nDtor: create signature on the fly?
    //nPropGet://TODO:IsType(Store.n(Store.n(p,iSignature),iReturnType);
    //nMember: find overload without arguments? //TODO

    nTypeAlias:
      Result:=true;//?
  end;
end;

procedure TStratoParserLogic.CheckPassed(p:xNode);
var
  b:boolean;
begin
  if not p.IsNone then
   begin
    b:=false;
    case p.Key of
      nVar,nVarByRef,nVarReadOnly,
      nFCall,nSCall,nVCall,nICall,
      nIteration,nIterPostEval,nAssign,
      nDefer,nThrow,nCatch,
      nDtor://,nPropGetCall,nPropSetCall:
        b:=true;
      nCodeBlock,nSelection:
        b:=true;//b:=p.EvaluatesTo=0;//TODO: descend into?
      nBinaryOp:
        b:=TStratoToken(p.v(vOperator)) in [stOpAssign..stOpAssignAnd];
      nUnaryOp:
        b:=TStratoToken(p.v(vOperator)) in [stOpInc,stOpDec];
      //more?
    end;
    if not b then
      Source.Error('statement without calls or assignments');
   end;
end;

procedure TStratoParserLogic.CheckType_Selection(p,pt:xNode);
var
  q,p1:xNode;
begin
  q:=p.r(iReturnType);
  if p.r(iDoTrue).IsNone then
    Sphere.SetRef(Local(p),iReturnType,pt)
  else
   begin
    p1:=q;
    if not(q.IsNone) and not(SameType(p1,pt)) then
      if SameType(pt,p1) then
        Sphere.SetRef(Local(p),iReturnType,pt);
   end;
end;

procedure TStratoParserLogic.CheckType_Operator(p,pt:xNode);
var
  q:xNode;
begin
  q:=p.r(iReturnType);
  if (q.IsNone) or (pt.IsNone) then
    Source.Error('binary operator operand type missing')
  else
   begin
    if not SameType(q,pt) then
     begin
      if SameType(pt,q) then
        Sphere.SetRef(Local(p),iReturnType,pt)
      else
        Source.Error('binary operator operand type mismatch');
     end;
    //else???
   end;
end;

procedure TStratoParserLogic.CheckType_Comparison(p,pt:xNode);
var
  ok:boolean;
  q,p1:xNode;
begin
  ok:=false;//default
  q:=p.r(iReturnType);
  Sphere.SetRef(Local(p),iReturnType,IntrinsicTypes[itBoolean]);
  if not(q.IsNone) and not(pt.IsNone) then
   begin
    if TStratoToken(p.v(vOperator))=stOpWhatIs then
      ok:=true
    else
     begin
      ok:=SameType(q,pt) or SameType(pt,p1);
     end;
    //TODO: if p1=Type_bool then support 'x<y<z'
   end;
  if not ok then
    Source.Error('comparison operand type mismatch');
end;

procedure TStratoParserLogic.CheckType_Assignment(p,pt,qt:xNode);
var
  q:xNode;
begin
  //Result:=0;//default
  q:=p.r(iTarget);
  if q.Key=nCast then //allow dirty cast
   begin
    if IsAddressable(q.r(iSubject)) then
     begin
      //TODO: if nArray
      if not SameType(pt,qt) then
        Source.Error('assignment type mismatch');
     end
    else
      Source.Error('assignment receiver not addressable');
    //TODO: check ByteSize's equal?
    //TODO: switch to enable pointer arith (default off!)
   end
  else
   begin
    if IsAddressable(q) then
     begin
      //while Sphere.n(r,vTypeNr)^=nArray do r:=Sphere.n(r,fTypeDecl)^;
      if not SameType(pt,qt) then
        Source.Error('assignment type mismatch');
     end
    else
      Source.Error('assignment receiver not addressable');
    //TODO: local lookup member ":=" (use TStratoToken(p2)!)
   end;
end;

procedure TStratoParserLogic.CheckType_Range(p1,p2:xNode);
begin
  if not SameType(p1,p2) then
    Source.Error('range terminals type mismatch');
end;

end.
