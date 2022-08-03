unit stratoGenPas;

interface

uses stratoSphere;

type
  TStratoGenPascal=class(TObject)
  public
    procedure GenerateSource(const FilePath:string);
  end;

implementation

uses SysUtils, Classes, stratoDecl, stratoLogic, stratoGenTools,
  stratoTokenizer;

{ TStratoGenPascal }

procedure TStratoGenPascal.GenerateSource(const FilePath: string);
type
  TCodeType=(
    ctPending,

    ctInterface,
    ctInterfaceType,
    ctInterfaceConst,
    ctInterfaceVar,
    ctInterface_Any,

    ctImplementation,
    ctImplementationType,
    ctImplementationConst,
    ctImplementationVar,
    ctImplementation_Any,

    ctInterfaceUses,
    ctImplementationUses,

    ctInitialization,
    ctFinalization);

var
  c:array of record
    p:xNode;
    t:TCodeType;
    l:cardinal;
    c:AnsiString;
   end;
  ci,cl:cardinal;
const
  cGrowStep=$1000;

  IndentPrefix='  ';//#9?

  function Add(p:xNode;level:cardinal):xNode;
  var
    i:cardinal;
  begin
    if not(p.IsNone) then
     begin
      i:=0;
      while (i<ci) and not(c[i].p.IsSame(p)) do inc(i);
      if i=ci then
       begin
        if ci=cl then
         begin
          //grow
          inc(cl,cGrowStep);
          SetLength(c,cl);
         end;
        c[i].p:=p;
        c[i].t:=ctPending;
        c[i].l:=level;
        c[i].c:='';
        inc(ci);
       end
      else
       begin
        if c[i].l<level then
          c[i].l:=level;
       end;
     end;
    Result:=p;
  end;

  procedure AddCode(p:xNode;t:TCodeType;level:cardinal;const code:AnsiString);
  var
    i:cardinal;
  begin
    if p.IsNone then i:=ci else i:=0;
    while (i<ci) and not(c[i].p.IsSame(p)) do inc(i);
    if i=ci then
     begin
      if ci=cl then
       begin
        //grow
        inc(cl,cGrowStep);
        SetLength(c,cl);
       end;
      c[i].p:=p;
      inc(ci);
     end
    else
     begin
      if c[i].t<>ctPending then raise Exception.Create('Duplicate Generate Detected');
     end;
    c[i].t:=t;
    c[i].l:=level;
    c[i].c:=code;
  end;

  function Signature(p:xNode;named:boolean;level:cardinal):AnsiString;
  var
    p1,p2,q0,q1:xNode;
    src:AnsiString;
    b:boolean;
  begin
    case p.Key of
    nSignature,nDtor:
     begin
      p1:=p;
      p2.none;
     end;
    nCtor:
     begin
      p1:=p.r(iSignature);
      p2:=IntrinsicTypes[itPointer];
      Add(p2,level+1);
     end;
    else
     begin
      p1:=p.r(iSignature);
      p2:=Add(p1.r(iReturnType),level);
     end;
    end;
    if p2.IsNone then src:='procedure' else src:='function';
    if named then src:=src+' '+rs(p);
    b:=true;

    if p.Key=nDtor then
     begin
      b:=false;
      src:=src+'(nThis: pointer';
     end
    else
     begin
      q1:=Add(p1.r(iSubject),level);
      if not q1.IsNone then
       begin
        b:=false;
        //TODO nThis
        //src:=src+'(var nThis: '+rs(q1);
        src:=src+'(nThis: pointer';
       end;

      q0.Start(p1,lArguments);
      while q0.Next(q1) do
       begin
        if b then
         begin
          src:=src+'(';
          b:=false;
         end
        else
          src:=src+';';
        if q1.Key=nSigArgByRef then src:=src+'var ';
        src:=src+rs(q1)+': '+rs(Add(q1.r(iType),level));
       end;
     end;

    if not b  then src:=src+')';
    if not p2.IsNone then src:=src+': '+rs(p2);
    src:=src+';';
    Result:=src;
  end;

  function CodeBlock(cb,LastArgVar,ResultVar:xNode;level:cardinal):AnsiString;
  type
    sStackStep=(
      sCodeBlockNext,//see StartCodeBlock
      sEndOnly,//see StartBeginEnd
      sStatement,
      sExpress,
      sResolveValue,
      sAssign1,
      sAssign2,
      sSelection1,
      sSelection2,
      sIteration1,
      sIteration2,
      sIteration3,
      sIteration4,
      sIteration5,
      sUnaryOp1,
      sBinaryOp1,
      sBinaryOp2,
      sFCall1,
      sCast1,
      sField1,
      sArrayIndex1,
      //add new above here
      s_Unknown);
  var
    stack:array of record
      Step:sStackStep;
      What:xNode;
      Code1,Code2:AnsiString;
    end;
    stackIndex,stackSize:integer;

    procedure Push(xStep:sStackStep;xWhat:xNode;
      const c1:AnsiString='';const c2:AnsiString='');
    {$IFDEF DEBUG}
    var
      i:integer;
    {$ENDIF}
    begin
      if stackIndex=stackSize then
       begin
        inc(stackSize,32);//growstep
        SetLength(stack,stackSize);
        {$IFDEF DEBUG}
        for i:=stackIndex to stackSize-1 do
         begin
          stack[i].Step:=s_Unknown;
          stack[i].What.none;
          stack[i].Code1:='';
          stack[i].Code2:='';
         end;
        {$ENDIF}
       end;
      stack[stackIndex].Step:=xStep;
      stack[stackIndex].What:=xWhat;
      stack[stackIndex].Code1:=c1;
      stack[stackIndex].Code2:=c2;
      inc(stackIndex);
    end;

    function Pop(var p:xNode;var c1,c2:AnsiString):sStackStep;
    begin
      if stackIndex=0 then
       begin
        p.none;
        Result:=s_Unknown;
       end
      else
       begin
        dec(stackIndex);
        Result:=stack[stackIndex].Step;
        p:=stack[stackIndex].What;
        c1:=stack[stackIndex].Code1;
        c2:=stack[stackIndex].Code2;
        {$IFDEF DEBUG}
        stack[stackIndex].Step:=s_Unknown;
        stack[stackIndex].What.none;
        stack[stackIndex].Code1:='';
        stack[stackIndex].Code2:='';
        {$ENDIF}
       end;
    end;

  var
    src_const,src_type,src_var,src_indent,src_body,src_value,c1,c2:AnsiString;
    p,p0,p1,p2:xNode;

    procedure StartCodeBlock(pp:xNode;const Suffix:AnsiString=';');
    var
      p0,p:xNode;
    begin
      p0.Start(pp,lCodeBlock_Locals);
      if not LastArgVar.IsNone then
       begin
        while p0.Next(p) and not(p.IsSame(LastArgVar)) do ;
        if not(p.IsSame(LastArgVar)) then
          raise Exception.Create('LastArgVar not found in CodeBlock_Locals');
        LastArgVar.none;
       end;
      while p0.Next(p) do
        src_var:=src_var+IndentPrefix+rs(p)+': '+rs(Add(p.r(iType),level))+';'#13#10;
      p.Start(pp,lCodeBlock_Statements);
      Push(sCodeBlockNext,p,src_indent,Suffix);
      src_body:=src_body+src_indent+'begin //'+rs(pp)+#13#10;
      src_indent:=src_indent+IndentPrefix;
    end;

    procedure StartBeginEnd(pp:xNode;const Suffix:AnsiString=';');
    begin
      Push(sEndOnly,none,src_indent,Suffix);
      src_body:=src_body+src_indent+'begin'#13#10;
      src_indent:=src_indent+IndentPrefix;
      if not(pp.IsNone) then Push(sStatement,pp);
    end;

  begin
    src_const:='';
    src_type:='';
    src_var:='';
    src_indent:='';
    src_body:='';
    src_value:='';

    stackIndex:=0;
    stackSize:=0;

    StartCodeBlock(cb);

    while stackIndex<>0 do
      case Pop(p,c1,c2) of

      sCodeBlockNext:
       begin
        p0:=p;
        if p0.Next(p) then
         begin
          Push(sCodeBlockNext,p0,c1,c2);
          Push(sStatement,p);
         end
        else
         begin
          //code block done
          if stackIndex<>0 then //see below
           begin
            src_indent:=c1;
            src_body:=src_body+src_indent+'end'+c2+#13#10;
           end;
         end;
       end;

      sEndOnly:
       begin
        src_indent:=c1;
        src_body:=src_body+src_indent+'end'+c2+#13#10;
       end;

      sStatement:
        //src_body:=src_body+src_indent+'//'+rs(p)+':'+AnsiString(KeyToStr(p.Key))+#13#10;
        case p.Key of

        nAssign:
         begin
          //src_body:=src_body+src_indent+'//'+rs(p)+':'+AnsiString(KeyToStr(p.Key))+#13#10;
          Push(sAssign1,p);
          p1:=p.r(iTarget);
          if p1.IsSame(ResultVar) then
            src_value:='Result'
          else
            Push(sResolveValue,p1);
         end;

        nSelection:
         begin
          Push(sSelection1,p);
          Push(sResolveValue,p.r(iPredicate));
         end;

        nIteration:
         begin
          p1:=p.r(iPredicate);
          if p1.Key=nRangeIndex then
           begin
            Push(sIteration4,p);
            Push(sResolveValue,p1.r(iRight).r(iLeft));
           end
          else
           begin
            Push(sIteration1,p);
            Push(sResolveValue,p1);
           end;
         end;

        nIterPostEval:
         begin
          Push(sIteration2,p);
          src_body:=src_body+src_indent+'repeat //'+rs(p)+#13#10;
          p1:=p.r(iBody);
          if p1.Key=nCodeBlock then
            StartCodeBlock(p1)
          else
            StartBeginEnd(p1);
         end;

        //TODO: dedulpicate sStatement en sResolveValue

        nUnaryOp:
         begin
          Push(sExpress,p);

          Push(sUnaryOp1,p);
          Push(sResolveValue,p.r(iRight));
         end;

        nFCall:
         begin
          Push(sExpress,p);

          p1:=Add(p.r(iTarget),level+1);
          src_value:=rs(p1);
          p0.Start(p,lArguments);
          if p0.Next(p) then
           begin
            Push(sFCall1,p0,src_value+'(');
            Push(sResolveValue,p.r(iValue));
            src_value:='';
           end;

         end;

        nSCall:
         begin
          //see also TStratoMachine.PerformSysCall
          p1:=p.r(iTarget).r(iValue);
          case StrToInt(string(p1.sphere.BinaryData(p1.index))) of

            xSCall_writeln:
             begin
              p0.Start(cb,lCodeBlock_Locals);
              p0.Next(p1);
              src_body:=src_body+src_indent+'WriteLn('+rs(p1)+');'#13#10;
             end;

            xSCall_filetostr:
             begin
              //uses StratoUtils?
              p0.Start(cb,lCodeBlock_Locals);
              p0.Next(p1);
              p0.Next(p2);
              src_body:=src_body+src_indent+'Result:=stratoFileToStr('+rs(p2)+');'#13#10;
             end;
            xSCall_strtofile:
             begin
              //uses StratoUtils?
              p0.Start(cb,lCodeBlock_Locals);
              p0.Next(p1);
              p0.Next(p2);
              src_body:=src_body+src_indent+'stratoStrToFile('+rs(p1)+','+rs(p2)+');'#13#10;
             end;
            xSCall_filetomem:
             begin
              //uses StratoUtils?
              p0.Start(cb,lCodeBlock_Locals);
              p0.Next(p1);
              p0.Next(p2);
              src_body:=src_body+src_indent+'Result:=stratoFileToMem('+rs(p2)+');'#13#10;
             end;
            xSCall_memtofile:
             begin
              //uses StratoUtils?
              p0.Start(cb,lCodeBlock_Locals);
              p0.Next(p1);
              p0.Next(p2);
              src_body:=src_body+src_indent+'stratoMemToFile('+rs(p1)+','+rs(p2)+');'#13#10;
             end;

            else raise Exception.Create('Unknown SysCall:'+
              string(p1.sphere.BinaryData(p1.index)));
          end;
         end;

        else
         begin
          src_body:=src_body+src_indent+rs(p)+';//'+AnsiString(KeyToStr(p.Key))+#13#10;
          //raise?
         end;
        end;

      sExpress:
        src_body:=src_body+src_indent+src_value+'; //'+rs(p)+#13#10;

      sResolveValue:
        case p.Key of
        nLiteral:
         begin
          p1:=p.r(iValue);
          if p1.IsNone then
            //raise?
          else
          if p.r(iType).IsSame(IntrinsicTypes[itString]) then
            src_value:=AnsiString(''''+StringReplace(
              UTF8ToString(p1.sphere.BinaryData(p1.index)),'''','''''',[rfReplaceAll])+'''')
          else
            src_value:=AnsiString(p1.sphere.BinaryData(p1.index));//TODO: encoding?
         end;
        nVar:
          if p.IsSame(ResultVar) then
            src_value:='Result'
          else
            src_value:=rs(p);
        nVarReadOnly:
          src_value:=rs(p);
        nConstant:
          //TODO: if type string then "'"
         begin
          p1:=p.r(iValue).r(iValue);
          src_value:=AnsiString(p1.sphere.BinaryData(p1.index));
         end;
        nUnaryOp:
         begin
          Push(sUnaryOp1,p);
          Push(sResolveValue,p.r(iRight));
         end;
        nBinaryOp:
         begin
          Push(sBinaryOp1,p);
          Push(sResolveValue,p.r(iLeft));
         end;

        nArrayIndex:
         begin
          p1:=p.r(iSubject);
          //if not(p1.r(iParent))=cb then Add(p1,level+1);//TODO
          src_value:=rs(p1);
          p0.Start(p,lArguments);
          if p0.Next(p) then
           begin
            Push(sArrayIndex1,p0,src_value+'[');
            Push(sResolveValue,p.r(iValue));
            src_value:='';
           end
          else
            src_value:=src_value+'[]';
         end;

        nFCall:
         begin
          p1:=Add(p.r(iTarget),level+1);
          src_value:=rs(p1);
          p0.Start(p,lArguments);
          if p0.Next(p) then
           begin
            Push(sFCall1,p0,src_value+'(');
            Push(sResolveValue,p.r(iValue));
            src_value:='';
           end;
         end;

        nCast:
         begin
          Push(sCast1,p);
          Push(sResolveValue,p.r(iSubject));
         end;

        nField:
         begin
          Push(sField1,p);
          Push(sResolveValue,p.r(iSubject));
         end;

        //xUnassigned?
        else
          raise Exception.Create('Unsupported value type '+p.AsString);
        end;

      sUnaryOp1:
       begin
        case TStratoToken(p.v(vOperator)) of
        stOpNot:c2:='not';
        stOpAdd:c2:='+';
        stOpSub:c2:='-';
        stOpInc:c2:='inc';
        stOpDec:c2:='dec';
        //TODO: more
        else raise Exception.Create('Unknown operator type');
        end;
        src_value:=c2+'('+src_value+')';
       end;
      sBinaryOp1:
       begin
        Push(sBinaryOp2,p,src_value);
        Push(sResolveValue,p.r(iRight));
        src_value:='';
       end;
      sBinaryOp2:
       begin
        case TStratoToken(p.v(vOperator)) of

        stOpEQ  :c2:='=';
        stOpNEQ :c2:='<>';
        stOpLT  :c2:='<';
        stOpLTE :c2:='<=';
        stOpGT  :c2:='>';
        stOpGTE :c2:='>=';

        stOpAnd :c2:='and';
        stOpOr  :c2:='or';
        //stOpNot, //"!"
        stOpXor :c2:='xor';

        stOpAdd :c2:='+';
        stOpSub :c2:='-';
        stOpMul :c2:='*';
        stOpDiv :c2:='div'; //TODO: if not integer then '/'?
        stOpMod :c2:='mod';
        //stOpInc, //"++"
        //stOpDec, //"--"
        stOpShl :c2:='shl';
        stOpShr :c2:='shr';
        //stOpRange, //".."

        //TODO: more
        else raise Exception.Create('Unknown operator type');
        end;
        src_value:='(('+c1+') '+c2+' ('+src_value+'))';
       end;

      sAssign1:
       begin
        Push(sAssign2,p,src_value);
        Push(sResolveValue,p.r(iValue));
        src_value:='';
       end;
      sAssign2:
       begin
        case TStratoToken(p.v(vOperator)) of
          stOpAssign:c2:=':=';
          stOpAssignAdd:c2:=':='+c1+'+';
          stOpAssignSub:c2:=':='+c1+'-';
          stOpAssignMul:c2:=':='+c1+'*';
          stOpAssignDiv:c2:=':='+c1+' div ';
          stOpAssignMod:c2:=':='+c1+' mod ';
          stOpAssignOr:c2:=':='+c1+' or ';
          stOpAssignAnd:c2:=':='+c1+' and ';
          else raise Exception.Create('Unsupported assignment operator '+p.AsString);
        end;
        src_body:=src_body+src_indent+c1+c2+
          src_value+';//'+rs(p)+#13#10;
        src_value:='';
       end;

      sSelection1:
       begin
        src_body:=src_body+src_indent+'if '+src_value+' then //'+rs(p)+#13#10;
        Push(sSelection2,p);
        p1:=p.r(iDoTrue);
        if p.r(iDoFalse).IsNone then c2:=';' else c2:='';
        if not p1.IsNone then
          if p1.Key=nCodeBlock then
            StartCodeBlock(p1,c2)
          else
            StartBeginEnd(p1,c2);
       end;
      sSelection2:
       begin
        p1:=p.r(iDoFalse);
        if not p1.IsNone then
         begin
          src_body:=src_body+src_indent+'else'+#13#10;
          if p1.Key=nCodeBlock then
            StartCodeBlock(p1)
          else
            StartBeginEnd(p1);
         end;
       end;

      sIteration1:
       begin
        src_body:=src_body+src_indent+'while '+src_value+' do //'+rs(p)+#13#10;
        p1:=p.r(iBody);
        if not p1.IsNone then
          if p1.Key=nCodeBlock then
            StartCodeBlock(p1)
          else
            StartBeginEnd(p1);
        //TODO: iReturnType
       end;

      sIteration2:
       begin
        Push(sIteration3,p);
        Push(sResolveValue,p.r(iPredicate));
       end;

      sIteration3:
       begin
        src_body:=src_body+src_indent+'until not('+src_value+');'#13#10;
        //TODO: iReturnType
       end;

      sIteration4:
       begin
        Push(sIteration5,p,src_value);
        Push(sResolveValue,p.r(iPredicate).r(iRight).r(iRight));
       end;

      sIteration5:
       begin
        src_body:=src_body+src_indent+'for '+rs(p.r(iPredicate).r(iLeft))+':='+c1+' to '+src_value+' do //'+rs(p)+#13#10;
        p1:=p.r(iBody);
        if not p1.IsNone then
          if p1.Key=nCodeBlock then
            StartCodeBlock(p1)
          else
            StartBeginEnd(p1);
        //TODO: iReturnType
       end;

      sField1:
       begin
        src_value:=src_value+'.'+rs(p.r(iTarget));
       end;

      sArrayIndex1:
       begin
        src_value:=c1+src_value;
        if p.Next(p1) then
         begin
          Push(sArrayIndex1,p0,src_value+',');
          Push(sResolveValue,p.r(iValue));
          src_value:='';
         end
        else
          src_value:=src_value+']';
       end;

      sFCall1:
       begin
        src_value:=c1+src_value;
        if p.Next(p1) then
         begin
          Push(sFCall1,p0,src_value+',');
          Push(sResolveValue,p.r(iValue));
          src_value:='';
         end
        else
          src_value:=src_value+')';
       end;

      sCast1:
       begin
        //TODO: resolve type?
        p1:=Add(p.r(iType),level);
        p0:=p.r(iSubject).r(iType);
        if p1.IsSame(IntrinsicTypes[itString]) and IsIntrinsicNumeric(p0) then
          src_value:='IntToStr('+src_value+')'
        else
        if IsIntrinsicNumeric(p1) and p0.IsSame(IntrinsicTypes[itString]) then
          src_value:='StrToInt('+src_value+')'
        else
          src_value:=rs(p1)+'('+src_value+')';
       end;

      //s_Unknown:
      else
        raise Exception.Create('Unexpected stack step');
      end;

    src_body:=src_body+'end';
    //src:=src+'end;'#13#10#13#10;

    //
    if src_var<>'' then src_body:='var'#13#10+src_var+src_body;
    if src_type<>'' then src_body:='type'#13#10+src_type+src_body;
    if src_const<>'' then src_body:='const'#13#10+src_const+src_body;

    Result:=src_body;
  end;

  procedure AddFuncProc(p:xNode;level:cardinal;const suffix:AnsiString);
  var
    sig:AnsiString;
  begin
    sig:=Signature(p,true,level+1);
    if suffix<>'' then sig:=sig+' //'+suffix;
    AddCode(p,ctInterface,level,sig);
    AddCode(none,ctImplementation,level,sig+#13#10+
      CodeBlock(p.r(iBody),none,none,level)+';'#13#10);
  end;

  procedure pType1(tt:TStratoIntrinsicType;const pName:AnsiString);
  var
    p:xNode;
    n:UTF8String;
  begin
    p:=IntrinsicTypes[tt];
    if not p.IsNone then
     begin
      n:=p.sphere.GetName(p.sphere.n(p.index));
      AddCode(p,ctInterfaceType,0,rs(p)+' = '+pName+'; //'+AnsiString(n));
     end;
  end;

  procedure pType2(pp:xNode;const sName:UTF8String;const pName:AnsiString);
  var
    p,p0:xNode;
    n:UTF8String;
  begin
    p0.Start(pp,lChildren);
    while p0.Next(p) do
     begin
      n:=p.sphere.GetName(p.sphere.n(p.index));
      if n=sName then break;
     end;
    if not p.IsNone then
      AddCode(p,ctInterfaceType,0,rs(p)+' = '+pName+'; //'+AnsiString(n));
  end;

var
  i,j:cardinal;
  p,p0,p1,p2,q,q0,q1,q2:xNode;
  f:TFileStream;
  src,src1,src2,n:AnsiString;
  ct:TCodeType;
  level:cardinal;
  ii:integer;
  b:boolean;

const
  CodeTypeName:array[TCodeType] of string=(
    'pend_',
    'intf_','intfT','intfC','intfV','intfX',
    'impl_','implT','implC','implV','implX',
    'intfU','implU','init0','final');

begin
  ci:=0;
  cl:=0;

  //defaults
  level:=1;
  AddCode(none,ctInterfaceUses,level,'Windows, SysUtils, StratoUtils');//Variants?

  //intrinsic types
  pType1(itPointer,'pointer');
  pType1(itBoolean,'boolean');
  pType1(itNumber,'integer');//NativeInt?
  //itObject, see below
  pType1(itString,'string');

  //'Strato'?
  p0.s(Spheres[0],0);
  p0.Start(p0,lChildren);//lSourceFile_NameSpaces

  if p0.Next(p) then
   begin
    pType2(p,'i8','ShortInt');
    pType2(p,'i16','SmallInt');
    pType2(p,'i32','integer');
    pType2(p,'i64','int64');
    pType2(p,'u8','byte');
    pType2(p,'u16','word');
    pType2(p,'u32','cardinal');
    pType2(p,'u64','Uint64');

    pType2(p,'f32','single');
    pType2(p,'f64','double');
    pType2(p,'f80','extended');
   end;
  //else raise?

  //global variables
  for i:=0 to SpheresCount-1 do
   begin
    p0.s(Spheres[i],0);
    p0.Start(p0,lSphere_Globals);
    while p0.Next(p) do
     begin
      p1:=p.r(iTarget);
      AddCode(p1,ctInterfaceVar,level,rs(p1)+': '+rs(Add(p1.r(iType),level))+'; //global '+rs(p));
      p2:=p1.r(iValue);
      if not p2.IsNone then
       begin
        AddCode(p2,ctInitialization,level,rs(p1)+':='+
          //TODO:
          rs(p2)+';');
       end;

      //Add(p);
     end;
   end;

  //initialization
  for i:=0 to SpheresCount-1 do
   begin
    p:=Spheres[i].r(0,iSphere_InitializationBlock);
    if not p.IsNone then
     begin
      AddCode(p,ctImplementation,level,'procedure '+rs(p)+';'#13#10+
        CodeBlock(p,none,none,level)+';'#13#10);
      AddCode(none,ctInitialization,level,rs(p)+';');
     end;
   end;

  //finalization
  for i:=SpheresCount-1 downto 0 do
   begin
    p:=Spheres[i].r(0,iSphere_FinalizationBlock);
    if not p.IsNone then
     begin
      AddCode(p,ctImplementation,level,'procedure '+rs(p)+';'#13#10+
        CodeBlock(p,none,none,level)+';'#13#10);
      AddCode(none,ctFinalization,level,rs(p)+';');
     end;
   end;

  //expand any listed
  level:=1;
  i:=0;
  while i<ci do
   begin
    if c[i].t=ctPending then
     begin
      if c[i].l>level then
        level:=c[i].l
      else
       begin
        c[i].l:=level;
        inc(level);
       end;
      ct:=ctInterface; //default
      src:='';
      p:=c[i].p;
      case p.Key of

      nArray:
       begin
        q:=Add(p.r(iType),level);
        ct:=ctInterfaceType;
        src:=rs(p)+' = array [0..'+AnsiString(IntToStr(
          p.v(vByteSize) div ByteSize(q)))+'-1] of '+rs(q)+';';
       end;

      nRecord:
       begin
        ct:=ctInterfaceType;
        src:=rs(p)+' = record'#13#10;
        p0.Start(p,lChildren);
        while p0.Next(p1) do
          case p1.Key of

          nVar:
           begin
            ii:=p1.v(vOffset);
            if ii<0 then
              src:=src+'    //'+rs(p1)+' offset '+AnsiString(IntToStr(ii))+#13#10
            else
             begin
              src:=src+'    '+rs(p1)+': '+rs(Add(p1.r(iType),level))+';// offset '+
                AnsiString(IntToStr(ii))+#13#10;
              //TODO: p1.v(vOffset)
             end;
           end;

{//TODO:?
          nMember:
           begin
            q0.Start(p1,lChildren);
            while q0.Next(q1);
              AddFuncProc(q1,level+1,'');
           end;
}

          else //TODO: raise?
            src:=src+'    //'+rs(p1)+' '+AnsiString(
              KeyToStr(p1.Key))+#13#10;
          end;
        src:=src+'  end;';
       end;

      nClass:
       begin
        ct:=ctInterfaceType;
        //TODO?
        src:=rs(p)+' = record //class'#13#10;
        if p.IsSame(IntrinsicTypes[itObject]) then
          src:=rs(p)+'_d = procedure(nThis: pointer);'#13#10'  '+src;
        src2:=rs(p)+'_v : record //vtable'#13#10+
          '    xx_dtor:'+rs(IntrinsicTypes[itObject])+'_d;'#13#10;

        //destructor pointer
        p0.Start(p,lChildren);
        while p0.Next(p1) and (p1.Key<>nDtor) do ;
        if p1.IsNone then
          AddCode(none,ctInitialization,level,rs(p)+'_v.xx_dtor:=nil')
        else
          AddCode(none,ctInitialization,level,rs(p)+'_v.xx_dtor:=@'+rs(p1));

        p0.Start(p,lChildren);
        while p0.Next(p1) do
          case p1.Key of

          nVar:
           begin
            ii:=p1.v(vOffset);
            if ii<0 then
              src:=src+'    //'+rs(p1)+' offset '+AnsiString(IntToStr(ii))+#13#10
            else
             begin
              src:=src+'    '+rs(p1)+': '+rs(Add(p1.r(iType),level))+';// offset '+
                AnsiString(IntToStr(ii))+#13#10;
              //TODO: p1.v(vOffset)
             end;
           end;

{//TODO:?
          nMember:
           begin
            ListFirst(p1,lItems,q1,q0);
            while q1.x<>0 do
             begin
              q2:=q1.r(iSignature);
              AddCode(q2,ctInterfaceType,level+2,rs(q2)+' = '+Signature(q2,false,level+2));
              n:=AnsiString(GetName(p1.v(iName)));
              src2:=src2+'    '+n+':'+rs(q2)+';'#13#10;
              AddCode(xxr(0),ctInitialization,level,rs(p)+'_v.'+n+':=@'+rs(q1)+';');
              AddFuncProc(q1,level+1,'');
              ListNext(q1,q0);
             end;
           end;

          nCtors:
           begin
            ListFirst(p1,lItems,q1,q0);
            while q1.x<>0 do
             begin
              Add(q1,level);
              ListNext(q1,q0);
             end;
           end;
}

          nDtor:
            AddFuncProc(p1,level,'dtor');

          else //TODO: raise?
            src:=src+'    //'+rs(p1)+' '+AnsiString(
              KeyToStr(p1.Key))+#13#10;
          end;
        src:=src+'  end;';
        AddCode(none,ctInterfaceVar,level,src2);
       end;

      nCtor:
       begin
        src:=Signature(p,true,level+1)+' //ctor';
        AddCode(none,ctImplementation,level,src+#13#10+
          CodeBlock(p.r(iBody),none,none,level)+';'#13#10);
       end;

      nClassRef:
       begin
        ct:=ctInterfaceType;
        src:=rs(p)+' = pointer; //cref '+rs(p.r(iTarget))+';';
       end;

      nOverload:
        case p.r(iParent).Key of

        nNameSpace:
         begin
          ct:=ctImplementation;
          p1:=p.r(iSignature).r(iReturnType);
          if p1.IsNone then
           begin
            src:='procedure ';
            p2.none;
           end
          else
           begin
            p0.Start(p.r(iBody),lCodeBlock_Locals);
            p0.Next(p2);
            src:='function ';
           end;
          src:=src+rs(p);
          p0.Start(p.r(iSignature),lArguments);
          q2.none;
          if not p0.IsNone then
           begin
            src:=src+'(';
            while p0.Next(q1) do
             begin
              q2:=q1.r(iArgVar);
              src:=src+rs(q2)+':'+rs(Add(q1.r(iType),level))+';';
             end;
            src[Length(src)]:=')';
           end;
          if not p1.IsNone then src:=src+':'+rs(Add(p1,level));
          src:=src+';'#13#10+CodeBlock(p.r(iBody),q2,p2,level)+';'#13#10;
         end;

        nClass:
          Add(p.r(iParent),level+1);//?

        else
          raise Exception.Create('Overload: unsupported parent '+KeyToStr(p.r(iParent).Key));
        end;

      else
        src:='//'+rs(c[i].p)+' '+
          AnsiString(KeyToStr(p.Key));//TODO: error?
      end;
      c[i].t:=ct;
      c[i].c:=src;
      //c[i].l see above
     end;
    inc(i);
   end;

  //now generate source code
  i:=Length(FilePath);
  while (i<>0) and (FilePath[i]<>'.') do dec(i);
  j:=i;
  while (j<>0) and (FilePath[j]<>PathDelim) do dec(j);
  //src:='program '//?
  //src:='library '//?
  src:='unit '
    +AnsiString(Copy(FilePath,j,i-j-1))+
    ';'#13#10#13#10'interface'#13#10#13#10;

  if false then//if debug
   begin
    src:=src+#13#10'{'#13#10;
    for i:=0 to ci-1 do
      src:=src+AnsiString(Format('%s %s %d %d'#13#10,
        [rs(c[i].p),CodeTypeName[c[i].t],c[i].l,Length(c[i].c)]));
    src:=src+#13#10'}'#13#10;
   end;

  j:=0;
  for i:=0 to ci-1 do
    if c[i].t=ctInterfaceUses then
     begin
      if j=0 then
       begin
        src:=src+'uses ';
        j:=1;
       end
      else
        src:=src+', ';
      src:=src+c[i].c;
     end;
  if j<>0 then src:=src+';'#13#10;

  j:=0;
  for i:=0 to ci-1 do
    if (c[i].t in [ctInterface..ctInterface_Any]) and (c[i].l>j) then
      j:=c[i].l;

  ct:=ctInterface;
  while j<>0 do
   begin
    for i:=ci-1 downto 0 do
      if (c[i].l=j) and (c[i].t in [ctInterface..ctInterface_Any]) and (c[i].c<>'') then
       begin
        case c[i].t of
        ctInterfaceType:
          if ct<>ctInterfaceType then
            src:=src+#13#10'type'#13#10;
        ctInterfaceConst:
          if ct<>ctInterfaceConst then
            src:=src+#13#10'const'#13#10;
        ctInterfaceVar:
          if ct<>ctInterfaceVar then
            src:=src+#13#10'var'#13#10;
        end;
        //src:=src+#13#10;
        ct:=c[i].t;
        if c[i].t<>ctInterface then src:=src+'  ';
        src:=src+c[i].c+#13#10;
       end;
    dec(j);
   end;

  src:=src+#13#10'implementation'#13#10#13#10;

  j:=0;
  for i:=0 to ci-1 do
    if c[i].t=ctImplementationUses then
     begin
      if j=0 then
       begin
        src:=src+'uses ';
        j:=1;
       end
      else
        src:=src+', ';
      src:=src+c[i].c;
     end;
  if j<>0 then src:=src+';'#13#10;

  j:=0;
  for i:=0 to ci-1 do
    if (c[i].t in [ctImplementation..ctImplementation_Any]) and (c[i].l>j) then
      j:=c[i].l;

  ct:=ctInterface;
  while j<>0 do
   begin
    for i:=ci-1 downto 0 do
      if (c[i].l=j) and (c[i].t in [ctImplementation..ctImplementation_Any]) and (c[i].c<>'') then
       begin
        case c[i].t of
        ctImplementationType:
          if ct<>ctImplementationType then
            src:=src+#13#10'type'#13#10;
        ctImplementationConst:
          if ct<>ctImplementationConst then
            src:=src+#13#10'const'#13#10;
        ctImplementationVar:
          if ct<>ctImplementationVar then
            src:=src+#13#10'var'#13#10;
        end;
        //src:=src+#13#10;
        ct:=c[i].t;
        if c[i].t<>ctImplementation then src:=src+'  ';
        src:=src+c[i].c+#13#10;
       end;
    dec(j);
   end;

  j:=0;
  for i:=0 to ci-1 do
    if c[i].t=ctInitialization then
     begin
      if j=0 then
       begin
        src:=src+#13#10'initialization'#13#10;
        j:=1;
       end;
      src:=src+'  '+c[i].c+#13#10;
     end;
  if j<>1 then src:=src+#13#10;

  j:=0;
  for i:=0 to ci-1 do
    if c[i].t=ctFinalization then
     begin
      if j=0 then
       begin
        src:=src+#13#10'finalization'#13#10;
        j:=1;
       end;
      src:=src+'  '+c[i].c+#13#10;
     end;
  if j<>1 then src:=src+#13#10;

  src:=src+#13#10'end.'#13#10;

  //write code to file
  f:=TFileStream.Create(FilePath,fmCreate);
  try
    f.Write(src[1],Length(src));
  finally
    f.Free;
  end;
end;

end.
