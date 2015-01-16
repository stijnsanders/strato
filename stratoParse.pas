unit stratoParse;

interface

uses stratoDecl, stratoSphere, stratoSource;

function StratoParseSource(Sphere: TStratoSphere;
  Source: TStratoSource): TStratoIndex;

implementation

uses SysUtils, stratoTokenizer, stratoRunTime, stratoFn, stratoLogic;

type
  TPrecedence=(
    p___,
    pCodeBlock,
      p_Statement,
    pThrow,pDefer,pCatch,
    pBrackets,
    pParentheses,
    pForBodyFirst,pForFirst,pForCrit,pForThen,pForCritDone,pForCritOnly,
      p_CodeBlockDone,
    pForBody,pIfThen,pIfElse,
    pUnTypedVar,
    pArgList,
      p_ArgList_Item,
    pAssignment,
      p_Juxta,
    pOr,
    pXor,
    pAnd,
    pEqual,
    pComparative,
    pShift,
    pAddSub,
    pMulDiv,
      p_Cast,
    pUnary,
    pTypeOf,pSizeOf,pAddressOf
  );

function StratoParseSource(Sphere: TStratoSphere;
  Source: TStratoSource): TStratoIndex;
var
  Locals:array of TStratoIndex;
  src:TStratoIndex;

  procedure ID(var n:TStratoName;var nn:UTF8String);
  begin
    nn:=Source.GetID;
    n:=Sphere.Dict.StrIdx(nn);
  end;

  function LookUpNameSpace(var Fresh:boolean):TStratoIndex;
  var
    n:TStratoName;
    nn:UTF8String;
    ns,p:TStratoIndex;
    nx:PStratoNameSpaceData;
  begin
    Fresh:=false;//default
    ID(n,nn);
    p:=0;
    ns:=Sphere.Header.FirstNameSpace;
    while (ns<>0)
      and not((Sphere[ns].ThingType=ttNameSpace) and (Sphere[ns].Name=n)) do
     begin
      p:=ns;
      ns:=Sphere[ns].Next;
     end;
    if ns=0 then
     begin
      //not found, create
      ns:=Sphere.Add(ttNameSpace,PStratoThing(nx));
      nx.Name:=n;
      nx.SourceFile:=src;//?
      //nx.SrcPos:=
      Sphere[p].Next:=ns;
      Fresh:=true;
     end;
    //resolve nested namespaces
    while Source.IsNext([stPeriod,stIdentifier]) do
     begin
      ID(n,nn);
      p:=Sphere[ns].FirstItem;
      while (p<>0) and not((Sphere[p].ThingType=ttNameSpace) and (Sphere[p].Name=n)) do
        p:=Sphere[p].Next;
      if p=0 then
       begin
        //not found, create
        if not Sphere.AddTo(Sphere[ns].FirstItem,
          ttNameSpace,n,p,PStratoThing(nx)) then
          Source.Error('duplicate namespace "'+nn+'"');
        nx.Parent:=ns;
        nx.Name:=n;
        nx.SourceFile:=src;//?
        //nx.SrcPos:=
        Fresh:=true;
       end;
      ns:=p;
     end;
    Result:=ns;
  end;

  procedure ParseImport;
  var
    ns,p:TStratoIndex;
    px:PStratoThing;
    ss:TStratoSource;
    alias:UTF8String;
    fn:string;
    b:boolean;
    i,l:integer;
  begin
    ns:=0;//default
    //alias?
    if Source.IsNext([stIdentifier,stOpEQ]) then
     begin
      alias:=Source.GetID;
      Source.Token;//stOpEQ
     end
    else
      alias:='';
    case Source.Token of
      stIdentifier:
       begin
        ns:=LookupNameSpace(b);
        //TODO: load from standard library !!!
        if b then
         begin
          fn:=Sphere.BasePath+string(Sphere.FQN(ns))+'.xs';
          if FileExists(fn) then
           begin
            b:=false;
            ss:=TStratoSource.Create;
            ss.LoadFromFile(fn);
            ns:=StratoParseSource(Sphere,ss);
           end;
         end;
        if b then
          Source.Error('unknown namespace "'+string(Sphere.FQN(ns))+'"');
       end;
      stStringLiteral:
       begin
        ss:=TStratoSource.Create;
        //TODO: resolve relative path, list of paths, system paths
        //TODO: allow duplicates? detect by full path?
        ss.LoadFromFile(string(Source.GetStr));
        ns:=StratoParseSource(Sphere,ss);
       end;
      else Source.Error('unsupported import subject syntax');
    end;
    Source.Skip(stSemiColon);
    //register
    if ns<>0 then
      if alias<>'' then //alias
       begin
        p:=Sphere.AddTo(Sphere[Locals[0]].FirstItem,
          ttImport,Sphere.Dict.StrIdx(alias),px);
        if p=0 then
          Source.Error('duplicate identifier "'+alias+'"')
        else
         begin
          px.SrcPos:=Source.SrcPos;
          px.Parent:=Locals[0];
          px.Target:=ns;
         end;
       end
      else
       begin
        //TODO: store locals in Sphere?
        l:=Length(Locals);
        i:=0;
        while (i<>l) and (Locals[i]<>ns) do inc(i);
        if i=l then
         begin
          SetLength(Locals,i+1);
          Locals[i]:=ns;
         end;
       end;
  end;

  function LookUp:TStratoIndex;
  var
    i,j,l:integer;
    n:TStratoName;
    nn:UTF8String;
    nsx:array of TStratoIndex;
    p,q:TStratoIndex;//absolute nsx?
    px:PStratoNameSpaceData;
  begin
    //assert Source.IsNext([stIdentifier]);
    Result:=0;//default
    l:=Length(Locals);
    SetLength(nsx,l);
    for i:=0 to l-1 do nsx[i]:=Locals[i];
    j:=l;
    repeat
      ID(n,nn);
      for i:=0 to l-1 do
        if nsx[i]<>0 then
         begin
          nsx[i]:=Sphere.Lookup(Sphere[nsx[i]].FirstItem,n);
          if nsx[i]=0 then
            dec(j)
          else
            if Sphere[nsx[i]].ThingType=ttImport then
              nsx[i]:=Sphere[nsx[i]].Target;
         end;
    until (j=0) or not(Source.IsNext([stPeriod,stIdentifier]));
    //assert Source.Token=stIdentifier
    case j of
      0://none found, try namespaces
       begin
        p:=Sphere.Header.FirstNameSpace;
        q:=0;
        while (p<>0) and (Sphere[p].Name<>n) do
         begin
          q:=p;
          p:=Sphere[p].Next;
         end;
        while Source.IsNext([stPeriod,stIdentifier]) do
         begin
          if (p=0) or ((Sphere[p].ThingType and tt__Resolvable)=0) then
           begin
            p:=Sphere.Add(ttNameSpace,PStratoThing(px));
            px.Name:=n;
            px.SourceFile:=src;//?
            //px.SrcPos:=
            Sphere[q].Next:=p;
           end;
          ID(n,nn);
          p:=Sphere[p].FirstItem;
          q:=0;
          while (p<>0) and (Sphere[p].Name<>n) do
           begin
            q:=p;
            p:=Sphere[p].Next;
           end;
         end;
        Result:=p;
       end;
      1://one thing found
       begin
        i:=0;
        while (i<>l) and (nsx[i]=0) do inc(i);
        Result:=nsx[i];
       end;
      else//multiple found
       begin
        nn:='';
        for i:=0 to l-1 do
          if nsx[i]<>0 then
            nn:=nn+','+Sphere.FQN(nsx[i]);
        Source.Error('multiple declarations "'+
          string(Copy(nn,2,Length(nn)-1))+'"');
       end;
    end;
  end;

  procedure LookUpNext(n:TStratoName;const nn:UTF8String;
    var p,ns:TStratoIndex;var b:boolean);
  var
    i,l:integer;
    px:PStratoNameSpaceData;
  begin
    if p=0 then
     begin
      l:=Length(Locals);
      i:=0;
      while (i<>l) and (p=0) do
       begin
        p:=Sphere.Lookup(Sphere[Locals[i]].FirstItem,n);
        inc(i);
       end;
      if p=0 then //still nothing, check namespaces
        p:=Sphere.Lookup(Sphere.Header.FirstNameSpace,n);
     end
    else
      if (Sphere[p].ThingType and tt__Resolvable)<>0 then
        p:=Sphere.Lookup(Sphere[p].FirstItem,n)
      else
        p:=0;//see placeholder below
    if (p<>0) and ((Sphere[p].ThingType and tt__Resolvable)<>0) then
      ns:=p
    else
     begin
      b:=true;//Source.Error see below
      //create to silence further errors?
      p:=Sphere.AddTo(Sphere[ns].FirstItem,ttNameSpace,n,PStratoThing(px));
      if p=0 then
        Source.Error('duplicate namespace "'+
          string(Sphere.FQN(ns)+'.'+nn)+'"')
      else
       begin
        px.Parent:=ns;
        px.SourceFile:=src;//?
        //px.SrcPos:=?
       end;
      ns:=p;
     end;
  end;

  function ParseLiteral(st0:TStratoToken):TStratoIndex;
  const
    stackGrowSize=$10;
  var
    st:TStratoToken;
    vt:TSTratoIndex;
    v:UTF8String;
    px:PStratoThing;
    stackSize,stackIndex:integer;
    stack:array of record
      p:TPrecedence;
      vt:TStratoIndex;
      v1,v2:UTF8String;
    end;
    procedure Combine(pp:TPrecedence);
    var
      p0:TPrecedence;
      v0,w:UTF8String;
      wt:TStratoIndex;
      done:boolean;
      i,j:int64;
    begin
      if vt<>0 then
       begin
        done:=false;
        while not(done) and (stackIndex<>0) and (stack[stackIndex-1].p>=pp) do
         begin
          dec(stackIndex);
          p0:=stack[stackIndex].p;
          v0:=stack[stackIndex].v1;
          w:=stack[stackIndex].v2;
          wt:=stack[stackIndex].vt;
          stack[stackIndex].v2:='';
          case p0 of
            pParentheses:
              done:=true;//always only one (need to parse ")" correctly)
            pUnary:
              case v0[1] of
                '-':v:='-'+w;//assert vt=TypeDecl_number
                '!'://not
                  if vt=TypeDecl_bool then
                    if w='0' then v:='1' else v:='0'
                  else
                  if vt=TypeDecl_number then
                    //TODO: numerical notations
                    if TryStrToInt64(string(v),i) then
                      v:=UTF8String(IntToStr(not(i)))
                    else
                      Source.Error('invalid numeric literal')
                  else
                    Source.Error('unsupported type for ''not''');
                else
                  Source.Error('unknown unary operator');
              end;
            pSizeOf:
              w:=IntToStr(ByteSize(Sphere,vt));
            pMulDiv,pAddSub,pShift,pAnd,pOr,pEqual,pComparative:
              if vt=TypeDecl_number then
                if TryStrToInt64(string(v),i) and TryStrToInt64(string(w),j) then
                  case v0[1] of
                    '*':v:=UTF8String(IntToStr(i*j));
                    '/':v:=UTF8String(IntToStr(i div j));
                    '%':v:=UTF8String(IntToStr(i mod j));
                    '+':v:=UTF8String(IntToStr(i+j));
                    '-':v:=UTF8String(IntToStr(i-j));
                    'L':v:=UTF8String(IntToStr(i shl j));
                    'R':v:=UTF8String(IntToStr(i shr j));
                    '&':v:=UTF8String(IntToStr(i and j));
                    '|':v:=UTF8String(IntToStr(i or j));
                    'X':v:=UTF8String(IntToStr(i xor j));
                    '=':
                     begin
                      vt:=TypeDecl_bool;
                      if i=j then v:='1' else v:='0';
                     end;
                    '<':
                     begin
                      vt:=TypeDecl_bool;
                      if i<j then v:='1' else v:='0';
                     end;
                    'l':
                     begin
                      vt:=TypeDecl_bool;
                      if i<=j then v:='1' else v:='0';
                     end;
                    '>':
                     begin
                      vt:=TypeDecl_bool;
                      if i>j then v:='1' else v:='0';
                     end;
                    'g':
                     begin
                      vt:=TypeDecl_bool;
                      if i>=j then v:='1' else v:='0';
                     end;
                    else
                      Source.Error('unknown operator');
                  end
                else
                  Source.Error('invalid numeric literal')
              else
                case v0[1] of
                  '+':v:=w+v;
                  '=':
                   begin
                    vt:=TypeDecl_bool;
                    //TODO: CanCast?
                    if (vt=wt) and (v=w) then v:='1' else v:='0';
                   end;
                  else
                    Source.Error('unsupported type for operator');
                end;
            else
              Source.Error('unexpected constant operator precedence');
          end;
         end;
       end;
    end;
    procedure Push(p:TPrecedence;const vv:UTF8String);
    begin
      Combine(p);
      if stackIndex=stackSize then //grow
       begin
        inc(stackSize,stackGrowSize);
        SetLength(stack,stackSize);
       end;
      stack[stackIndex].p:=p;
      stack[stackIndex].v1:=vv;
      stack[stackIndex].v2:=v;
      stack[stackIndex].vt:=vt;
      inc(stackIndex);
      vt:=0;
      v:='';
    end;
  begin
    st:=st_Unknown;
    Result:=0;//default
    stackSize:=0;
    stackIndex:=0;
    vt:=0;
    v:='';
    while (Result=0) or (stackIndex<>0) do
     begin
      if st=st_Unknown then st:=st0 else st:=Source.Token;
      case st of
        stIdentifier:
         begin
          Result:=LookUp;
          if Result=0 then
           begin
            Source.Error('undefined constant value');
            Result:=Sphere.Add(ttVar,px);//avoid further errors
           end
          else
            case Sphere[Result].ThingType of
              ttConstant,ttVar:
               begin
                Result:=Sphere[Result].InitialValue;
                if (Result<>0) and (Sphere[Result].ThingType=ttLiteral) then
                 begin
                  px:=Sphere[Result];
                  if px.InitialValue=0 then
                    Source.Error('constant without value')
                  else
                   begin
                    Result:=px.InitialValue;
                    v:=Sphere.GetBinaryData(px.InitialValue);
                    vt:=px.EvaluatesTo;
                   end;
                 end;
               end;
              else
               begin
                Source.Error('unsupported constant value');
                Result:=0;
                vt:=0;
                v:='';
               end;
            end;
         end;
        stStringLiteral:
         begin
          vt:=TypeDecl_string;
          v:=Source.GetStrs;
         end;
        stNumericLiteral:
         begin
          vt:=TypeDecl_number;
          v:=Source.GetID;
         end;
        {//TODO:
        stAOpen://JSON?
        stBOpen://array
        stPOpen://tuple? expression?
        }
        stOpAdd://unary
          Push(pUnary,'+');
        stOpSub://unary
          Push(pUnary,'-');
        stOpSizeOf://unary
          Push(pSizeOf,'');
        stPOpen://expression (tuple?)
          Push(pParentheses,'');
        stPClose:
          if stackIndex=0 then
            Source.Error('unexpected token')
          else
            Combine(pParentheses);
        else
         begin
          Source.Error('unsupported literal syntax');
          vt:=TypeDecl_type;//break out of loop
         end;
      end;
      if vt<>0 then
        if Source.IsNextBetween(stOpAssign,stOpTypeIs) then
         begin
          st:=Source.Token;
          case st of
            stOpAdd:Push(pAddSub,'+');
            stOpSub:Push(pAddSub,'-');
            stOpMul:Push(pMulDiv,'*');
            stOpDiv:Push(pMulDiv,'/');
            stOpMod:Push(pMulDiv,'%');
            stOpShl:Push(pShift,'L');
            stOpShr:Push(pShift,'R');
            stOpAnd:Push(pAnd,'&');
            stOpOr:Push(pOr,'|');
            stOpNot:Push(pUnary,'!');
            stOpXor:Push(pXor,'X');
            stOpEQ:Push(pEqual,'=');
            stOpLT:Push(pComparative,'<');
            stOpLTE:Push(pComparative,'l');
            stOpGT:Push(pComparative,'>');
            stOpGTE:Push(pComparative,'g');
            else Source.Error('unsupported constant operator');
          end;
         end
        else
         begin
          Combine(p_ArgList_Item);//something between pParentheses and the operators
          if stackIndex=0 then
           begin
            if vt=0 then
             begin
              Result:=Sphere.Add(ttLiteral,px);
              Source.Error('literal of undetermined type');
             end
            else
              Result:=Sphere.Add(ttLiteral,px);
            px.SrcPos:=Source.SrcPos;
            px.EvaluatesTo:=vt;
            px.InitialValue:=Sphere.AddBinaryData(v);
           end;
         end;
     end;
  end;

  function ParseInteger:cardinal;//integer?
  var
    p:TStratoIndex;
    px:PStratoThing;
  begin
    p:=ParseLiteral(Source.Token);
    px:=Sphere[p];
    if p=0 then
      Source.Error('missing integer value')
    else
      case px.ThingType of
        ttLiteral:
          if px.EvaluatesTo=TypeDecl_number then
           begin
            //TODO: more numeric notations?
            if not TryStrToInt(string(Sphere.GetBinaryData(px.InitialValue)),integer(Result)) then
              Source.Error('invalid integer literal');
           end
          else
            Source.Error('integer literal expected')
        else
          Source.Error('invalid integer value');
      end;
  end;

  function LookUpType(const tname:string):TStratoIndex;
  var
    p:TStratoIndex;
    qx:PStratoThing;
    i,ptr:cardinal;
  begin
    ptr:=0;
    while Source.IsNext([stCaret]) do inc(ptr);
    Result:=LookUp;
    if Result=0 then Source.Error('undefined '+tname) else
     begin
      case Sphere[Result].ThingType of
        ttVar:
          Result:=Sphere[Result].EvaluatesTo;//take var's type
        ttAlias:
          Result:=Sphere[Result].Target;//assert never another ttAlias
        ttSignature:
         begin
          //TODO: ttDelegate? (keep signature and subject)
          p:=Result;
          Result:=Sphere.Add(ttPointer,qx);
          qx.Parent:=Sphere[p].Parent;
          qx.SrcPos:=Source.SrcPos;
          qx.ByteSize:=SystemWordSize;
          qx.EvaluatesTo:=p;
         end;
        else
          if (Sphere[Result].ThingType and tt__IsType)=0 then
            Source.Error(tname+' is not a type');
      end;
      //array
      if Source.IsNext([stBOpen]) then
        if Source.IsNext([stBClose]) then
          Source.Error('//TODO: dyn array')
        else
         begin
          i:=ParseInteger;
          Source.Skip(stBClose);//TODO: force
          p:=Result;
          Result:=Sphere.Add(ttArray,qx);
          qx.Parent:=Sphere[p].Parent;
          qx.SrcPos:=Source.SrcPos;
          qx.Name:=Sphere[p].Name;
          qx.ElementType:=p;
          qx.ByteSize:=ByteSize(Sphere,p)*i;
         end;
     end;
    if Result<>0 then
      while ptr<>0 do
       begin
        dec(ptr);
        //TODO: store somewhere?
        p:=Sphere.Add(ttPointer,qx);
        qx.Parent:=Sphere[Result].Parent;
        qx.SrcPos:=Source.SrcPos;
        qx.ByteSize:=SystemWordSize;
        qx.EvaluatesTo:=Result;
        Result:=p;
       end;
  end;

  procedure ParseRecordDecl(x:TStratoIndex);
  var
    p:TStratoIndex;
    px:PStratoThing;
    offset,i:cardinal;
    st:TStratoToken;
    b:boolean;
    fn:UTF8String;
  begin
    //assert previous token stAOpen
    while not(Source.IsNext([stAClose])) and Source.NextToken(st) do
      case st of

        stIdentifier:
         begin
          offset:=OffsetUseDefault;//default
          p:=0;//default
          fn:=Source.GetID;
          if Source.IsNext([stColon]) then
            case Source.Token of
              stIdentifier:
                p:=LookUpType('field type');
              stAOpen:
               begin
                p:=Sphere.Add(ttRecord,px);
                px.Name:=Sphere.Dict.StrIdx(fn);
                px.Parent:=x;
                px.SrcPos:=Source.SrcPos;
                //TODO: use custom stack instead of recursion
                ParseRecordDecl(p);
                //TODO: add struct/typedecl itself to something? x?ns?
               end;
              //more?
              else Source.Error('unsupported record field type syntax');
            end;
          //TODO: support constants, members of this structure
          if Source.IsNext([stAt,stNumericLiteral]) then
           begin
            if not TryStrToInt(string(Source.GetID),integer(offset)) then
              Source.Error('record field offset not an integer');
            b:=true;
            while b do
             begin
              if Source.IsNext([stOpAdd,stNumericLiteral]) then
                st:=stOpAdd
              else
              if Source.IsNext([stOpSub,stNumericLiteral]) then
                st:=stOpSub
              else
                st:=st_Unknown;
              if st=st_Unknown then b:=false else
                if TryStrToInt(string(Source.GetID),integer(i)) then
                  if st=stOpAdd then inc(offset,i) else dec(offset,i)
                else
                  Source.Error('record field offset delta not an integer');
             end;
           end;
          Source.Skip(stSemiColon);

          //register
          if StratoRecordAddField(Sphere,x,fn,p,offset,p,px) then
            px.SrcPos:=Source.SrcPos
          else
            Source.Error('duplicate record field "'+fn+'"');
         end;

        //stQuestionMark: nested interface?
        //more?

        else Source.Error('unsupported record field syntax');
      end;
  end;

  function ParseSignature(ns:TStratoIndex;const name:UTF8String):TStratoIndex;
  var
    st:TStratoToken;
    p,q,Signature,NoType:TStratoIndex;
    rx:PStratoThing;
    argName:UTF8String;
    byref:boolean;

    function AddArgument:boolean;
    var
      tt:cardinal;
      r:TStratoIndex;
    begin
      if byref then tt:=ttArgByRef else tt:=ttArgument;
      byref:=false;
      r:=Sphere.AddTo(Sphere[Signature].FirstArgument,
        tt,Sphere.Dict.StrIdx(argName),rx);
      if r=0 then
        Source.Error('duplicate argument "'+argName+'"')
      else
       begin
        if (p=0) and (NoType=0) then NoType:=r;
        rx.Parent:=Signature;
        rx.SrcPos:=Source.SrcPos;
        rx.EvaluatesTo:=p;
        //rx.InitialValue:=//TODO
       end;
      Result:=r<>0;
    end;

  begin
    //assert one past token stPOpen
    Signature:=Sphere.Add(ttSignature,rx);
    rx.Name:=Sphere.Dict.StrIdx(name);
    rx.Parent:=ns;
    rx.SrcPos:=Source.SrcPos;
    Result:=Signature;
    NoType:=0;
    byref:=false;
    st:=stIdentifier;//default (something not stPClose really)
    while (st<>stPClose) and Source.NextToken(st) do
      case st of

        stCaret:
          if byref then
            Source.Error('unsupported argument syntax')
          else
            byref:=true;

        stIdentifier:
         begin
          argName:=Source.GetID;
          p:=0;//default
          q:=0;//default
          st:=Source.Token;
          case st of
            stColon: //argument type
             begin
              p:=LookUpType('argument type');
              while NoType<>0 do
               begin
                rx:=Sphere[NoType];
                //assert rx.EvaluatesTo=0
                rx.EvaluatesTo:=p;
                NoType:=rx.Next;
               end;
              if Source.IsNext([stOpEQ]) then //default value
                q:=ParseLiteral(Source.Token);
              if AddArgument then rx.InitialValue:=q;
              if Source.IsNext([stComma]) or Source.IsNext([stSemiColon]) then ;//skip
             end;
            stComma:
              AddArgument;
            stOpEq:
             begin
              q:=ParseLiteral(Source.Token);
              if byref then
                Source.Error('default value on by-reference-argument not supported');
              if AddArgument then rx.InitialValue:=q;
              if not Source.IsNext([stComma]) then
                Source.Error('argument with default value but no type'+
                  ' requires a subsequent argument with type');
             end;
            else Source.Error('unsupported argument syntax');
          end;
         end;

        //TODO: byref? stAt?

        stPClose:;//done
        else Source.Error('unsupported argument syntax');
      end;
    //TODO: check default values with type
    if Sphere[ns].ThingType<>ttNameSpace then //strRecord,strTypeDecl
      Sphere[Signature].Target:=ns;
    if Source.IsNext([stColon,stIdentifier]) then
      Sphere[Signature].EvaluatesTo:=LookUpType('returns type');
  end;

  procedure ParseEnumeration(p:TStratoIndex;px:PStratoThing);
  var
    st:TStratoToken;
    n:TStratoName;
    nn:UTF8String;
    q:TStratoIndex;
    qx:PStratoThing;
    e:cardinal;
  begin
    e:=0;
    st:=stIdentifier;//default (something not stPClose really)
    while (st<>stPClose) and Source.NextToken(st) do
      case st of
        stIdentifier:
         begin
          ID(n,nn);
          if Source.IsNext([stOpEQ]) then e:=ParseInteger;
          q:=Sphere.AddTo(px.FirstItem,ttConstant,n,qx);
          if q=0 then
            Source.Error('duplicate enumeration entry "'+nn+'"')
          else
           begin
            qx.Parent:=p;
            qx.SrcPos:=Source.SrcPos;
            qx.EvaluatesTo:=p;
            qx.InitialValue:=e;
            inc(e);
           end;
         end;
        stComma:;//ignore
        stPClose:;//done
        else Source.Error('unsupported enumeration syntax');
      end;
  end;

  procedure ParseInterfaceDecl(x:TStratoIndex);
  var
    p,q:TStratoIndex;
    qx:PSTratoThing;
    st:TStratoToken;
    n:TStratoName;
    nn:UTF8String;
  begin
    //assert previous token stAOpen
    while not(Source.IsNext([stAClose])) and Source.NextToken(st) do
      case st of

        stIdentifier:
         begin
          ID(n,nn);
          case Source.Token of
            stColon:
              case Source.Token of
                stIdentifier:
                 begin
                  p:=LookUpType('field type');
                  q:=Sphere.AddTo(Sphere[x].FirstItem,ttVar,n,qx);
                  if q=0 then
                    Source.Error('duplicate interface field "'+nn+'"')
                  else
                   begin
                    qx.Parent:=x;
                    qx.SrcPos:=Source.SrcPos;
                    qx.EvaluatesTo:=p;
                   end;
                 end;
                //more?
                else Source.Error('unsupported interface field type syntax');
              end;
            stPOpen://signature
             begin
              p:=Sphere.Lookup(Sphere[x].FirstItem,n);
              if (p=0) or (Sphere[p].ThingType<>ttFunction) then
               begin
                if p<>0 then Source.Error('duplicate identifier "'+nn+'"');
                p:=Sphere.AddTo(Sphere[x].FirstItem,ttFunction,n,qx);
                qx.Parent:=x;
                qx.SrcPos:=Source.SrcPos;
               end;
              StratoFnAddOverload(Sphere,Source,p,ParseSignature(x,nn),src,nn);
             end;
            else Source.Error('unsupported interface field syntax');
          end;
          Source.Skip(stSemiColon);
         end;

        else Source.Error('unsupported interface field syntax');
      end;
  end;

const
  stackGrowSize=$100;
var
  stackSize,stackIndex:integer;
  stack:array of record
    p:TPrecedence;
    t:TStratoIndex;
  end;
  cb:TStratoIndex;
  cbInhCalled:boolean;

  procedure Push(p:TPrecedence;t:TStratoIndex);
  begin
    if stackIndex=stackSize then
     begin
      inc(stackSize,stackGrowSize);//grow
      SetLength(stack,stackSize);
     end;
    stack[stackIndex].p:=p;
    stack[stackIndex].t:=t;
    inc(stackIndex);
  end;

  procedure CodeLookup(n:TStratoName;var p:TStratoIndex);
  var
    i,l:integer;
    p0,q,r,s:TStratoIndex;
    px,cx:PStratoThing;
  begin
    p0:=p;
    //TODO: ttImport, ttAlias
    if p<>0 then
      r:=p //see below: search by type
    else
     begin
      cx:=Sphere[cb];
      r:=0;
      p:=Sphere.Lookup(cx.FirstItem,n);
      //not found? check stack
      if p=0 then
       begin
        i:=stackIndex;
        while (i<>0) and (p=0) do
         begin
          dec(i);
          case stack[i].p of
            pCodeBlock:
              p:=Sphere.Lookup(Sphere[stack[i].t].FirstItem,n);
            pCatch:
             begin
              q:=Sphere[stack[i].t].FirstArgument;
              if (q<>0) and (Sphere[q].Name=n) then p:=q;
             end;
          end;
         end;
        if p=0 then
          p:=Sphere.Lookup(cx.FirstItem,n);
       end;
      //not found? check under 'this'
      if (p=0) and (cx.FirstItem<>0) and
        (Sphere[cx.FirstItem].ThingType=ttThis) then
       begin
        p:=cx.FirstItem;
        r:=p;//see below: search by type
       end;
     end;
    if r<>0 then
     begin
      q:=p;
      if (Sphere[p].ThingType and tt__Resolvable)=0 then p:=0 else
        p:=Sphere.Lookup(Sphere[p].FirstItem,n);
      //nothing, is it typed? search typedecl
      if p=0 then
       begin
        r:=ResType(Sphere,q);
        if (r<>0) and (Sphere[r].ThingType=ttArray) then
          r:=0;//r:=Sphere[r].ElementType;
        if r<>0 then
         begin
          s:=Sphere.Lookup(Sphere[r].FirstItem,n);
          while (s=0) and (r<>0) and (Sphere[r].ThingType=ttClass) do
           begin
            r:=Sphere[r].InheritsFrom;
            if r=0 then
              s:=0
            else
              s:=Sphere.Lookup(Sphere[r].FirstItem,n);
           end;
          r:=s;
         end;
        if r<>0 then
         begin
          p:=Sphere.Add(ttVarIndex,px);
          px.Name:=n;
          px.Parent:=q;
          px.SrcPos:=Source.SrcPos;
          px.Target:=r;
          if (Sphere[r].ThingType and tt__Typed)<>0 then
            px.EvaluatesTo:=Sphere[r].EvaluatesTo;
         end;
       end;
     end;
    //not found? check locals
    if (p=0) and (p0=0) then
     begin
      l:=Length(Locals);
      i:=0;
      while (i<>l) and (p=0) do
       begin
        p:=Sphere.Lookup(Sphere[Locals[i]].FirstItem,n);
        inc(i);
       end;
      if p=0 then //still nothing, check namespaces
        p:=Sphere.Lookup(Sphere.Header.FirstNameSpace,n);
     end;
  end;

  procedure Combine(zz:TPrecedence;var q:TStratoIndex);
  var
    z,z00:TPrecedence;
    p,r:TStratoIndex;
    px,qx:PStratoThing;
    done:boolean;
  begin
    if (q<>0) or (zz<=pParentheses) then
     begin
      done:=false;
      while not(done) and (stackIndex<>0) and (stack[stackIndex-1].p>=zz) do
       begin
        dec(stackIndex);
        z:=stack[stackIndex].p;
        p:=stack[stackIndex].t;
        px:=Sphere[p];
        //qx:=Sphere[q];
        {$IFDEF DEBUG}
        stack[stackIndex].p:=p___;
        stack[stackIndex].t:=0;
        {$ENDIF}
        z00:=z;
        case z of
          pCodeBlock:
            //see also stAClose in main loop!
            done:=true;//always only one (need to parse "}" correctly)
          pIfThen:
           begin
            px.DoThen:=q;
            StratoSelectionCheckType(Sphere,p);
            z:=pIfElse;//now parse 'else' bit
           end;
          pIfElse:
           begin
            px.DoElse:=q;
            StratoSelectionCheckType(Sphere,p);
           end;
          pArgList:
           begin
            if q<>0 then
             begin
              StratoFnCallAddArgument(Sphere,p,q,qx);
              qx.Parent:=cb;
              qx.SrcPos:=Source.SrcPos;
             end;
            //subject stored in px.Target when Push(pArgList
            if px.Target<>0 then
             begin
              StratoFnCallFindSignature(Sphere,p,px.Target);
              if px.Target=0 then
                Source.Error('no function overload found with these arguments');
             end;
            done:=true;//always only one (need to parse ")" correctly)
           end;
          pBrackets:
           begin
            r:=Sphere.Add(ttVarIndex,qx);
            qx.Parent:=p;
            qx.SrcPos:=Source.SrcPos;
            qx.EvaluatesTo:=ResType(Sphere,p);
            qx.FirstArgument:=q;
            p:=r;
            done:=true;//always only one (need to parse "]" correctly)
           end;
          pParentheses:
           begin
            p:=q;
            done:=true;//always only one (need to parse ")" correctly)
           end;
          pForBodyFirst:
           begin
            if (q<>0) and (Sphere[q].ThingType=ttCodeBlock)
              and (Sphere[q].EvaluatesTo<>0) then
              Source.Error('unexpected iteration body with return value');
            px.Body:=q;
            z:=pForFirst;//now parse criterium
           end;
          pForFirst://see also stAClose
            if zz=pParentheses then //already closing? take this as crit
             begin
              px.DoIf:=q;
              if px.Body=0 then z:=pForBody;
             end
            else
             begin
              px.DoElse:=q;//px.DoFirst:=q;
              z:=pForCrit;
             end;
          pForCrit,pForCritOnly:
           begin
            if q<>0 then
             begin
              if not SameType(Sphere,ResType(Sphere,q),TypeDecl_bool) then
                Source.Error('iteration criterium does not evaluate to boolean');
              px.DoIf:=q;
             end;
            if zz=pParentheses then
             begin
              if px.Body=0 then z:=pForBody;
             end
            else
              if z<>pForCritOnly then z:=pForThen;
           end;
          pForThen:
           begin
            if q<>0 then px.DoThen:=q;//else assert already set by stPClose
            if zz=pParentheses then
             begin
              if px.Body=0 then z:=pForBody;
             end
            else
              z:=pForCritDone;
           end;
          pForCritDone:
           begin
            if q<>0 then
              Source.Error('unexpected iteration criterium syntax');
            if px.Body=0 then z:=pForBody;
           end;
          pForBody:
           begin
            if (q<>0) and (Sphere[q].ThingType=ttCodeBlock)
              and (Sphere[q].EvaluatesTo<>0) then
              Source.Error('unexpected iteration body with return value');
            px.Body:=q;
           end;
          pUnary:
           begin
            px.EvaluatesTo:=ResType(Sphere,q);
            px.Right:=q;
           end;
          pSizeOf:
           begin
            px.EvaluatesTo:=TypeDecl_number;
            px.Right:=q;//?
           end;
          pTypeOf:
           begin
            px.EvaluatesTo:=TypeDecl_type;
            px.Right:=q;
           end;
          pAddressOf:
           begin
            //TODO: check ttVar?
            px.ValueFrom:=q;
            q:=ResType(Sphere,q);
            while (q<>0) and (Sphere[q].ThingType=ttArray) do
              q:=Sphere[q].ElementType;
            px.EvaluatesTo:=Sphere.Add(ttPointer,qx);
            qx.Parent:=cb;
            qx.SrcPos:=Source.SrcPos;
            qx.ByteSize:=SystemWordSize;
            qx.EvaluatesTo:=q;
           end;
          pMulDiv,pAddSub,pShift,pAnd,pOr:
           begin
            px.Right:=q;
            StratoOperatorCheckType(Sphere,p);
           end;
          pEqual,pComparative:
           begin
            px.Right:=q;
            px.EvaluatesTo:=TypeDecl_bool;
           end;
          pAssignment:
           begin
            while (px.ValueFrom<>0) and (Sphere[px.ValueFrom].ThingType=ttAssign) do
              px:=Sphere[px.ValueFrom];
            px.ValueFrom:=q;
            if (stackIndex<>0) and (stack[stackIndex-1].p=pUnTypedVar) then
             begin
              px.EvaluatesTo:=ResType(Sphere,q);
              Sphere[stack[stackIndex-1].t].EvaluatesTo:=px.EvaluatesTo;
              if px.EvaluatesTo<>0 then
                inc(Sphere[cb].ByteSize,ByteSize(Sphere,px.EvaluatesTo));
             end
            else
              px.EvaluatesTo:=ResType(Sphere,px.AssignTo);
            //TODO: check types here? (or at run-time?)
            if not SameType(Sphere,
              ResType(Sphere,px.ValueFrom),
              ResType(Sphere,px.AssignTo)) then
              Source.Error('assignment type mismatch');
            //TODO: auto-cast?
            //TODO: if ValueFrom=ttFunction, AssignTo=ttSignature: find suitable signature
            if px.EvaluatesTo<>0 then
             begin
              q:=px.AssignTo;
              qx:=Sphere[q];
              if (q<>0) and (qx.ThingType=ttVar) and (qx.EvaluatesTo=0)
                and (ResType(Sphere,p)<>0)
                then
               begin
                r:=ResType(Sphere,px.ValueFrom);
                qx.EvaluatesTo:=r;
                qx.Offset:=Sphere[cb].ByteSize;
                inc(Sphere[cb].ByteSize,ByteSize(Sphere,r));
               end;
             end;
           end;
          pUnTypedVar:
           begin
            //see also pAssignment above and stColon below
            //assert x0.ThingType=ttVar
            if px.EvaluatesTo=0 then
              Source.Error('no type for local var "'+string(Sphere.FQN(p))+'"');
            p:=q;
           end;
          pDefer,pThrow:px.Target:=q;
          pCatch:px.Body:=q;
          //else ?
        end;
        if z=z00 then
          q:=p
        else
         begin
          stack[stackIndex].p:=z;
          stack[stackIndex].t:=p;
          inc(stackIndex);
          q:=0;
          done:=true;
         end;
       end;
     end;
  end;

  procedure Juxta(var p:TStratoIndex);
  var
    q:TStratoIndex;
    qx:PStratoThing;
  begin
    if p<>0 then Combine(p_Juxta,p);
    if p<>0 then
     begin
      if SameType(Sphere,ResType(Sphere,p),TypeDecl_bool) then
       begin
        q:=Sphere.Add(ttSelection,qx);
        qx.Parent:=cb;
        qx.SrcPos:=Source.SrcPos;
        qx.DoIf:=p;
        Push(pIfThen,q);
       end
      else
       begin
        Combine(pIfThen,p);
        if p<>0 then Source.Error('missing operator or semicolon');
       end;
      p:=0;
     end;
  end;

  procedure PushBinary(p:TPrecedence;st:TStratoToken;var q:TStratoIndex);
  var
    r:TStratoIndex;
    rx:PStratoThing;
  begin
    Combine(p,q);
    if q=0 then
      Source.Error('no left side defined for binary operator')
    else
     begin
      r:=Sphere.Add(ttBinaryOp,rx);
      rx.Parent:=cb;
      rx.SrcPos:=Source.SrcPos;
      rx.Op:=cardinal(st);
      rx.Left:=q;
      Push(p,r);
      q:=0;
     end;
  end;

  procedure CheckPassed(p:TStratoIndex);
  var
    px:PStratoThing;
    b:boolean;
  begin
    if p<>0 then
     begin
      b:=false;
      px:=Sphere[p];
      case px.ThingType of
        ttVar,ttFnCall,ttIteration,ttIterationPE,ttAssign,
        ttDeferred,ttThrow,ttCatch,ttDestructor:
          b:=true;
        ttCodeBlock,ttSelection:
          b:=true;//b:=px.EvaluatesTo=0;//TODO: descend into?
        ttBinaryOp:b:=TStratoToken(px.Op) in [stOpAssign..stOpAssignAnd];
        ttUnaryOp:b:=TStratoToken(px.Op) in [stOpInc,stOpDec];
        //more?
      end;
      if not b then
        Source.Error('statement without calls or assignments');
     end;
  end;

  procedure CbAdd(p:TStratoIndex);
  var
    q:TStratoIndex;
    px,qx:PStratoThing;
  begin
    if p<>0 then
     begin
      px:=Sphere[p];
      if px.Parent=cb then q:=p else
       begin
        //member of another chain, create an alias
        q:=Sphere.Add(ttAlias,qx);
        qx.Parent:=cb;
        qx.SrcPos:=Source.SrcPos;
        qx.Target:=p;
       end;
      {$IFDEF DEBUG}
      if px.Next<>0 then
        raise Exception.Create('broken chain detected');
      {$ENDIF}
      Sphere.Append(Sphere[cb].FirstStatement,q);
     end;
  end;

var
  n:TStratoName;
  nn,fqn:UTF8String;
  st:TStratoToken;
  ns,p,q,r:TStratoIndex;
  px,qx,rx:PStratoThing;
  i:cardinal;
  b:boolean;
begin
  Result:=0;//default
  src:=Sphere.Add(ttSourceFile,px);
  PStratoSourceFile(px).FileName:=Sphere.AddBinaryData(UTF8String(Source.FilePath));
  PStratoSourceFile(px).FileSize:=0;//TODO: FileSize, FileCRC32, FileDate
  PStratoSourceFile(px).SrcPosLineIndex:=Source.LineIndex;
  if not Source.IsEmpty then
   begin
    //namespace
    if Source.IsNext([stIdentifier]) then
      ns:=LookUpNameSpace(b)
    else
     begin
      //default: use file name
      nn:=UTF8String(ChangeFileExt(ExtractFileName(Source.FilePath),''));
        //(''''+StringReplace(Source.FilePath),'''','''''',[rfReplaceAll])+'''');?
      ns:=Sphere.Add(ttNameSpace,px);
      px.Name:=Sphere.Dict.StrIdx(nn);
      //TODO: split by '.'
      p:=Sphere.Header.FirstNameSpace;
      //assert p<>0 since Sphere.FirstGlobalNameSpace is runtime
      if not Sphere.AddTo(p,ns) then
        Source.Error('duplicate namespace "'+string(nn)+'"');
      qx:=Sphere[ns];
      PStratoNameSpaceData(qx).SourceFile:=src;
     end;
    Result:=ns;
    //PStratoSourceFile(px).NameSpace:=ns;//?
    SetLength(Locals,2);
    Locals[0]:=ns;
    Locals[1]:=Sphere.Header.FirstNameSpace;//runtime

    stackIndex:=0;
    stackSize:=stackGrowSize;
    SetLength(stack,stackSize);

    //TODO: check stIdentifier not used in declaration?

    cb:=0;
    cbInhCalled:=false;
    while Source.NextToken(st) do
      if cb=0 then //declarations
        case st of

          stImport: //import a namespace
            ParseImport;

          stIdentifier: //declaration
           begin
            //lookup
            p:=0;
            b:=false;
            ID(n,nn);
            fqn:=nn;
            ns:=Locals[0];
            while Source.IsNext([stPeriod,stIdentifier]) do
             begin
              LookUpNext(n,nn,p,ns,b);
              ID(n,nn);
              fqn:=fqn+'.'+nn;
             end;
            if b then Source.Error('unknown namespace "'+string(fqn)+'"');

            //operator override?
            if Source.IsNext([stPeriod,stStringLiteral]) then
             begin
              LookUpNext(n,nn,p,ns,b);
              //ID(n,nn); but with GetStr:
              nn:=Source.GetStr;
              n:=Sphere.Dict.StrIdx(nn);
             end;

            st:=Source.Token;
            case st of

              stColon:
               begin
                case Source.Token of
                  stIdentifier:
                   begin
                    q:=LookUpType('type');
                    //property
                    if Source.IsNext([stAOpen]) then
                     begin
                      p:=Sphere.AddTo(Sphere[ns].FirstItem,ttProperty,n,px);
                      if p=0 then
                       begin
                        Source.Error('duplicate identifier "'+nn+'"');
                        p:=Sphere.Add(ttProperty,px);
                        px.Name:=n;
                       end;
                      px.Parent:=ns;
                      px.SrcPos:=Source.SrcPos;
                      px.EvaluatesTo:=q;
                      cb:=StratoFnCodeBlock(Sphere,p,ns,q,n,Source.SrcPos);
                      px.ValueFrom:=cb;
                      cbInhCalled:=false;
                      p:=0;
                     end
                    else
                    //class
                    if Source.IsNext([stOpEq,stAOpen]) then
                     begin
                      if q=0 then
                        Source.Error('undeclared base class')
                      else
                      if Sphere[q].ThingType<>ttClass then
                        Source.Error('base class must be a class');
                      p:=Sphere.AddTo(Sphere[ns].FirstItem,ttClass,n,px);
                      if p=0 then
                       begin
                        Source.Error('duplicate identifier "'+nn+'"');
                        p:=Sphere.Add(ttClass,px);
                        px.Name:=n;
                       end;
                      px.Parent:=ns;
                      px.SrcPos:=Source.SrcPos;
                      px.InheritsFrom:=q;
                      if q<>0 then px.ByteSize:=Sphere[q].ByteSize;
                      Source.Skip(stAOpen);
                      ParseRecordDecl(p);
                     end
                    else
                     begin
                      p:=Sphere.AddTo(Sphere[ns].FirstItem,ttVar,n,px);
                      if p=0 then
                       begin
                        Source.Error('duplicate identifier "'+nn+'"');
                        p:=Sphere.Add(ttVar,px);//placeholder to prevent errors
                        px.Name:=n;
                       end;
                      px.Parent:=ns;
                      px.SrcPos:=Source.SrcPos;
                      px.EvaluatesTo:=q;
                      if Source.IsNext([stOpEQ]) then
                        px.InitialValue:=ParseLiteral(Source.Token);
                      //TODO: check InitialValue.EvaluatesTo with EvaluatesTo
                      Sphere.AddGlobalVar(p);//sets px.Offset
                     end;
                    Source.Skip(stSemiColon);
                   end;
                  //more?
                  else
                   begin
                    Source.Error('unsupported type syntax');
                    //ns.Add(n, anyway? placeholder?
                   end;
                end;
               end;

              stOpEQ://type, constant or enum
                if Source.IsNext([stPOpen,stIdentifier]) then
                 begin
                  p:=Sphere.AddTo(Sphere[ns].FirstItem,ttEnumeration,n,px);
                  if p=0 then
                   begin
                    Source.Error('duplicate identifier "'+nn+'"');
                    p:=Sphere.Add(ttEnumeration,px);
                    px.Name:=n;
                   end;
                  px.Parent:=ns;
                  px.SrcPos:=Source.SrcPos;
                  px.ByteSize:=SystemWordSize;
                  ParseEnumeration(p,px);
                 end
                else
                 begin
                  //type or constant declaration
                  st:=Source.Token;
                  case st of
                    stIdentifier:
                     begin
                      p:=Lookup;
                      px:=Sphere[p];
                      if p=0 then
                        Source.Error('unknown type or constant')
                      else
                        case px.ThingType of
                          ttVar,ttConstant://initial value from var
                            if px.InitialValue=0 then
                              Source.Error('constant without value')
                            else
                             begin
                              q:=Sphere.AddTo(Sphere[ns].FirstItem,ttConstant,n,qx);
                              if q=0 then
                                Source.Error('duplicate identifier "'+nn+'"')
                              else
                               begin
                                qx.Parent:=ns;
                                qx.SrcPos:=Source.SrcPos;
                                qx.InitialValue:=px.InitialValue;
                                qx.EvaluatesTo:=Sphere[px.InitialValue].EvaluatesTo;
                               end;
                             end;
                          ttLiteral://constant
                           begin
                            q:=Sphere.AddTo(Sphere[ns].FirstItem,ttConstant,n,qx);
                            if q=0 then
                              Source.Error('duplicate identifier "'+nn+'"')
                            else
                             begin
                              qx.Parent:=ns;
                              qx.SrcPos:=Source.SrcPos;
                              qx.InitialValue:=p;
                              qx.EvaluatesTo:=Sphere[p].EvaluatesTo;
                             end;
                           end;
                          ttTypeDecl,ttRecord,ttEnumeration:
                            if Source.IsNext([stBOpen]) then //array
                              if Source.IsNext([stBClose]) then //dyn array
                               begin
                                Source.Error('//TODO: dyn arrays');
                               end
                              else
                               begin
                                i:=ParseInteger;
                                //TODO: multidimensional arrays, array of array
                                Source.Skip(stBClose);//TODO: force
                                q:=Sphere.AddTo(Sphere[ns].FirstItem,ttArray,n,qx);
                                if q=0 then
                                  Source.Error('duplicate identifier "'+nn+'"')
                                else
                                 begin
                                  qx.Parent:=ns;
                                  qx.SrcPos:=Source.SrcPos;
                                  qx.ElementType:=p;
                                  if p<>0 then qx.ByteSize:=ByteSize(Sphere,p)*i;
                                 end;
                               end
                            else //type alias
                             begin
                              q:=Sphere.AddTo(Sphere[ns].FirstItem,ttAlias,n,qx);
                              if q=0 then
                                Source.Error('duplicate identifier "'+nn+'"')
                              else
                               begin
                                qx.Parent:=ns;
                                qx.SrcPos:=Source.SrcPos;
                                qx.Target:=p;
                               end;
                             end;
                          else
                            Source.Error('unsupported type or constant reference');
                        end;
                     end;
                    stStringLiteral,stNumericLiteral,stBOpen,stPOpen://constant
                     begin
                      p:=Sphere.AddTo(Sphere[ns].FirstItem,ttConstant,n,px);
                      if p=0 then
                       begin
                        Source.Error('duplicate identifier "'+nn+'"');
                        p:=Sphere.Add(ttConstant,px);
                        px.Name:=n;
                       end;
                      px.Parent:=ns;
                      px.SrcPos:=Source.SrcPos;
                      px.InitialValue:=ParseLiteral(st);
                      if px.InitialValue<>0 then
                        px.EvaluatesTo:=Sphere[px.InitialValue].EvaluatesTo;
                     end;
                    //stPOpen://enumeration: see above
                    stAOpen://record (aka struct)
                     begin
                      p:=Sphere.AddTo(Sphere[ns].FirstItem,ttRecord,n,px);
                      if p=0 then
                       begin
                        Source.Error('duplicate identifier "'+nn+'"');
                        p:=Sphere.Add(ttRecord,px);
                        px.Name:=n;
                       end;
                      px.Parent:=ns;
                      px.SrcPos:=Source.SrcPos;
                      ParseRecordDecl(p);
                     end;
                    stCaret:
                     begin
                      p:=LookUpType('pointer type');
                      if p=0 then
                       begin
                        Source.Error('unknown pointer type');
                        p:=Sphere.Add(ttTypeDecl,px);
                        px.Name:=n;
                        px.Parent:=ns;
                        px.SrcPos:=Source.SrcPos;
                       end;
                      q:=Sphere.AddTo(Sphere[ns].FirstItem,ttPointer,n,qx);
                      if q=0 then
                        Source.Error('duplicate identifier "'+nn+'"')
                      else
                       begin
                        qx.Parent:=ns;
                        qx.SrcPos:=Source.SrcPos;
                        qx.ByteSize:=SystemWordSize;
                        qx.EvaluatesTo:=p;
                       end;
                     end;
                    stOpSizeOf:
                     begin
                      p:=Sphere.AddTo(Sphere[ns].FirstItem,ttConstant,n,px);
                      if p=0 then
                       begin
                        Source.Error('duplicate identifier "'+nn+'"');
                        p:=Sphere.Add(ttConstant,px);
                        px.Name:=n;
                       end;
                      px.Parent:=ns;
                      px.SrcPos:=Source.SrcPos;
                      px.EvaluatesTo:=TypeDecl_Number;
                      q:=LookUpType('sizeof type');
                      if q=0 then
                        Source.Error('unknown sizeof type')
                      else
                       begin
                        i:=ByteSize(Sphere,q);
                        //TODO: ttSizeOf?
                        q:=Sphere.Add(ttLiteral,qx);
                        qx.SrcPos:=Source.SrcPos;
                        qx.EvaluatesTo:=TypeDecl_number;
                        qx.InitialValue:=Sphere.AddBinaryData(IntToStr(i));
                        px.InitialValue:=q;
                       end;
                     end;
                    else
                      Source.Error('unsupported type or constant');
                   end;
                 end;

              stPOpen://parameter list
               begin
                p:=ParseSignature(ns,nn);
                case Source.Token of
                  stSemiColon:
                   begin
                    //just a signature? add to namespace
                    r:=0;
                    q:=Sphere[ns].FirstItem;
                    while (q<>0) and (Sphere[q].Name<>n) do
                     begin
                      r:=q;
                      q:=Sphere[q].Next;
                     end;
                    if q=0 then
                      if r=0 then
                        Sphere[ns].FirstItem:=p
                      else
                        Sphere[r].Next:=p
                    else
                      if Sphere[q].ThingType=ttSignature then
                       begin
                        //signature to forward overload? create ttFunction here
                        if r=0 then
                          Sphere[ns].FirstItem:=Sphere[q].Next
                        else
                          Sphere[r].Next:=Sphere[q].Next;
                        Sphere[q].Next:=0;
                        r:=Sphere.AddTo(Sphere[ns].FirstItem,ttFunction,n,rx);
                        if r=0 then
                         begin
                          Source.Error('duplicate identifier "'+nn+'"');
                          r:=Sphere.Add(ttFunction,rx);
                          rx.Name:=n;
                         end;
                        rx.Parent:=ns;
                        rx.SrcPos:=Source.SrcPos;
                        StratoFnAddOverload(Sphere,Source,r,q,src,nn);
                        StratoFnAddOverload(Sphere,Source,r,p,src,nn);
                       end
                      else
                        Source.Error('duplicate identifier "'+nn+'"');
                   end;
                  stAOpen://code block
                   begin
                    q:=Sphere.Lookup(Sphere[ns].FirstItem,n);
                    if q=0 then
                     begin
                      q:=Sphere.AddTo(Sphere[ns].FirstItem,ttFunction,n,qx);
                      if q=0 then
                       begin
                        Source.Error('duplicate identifier "'+nn+'"');
                        q:=Sphere.Add(ttFunction,qx);
                        qx.Name:=n;
                       end;
                      qx.Parent:=ns;
                      qx.SrcPos:=Source.SrcPos;
                     end
                    else
                      case Sphere[q].ThingType of
                        ttFunction:;//ok!
                        ttSignature://signature forwarded, function now?
                          if SameType(Sphere,p,q) then
                           begin
                            raise Exception.Create('//TODO: replace sig with fn');
                           end
                          else
                           begin
                            Source.Error('//TODO: fn decl with forward sig mismatch');
                            q:=Sphere.Add(ttFunction,qx);
                            qx.Name:=n;
                            qx.Parent:=ns;
                            qx.SrcPos:=Source.SrcPos;
                           end;
                        ttClass://constructor
                         begin
                          //Sphere[p].EvaluatesTo:=q;
                          Sphere[p].Target:=q;
                          cbInhCalled:=false;
                         end;
                        else
                         begin
                          Source.Error('duplicate identifier "'+nn+'"');
                          q:=Sphere.Add(ttFunction,qx);
                          qx.Name:=n;
                          qx.Parent:=ns;
                          qx.SrcPos:=Source.SrcPos;
                         end;
                      end;
                    cb:=StratoFnOverloadCodeBlock(Sphere,Source,q,p,src,nn);
                    cbInhCalled:=false;
                    p:=0;
                   end;
                  else Source.Error('unsupported signature syntax');
                end;
               end;

              stOpAssign://":="
                if Source.IsNext([stAOpen]) then
                 begin
                  //accept only one object:={}
                  p:=Sphere.AddTo(Sphere[ns].FirstItem,ttClass,n,px);
                  if p=0 then
                   begin
                    Source.Error('duplicate identifier');
                    p:=Sphere.Add(ttClass,px);
                    px.Name:=n;
                   end;
                  px.Parent:=ns;
                  px.SrcPos:=Source.SrcPos;
                  if TypeDecl_object=0 then
                    TypeDecl_object:=p
                  else
                    Source.Error('only one master base class allowed');
                  ParseRecordDecl(p);
                 end
                else
                  Source.Error('unsupported declaration syntax');

              //stBOpen://TODO: array property (with overloads of same name?)

              //stAOpen:?

              else
                Source.Error('unexpected stray identifier');
            end;
           end;

          stStringLiteral,stNumericLiteral:
           begin
            Source.Error('unexpected literal');
            ParseLiteral(st);
           end;

          stQuestionMark: //interface
           begin
            p:=0;
            b:=false;
            ID(n,nn);
            fqn:=nn;
            ns:=Locals[0];
            while Source.IsNext([stPeriod,stIdentifier]) do
             begin
              LookUpNext(n,nn,p,ns,b);
              ID(n,nn);
              fqn:=fqn+'.'+nn;
             end;
            if b then Source.Error('unknown namespace "'+string(fqn)+'"');

            st:=Source.Token;
            case st of
              stPOpen:
                if Source.IsNextID([stPClose,stAOpen]) or
                  Source.IsNextID([stPClose,stOpEQ,stAOpen]) then
                 begin //inherit this interface
                  q:=LookUp;
                  if q=0 then
                    Source.Error('undeclared base interface');
                  Source.Skip(stPClose);
                  Source.Skip(stOpEQ);//if?
                  Source.Skip(stAOpen);
                  p:=Sphere.AddTo(Sphere[ns].FirstItem,ttInterface,n,px);
                  if p=0 then
                   begin
                    Source.Error('duplicate identifier');
                    p:=Sphere.Add(ttInterface,px);
                    px.Name:=n;
                   end;
                  px.Parent:=ns;
                  px.SrcPos:=Source.SrcPos;
                  px.ByteSize:=SystemWordSize;
                  px.InheritsFrom:=q;
                  ParseInterfaceDecl(p);
                 end;
              stAOpen:
               begin
                if not Sphere.AddTo(Sphere[ns].FirstItem,ttInterface,n,p,px) then
                  Source.Error('duplicate identifier "'+nn+'"');
                px.Parent:=ns;
                px.SrcPos:=Source.SrcPos;
                px.ByteSize:=SystemWordSize;
                ParseInterfaceDecl(p);
               end;
              else
                Source.Error('unsupported interface syntax');
            end;
           end;

          stOpSub://'-': destructor?
            if Source.IsNextID([stPOpen,stPClose,stAOpen]) then
             begin
              //lookup
              p:=0;
              b:=false;
              ID(n,nn);
              fqn:=nn;
              ns:=Locals[0];
              while Source.IsNext([stPeriod,stIdentifier]) do
               begin
                LookUpNext(n,nn,p,ns,b);
                ID(n,nn);
                fqn:=fqn+'.'+nn;
               end;
              if b then Source.Error('unknown namespace "'+string(fqn)+'"');
              //ParseSignature? destructor doesn't have arguments/overloads
              Source.Skip(stPOpen);
              Source.Skip(stPClose);
              Source.Skip(stAOpen);
              //find class destructor is for
              q:=Sphere.Lookup(Sphere[ns].FirstItem,n);
              if q=0 then
               begin
                Source.Error('destructor for unknown class');
                q:=Sphere.Add(ttClass,qx);
                qx.Name:=n;
               end
              else
                if Sphere[q].ThingType<>ttClass then
                 begin
                  Source.Error('destructor only supported on class');
                  q:=Sphere.Add(ttClass,qx);
                  qx.Name:=n;
                 end;
              //check any destructor already
              r:=Sphere[q].FirstItem;
              while (r<>0) and (Sphere[r].ThingType<>ttDestructor) do
                r:=Sphere[r].Next;
              if r<>0 then
                Source.Error('duplicate destructor');
              //add
              r:=Sphere.AddTo(Sphere[q].FirstItem,ttDestructor,n,rx);
              rx.Parent:=q;
              rx.SrcPos:=Source.SrcPos;
              //signature
              p:=Sphere.Add(ttSignature,px);
              px.Name:=n;
              px.Parent:=r;
              px.SrcPos:=Source.SrcPos;
              px.Target:=q;
              rx.Target:=p;
              //start code block
              cb:=StratoFnCodeBlock(Sphere,r,q,0,0,Source.SrcPos);
              rx.Body:=cb;
              cbInhCalled:=false;
              p:=0;
             end
            else
              Source.Error('unexpected token');

          stAOpen:
           begin
            ns:=Locals[0];
            cb:=Sphere.Add(ttCodeBlock,qx);
            qx.Parent:=ns;
            qx.SrcPos:=Source.SrcPos;
            if PStratoSourceFile(Sphere[src]).InitializationCode=0 then
             begin
              PStratoSourceFile(Sphere[src]).InitializationCode:=cb;
              Sphere.Append(
                PStratoNameSpaceData(Sphere[ns]).FirstInitialization,cb);
              p:=Sphere.Add(ttAlias,px);
              px.Parent:=ns;
              px.SrcPos:=Source.SrcPos;
              px.Target:=cb;
              Sphere.Append(Sphere.Header.FirstInitialization,p);
             end
            else
            if PStratoSourceFile(Sphere[src]).FinalizationCode=0 then
             begin
              PStratoSourceFile(Sphere[src]).FinalizationCode:=cb;
              Sphere.Prepend(
                PStratoNameSpaceData(Sphere[ns]).FirstFinalization,cb);
              p:=Sphere.Add(ttAlias,px);
              px.Parent:=ns;
              px.SrcPos:=Source.SrcPos;
              px.Target:=cb;
              Sphere.Prepend(Sphere.Header.FirstFinalization,p);
             end
            else
              Source.Error('Initialization and finalization code already declared.');
            p:=0;
           end;

          stSemiColon:;//stray semicolon? ignore

          //stPOpen?

          st_Unknown:Source.Error('unknown token');
          else Source.Error('unexpected token');
        end

      else //code block
        case st of

          stIdentifier:
           begin
            Juxta(p);
            b:=true;
            ID(n,nn);
            fqn:=nn;
            while Source.IsNext([stPeriod,stIdentifier]) do
             begin
              b:=false;
              CodeLookup(n,p);
              if p=0 then
               begin
                Source.Error('undeclared identifier "'+string(fqn)+'"');
                //TODO: silence further errors
                p:=Sphere.Add(ttNameSpace,px);//silence further errors
                px.Name:=Sphere.Dict.StrIdx('!!!'+nn);//n
                px.SrcPos:=Source.SrcPos;
               end;
              ID(n,nn);
              fqn:=fqn+'.'+nn;
             end;
            r:=p;
            CodeLookup(n,p);
            if (p<>0) and (Sphere[p].ThingType=ttNameSpace) then p:=0;
            if p=0 then
              if b then
               begin
                if not Sphere.AddTo(Sphere[cb].FirstItem,ttVar,n,p,px) then
                  Source.Error('duplicate identifier "'+string(fqn)+'"');
                px.Parent:=cb;
                px.SrcPos:=Source.SrcPos;
                px.Offset:=Sphere[cb].ByteSize;
                Push(pUnTypedVar,p);//see stColon below
               end
              else
               begin
                //check variable object method/field pointer
                //TODO: this recursive??!! (via stack!)
                if r<>0 then
                 begin
                  b:=false;
                  q:=r;
                  CodeLookup(n,q);
                  while Source.IsNext([stPeriod,stIdentifier]) do
                   begin
                    ID(n,nn);
                    fqn:=fqn+'.'+nn;
                    if p<>0 then CodeLookup(n,q);
                   end;
                  if q<>0 then
                   begin
                    p:=Sphere.Add(ttVarIndex,px);
                    px.Name:=n;
                    px.Parent:=r;
                    px.SrcPos:=Source.SrcPos;
                    px.Target:=q;
                    //qx.EvaluatesTo:=ResType(p);
                   end;
                 end;
                //really found nothing?
                if p=0 then
                 begin
                  Source.Error('undeclared identifier "'+string(fqn)+'"');
                  p:=Sphere.Add(ttVar,px);//silence further errors
                  px.Name:=n;
                 end;
               end;
           end;

          stPeriod://"."
           begin
            ID(n,nn);
            fqn:=Sphere.FQN(p)+'.'+nn;
            while Source.IsNext([stPeriod,stIdentifier]) do
             begin
              CodeLookup(n,p);
              if p=0 then
               begin
                Source.Error('undeclared identifier "'+string(fqn)+'"');
                //TODO: silence further errors
                p:=Sphere.Add(ttNameSpace,px);//silence further errors
                px.Name:=Sphere.Dict.StrIdx('!!!'+nn);//n
                px.SrcPos:=Source.SrcPos;
               end;
              ID(n,nn);
              fqn:=fqn+'.'+nn;
             end;
            CodeLookup(n,p);
            if p=0 then
             begin
              Source.Error('undeclared identifier "'+string(fqn)+'"');
              p:=Sphere.Add(ttVar,px);//silence further errors
              px.Name:=n;
             end;
           end;

          stStringLiteral:
           begin
            Juxta(p);
            p:=Sphere.Add(ttLiteral,px);
            px.Parent:=cb;
            px.SrcPos:=Source.SrcPos;
            px.EvaluatesTo:=TypeDecl_string;
            if (stackIndex<>0) and ((stack[stackIndex-1].p=pIfThen)
              or (stack[stackIndex-1].p=pIfElse)) then
              px.InitialValue:=Sphere.AddBinaryData(Source.GetStr)
            else
              px.InitialValue:=Sphere.AddBinaryData(Source.GetStrs);
           end;
          stNumericLiteral:
           begin
            Juxta(p);
            p:=Sphere.Add(ttLiteral,px);
            px.Parent:=cb;
            px.SrcPos:=Source.SrcPos;
            px.EvaluatesTo:=TypeDecl_number;
            px.InitialValue:=Sphere.AddBinaryData(Source.GetID);
           end;

          stColon:
           begin
            //Combine(p_ArgList_Item,p);
            if (stackIndex<>0) and (stack[stackIndex-1].p=pUnTypedVar) then //local declaration(s)?
             begin
              p:=0;
              q:=LookUpType('type');
              while (stackIndex<>0) and (stack[stackIndex-1].p=pUnTypedVar) do
               begin
                dec(stackIndex);
                r:=stack[stackIndex].t;//assert ttVar
                if p=0 then p:=r;
                rx:=Sphere[r];
                rx.EvaluatesTo:=q;//assert was 0
                if q<>0 then
                 begin
                  rx.Offset:=Sphere[cb].ByteSize;
                  inc(Sphere[cb].ByteSize,ByteSize(Sphere,q));
                 end;
                {$IFDEF DEBUG}
                stack[stackIndex].p:=p___;
                stack[stackIndex].t:=0;
                {$ENDIF}
               end;
              if (p<>0) and Source.IsNext([stSemiColon]) then p:=0;//don't add as statement
             end
            else //cast
             begin
              Combine(p_Cast,p);//p_juxta?
              if p=0 then
                Source.Error('no value to cast')
              else if ResType(Sphere,p)=0 then
                Source.Error('can''t cast value "'+Sphere.FQN(p)+'" '+
                  IntToHex(Sphere[p].ThingType,4));
              q:=Sphere.Add(ttCast,qx);
              qx.Parent:=cb;
              qx.SrcPos:=Source.SrcPos;
              qx.Target:=p;
              qx.EvaluatesTo:=LookUpType('cast type');
              p:=q;
             end;
           end;

          stPOpen:
            if p=0 then
              Push(pParentheses,0)
            else
             begin
              //Combine here?
              px:=Sphere[p];
              //start an argument list?
              if (px.ThingType=ttFunction)
                or (px.ThingType=ttVarIndex) //and px.Target.ThingType=ttFunction
                or (px.ThingType=ttClass) //constructor?
                or (px.ThingType=ttInherited) 
                or ((px.ThingType=ttVar) and (px.EvaluatesTo<>0)
                  and (Sphere[px.EvaluatesTo].ThingType=ttPointer)
                  and (Sphere[Sphere[px.EvaluatesTo].EvaluatesTo].ThingType=ttSignature)
                )
                or (px.ThingType=ttThis) //constructor/destructor call
                then //TODO: dedicated function GivesSignature
               begin
                q:=Sphere.Add(ttFnCall,qx);
                qx.Name:=n;//TODO: from px?
                qx.Parent:=cb;
                qx.SrcPos:=Source.SrcPos;
                qx.Target:=p;//see Combine pArgList
                Push(pArgList,q);
               end
              else
              //start a selection?
              if SameType(Sphere,ResType(Sphere,p),TypeDecl_bool) then
               begin
                //see also Juxta
                q:=Sphere.Add(ttSelection,qx);
                qx.Parent:=cb;
                qx.SrcPos:=Source.SrcPos;
                qx.DoIf:=p;
                Push(pIfThen,q);
               end
              else
              //nothing found!
               begin
                if (stackIndex<>0) and (stack[stackIndex-1].p=pUnTypedVar) and
                  (stack[stackIndex-1].t=p) then
                  Source.Error('unknown function "'+string(Sphere.Dict[px.Name])+'"')
                else
                  Source.Error('function expected "'+string(Sphere.FQN(p))+'"');
                //create one to silence errors
                q:=Sphere.Add(ttFnCall,qx);
                qx.Name:=n;//?
                qx.Parent:=cb;
                qx.SrcPos:=Source.SrcPos;
                Push(pArgList,q);
               end;
              p:=0;
             end;
          stPClose:
            if stackIndex=0 then
              Source.Error('unexpected token')
            else
              Combine(pParentheses,p);

          stComma:
           begin
            Combine(p_ArgList_Item,p);
            if (stackIndex=0) or (p=0) then
              Source.Error('unexpected token')
            else
            if (stack[stackIndex-1].p=pArgList) then
             begin
              //assert Sphere[p].ThingType=ttFnCall
              StratoFnCallAddArgument(Sphere,stack[stackIndex-1].t,p,px);
              px.Parent:=cb;
              px.SrcPos:=Source.SrcPos;
             end
            else
            if (stack[stackIndex-1].p=pUnTypedVar) and (p<>stack[stackIndex-1].t) then
              if (Sphere[p].ThingType=ttAssign) and (TStratoToken(Sphere[p].Op)=stOpAssign) then
                CbAdd(p)
              else
                Source.Error('unexpected syntax in local variable declaration');
            p:=0;
           end;

          stAOpen:
            if Source.IsNext([stIdentifier,stColon]) or
              Source.IsNext([stStringLiteral,stColon]) or
              Source.IsNext([stNumericLiteral,stColon]) then
             begin
              //TODO: JSON
              Source.Error('//TODO:JSON');
             end
            else
             begin
              Combine(p_Juxta,p);
              //check part of &({}{}) syntax
              if (stackIndex<>0) and (stack[stackIndex-1].p=pForCrit) then
               begin
                stack[stackIndex-1].p:=pForThen;
                Sphere[stack[stackIndex-1].t].DoIf:=p;
                if not SameType(Sphere,ResType(Sphere,p),TypeDecl_bool) then
                  Source.Error('iteration criterium does not evaluate to boolean');
                p:=0;
               end
              else
                Juxta(p);
              //start code block
              Push(pCodeBlock,cb);
              q:=cb;
              cb:=Sphere.Add(ttCodeBlock,qx);
              qx.Parent:=q;
              qx.SrcPos:=Source.SrcPos;
              p:=0;
             end;
          stAClose:
           begin
            Combine(p_Statement,p);
            if p<>0 then
             begin
              r:=ResType(Sphere,p);
              Sphere[cb].EvaluatesTo:=r;
              //TODO: if parent is overload with return value, assign
              CheckPassed(p);
              CbAdd(p);
             end;
            while (stackIndex<>0) and (stack[stackIndex-1].p<>pCodeBlock) do
             begin
              dec(stackIndex);
              if stack[stackIndex].p=pIfElse then
                Source.Error('if-then without else')
              else
                Source.Error('unexpected incomplete syntax');//+??[stack[stackIndex].p]);
              {$IFDEF DEBUG}
              stack[stackIndex].p:=p___;
              stack[stackIndex].t:=0;
              {$ENDIF}
             end;
            if stackIndex=0 then
             begin
              //return to declarations
              p:=cb;
              cb:=0;

              //code block done: checks
              r:=Sphere[p].Parent;
//TODO: check {{{@@@
              if r<>0 then
               begin
                rx:=Sphere[r];

                //property get code block done? check set code block
                if (rx.ThingType=ttProperty) and (rx.ValueFrom=p)
                  and Source.IsNext([stAOpen]) then
                 begin
                  //TODO: check code block's EvaluatesTo with property's?
                  cb:=StratoFnCodeBlock(Sphere,r,
                    rx.Parent,rx.EvaluatesTo,rx.Name,Source.SrcPos);
                  rx.AssignTo:=cb;
                  cbInhCalled:=false;
                 end;

                //constructor block done? check inherited called
                if (rx.ThingType=ttConstructor) and not(cbInhCalled)
                  and (rx.Parent<>TypeDecl_object) and (rx.Parent<>0) then
                 begin
                  r:=StratoFnCallFindInherited(Sphere,rx,0);
                  if r=0 then
                    Source.Error('unable to find base constructor')
                  else
                   begin
                    q:=Sphere.Add(ttFnCall,qx);
                    qx.Name:=Sphere[Sphere[r].Parent].Name;//?
                    qx.Parent:=p;
                    qx.SrcPos:=Source.SrcPos;
                    qx.Body:=Sphere[r].Body;
                    qx.Target:=Sphere.Add(ttVarIndex,rx);
                    rx.Parent:=Sphere[p].FirstItem;//assert ttThis
                    rx.SrcPos:=Source.SrcPos;
                    rx.Target:=r;
                    StratoFnArgByValues(Sphere,q,
                      Sphere[r].FirstArgument,
                      Sphere[Sphere[p].Parent].FirstArgument);
                    //insert first into code block
                    qx.Next:=Sphere[p].FirstStatement;
                    Sphere[p].FirstStatement:=q;
                    //arguments
                    //restore rx for below!
                    rx:=Sphere[Sphere[p].Parent];
                   end;
                 end;

                //destructor block done? check inherited called
                if (rx.ThingType=ttDestructor) and not(cbInhCalled)
                  and (rx.Parent<>TypeDecl_object) then
                 begin
                  q:=rx.Parent;
                  if q<>0 then q:=Sphere[q].InheritsFrom;
                  r:=0;
                  while (r=0) and (q<>0) do
                   begin
                    r:=Sphere[q].FirstItem;
                    while (r<>0) and (Sphere[r].ThingType<>ttDestructor) do
                      r:=Sphere[r].Next;
                    if r=0 then q:=Sphere[q].InheritsFrom;
                   end;
                  if r=0 then
                    Source.Error('unable to find base destructor')
                  else
                   begin
                    q:=Sphere.Add(ttFnCall,qx);
                    qx.Name:=Sphere[Sphere[r].Parent].Name;//?
                    qx.Parent:=p;
                    qx.SrcPos:=Source.SrcPos;
                    qx.Body:=Sphere[r].Body;
                    qx.Target:=Sphere.Add(ttVarIndex,rx);
                    rx.Parent:=Sphere[p].FirstItem;//assert ttThis
                    rx.SrcPos:=Source.SrcPos;
                    rx.Target:=r;//Sphere[r].Signature;
                    //qx.FirstArgument:=?
                    //append to code block
                    Sphere.Append(Sphere[p].FirstStatement,r);
                   end;
                 end;

               end;
             end
            else
             begin
              //pop from stack
              p:=cb;
              dec(stackIndex);
              //assert stack[stackIndex].p=pCodeBlock
              cb:=stack[stackIndex].t;
              {$IFDEF DEBUG}
              stack[stackIndex].p:=p___;
              stack[stackIndex].t:=0;
              {$ENDIF}

              //check part of &({}{}) syntax:
              if stackIndex<>0 then
                case stack[stackIndex-1].p of
                  pForFirst:
                   begin
                    //CheckPassed(p);
                    stack[stackIndex-1].p:=pForCrit;
                    //DoFirst
                    Sphere[stack[stackIndex-1].t].DoElse:=p;//assert was 0
                    //re-link declared items //TODO: detect duplicate ids?
                    px:=Sphere[p];
                    //assert px.ThingType=ttCodeBlock
                    MoveChain(Sphere,px.FirstItem,cb);
                    inc(Sphere[cb].ByteSize,px.ByteSize);
                    px.ByteSize:=0;
                    p:=0;
                   end;
                  //pForCrit,pForCritOnly:assert never
                  pForThen:
                   begin
                    stack[stackIndex-1].p:=pForCritDone;
                    Sphere[stack[stackIndex-1].t].DoThen:=p;//assert was 0
                    p:=0;
                   end;
                  pForBodyFirst:
                   begin
                    if (p<>0) and (Sphere[p].ThingType=ttCodeBlock)
                      and (Sphere[p].EvaluatesTo<>0) then
                      Source.Error('unexpected iteration body with return value');
                    if Source.IsNext([stPOpen]) then //parentheses?
                      stack[stackIndex-1].p:=pForFirst
                    else //assert boolean expression
                      stack[stackIndex-1].p:=pForCritOnly;
                    Sphere[stack[stackIndex-1].t].Body:=p;//assert was 0
                    px:=Sphere[p];
                    //assert px.ThingType=ttCodeBlock
                    MoveChain(Sphere,px.FirstItem,cb);
                    inc(Sphere[cb].ByteSize,px.ByteSize);
                    px.ByteSize:=0;
                    p:=0;
                   end;
                  else
                    Combine(p_CodeBlockDone,p);
                end;

              //add to parent chain
              if (p<>0) and (ResType(Sphere,p)=0) then CbAdd(p);
             end;
            p:=0;
           end;
          stSemiColon:
           begin
            Combine(p_Statement,p);
            if p<>0 then
             begin
              CheckPassed(p);
              CbAdd(p);
             end;
            p:=0;
           end;

          stOpAssign,
          stOpAssignAdd,stOpAssignSub,
          stOpAssignMul,stOpAssignDiv,stOpAssignMod,
          stOpAssignOr,stOpAssignAnd:
           begin
            Combine(pAssignment,p);
            if p=0 then
              Source.Error('no left side defined for assignment')
            else
             begin
              q:=Sphere.Add(ttAssign,qx);
              qx.Parent:=cb;
              qx.SrcPos:=Source.SrcPos;
              qx.Op:=cardinal(st);
              px:=Sphere[p];
              if px.ThingType=ttAssign then
               begin
                qx.AssignTo:=px.ValueFrom;
                px.ValueFrom:=q;
                Push(pAssignment,p);
               end
              else
               begin
                qx.AssignTo:=p;
                Push(pAssignment,q);
               end;
              p:=0;
             end;
           end;
          stOpEQ,stOpNEQ:
            PushBinary(pEqual,st,p);
          stOpLT,stOpLTE,stOpGT,stOpGTE:
            PushBinary(pComparative,st,p);
          stOpAdd,stOpSub:
            if p=0 then //unaryOperator
             begin
              q:=Sphere.Add(ttUnaryOp,qx);
              qx.Parent:=cb;
              qx.SrcPos:=Source.SrcPos;
              qx.Op:=cardinal(st);
              Push(pUnary,q);
             end
            else
              PushBinary(pAddSub,st,p);
          stOpMul,stOpDiv,stOpMod:
            PushBinary(pMulDiv,st,p);
          stOpShl,stOpShr:
            PushBinary(pShift,st,p);
          stOpAnd:
            PushBinary(pAnd,st,p);
          stOpOr:
            PushBinary(pOr,st,p);
          stOpXor:
            PushBinary(pXor,st,p);
          stOpNot:
           begin
            q:=Sphere.Add(ttUnaryOp,qx);
            qx.Parent:=cb;
            qx.SrcPos:=Source.SrcPos;
            qx.Op:=cardinal(st);
            Push(pUnary,q);
           end;

          stOpInc,stOpDec:
            if p=0 then
              Source.Error('increment/decrement operators only allowed as suffix')
            else
             begin
              q:=Sphere.Add(ttUnaryOp,qx);
              qx.Parent:=cb;
              qx.SrcPos:=Source.SrcPos;
              qx.Op:=cardinal(st);
              qx.EvaluatesTo:=ResType(Sphere,p);
              qx.Right:=p;
              p:=q;
             end;
          stOpSizeOf:
            if p=0 then
             begin
              q:=Sphere.Add(ttUnaryOp,qx);
              qx.Parent:=cb;
              qx.SrcPos:=Source.SrcPos;
              qx.Op:=cardinal(st);
              Push(pSizeOf,q);
             end
            else
              Source.Error('sizeof operator only allowed as prefix');

          stThis://"@@"
           begin
            Juxta(p);
            //see also StratoFnAddOverload
            q:=cb;
            p:=0;
            while (q<>0) and (p=0) do
             begin
              p:=Sphere.Lookup(Sphere[q].FirstItem,Sphere.Dict.StrIdx('@@'));
              if p=0 then
               begin
                q:=Sphere[q].Parent;
                if (q<>0) and (Sphere[q].ThingType<>ttCodeBlock) then q:=0;
               end;
             end;
            if p=0 then
             begin
              Source.Error('"@@" undefined');
              p:=Sphere.Add(ttThis,px);//add anyway to avoid further errors
              px.Name:=Sphere.Dict.StrIdx('@@');
              px.Parent:=cb;
              px.SrcPos:=Source.SrcPos;
             end
            else
             begin
              //destructor call?
              if (stackIndex<>0) and (stack[stackIndex-1].p=pUnary) and
                (Sphere[stack[stackIndex-1].t].Op=cardinal(stOpSub)) and
                Source.IsNext([stPOpen,stPClose]) then
               begin
                q:=Sphere.Add(ttDestructor,qx);
                qx.Parent:=cb;
                qx.SrcPos:=Source.SrcPos;
                qx.Target:=p;
                p:=q;
                Source.Skip(stPOpen);
                Source.Skip(stPClose);
                dec(stackIndex);
                {$IFDEF DEBUG}
                stack[stackIndex].p:=p___;
                stack[stackIndex].t:=0;
                {$ENDIF}
               end;
             end;
           end;
          stResult://"??"
           begin
            Juxta(p);
            p:=Sphere.Lookup(Sphere[cb].FirstItem,Sphere[Sphere[cb].Parent].Name);
            if p=0 then
             begin
              Source.Error('"??" undefined');
              p:=Sphere.Add(ttVar,px);//add anyway to avoid further errors
              px.Name:=Sphere.Dict.StrIdx('??');
              px.Parent:=cb;
              px.SrcPos:=Source.SrcPos;
             end;
           end;

          stAmpersand://"&": iteration
            case Source.Token of
              stPOpen://&(){}
               begin
                q:=Sphere.Add(ttIteration,qx);
                qx.Parent:=cb;
                qx.SrcPos:=Source.SrcPos;
                Push(pForFirst,q);
               end;
              stAOpen://&{}()
               begin
                q:=Sphere.Add(ttIterationPE,qx);
                qx.Parent:=cb;
                qx.SrcPos:=Source.SrcPos;
                Push(pForBodyFirst,q);
                //start code block: (see also stAOpen)
                Push(pCodeBlock,cb);
                q:=cb;
                cb:=Sphere.Add(ttCodeBlock,qx);
                qx.Parent:=q;
                qx.SrcPos:=Source.SrcPos;
                p:=0;
               end;
              else Source.Error('unsupported iteration syntax');
            end;

          stTry://":::"
           begin
            Combine(p_Statement,p);
            q:=Sphere.Add(ttTry,qx);
            qx.Parent:=cb;
            qx.SrcPos:=Source.SrcPos;
           end;
          stDefer://">>>"
           begin
            Combine(p_Statement,p);
            q:=Sphere.Add(ttDeferred,qx);
            qx.Parent:=cb;
            qx.SrcPos:=Source.SrcPos;
            Push(pDefer,q);
           end;
          stCatch://"???"
           begin
            Combine(p_Statement,p);
            q:=Sphere.Add(ttCatch,qx);
            qx.Parent:=cb;
            qx.SrcPos:=Source.SrcPos;
            Push(pCatch,q);
            if Source.IsNext([stPOpen,stIdentifier,stColon,stIdentifier]) then
             begin
              ID(n,nn);
              r:=Sphere.Add(ttVar,rx);
              rx.Name:=n;
              rx.Parent:=cb;
              rx.SrcPos:=Source.SrcPos;
              qx.FirstArgument:=r;
              Source.Skip(stColon);
              qx.DoIf:=LookUpType('catch filter');
              rx.EvaluatesTo:=qx.DoIf;
              rx.Offset:=Sphere[cb].ByteSize;//?
              Source.Skip(stPClose);//TODO: enforce
              //TODO: check type is by reference (or SystemWordSize?)
             end
            else
            if Source.IsNext([stPOpen,stIdentifier]) then
             begin
              qx.DoIf:=LookUpType('catch filter');
              Source.Skip(stPClose);//TODO: enforce
             end;
           end;
          stThrow://"!!!"
           begin
            Juxta(p);
            q:=Sphere.Add(ttThrow,qx);
            qx.Parent:=cb;
            qx.SrcPos:=Source.SrcPos;
            Push(pThrow,q);
           end;

          stBOpen://"["
            if p=0 then
              Source.Error('unsupported syntax')
            else
             begin
              Push(pBrackets,p);
              p:=0;
             end;
          stBClose://"]"
            if stackIndex=0 then
              Source.Error('unexpected token')
            else
              Combine(pBrackets,p);

          stAt://"@"
           begin
            Juxta(p);
            q:=Sphere.Add(ttAddressOf,qx);
            qx.Parent:=cb;
            qx.SrcPos:=Source.SrcPos;
            Push(pAddressOf,q);
           end;

          stQuestionMark://"?"
           begin
            q:=Sphere.Add(ttUnaryOp,qx);
            qx.Parent:=cb;
            qx.SrcPos:=Source.SrcPos;
            qx.Op:=cardinal(st);
            Push(pTypeOf,q);
           end;

          stCaret://"^"
            if p=0 then
              Source.Error('unsupported syntax')
            else
             begin
              px:=Sphere[p];
              if ((px.ThingType and tt__Typed)<>0)
                and (Sphere[px.EvaluatesTo].ThingType=ttPointer)
                then
               begin
                q:=Sphere.Add(ttDereference,qx);
                qx.Parent:=cb;
                qx.SrcPos:=Source.SrcPos;
                qx.EvaluatesTo:=Sphere[px.EvaluatesTo].EvaluatesTo;
                qx.ValueFrom:=p;
                p:=q;
               end
              else
                Source.Error('dereference expected on pointer');
             end;

          stInherited://"@@@"
           begin
            q:=Sphere[cb].Parent;
            if q<>0 then
              case Sphere[q].ThingType of
                ttConstructor,ttDestructor:
                  q:=Sphere[q].Parent;
                ttOverload:
                  q:=Sphere[Sphere[q].Parent].Parent;
                //TODO: ttProperty!
                else q:=0;
              end;
            if (q<>0) and (Sphere[q].ThingType=ttClass) then
             begin
              if q<>0 then
               begin
                qx:=Sphere[q];
                if qx.ThingType=ttClass then q:=qx.InheritsFrom else q:=0;
               end;
              if q=0 then
                Source.Error('"@@@" undefined')
              else
               begin
                p:=Sphere.Add(ttInherited,px);
                px.Parent:=cb;
                px.SrcPos:=Source.SrcPos;
                px.EvaluatesTo:=q;
               end;
             end
            else
              Source.Error('"@@@" undefined');
            if p<>0 then cbInhCalled:=true;
           end;

          //TODO:
          stImport,//"<<<"

          stOpTypeIs://"?="

            //TODO
            Source.Error('unsupported syntax');

          else Source.Error('unsupported syntax');//'unexpected token');
        end;
   end;
  if stackIndex<>0 then
     Source.Error('unexpected end of source ('+IntToStr(stackIndex)+')');
end;

end.
