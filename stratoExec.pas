unit stratoExec;

interface

uses SysUtils, stratoDecl, stratoSphere;

type
  TStratoMachine=class(TObject)
  private
    FMem:array of byte; //TODO: array of cardinal?int64? or other way to force alignment?

    FMemSize,FMemIndex,FMemAllocIndex:cardinal;
    //FGlobals:array of Sphere:TStratoSphere; Address:cardinal; end;?
    procedure AllocateGlobals(Sphere:TStratoSphere);
    procedure Perform(Sphere:TStratoSphere;Entry:TStratoIndex);
    procedure LiteralToMemory(Sphere:TStratoSphere;p:TStratoIndex;addr:cardinal);
    procedure PerformSysCall(Sphere:TStratoSphere;Fn:TStratoIndex;Ptr:cardinal);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Run(Sphere:TStratoSphere);
  end;

implementation

uses stratoFn, stratoRunTime, stratoTokenizer, stratoLogic;

const
  InitialMemSize=$10000;//?
  //BaseMemPtr=$100;//some null-pointer safe-guard
  BaseMemPtr=8;
  FirstAllocMemPtr=$2000;
  MaxStackMemPtr=FirstAllocMemPtr-$10;

  SphereBasePtr=BaseMemPtr;//TODO: replace with lookup in TStratoMachine.FGlobals

//Debug:
type
  TCArr=array[0..$FFFFFF] of cardinal;
  PCArr=^TCArr;


{ TStratoMachine }

constructor TStratoMachine.Create;
begin
  inherited Create;
  FMemSize:=InitialMemSize;
  SetLength(FMem,FMemSize);
  FMemIndex:=BaseMemPtr;
  FMemAllocIndex:=FirstAllocMemPtr;
end;

destructor TStratoMachine.Destroy;
begin
  SetLength(FMem,0);
  inherited;
end;

procedure TStratoMachine.Run(Sphere: TStratoSphere);
var
  n:TStratoName;
  main,shell,p,f0,f1:TStratoIndex;
  i:cardinal;
begin
  //find 'main'
  n:=Sphere.Dict.StrIdx('main');
  main:=0;//Lookup(FirstGlobalNameSpace? assert main not in runtime ns
  p:=Sphere.Header.FirstNameSpace;
  while (main=0) and (p<>0) do
   begin
    p:=Sphere[p].Next;
    main:=Sphere.Lookup(Sphere[p].FirstItem,n);
   end;
  if (main=0) or (Sphere[main].ThingType<>ttFunction) then
    Sphere.Error(0,'No function "main" defined in root namespaces')
  else
   begin
    //find type decl 'shell.shell'
    n:=Sphere.Dict.StrIdx('shell');
    p:=Sphere.Lookup(Sphere.Header.FirstNameSpace,n);
    if p=0 then shell:=0 else
      shell:=Sphere.Lookup(Sphere[p].FirstItem,n);
    if (shell=0) or (Sphere[shell].ThingType<>ttRecord) then
      Sphere.Error(0,'No declaration for "shell.shell"')
    else
     begin
      AllocateGlobals(Sphere);

      //create 'shell.shell' instance
      i:=FMemIndex;//? nothing yet...
      inc(FMemIndex,Sphere[shell].ByteSize);
      f0:=Sphere.Add(ttVar,'');
      Sphere[f0].Parent:=Sphere.Header.FirstNameSpace;//see Addr
      Sphere[f0].EvaluatesTo:=shell;
      Sphere[f0].Offset:=i-SphereBasePtr;
      Move(i,FMem[i],SystemWordSize);//??
      //TODO: shell... for now assert (s2 as TStratoTypeDecl).ByteSize=0

      //create main call
      f1:=Sphere.Add(ttFnCall,'main');
      Sphere[f1].Subject:=main;
      StratoFnCallAddArgument(Sphere,f1,f0);
      StratoFnCallFindSignature(Sphere,f1);
      if Sphere[f1].Signature=0 then
        Sphere.Error(0,'No overload found "main(Shell)"')
      else
        Perform(Sphere,f1);

     end;
   end;
end;

procedure TStratoMachine.AllocateGlobals(Sphere: TStratoSphere);
var
  p,q,r:TStratoIndex;
begin
  //allocate memory
  //FGlobals[].Sphere:=Sphere;//SphereBasePtr
  //FGlobals[].Address:=FMemIndex;
  inc(FMemIndex,Sphere.Header.GlobalByteSize);

  //initialize
  p:=Sphere.Header.FirstGlobalVar;
  while p<>0 do
   begin
    //assert Sphere[p].ThingType=strGlobal
    q:=Sphere[p].Subject;
    //assert Sphere[q].ThingType=strVar
    r:=Sphere[q].InitialValue;
    if r<>0 then LiteralToMemory(Sphere,r,SphereBasePtr+Sphere[q].Offset);
    p:=Sphere[p].Next;
   end;
end;

procedure TStratoMachine.Perform(Sphere:TStratoSphere;Entry:TStratoIndex);
var
  //TODO: store stack in FMem
  stackIndex,stackLength:integer;
  stack:array of record
    p,p1,p2:TStratoIndex;
    bp:cardinal;
  end;

  procedure Push(p,p1,p2:TStratoIndex;bp:cardinal);
  begin
    if stackIndex=stackLength then
     begin
      //TODO: stack overflow? use mem?
      inc(stackLength,$100);//grow
      SetLength(stack,stackLength);
     end;
    stack[stackIndex].p:=p;
    stack[stackIndex].p1:=p1;
    stack[stackIndex].p2:=p2;
    stack[stackIndex].bp:=bp;
    inc(stackIndex);
  end;

  procedure Pop(var p,p1,p2:TStratoIndex;var bp:cardinal);
  begin
    dec(stackIndex);
    p:=stack[stackIndex].p;
    p1:=stack[stackIndex].p1;
    p2:=stack[stackIndex].p2;
    bp:=stack[stackIndex].bp;
    {$IFDEF DEBUG}
    stack[stackIndex].p:=0;
    stack[stackIndex].p1:=0;
    stack[stackIndex].p2:=0;
    stack[stackIndex].bp:=0;
    {$ENDIF}
  end;

  procedure BreakLoop;
  var
    q,p1,p2:TStratoIndex;
    qx:PStratoThing;
    dd:array of record
      cb,p:TStratoIndex;
      bp:cardinal;
    end;
    ddi,ddl,bp:cardinal;
  begin
    ddi:=0;
    ddl:=0;
    q:=0;
    qx:=nil;
    while (stackIndex<>0) and ((q=0) or (
      (qx.ThingType<>ttIteration) and
      (qx.ThingType<>ttIterationPE) and
      (qx.ThingType<>ttFnCall))) do
     begin
      Pop(q,p1,p2,bp);
      qx:=Sphere[q];
      if (q<>0) and (qx.ThingType=ttCodeBlock) then
       begin
        p1:=q;
        q:=qx.FirstStatement;
        while (q<>0) and (Sphere[q].ThingType<>ttDeferred) do
          q:=Sphere[q].Next;
        if q<>0 then
         begin
          if ddi=ddl then //grow
           begin
            inc(ddl,$100);
            SetLength(dd,ddl);
           end;
          dd[ddi].cb:=p1;
          dd[ddi].p:=q;
          dd[ddi].bp:=bp;
          inc(ddi);
         end;
       end;
     end;
    //push any deferred from interrupted code blocks
    while (ddi<>0) do
     begin
      dec(ddi);
      p1:=dd[ddi].cb;
      q:=dd[ddi].p;
      bp:=dd[ddi].bp;
      Push(p1,0,0,bp);//dummy restore value, with cb here for Addr
      Push(q,q,0,bp);
      Push(Sphere[q].Subject,0,0,bp);
     end;
  end;

  procedure Throw(var bp:cardinal;vt:TStratoIndex;vp:cardinal);
  var
    p,p1,p2,q,cc:TStratoIndex;
    px:PStratoThing;
    dd:array of record
      cb,p:TStratoIndex;
      bp:cardinal;
    end;
    ddi,ddl,xp,i:cardinal;
  begin
    //find suitable catch
    ddi:=0;
    ddl:=0;
    cc:=0;
    p:=0;
    while (stackIndex<>0) and (cc=0) do
     begin
      p:=0;
      while (stackIndex<>0) and ((p=0) or (Sphere[p].ThingType<>ttCodeBlock)) do
        Pop(p,p1,p2,xp);
      if p<>0 then
       begin
        if p1<>0 then p1:=Sphere[p1].Next;
        while (p1<>0) and (cc=0) do
         begin
          px:=Sphere[p1];
          case px.ThingType of
            ttCatch://check catch filter
             begin
              if (px.ItemType=0) or SameType(Sphere,vt,px.ItemType) then
                cc:=p1
              else
                p1:=px.Next;
             end;
            ttTry:
             begin
              //exception happend before this 'try' (":::")
              //so disregard any following 'catch's
              p1:=0;
             end;
            else p1:=px.Next;
          end;
         end;
        if cc=0 then
         begin
          //check interrupted code block for any deferred
          q:=Sphere[p].FirstStatement;
          while (q<>0) and (Sphere[q].ThingType<>ttDeferred) do
            q:=Sphere[q].Next;
          if q<>0 then
           begin
            if ddi=ddl then //grow
             begin
              inc(ddl,$100);
              SetLength(dd,ddl);
             end;
            dd[ddi].cb:=p;
            dd[ddi].p:=q;
            dd[ddi].bp:=xp;
            inc(ddi);
           end;
         end;
       end;
     end;
    //catch thrown
    if cc=0 then
     begin
      //TODO: output exception type and value
      //TODO: stack dump?
      stackIndex:=0;
      Writeln('Abnormal Termination ('+IntToStr(p)+')');
      Exitcode:=1;
     end
    else
     begin
      Push(p,cc,p2,bp);
      Push(Sphere[cc].Subject,0,0,bp);

      //TODO: proper allocation! since catch var is set post cb.ByteSize, that memory could be in use!!
      //vt:=//TODO: cast to cc.ItemType
      i:=Sphere[vt].ByteSize;
      Move(FMem[vp],FMem[xp+Sphere[Sphere[cc].FirstItem].Offset],i);

      bp:=xp+Sphere[p].ByteSize+i;

      //but first any deferred from interrupted code blocks
      while (ddi<>0) do
       begin
        dec(ddi);
        p1:=dd[ddi].cb;
        q:=dd[ddi].p;
        bp:=dd[ddi].bp;
        Push(p1,0,0,bp);//dummy restore value, with cb here for Addr
        Push(q,q,0,bp);
        Push(Sphere[q].Subject,0,0,bp);
       end;

     end;
  end;

var
  p,q,r,vt,p0,p1,p2,vt0,pe:TStratoIndex;
  px,qx,rx:PStratoThing;
  i,j,k,mp,np,vp,xp:cardinal;

  procedure vtp(nvt,nvp:cardinal);
  begin
    vt:=nvt;
    vp:=nvp;
    vt0:=0;
  end;
begin
  stackIndex:=0;
  stackLength:=0;
  p:=Entry;
  pe:=Entry;
  p1:=0;
  mp:=FMemIndex;
  np:=mp;
  vt:=0;//below: set vt:=0 when value at vp used for something
  vp:=0;
  while (p<>0) or (stackIndex<>0) do
   begin
    //pop from stack: see end of loop below
    p0:=p;//see check below
    vt0:=vt;//see check below
    px:=Sphere[p];
    if px.Source<>0 then pe:=p;
    //assert q=0 or ((q.ThingType and str__Typed)<>0)
    case px.ThingType of

      ttFnCall:
        if (px.Signature=0) or (px.Body=0) then
          Sphere.Error(pe,'call without function overload')
        else
        if (p1=0) and (px.Subject<>0)
          and (Sphere[px.Subject].ThingType=ttVarIndex) then
         begin //get address for 'this' ("@@")
          //q:=Sphere.Lookup(Sphere[px.Body].FirstItem,Sphere.Dict.StrIdx('@@'));
          q:=Sphere[px.Body].FirstItem;
          while (q<>0) and (Sphere[q].ThingType<>ttThis) do q:=Sphere[q].Next;
          if q=0 then
            Sphere.Error(pe,'Could not find "@@"')
          else
           begin
            Push(p,TypeDecl_void,q,mp);
            p:=px.Subject;
           end;
         end
        else
        if (p1=0) or (p1=TypeDecl_void) then
         begin
          qx:=Sphere[px.Subject];
          if p1=TypeDecl_void then //store address for 'this'
           begin
//            if vt=0 then
//              Sphere.Error(pe,'Could not get value for "@@"')
//            else
             begin
              //TODO: SameType? vt px.Signature.Subject
              {
              i:=Sphere[vt].ByteSize;
              if i<>0 then Move(FMem[vp],FMem[np+Sphere[p2].Offset],i);
              }
              Move(vp,FMem[np+Sphere[p2].Offset],SystemWordSize);//see ttThis
              vt:=0;
             end;
            qx:=Sphere[qx.Subject];
           end;
          //start evaluating arguments
          p1:=px.FirstArgument;
          p2:=qx.FirstArgument;
          if (p1<>0) and (p2=0) then
            Sphere.Error(pe,'overload without argument values in code block')
          else
           begin
            //push self (see ttThrow with Subject=0 for break/abort)
            Push(p,p,0,np);
            if p1=0 then
             begin
              //no arguments!
              p:=px.Body;
             end
            else
             begin
              //TODO: function's parent an interface? determine implementing object (see if px.Body=0 above)

              //push function's code block (also for Addr)
              Push(px.Body,0,0,np);

              //start getting the first argument
              qx:=Sphere[p1];
              //if qx.ThingType<>ttArgByRef then
              if qx.InitialValue<>0 then
                LiteralToMemory(Sphere,qx.InitialValue,np+Sphere[p2].Offset);
              Push(p,p1,p2,np);
              inc(np,Sphere[px.Body].ByteSize);
              p:=qx.Subject;
             end;
           end;
         end
        else
        if p1=p then//output result value
         begin
          if Sphere[px.Signature].EvaluatesTo<>0 then
           begin
            q:=Sphere.Lookup(Sphere[px.Body].FirstItem,Sphere[px.Signature].Name);
            if q=0 then
              Sphere.Error(pe,'return value not found')
            else
              vtp(Sphere[px.Signature].EvaluatesTo,mp+Sphere[q].Offset);
           end;
         end
        else
         begin
          //store argument value
          if vt=0 then
            Sphere.Error(pe,'no value for argument "'+
              string(Sphere.Dict.Str[Sphere[p1].Name])+'"')
          else
           begin
            if Sphere[p2].ThingType=ttVarByRef then
              Move(vp,FMem[mp+Sphere[p2].Offset],SystemWordSize)
            else
             begin
              //assert p2.ThingType=ttArgument
              i:=Sphere[vt].ByteSize;
              if i<>0 then Move(FMem[vp],FMem[mp+Sphere[p2].Offset],i);
             end;
            vt:=0;
           end;
          //next argument
          p1:=Sphere[p1].Next;
          p2:=Sphere[p2].Next;//assert sequence of arguments correctly added as vars to codeblock
          np:=mp;
          if p1<>0 then
           begin
            qx:=Sphere[p1];
            if qx.InitialValue<>0 then
              LiteralToMemory(Sphere,qx.InitialValue,mp+Sphere[p2].Offset);
            Push(p,p1,p2,np);
            p:=qx.Subject;
           end;
          //else p:=px.Body: pushed onto stack already
         end;

      ttCodeBlock:
        if p1=0 then //begin block
         begin
          if px.FirstStatement<>0 then
           begin
            Push(p,px.FirstStatement,0,np);
            //initial values
            r:=px.FirstItem;
            while r<>0 do
             begin
              rx:=Sphere[r];
              if rx.InitialValue<>0 then //assert literal
                LiteralToMemory(Sphere,rx.InitialValue,np+rx.Offset);
              r:=rx.Next;
             end;
            //first statement
            inc(np,px.ByteSize);
            p:=px.FirstStatement;
           end;
         end
        else
         begin
          //assert p=cb
          q:=Sphere[p1].Next;
          if q=0 then //code block done
           begin
            //TODO: if px.EvaluatesTo<>0 then if vt=0 then? SameType?
            if px.EvaluatesTo=0 then vt:=0;
            //check any deferred
            q:=px.FirstStatement;
            while (q<>0) and (Sphere[q].ThingType<>ttDeferred) do
              q:=Sphere[q].Next;
            if q<>0 then
             begin
              Push(0,0,vt,vp);//keep result value
              vt:=0;
              Push(q,q,p2,mp);
              p:=Sphere[q].Subject;
             end;
            np:=mp;
           end
          else//next statement
           begin
            Push(p,q,p2,mp);
            p:=q;
            vt:=0;
            np:=mp+px.ByteSize;
           end;
         end;
      ttDeferred://when p1=0: don't run now, see ttThrow,ttCodeBlock
        if p1<>0 then
         begin
          //next deferred command?
          np:=mp;
          q:=Sphere[p1].Next;
          while (q<>0) and (Sphere[q].ThingType<>ttDeferred) do
            q:=Sphere[q].Next;
          if q=0 then
           begin
            //done
            Pop(q,r,vt,vp);//restore result value
           end
          else
           begin
            Push(q,q,p2,mp);
            p:=Sphere[q].Subject;
            vt:=0;
           end;
         end;
      ttCatch://when p1=0: don't run now, see ttThrow
        ;//if p1<>0 then p:=px.Subject;//see ttThrow

      ttSysCall:
        try
          PerformSysCall(Sphere,p,np);
        except
          on e:Exception do //TODO: in-lang throw
            Sphere.Error(pe,'['+e.ClassName+']'+e.Message);
        end;

      ttFunction:
       begin
        //address of
        //TODO: get type!
        q:=Sphere.Add(ttPointer,'');
        qx:=Sphere[q];
        qx.ByteSize:=SystemWordSize;
        qx.EvaluatesTo:=px.Signature;
        vtp(q,np);
        Move(p,FMem[np],SystemWordSize);
        inc(np,Sphere[vt].ByteSize);
       end;
      ttVar,ttVarIndex,ttThis://vp:=Addr(p);
       begin
        if p1=0 then vtp(px.EvaluatesTo,0);
        q:=p;
        p:=0;
        while (q<>0) and (p=0) do
         begin
          qx:=Sphere[q];
          case qx.ThingType of

            //ttRecord://TODO: dereference pointer

            ttVar:
              inc(vp,qx.Offset);

            ttNameSpace://global var
             begin
              inc(vp,SphereBasePtr);
              q:=0;//end loop
             end;

            ttCodeBlock:
             begin
              i:=stackIndex;
              while (i<>0) and (stack[i-1].p<>q) do dec(i);
              if i=0 then
                Sphere.Error(pe,'var relative to code block not currently in execution')
              else
                inc(vp,stack[i-1].bp);
              q:=0;//end loop
             end;

            ttVarIndex:
              if qx.FirstArgument=0 then
                inc(vp,Sphere[qx.Subject].Offset)
              else
              if p1=0 then
               begin
                Push(q,q,vt,vp);
                vt:=0;
                p:=qx.FirstArgument;
                //TODO: multi-dim-array?
               end
              else
              if vt=0 then
                Sphere.Error(pe,'Array index without value')
              else
              //TODO: SameType? find operator overload?
              if vt<>TypeDecl_number then
                Sphere.Error(pe,'Array index not number')
              else
               begin
                Move(FMem[vp],i,SystemWordSize);
                j:=0;//default
                //TODO: check p2 px.EvaluatesTo
                if Sphere[px.EvaluatesTo].ThingType=ttArray then
                 begin
                  p2:=Sphere[px.EvaluatesTo].ItemType;
                  j:=Sphere[p2].ByteSize;
                 end
                else
                  Sphere.Error(pe,'Unexpected index not into array');
                //TODO: check range? (px.ByteSize)
                vtp(p2,mp+i*j);
                if qx.Subject<>0 then
                  inc(vp,Sphere[qx.Subject].Offset);
               end;

            ttThis:
             begin
              i:=stackIndex;
              while (i<>0) and (stack[i-1].p<>qx.Parent) do dec(i);
              if i=0 then
                Sphere.Error(pe,'this unknown')
              else
               begin
                mp:=stack[i-1].bp+qx.Offset;
                xp:=0;
                Move(FMem[mp],xp,SystemWordSize);
                inc(vp,xp);
               end;
              q:=0;
             end;

            else
              Sphere.Error(pe,'invalid relativity chain');
          end;
          if q<>0 then q:=Sphere[q].Parent;
         end;
        //TODO: on error throw access violation!
        //TODO: detect out of memory!!!
       end;
      ttConstant:
       begin
        vtp(px.EvaluatesTo,np);
        inc(np,Sphere[vt].ByteSize);
        LiteralToMemory(Sphere,px.InitialValue,vp);
       end;
      ttLiteral:
       begin
        vtp(px.EvaluatesTo,np);
        inc(np,Sphere[vt].ByteSize);
        LiteralToMemory(Sphere,p,vp);
       end;
      ttUnaryOp:
        if px.Right=0 then
          Sphere.Error(pe,'unary operator without subject')
        else
        if p1=0 then
         begin
          Push(p,p,0,np);
          p:=px.Right;
          vt:=0;
         end
        else
        if vt=0 then
          Sphere.Error(pe,'unary operator without value')
        else
          case TStratoToken(px.Op) of
            stOpSub,stOpInc,stOpDec:
              if vt=TypeDecl_number then
               begin
                Move(FMem[vp],i,SystemWordSize);
                case TStratoToken(px.Op) of
                  stOpSub:i:=-i;
                  stOpInc:inc(i);
                  stOpDec:dec(i);
                end;
                Move(i,FMem[vp],SystemWordSize);
                inc(np,SystemWordSize);
                vt0:=0;//silence unused error
               end
              else
                Sphere.Error(pe,'//TODO: more operator stuff');
            stOpNot:
              if vt=TypeDecl_bool then
               begin
                Move(FMem[vp],i,SystemWordSize);
                if i=0 then i:=1 else i:=0;
                Move(i,FMem[vp],SystemWordSize);
                vt0:=0;//silence unused error
               end
              else
                Sphere.Error(pe,'//TODO: more operator stuff');
            //TODO: more!
            else
              Sphere.Error(pe,'unsupported unary operator');
          end;
      ttBinaryOp:
        if p1=0 then //evaluate left
          if px.Left=0 then
            Sphere.Error(pe,'binary operator without left side')
          else
           begin
            Push(p,p,0,np);
            p:=px.Left;
           end
        else
        if p2=0 then //evaluate right
          if vt=0 then
            Sphere.Error(pe,'binary operator left side without value')
          else
            if px.Right=0 then
              Sphere.Error(pe,'binary operator without right side')
            else
             begin
              Push(0,0,vt,vp);//store result left on stack
              vt:=0;
              Push(p,p,p,np);
              p:=px.Right;
             end
        else //perform operator
          if vt=0 then
            Sphere.Error(pe,'binary operator right side without value')
          else
           begin
            Pop(p1,p2,q,xp);//left: q@xp right: vt@vp
            np:=mp;
            vt0:=0;//drop value
            case TStratoToken(px.Op) of
              stOpEQ:
               begin
                i:=0;
                if SameType(Sphere,q,vt) then
                 begin
                  //TODO: .ByteSize
                  Move(FMem[xp],i,SystemWordSize);
                  Move(FMem[vp],j,SystemWordSize);
                  if i=j then i:=1 else i:=0;
                 end;
                vtp(TypeDecl_bool,np);
                inc(np,SystemWordSize);
                Move(i,FMem[vp],SystemWordSize);
               end;
              stOpAdd:
               begin
                //assert SameType(q.EvaluatesTo,pp.EvaluatesTo)
                if (q=TypeDecl_number) and (vt=TypeDecl_number) then
                 begin
                  Move(FMem[xp],i,SystemWordSize);
                  Move(FMem[vp],j,SystemWordSize);
                  k:=i+j;
                  vtp(TypeDecl_number,np);
                  inc(np,SystemWordSize);
                  Move(k,FMem[vp],SystemWordSize);
                 end
                else
                if (q=TypeDecl_string) and (vt=TypeDecl_string) then
                 begin
                  //TODO: strings in memory (by runtime?)
                  Move(FMem[xp],i,SystemWordSize);
                  Move(FMem[vp],j,SystemWordSize);
                  k:=Sphere.AddBinaryData(
                    Sphere.GetBinaryData(i)+
                    Sphere.GetBinaryData(j)
                  );
                  vtp(TypeDecl_string,np);
                  inc(np,SystemWordSize);
                  Move(k,FMem[vp],SystemWordSize);
                 end
                else
                  Sphere.Error(pe,'//TODO: more operator stuff');
               end;
              stOpSub,stOpMul,stOpDiv,stOpMod,stOpShl,stOpShr:
               begin
                //assert SameType(q.EvaluatesTo,pp.EvaluatesTo)
                if (q=TypeDecl_number) and (vt=TypeDecl_number) then
                 begin
                  Move(FMem[xp],i,SystemWordSize);
                  Move(FMem[vp],j,SystemWordSize);
                  case TStratoToken(px.Op) of
                    //stOpAdd:k:=i+j;//see above
                    stOpSub:k:=i-j;
                    stOpMul:k:=i*j;
                    stOpDiv:k:=i div j;
                    stOpMod:k:=i mod j;
                    //stOpInc:
                    //stOpDec:
                    stOpShl:k:=i shl j;
                    stOpShr:k:=i shr j;
                  end;
                  vtp(TypeDecl_number,np);
                  inc(np,SystemWordSize);
                  Move(k,FMem[vp],SystemWordSize);
                 end
                else
                  Sphere.Error(pe,'//TODO: more operator stuff');
               end;
              stOpNEQ,stOpLT,stOpLTE,stOpGT,stOpGTE:
               begin
                //assert SameType(q.EvaluatesTo,pp.EvaluatesTo)
                if (q=TypeDecl_number) and (vt=TypeDecl_number) then
                 begin
                  Move(FMem[xp],i,SystemWordSize);
                  Move(FMem[vp],j,SystemWordSize);
                  k:=0;
                  case TStratoToken(px.Op) of
                    stOpNEQ:if i<>j then k:=1;
                    stOpLT:if i<j then k:=1;
                    stOpLTE:if i<=j then k:=1;
                    stOpGT:if i>j then k:=1;
                    stOpGTE:if i>=j then k:=1;
                  end;
                  vtp(TypeDecl_bool,np);
                  inc(np,SystemWordSize);
                  Move(k,FMem[vp],SystemWordSize);
                 end
                else
                  Sphere.Error(pe,'//TODO: more operator stuff');
               end;
              stOpAnd,stOpOr,stOpXor:
                if (q=TypeDecl_bool) and (vt=TypeDecl_bool) then
                 begin
                  Move(FMem[xp],i,SystemWordSize);
                  Move(FMem[vp],j,SystemWordSize);
                  k:=0;
                  case TStratoToken(px.Op) of
                    stOpAnd:if (i<>0) and (j<>0) then k:=1 else k:=0;
                    stOpOr:if (i<>0) or (j<>0) then k:=1 else k:=0;
                    stOpXor:if (i<>0) xor (j<>0) then k:=1 else k:=0;
                  end;
                  vtp(TypeDecl_bool,np);
                  inc(np,SystemWordSize);
                  Move(k,FMem[vp],SystemWordSize);
                 end;
              //TODO: more!
              else
                Sphere.Error(pe,'unsupported binary operator');
            end;
           end;
      ttAssign:
        if p1=0 then //evaluate ValueFrom
          if px.ValueFrom=0 then
            Sphere.Error(pe,'assignment without right side')
          else if px.AssignTo=0 then
            Sphere.Error(pe,'assignment without left side')
          else
           begin
            Push(p,px.ValueFrom,0,np);
            p:=px.AssignTo;
           end
        else
        if p1=px.ValueFrom then
          if vt=0 then
            Sphere.Error(pe,'assignment right side without value')
          else
           begin
            Push(0,0,vt,vp);//store address
            vt:=0;
            Push(p,TypeDecl_void,0,mp);
            p:=px.ValueFrom;//TODO: ttCast?
           end
        else
          if vt=0 then
            Sphere.Error(pe,'assignment left side without address')
          else
           begin
            Pop(p1,p2,q,xp);
            if not SameType(Sphere,q,vt) then //px.EvaluatesTo?
              Sphere.Error(pe,'assignment type mismatch')
            else
              case TStratoToken(px.Op) of
                stOpAssign:
                  Move(FMem[vp],FMem[xp],Sphere[vt].ByteSize);
                stOpAssignAdd:
                  if vt=TypeDecl_number then
                   begin
                    Move(FMem[xp],i,SystemWordSize);
                    Move(FMem[vp],j,SystemWordSize);
                    i:=i+j;
                    Move(i,FMem[xp],SystemWordSize);
                   end
                  else
                  if vt=TypeDecl_string then
                   begin
                    Move(FMem[xp],i,SystemWordSize);
                    Move(FMem[vp],j,SystemWordSize);
                    i:=Sphere.AddBinaryData(
                      Sphere.GetBinaryData(i)+
                      Sphere.GetBinaryData(j));
                    Move(i,FMem[xp],SystemWordSize);
                   end
                  else
                    Sphere.Error(pe,'//TODO: more operator stuff');
                stOpAssignSub, //"-="
                stOpAssignMul, //"*="
                stOpAssignDiv, //"/="
                stOpAssignMod, //"%="
                stOpAssignOr, //"||="
                stOpAssignAnd:
                  if vt=TypeDecl_number then
                   begin
                    Move(FMem[xp],i,SystemWordSize);
                    Move(FMem[vp],j,SystemWordSize);
                    case TStratoToken(px.Op) of
                      stOpAssignSub:i:=i-j;
                      stOpAssignMul:i:=i*j;
                      stOpAssignDiv:i:=i div j;
                      stOpAssignMod:i:=i mod j;
                      stOpAssignOr: i:=i or j;
                      stOpAssignAnd:i:=i and j;
                    end;
                    Move(i,FMem[xp],SystemWordSize);
                   end
                  else
                    Sphere.Error(pe,'//TODO: more operator stuff');
                else
                  Sphere.Error(pe,'//TODO: assignment operators');
              end;
            vt0:=0;
            np:=mp;
           end;
      ttSelection:
        if (p1=0) and (px.DoIf<>0) then
         begin
          Push(p,px.DoIf,0,np);
          p:=px.DoIf;
         end
        else
         begin
          if px.DoIf=0 then
            p:=px.DoThen
          else
           begin
            //assert vt=0 or vt=TypeDecl_bool
            //TODO: reference var check null
            if vt=TypeDecl_bool then
             begin
              Move(FMem[vp],i,SystemWordSize);
              if i<>0 then p:=px.DoThen else p:=px.DoElse;
             end
            else
              Sphere.Error(pe,'selection criterium value is not boolean');
            vt:=0;
            np:=mp;
           end;
         end;
      ttIteration:
        if (p1=0) and (px.DoFirst<>0) then
         begin
          Push(p,p,0,np);
          p:=px.DoFirst;
         end
        else
         begin
          i:=0;//default
          if px.DoIf=0 then i:=1 else
            if p1=px.DoIf then
             begin
              if vt=TypeDecl_bool then
                Move(FMem[vp],i,SystemWordSize)
              else
                Sphere.Error(pe,'iteration criterium value is not boolean');
              vt:=0;
              np:=mp;
             end
            else
             begin
              Push(p,px.DoIf,0,mp);
              p:=px.DoIf;
              vt:=0;//silence DoFirst/DoThen leftover
             end;
          //perform Body,DoThen
          if (i<>0) and not((px.Body=0) and (px.DoThen=0)) then
           begin
            Push(p,p,0,mp);
            p:=px.Body;
            if px.DoThen<>0 then Push(px.DoThen,0,0,mp);
           end;
         end;
      ttIterationPE:
        if (p1=0) and (px.DoFirst<>0) then
         begin
          Push(p,p,0,np);
          p:=px.DoFirst;
         end
        else
        if (px.Body<>0) and (p1=p) then
         begin
          Push(p,px.Body,0,np);
          p:=px.Body;
          vt:=0;//silence DoFirst/DoThen leftover
         end
        else
         begin
          i:=0;//default
          if px.DoIf=0 then i:=1 else
            if p1=px.DoIf then
             begin
              if vt=TypeDecl_bool then
                Move(FMem[vp],i,SystemWordSize)
              else
                Sphere.Error(pe,'iteration criterium value is not boolean');
              vt:=0;
              np:=mp;
             end
            else
             begin
              Push(p,px.DoIf,0,np);
              p:=px.DoIf;
             end;
          if (i<>0) and not((px.Body=0) and (px.DoThen=0)) then
           begin
            Push(p,p,0,np);
            p:=px.DoThen;
           end;
         end;
      ttCast:
        if p1=0 then
          if px.Subject=0 then
            Sphere.Error(pe,'cast without subject')
          else
           begin
            Push(p,px.Subject,0,np);
            p:=px.Subject;
           end
        else
          if vt=0 then
            Sphere.Error(pe,'no value to cast')
          else
           begin
            //TODO:
            //see also SameType
            if (vt=TypeDecl_string) and (px.EvaluatesTo=TypeDecl_number) then
             begin
              Move(FMem[vp],i,SystemWordSize);
              if not TryStrToInt(string(Sphere.GetBinaryData(i)),integer(j)) then
                Sphere.Error(pe,'invalid integer value');//TODO: raise
              vtp(TypeDecl_number,np);
              inc(np,SystemWordSize);
              Move(j,FMem[vp],SystemWordSize);
             end
            else
            if (vt=TypeDecl_number) and (px.EvaluatesTo=TypeDecl_string) then
             begin
              Move(FMem[vp],i,SystemWordSize);
              j:=Sphere.AddBinaryData(UTF8String(IntToStr(i)));
              vtp(TypeDecl_string,np);
              inc(np,SystemWordSize);
              Move(j,FMem[vp],SystemWordSize);
             end
            else
            if (vt=TypeDecl_bool) and (px.EvaluatesTo=TypeDecl_string) then
             begin
              Move(FMem[vp],i,SystemWordSize);
              if i=0 then
                j:=Sphere.AddBinaryData('0')
              else
                j:=Sphere.AddBinaryData('1');
              vtp(TypeDecl_string,np);
              inc(np,SystemWordSize);
              Move(j,FMem[vp],SystemWordSize);
             end
            else
            if (vt=TypeDecl_number)
              and (Sphere[px.EvaluatesTo].ThingType=ttEnumeration) then
             begin
              Move(FMem[vp],i,SystemWordSize);
              //check enumeration item?
              vtp(px.EvaluatesTo,np);
              inc(np,SystemWordSize);
              Move(i,FMem[vp],SystemWordSize);
             end
            else
            if (Sphere[vt].ThingType=ttEnumeration)
              and (px.EvaluatesTo=TypeDecl_number) then
             begin
              Move(FMem[vp],i,SystemWordSize);
              vtp(px.EvaluatesTo,np);
              inc(np,SystemWordSize);
              Move(i,FMem[vp],SystemWordSize);
             end
            else
              Sphere.Error(pe,'unsupported cast');//TODO: display types
           end;

      ttThrow:
        if px.Subject=0 then //break
         begin
          BreakLoop;
          vt:=0;
         end
        else
        if (px.Subject=0) or (p=p1) then //throw
         begin
          Throw(np,vt,vp);
          vt:=0;
         end
        else //get object to throw
         begin
          Push(p,p,0,np);
          p:=px.Subject;
         end;

      ttAddressOf:
        if p1=0 then
         begin
          Push(p,p,0,np);
          p:=px.ValueFrom;
         end
        else
         begin
          i:=vp;
          vtp(px.EvaluatesTo,np);
          Move(i,FMem[np],SystemWordSize);
          inc(np,SystemWordSize);
         end;
      ttDereference:
        if p1=0 then
         begin
          Push(p,p,0,np);
          p:=px.ValueFrom;
         end
        else
         begin
          vtp(px.EvaluatesTo,vp);
          Move(FMem[vp],vp,SystemWordSize);
          //TODO: access violation? bounds check?
         end;
      ttVarByRef:
       begin
        q:=px.Parent;//assert code block
        vp:=px.Offset;
        i:=stackIndex;
        while (i<>0) and (stack[i-1].p<>q) do dec(i);
        if i=0 then
          Sphere.Error(pe,'var-by-ref relative to code block not currently in execution')
        else
          inc(vp,stack[i-1].bp);
        Move(FMem[vp],i,SystemWordSize);
        vtp(px.EvaluatesTo,i);
       end;

      //TODO: more

      else
       begin
        Sphere.Error(pe,'unknown logic item '+IntToHex(px.ThingType,4));
        p:=0;
       end;
    end;
    //checks
    if p=p0 then
      p:=0//nothing new? force pop
    else
     begin//else clear 'indexer' p1, check q
      p1:=0;
      //p2:=0;?
     end;
    if (vt0<>0) and (vt=vt0) then
     begin
      Sphere.Error(pe,'unused resulting value');//warning? info(q)?
      vt:=0;
     end;
    //check stack overflow
    if np>=MaxStackMemPtr then
      raise Exception.Create('Stack Overflow');//TODO: Throw(
    //pop
    while (p=0) and (stackIndex<>0) do Pop(p,p1,p2,mp);
    //TODO: except here? (create exception object, Throw()...)
   end;
end;

procedure TStratoMachine.LiteralToMemory(Sphere:TStratoSphere;
  p:TStratoIndex;addr:cardinal);
var
  px:PStratoThing;
  i:integer;
begin
  px:=Sphere[p];
  if px.ThingType<>ttLiteral then
    Sphere.Error(p,'literal expected '+IntToHex(px.ThingType,4))
  else
    //TODO: runtime!
    if px.EvaluatesTo=TypeDecl_number then
      if TryStrToInt(string(Sphere.GetBinaryData(px.InitialValue)),i) then
        Move(i,FMem[addr],SystemWordSize)
      else
        Sphere.Error(p,'error evaluating global var initial value')
    else
    if px.EvaluatesTo=TypeDecl_string then
     begin
      //TODO: store strings in conceptual mem (see also stratoRunTime)
      i:=px.InitialValue;//strBinaryData
      Move(i,FMem[addr],SystemWordSize);
     end
    else
    if px.EvaluatesTo=TypeDecl_bool then
     begin
      if Sphere.GetBinaryData(px.InitialValue)='0' then i:=0 else i:=1;
      Move(i,FMem[addr],SystemWordSize);
     end
    else
      Sphere.Error(p,'unsupported literal type');
end;

procedure TStratoMachine.PerformSysCall(Sphere: TStratoSphere;
  Fn: TStratoIndex; Ptr: cardinal);
var
  i,j:cardinal;
begin
  case Sphere[Fn].Op of

    stratoSysCall_writeln:
     begin
      //assert chain is there: strFnCall>strArgument>strVar with string data
      Move(FMem[Ptr-SystemWordSize],i,SystemWordSize);
      Writeln(Sphere.GetBinaryData(i));
     end;
    stratoSysCall_malloc:
     begin
      Move(FMem[Ptr-SystemWordSize],i,SystemWordSize);
      j:=FMemAllocIndex;
      inc(FMemAllocIndex,i);
      while FMemAllocIndex>FMemSize do
       begin
        inc(FMemSize,InitialMemSize);//grow
        SetLength(FMem,FMemSize);
        //TODO: out of memory?
       end;
      //TODO: register mem as in use
      Move(j,FMem[Ptr-SystemWordSize-SystemWordSize],SystemWordSize);
     end;

    //TODO:
    //stratoSysCall_realloc:
    //stratoSysCall_mfree:

    else Sphere.Error(Fn,'unknown system call '+IntToStr(Sphere[Fn].Op));
  end;
end;

end.
