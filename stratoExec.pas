unit stratoExec;

interface

uses SysUtils, stratoDecl, stratoSphere, stratoDebug, stratoDebugView;

type
  TStratoMachine=class(TObject)
  private
    FMem:array of byte; //TODO: array of cardinal?int64? or other way to force alignment?
    FMemSize,FMemIndex,FMemAllocIndex:cardinal;
    //FGlobals:array of Sphere:TStratoSphere; Address:cardinal; end;?
    FDebugView:TfrmDebugView;
    FDebugCount:integer;
    procedure AllocateGlobals(Sphere:TStratoSphere);
    procedure Perform(Sphere:TStratoSphere;Entry:TStratoIndex);
    procedure LiteralToMemory(Sphere:TStratoSphere;p,q:TStratoIndex;addr:cardinal);
    procedure PerformSysCall(Sphere:TStratoSphere;Fn:TStratoIndex;Ptr:cardinal);
  public
    constructor Create(DoDebug:boolean=false);
    destructor Destroy; override;
    procedure Run(Sphere:TStratoSphere);
  end;

implementation

uses Windows, stratoFn, stratoRunTime, stratoTokenizer, stratoLogic, ComCtrls;

const
  InitialMemSize=$100000;//?
  {$IFDEF DEBUG}
  BaseMemPtr=100;//some null-pointer safe-guard
  FirstAllocMemPtr=10000;
  {$ELSE}
  BaseMemPtr=$100;//some null-pointer safe-guard
  FirstAllocMemPtr=$10000;
  {$ENDIF}
  MaxStackMemPtr=FirstAllocMemPtr-$10;

  SphereBasePtr=BaseMemPtr;//TODO: replace with lookup in TStratoMachine.FGlobals

{$IFDEF DEBUG}
type
  TCArr=array[0..$FFFFFF] of cardinal;
  PCArr=^TCArr;
{ add watches (Ctrl+F5):
    stack
    PCArr(@FMem[BaseMemPtr])^
    PCArr(@FMem[FirstAllocMemPtr])^
    (mp-BaseMemPtr) div 4
    (np-BaseMemPtr) div 4
    (vp-BaseMemPtr) div 4
    (xp-BaseMemPtr) div 4
}
{$ENDIF}

function IntToStr0(x:cardinal):string;
begin
  if x=0 then Result:='' else Result:=Format('%d',[x]);//IntToStr(x);
end;

{ TStratoMachine }

constructor TStratoMachine.Create(DoDebug:boolean);
begin
  inherited Create;
  FMemSize:=InitialMemSize;
  SetLength(FMem,FMemSize);
  FMemIndex:=BaseMemPtr;
  FMemAllocIndex:=FirstAllocMemPtr;
  FDebugCount:=0;
  //TODO: restore previous position
  if DoDebug then
    FDebugView:=TfrmDebugView.Create(nil)
  else
    FDebugView:=nil;
end;

destructor TStratoMachine.Destroy;
begin
  SetLength(FMem,0);
  FreeAndNil(FDebugView);
  inherited;
end;

procedure TStratoMachine.Run(Sphere: TStratoSphere);
var
  p:TStratoName;
begin
  if FDebugView<>nil then FDebugView.Show;
  AllocateGlobals(Sphere);
  //TODO: halt on unhandled exception?
  p:=Sphere.r(pHeader,tf_FirstInitialization);
  while p<>0 do
   begin
    Perform(Sphere,p);
    p:=Sphere.r(p,tfNext);
   end;
  p:=Sphere.r(pHeader,tf_FirstFinalization);
  while p<>0 do
   begin
    Perform(Sphere,p);
    p:=Sphere.r(p,tfNext);
   end;

  if FDebugView<>nil then FDebugView.Done;
end;

procedure TStratoMachine.AllocateGlobals(Sphere: TStratoSphere);
var
  p,q,r:TStratoIndex;
begin
  //allocate memory
  //FGlobals[].Sphere:=Sphere;//SphereBasePtr
  //FGlobals[].Address:=FMemIndex;
  inc(FMemIndex,Sphere.v(pHeader,tf_GlobalByteSize));

  //initialize
  p:=Sphere.r(pHeader,tf_FirstGlobalVar);
  while p<>0 do
   begin
    //assert Sphere[p].ThingType=ttGlobal
    q:=Sphere.r(p,tfTarget);
    //assert Sphere[q].ThingType=ttVar
    r:=Sphere.r(q,tfInitialValue);
    if r<>0 then LiteralToMemory(Sphere,r,
      Sphere.r(q,tfEvaluatesTo),SphereBasePtr+Sphere.v(q,tfOffset));
    p:=Sphere.r(p,tfNext);
   end;
end;

procedure TStratoMachine.Perform(Sphere:TStratoSphere;Entry:TStratoIndex);
var
  //TODO: store stack in FMem
  //TODO: TStratoMachineThread object for these:
  stackIndex,stackLength:cardinal;
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
    qt:TStratoThingType;
    dd:array of record
      cb,p:TStratoIndex;
      bp:cardinal;
    end;
    ddi,ddl,bp:cardinal;
  begin
    ddi:=0;
    ddl:=0;
    q:=0;
    qt:=0;
    while (stackIndex<>0) and
      not((qt=ttIteration) or (qt=ttIterationPE) or (qt=ttFnCall)) do
     begin
      Pop(q,p1,p2,bp);
      if (q<>0) and (Sphere.t(q)=ttCodeBlock) then
       begin
        p1:=q;
        q:=Sphere.r(q,tfFirstStatement);
        while (q<>0) and (Sphere.t(q)<>ttDeferred) do
          q:=Sphere.r(q,tfNext);
        if q<>0 then
         begin
          qt:=Sphere.t(q);
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
      Push(Sphere.r(q,tfTarget),0,0,bp);
     end;
  end;

  procedure Throw(var bp:cardinal;vt:TStratoIndex;vp:cardinal);
  var
    p,p1,p2,q,cc:TStratoIndex;
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
      while (stackIndex<>0) and ((p=0) or (Sphere.t(p)<>ttCodeBlock)) do
        Pop(p,p1,p2,xp);
      if p<>0 then
       begin
        if p1<>0 then p1:=Sphere.r(p1,tfNext);
        while (p1<>0) and (cc=0) do
         begin
          case Sphere.t(p1) of
            ttCatch://check catch filter
             begin
              q:=Sphere.r(p1,tfTarget);
              if (q=0) or SameType(Sphere,vt,q) then
                cc:=p1
              else
                p1:=Sphere.r(p1,tfNext);
             end;
            ttTry:
             begin
              //exception happend before this 'try' (":::")
              //so disregard any following 'catch's
              p1:=0;
             end;
            else p1:=Sphere.r(p1,tfNext);
          end;
         end;
        if cc=0 then
         begin
          //check interrupted code block for any deferred
          q:=Sphere.r(p,tfFirstStatement);
          while (q<>0) and (Sphere.t(q)<>ttDeferred) do
            q:=Sphere.r(q,tfNext);
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
      Push(Sphere.r(cc,tfBody),0,0,bp);

      //TODO: proper allocation! since catch var is set post cb.ByteSize, that memory could be in use!!
      //vt:=//TODO: cast to cc.ItemType
      i:=ByteSize(Sphere,vt);
      Move(FMem[vp],FMem[xp+Sphere.v(Sphere.r(cc,tfFirstArgument),tfOffset)],i);

      bp:=xp+ByteSize(Sphere,p)+i;

      //but first any deferred from interrupted code blocks
      while (ddi<>0) do
       begin
        dec(ddi);
        p1:=dd[ddi].cb;
        q:=dd[ddi].p;
        bp:=dd[ddi].bp;
        Push(p1,0,0,bp);//dummy restore value, with cb here for Addr
        Push(q,q,0,bp);
        Push(Sphere.r(q,tfTarget),0,0,bp);
       end;

     end;
  end;

var
  p,q,r,vt,p0,p1,p2,vt0,pe:TStratoIndex;
  i,j,k,mp,np,vp,xp,OpCount:cardinal;
  ii,jj:int64;

  procedure vtp(nvt,nvp:cardinal);
  begin
    vt:=nvt;
    vp:=nvp;
    vt0:=0;
  end;

  function RefreshDebugView: boolean;
  var
    li,li1:TListItem;
    pp,q1,q2:TStratoIndex;
    py,px,ii,jj:cardinal;
  begin
    //TODO: move this to other unit
    inc(OpCount);
    if (FDebugView<>nil) and (FDebugView.cbKeepTrail.Checked) then
     begin
      FDebugView.lvTrail.Items.BeginUpdate;
      try
        li:=FDebugView.lvTrail.Items.Add;
        li.Caption:=IntToStr(OpCount);
        li.SubItems.Add(IntToStr(p));
        li.SubItems.Add(IntToStr0(p1));
        li.SubItems.Add(IntToStr0(p2));
        li.SubItems.Add(IntToStr(mp));
        li.SubItems.Add(IntToStr(np));
        li.SubItems.Add(StratoDumpThing(Sphere,p));
        li.SubItems.Add(IntToStr0(vt));
        li.SubItems.Add(IntToStr0(vp));
        if (vt=0) or (vp=0) then
         begin
          li.SubItems.Add('');
          li.SubItems.Add('');
         end
        else
         begin
          Move(FMem[vp],i,SystemWordSize);
          li.SubItems.Add(Format('%.8x',[i]));
          li.SubItems.Add(IntToStr(i));
         end;
        //timestamp?
      finally
        FDebugView.lvTrail.Items.EndUpdate;
      end;
      FDebugView.lvTrail.ItemFocused:=li;
      li.MakeVisible(false);
     end;

    Result:=false;
    if (FDebugView<>nil) and (FDebugView.CheckBreakPoint(p)) then
     begin
      li:=nil;//default;
      FDebugView.lvStack.Items.BeginUpdate;
      try
        FDebugView.lvStack.Items.Clear;
        i:=0;
        while i<stackIndex do
         begin
          li:=FDebugView.lvStack.Items.Add;
          li.Caption:=IntToStr(i);
          li.SubItems.Add(IntToStr(stack[i].p));
          li.SubItems.Add(IntToStr(stack[i].p1));
          li.SubItems.Add(IntToStr(stack[i].p2));
          li.SubItems.Add(IntToStr(stack[i].bp));
          if stack[i].p=0 then
            li.SubItems.Add('')
          else
            li.SubItems.Add(StratoDumpThing(Sphere,stack[i].p));
          inc(i);
         end;
      finally
        FDebugView.lvStack.Items.EndUpdate;
      end;
      if li<>nil then li.MakeVisible(false);
      FDebugView.lvStack.Selected:=li;
      li1:=nil;//default
      FDebugView.lvMem.Items.BeginUpdate;
      try
        FDebugView.lvMem.Items.Clear;
        i:=BaseMemPtr;
        px:=BaseMemPtr;
        k:=0;
        ii:=0;
        if Sphere.v(pHeader,tf_GlobalByteSize)=0 then pp:=0 else
          pp:=Sphere.v(pHeader,tf_FirstGlobalVar);
        while ((i<FMemIndex) or (i<np)) and (ii<80) do
         begin
          inc(ii);
          Move(FMem[i],j,4);
          li1:=FDebugView.lvMem.Items.Add;
          li1.Caption:=IntToStr(i);
          li1.SubItems.Add(Format('%.8x',[j]));
          li1.SubItems.Add(IntToStr(j));
          if pp=0 then
           begin
            while (k<stackIndex) and not((stack[k].p<>0) and
              (Sphere.t(stack[k].p)=ttCodeBlock) and
              (Sphere.v(stack[k].p,tfByteSize)<>0) and
              (stack[k].bp>=i)) do inc(k);
            if (k=stackIndex) or (stack[k].p=0) then
              if (p<>0) and (Sphere.t(p)=ttCodeBlock) then
               begin
                pp:=Sphere.r(p,tfFirstItem);
                px:=mp;
               end
              else
                pp:=0
            else
             begin
              pp:=Sphere.r(stack[k].p,tfFirstItem);
              px:=stack[k].bp;
             end;
           end;
          if (pp=0) or (px>i) then
           begin
            li1.SubItems.Add('');
            jj:=4;
           end
          else
           begin
            //TODO: accurately keep px+.Offset equal to i !!!
            if Sphere.t(pp)=ttGlobal then
             begin
              li1.SubItems.Add(Format('%d: %s',[pp,
                StratoDumpThing(Sphere,Sphere.r(pp,tfTarget))]));
              jj:=ByteSize(Sphere,Sphere.rr(pp,[tfTarget,tfEvaluatesTo]));
              while (pp<>0) and (px+Sphere.v(Sphere.r(pp,tfTarget),tfOffset)<=i) do
                pp:=Sphere.r(pp,tfNext);
             end
            else
             begin
              li1.SubItems.Add(Format('%d:%d: %s',[Sphere.r(pp,tfParent),pp,
                StratoDumpThing(Sphere,pp)]));
              jj:=ByteSize(Sphere,Sphere.r(pp,tfEvaluatesTo));
              while (pp<>0) and (px+Sphere.v(pp,tfOffset)<=i) do
                pp:=Sphere.r(pp,tfNext);
             end;
           end;
          if jj=0 then jj:=4;
          while (jj and 3)<>0 do inc(jj);
          inc(i,jj);
         end;
        i:=FirstAllocMemPtr;
        if i<FMemAllocIndex then
         begin
          li:=FDebugView.lvMem.Items.Add;
          li.Caption:='';
          li.SubItems.Add('');
          li.SubItems.Add('');
          li.SubItems.Add('');
         end;
        while i<FMemAllocIndex do
         begin
          Move(FMem[i],j,4);
          li:=FDebugView.lvMem.Items.Add;
          li.Caption:=IntToStr(i);
          li.SubItems.Add(Format('%.8x',[j]));
          li.SubItems.Add(IntToStr(j));
          li.SubItems.Add('');//TODO: listen to constructors? malloc?
          inc(i,4);
         end;
      finally
        FDebugView.lvMem.Items.EndUpdate;
      end;
      if li1<>nil then li1.MakeVisible(false);
      FDebugView.lvMem.Selected:=li1;
      FDebugView.txtUpNext.Lines.BeginUpdate;
      try
        FDebugView.txtUpNext.Lines.Clear;
        FDebugView.txtUpNext.Lines.Add(Format('p : %d: %s [mp:%d,np:%d]',
          [p,StratoDumpThing(Sphere,p),mp,np]));
        if p1=0 then
          FDebugView.txtUpNext.Lines.Add('p1')
        else if p1>=IndexStep1 then
          FDebugView.txtUpNext.Lines.Add('p1: '+IntToStr(p1))
        else
          FDebugView.txtUpNext.Lines.Add(Format('p1: %d: %s',
            [p1,StratoDumpThing(Sphere,p1)]));
        if p2=0 then
          FDebugView.txtUpNext.Lines.Add('p2')
        else if p2>=IndexStep1 then
          FDebugView.txtUpNext.Lines.Add('p2: '+IntToStr(p2))
        else
          FDebugView.txtUpNext.Lines.Add(Format('p2: %d: %s',
            [p2,StratoDumpThing(Sphere,p2)]));
        if vt<>0 then
         begin
          FDebugView.txtUpNext.Lines.Add(Format('vt: %d: %s',
            [vt,StratoDumpThing(Sphere,vt)]));
          Move(FMem[vp],j,4);//TODO: ByteSize(Sphere,vt);
          FDebugView.txtUpNext.Lines.Add(Format('vp: @=%d x=%.8x v=%d',[vp,j,j]));
         end;
      finally
        FDebugView.txtUpNext.Lines.EndUpdate;
      end;
      try
        if (pe<>0) and StratoGetSourceFile(Sphere,pe,pp,py,px,q1,q2) then
          FDebugView.ShowSource(Sphere,pp,py,px)
        else
          FDebugView.txtSourceView.Clear;
      except
        FDebugView.txtSourceView.Text:=#13#10#13#10'?????';
      end;
      if FDebugView.WaitNext then
       begin
        inc(FDebugCount);
        Result:=true;
       end;
     end;
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
  OpCount:=0;
  while (p<>0) or (stackIndex<>0) do
   begin
    //pop from stack: see end of loop below
    p0:=p;//see check below
    vt0:=vt;//see check below
    if Sphere.v(p,tfSrcPos)<>0 then pe:=p;//else look up stack?
    //assert q=0 or ((q.ThingType and str__Typed)<>0)

    if RefreshDebugView then
      asm int 3 end;//DebugBreak;//forced breakpoint

    case Sphere.t(p) of

      ttAlias:p:=Sphere.r(p,tfTarget);

      ttFnCall:
        if Sphere.r(p,tfTarget)=0 then
          Sphere.Error(pe,'call without function overload')
        else
         begin
          q:=Sphere.r(p,tfTarget);
          //step 0: determine call target
          if p1=0 then
           begin
            r:=0;
            p2:=0;
            mp:=np;
            case Sphere.t(q) of
              ttOverload:
                r:=Sphere.r(q,tfBody);
              ttVar,ttArrayIndex:
               begin
                Push(p,IndexStep1,0,mp);
                p:=q;
               end;
              ttField:
               begin
                Push(p,IndexStep1,0,mp);
                p:=Sphere.r(q,tfSubject);
               end;
              ttConstructor:
               begin
                r:=Sphere.r(q,tfBody);
                if Sphere.v(p,tfName)<>Name_Inherited then
                 begin
                  //store default nil pointer, base class
                  //see also StratoFnAddOverload
                  i:=0;
                  Move(i,FMem[np],SystemWordSize);
                  p2:=Sphere.r(p,tfEvaluatesTo);//assert ttClass
                  Move(p2,FMem[np+SystemWordSize],SystemWordSize);
                 end
                else
                 begin
                  //constructor calling constructor: copy this and original class
                  //see also StratoFnAddOverload
                  i:=stackIndex;
                  while (i<>0) and (Sphere.t(stack[i-1].p)<>ttFnCall) do
                    dec(i);
                  if i=0 then
                    Sphere.Error(pe,'Unable to obtain parent constructor base pointer')//TODO:throw
                  else
                   begin
                    //assert stack[i-1].p].Target.ThingType=ttConstructor
                    Move(FMem[stack[i-1].bp],FMem[np],SystemWordSize*2);
                   end;
                  //assert vp=0
                  p2:=Sphere.rr(q,[tfSignature,tfEvaluatesTo]);
                 end;
               end;
              ttDestructor:
                //r:=qx.Body;
                Sphere.Error(pe,'Unexpected direct call of destructor');
              //TODO: ttInterface? ttClass?
              else
                Sphere.Error(pe,'Unexpected call target');
            end;
           end
          else
            r:=p2;
          //step 1: target evaluated (see above: ttVar,ttField) store ttThis
          if p1=IndexStep1 then
           begin
            r:=0;
            case Sphere.t(q) of
              ttVar,ttArrayIndex:
                if vt=0 then
                  Sphere.Error(pe,'unable to resolve call target variable')
                else
                  case Sphere.t(vt) of
                    ttClassRef:
                     begin
                      Move(FMem[vp],p2,SystemWordSize);
                      r:=StratoFnCallFindVirtual(Sphere,p2,Sphere.r(p,tfTarget));
                     end;
                    else
                      Sphere.Error(pe,'unexpected call target variable type');
                  end;
              ttField:
               begin
                if (vp=0) or (vt=0) then
                  p2:=vt //this null? assert in constructor
                else
                  case Sphere.t(vt) of
                    ttClass:
                     begin
                      Move(FMem[vp],vp,SystemWordSize);//dereference first
                      //assert object._baseclass @-SystemWordSize
                      Move(FMem[vp-SystemWordSize],p2,SystemWordSize);
                     end;
                    ttClassRef:
                      Move(FMem[vp],p2,SystemWordSize);
                    else
                      p2:=vt;//more checks? error?
                  end;
                if (p2=vt) or (Sphere.v(p,tfName)=Name_Inherited) then
                  r:=Sphere.r(q,tfTarget)
                else
                  if TypeDecl_object=0 then
                   begin
                    Sphere.Error(p,'base class for dynamic calls not defined');
                    r:=Sphere.r(q,tfTarget);
                   end
                  else
                    r:=StratoFnCallFindVirtual(Sphere,p2,Sphere.r(q,tfTarget));//!
               end;
              else
                raise Exception.Create('//TODO');
            end;
            if r=0 then
              Sphere.Error(p,'dynamic implementation not found')
            else
             begin
              case Sphere.t(r) of
                ttOverload,ttDestructor:
                 begin
                  //get address for 'this' ("@@")
                  q:=Sphere.rr(r,[tfBody,tfFirstItem]);
                  while (q<>0) and (Sphere.t(q)<>ttThis) do
                    q:=Sphere.r(q,tfNext);
                  if q=0 then
                    Sphere.Error(pe,'Could not find "@@"')
                  else
                    //TODO: SameType? vt px.Target.Target
                    //not FMem[vp] here! see ttThis under ttVar below
                    Move(vp,FMem[mp+Sphere.v(q,tfOffset)],SystemWordSize);
                 end;
                ttConstructor:
                 begin
                  //store default nil pointer, base class (p2 determined above)
                  //see also StratoFnAddOverload
                  i:=0;
                  Move(i,FMem[mp],SystemWordSize);//@@
                  Move(p2,FMem[mp+SystemWordSize],SystemWordSize);//_baseclass=?@@
                 end
                else
                  Sphere.Error(pe,'unexpected virtual call target');
              end;
              if r<>0 then r:=Sphere.r(r,tfBody);
             end;
            vt:=0;
           end;
          //found a body, or enumerating arguments?
          if r<>0 then
           begin
            //step 2: evaluate arguments
            if p1<>p then
             begin
              //step 2a: start evaluating arguments
              if Sphere.t(r)=ttCodeBlock then
               begin
                p1:=Sphere.r(p,tfFirstArgument);
                p2:=Sphere.rr(r,[tfParent,tfFirstArgument]);
                if (p1<>0) and (p2=0) then
                  Sphere.Error(pe,'overload without argument values in code block')
                else
                  if p1<>0 then
                   begin
                    //push function's code block (to address variables)
                    Push(r,0,0,np);
                    //start getting the first argument
                    //TODO: //if qx.ThingType<>ttArgByRef then
                    if Sphere.r(p1,tfInitialValue,q) then
                      LiteralToMemory(Sphere,q,
                        Sphere.r(p1,tfEvaluatesTo),np+Sphere.v(p2,tfOffset));
                    Push(p,p1,p2,np);
                    inc(np,Sphere.v(r,tfByteSize));
                    p:=Sphere.r(p1,tfTarget);
                   end;
               end
              else
              //step 2b: store argument value, next argument?
               begin
                //store argument value
                if vt=0 then
                  Sphere.Error(pe,'no value for argument "'+
                    string(Sphere.Dict.Str[Sphere.v(p1,tfName)])+'"')
                else
                 begin
                  if Sphere.t(p2)=ttVarByRef then
                    Move(vp,FMem[mp+Sphere.v(p2,tfOffset)],SystemWordSize)
                  else
                   begin
                    //assert p2.ThingType=ttArgument
                    i:=ByteSize(Sphere,vt);
                    if i<>0 then Move(FMem[vp],FMem[mp+Sphere.v(p2,tfOffset)],i);
                   end;
                  vt:=0;
                 end;
                //next argument
                p1:=Sphere.r(p1,tfNext);
                p2:=Sphere.r(p2,tfNext);//assert sequence of arguments correctly added as vars to codeblock
                np:=mp;
                if p1=0 then
                  Pop(r,p1,p2,xp)
                else
                 begin
                  if Sphere.r(p1,tfInitialValue,q) then
                    LiteralToMemory(Sphere,q,
                      Sphere.r(p1,tfEvaluatesTo),mp+Sphere.v(p2,tfOffset));
                  Push(p,p1,p2,mp);
                  p:=Sphere.r(p1,tfTarget);
                 end;
               end;
              //step 3: jump to target body
              if (p1<>p) and (p=p0) then
               begin
                //push self (see ttThrow with Target=0 for break/abort)
                Push(p,p,r,np);
                p:=r;//ttCodeBlock
               end;
             end
            else
            //step 4: output result value
            if Sphere.t(Sphere.r(r,tfParent))=ttConstructor then
             begin
              xp:=mp;//+Sphere[q].Offset;//assert 0 (see StratoFnAddOverload)
              //called by constructor? cascade 'this'
              if Sphere.v(p,tfName)=Name_Inherited then
               begin
                i:=stackIndex;
                if i<>0 then dec(i);
                while (i<>0) and not((stack[i-1].p<>0) and
                  (Sphere.t(stack[i-1].p)=ttFnCall)) do dec(i);
                if i=0 then
                  Sphere.Error(pe,'Could not find current code block for call')
                else
                 begin
                  //assert stack[i-1].p's eventual target is a ttConstructor
                  xp:=stack[i-1].bp;
                  Move(FMem[mp],FMem[xp],SystemWordSize);
                 end;
               end;
              vtp(Sphere.r(p,tfEvaluatesTo),xp);
             end
            else
              if Sphere.r(p,tfEvaluatesTo)<>0 then
               begin
                //assert first value in code block is return value (after this)
                q:=Sphere.r(r,tfFirstItem);
                if (q<>0) and (Sphere.t(q)=ttThis) then
                  q:=Sphere.r(q,tfNext);//assert t(p.Target)=ttField
                vtp(Sphere.r(q,tfEvaluatesTo),mp+Sphere.v(q,tfOffset));
               end;
           end;
         end;

      ttCodeBlock:
       begin
        if p1=0 then //begin block
         begin
          mp:=np;
          q:=Sphere.r(p,tfFirstStatement);
          if q<>0 then
           begin
            Push(p,q,0,mp);
            //initial values
            r:=Sphere.r(p,tfFirstItem);
            while r<>0 do
             begin
              if Sphere.t(r)<>ttThis then
               begin
                q:=Sphere.r(r,tfInitialValue);
                if q<>0 then LiteralToMemory(Sphere,q,
                  Sphere.r(r,tfEvaluatesTo),mp+Sphere.v(r,tfOffset));
               end;
              //else assert ttThis value provided by ttFnCall
              r:=Sphere.r(r,tfNext);
             end;
            //first statement
            np:=mp+Sphere.v(p,tfByteSize);
            p:=Sphere.r(p,tfFirstStatement);
           end;
         end
        else
         begin
          //assert p=cb
          q:=Sphere.r(p1,tfNext);
          if q<>0 then
           begin
            //next statement
            Push(p,q,p2,mp);
            np:=mp+Sphere.v(p,tfByteSize);
            p:=q;
            vt:=0;
           end
          else
           begin
            //code block done
            if Sphere.r(p,tfEvaluatesTo)=0 then
              vt:=0
            else
              vt0:=0;//silence unused error
            //check any deferred
            q:=Sphere.r(p,tfFirstStatement);
            while (q<>0) and (Sphere.t(q)<>ttDeferred) do
              q:=Sphere.r(q,tfNext);
            if q<>0 then
             begin
              Push(0,0,vt,vp);//keep result value
              vt:=0;
              Push(p,IndexStep1,0,mp);
              Push(q,q,p2,mp);
              np:=mp+Sphere.v(p,tfByteSize);
              p:=Sphere.r(q,tfTarget);
             end;
           end;
         end;
       end;

      ttDeferred://when p1=0: don't run now, see ttThrow,ttCodeBlock
        if p1<>0 then
         begin
          //next deferred command?
          np:=mp;
          q:=Sphere.r(p1,tfNext);
          while (q<>0) and (Sphere.t(q)<>ttDeferred) do
            q:=Sphere.r(q,tfNext);
          if q<>0 then
           begin
            Push(q,q,p2,mp);
            p:=Sphere.r(q,tfTarget);
            vt:=0;
           end
          else
           begin
            //done
            Pop(q,q,q,xp);//pop pushed ttCodeBlock for base pointer
            Pop(q,r,vt,vp);//restore result value
           end;
         end;

      ttCatch://when p1=0: don't run now, see ttThrow
        ;//if p1<>0 then p:=p.Target;//see ttThrow

      ttSysCall:
        try
          PerformSysCall(Sphere,p,mp);
        except
          on e:Exception do //TODO: in-lang throw
            Sphere.Error(pe,'['+e.ClassName+']'+e.Message);
        end;

      //ttMember?
      ttOverload:
       begin
        //address of
        vtp(Sphere.Add(ttPointer,
          [tfByteSize,SystemWordSize
          ,tfEvaluatesTo,Sphere.r(p,tfSignature)//ttSignature
          ]),np);
        Move(p,FMem[np],SystemWordSize);
        inc(np,SystemWordSize);
       end;

      ttVar://vp:=Addr(p);
       begin
        if p1=0 then vtp(Sphere.r(p,tfEvaluatesTo),0);
        q:=p;
        p:=0;
        while (q<>0) and (p=0) do
         begin
          case Sphere.t(q) of

            //ttRecord://TODO: dereference pointer

            ttVar:
              inc(vp,Sphere.v(q,tfOffset));

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

            ttThis:
             begin
              //assert Sphere[qx.Parent].ThingType=ttCodeBlock
              i:=stackIndex;
              while (i<>0) and (stack[i-1].p<>Sphere.r(q,tfParent)) do dec(i);
              if i=0 then
                Sphere.Error(pe,'this unknown')
              else
               begin
                mp:=stack[i-1].bp+Sphere.v(q,tfOffset);
                xp:=0;
                Move(FMem[mp],xp,SystemWordSize);
                inc(vp,xp);
               end;
              q:=0;
             end;

            else
              Sphere.Error(pe,'invalid relativity chain');
          end;
          if q<>0 then q:=Sphere.r(q,tfParent);
         end;
        //TODO: on error throw access violation!
        //TODO: detect out of memory!!!
       end;

      ttThis:
       begin
        //assert Sphere[px.Parent].ThingType=ttCodeBlock
        i:=stackIndex;
        while (i<>0) and (stack[i-1].p<>Sphere.r(p,tfParent)) do dec(i);
        if i=0 then
          Sphere.Error(pe,'this unknown')
        else
          vtp(Sphere.r(p,tfEvaluatesTo),stack[i-1].bp+Sphere.v(p,tfOffset));
       end;

      ttField:
        if p1=0 then
         begin
          Push(p,IndexStep1,0,mp);
          p:=Sphere.r(p,tfSubject);
         end
        else
        if p1=IndexStep1 then
          if vt=0 then
            Sphere.Error(pe,'ttField could not determine subject')
          else
           begin
            q:=Sphere.r(p,tfTarget);
            case Sphere.t(q) of
              ttVar,ttVarByRef,ttThis: //assert qx.Target=vt
               begin
                //auto-dereference
                case Sphere.t(vt) of
                  ttClass:
                    Move(FMem[vp],vp,SystemWordSize);
                  //more?
                end;
                vtp(Sphere.r(q,tfEvaluatesTo),vp+Sphere.v(q,tfOffset));
               end;
              //more?
              else Sphere.Error(pe,'unexpected ttField subject '+
                IntToHex(Sphere.t(q),4));
            end;
           end;

      ttArrayIndex:
        if p1=0 then
         begin
          Push(p,IndexStep1,0,np);
          p:=Sphere.r(p,tfTarget);
         end
        else
        if p1=IndexStep1 then
         begin
          Push(p,IndexStep2,vt,vp);
          p:=Sphere.rr(p,[tfFirstArgument,tfTarget]);//ttArgument
          vt0:=0;
         end
        else
        //if p1=IndexStep2
         begin
          if vt<>TypeDecl_number then
            Sphere.Error(pe,'invalid array index value type')
          else
           begin
            Move(FMem[vp],i,SystemWordSize);
            q:=Sphere.r(p,tfEvaluatesTo);
            vtp(q,mp+ByteSize(Sphere,q)*i);
           end;
         end;

      ttConstant:
       begin
        q:=Sphere.r(p,tfEvaluatesTo);
        vtp(q,np);
        inc(np,ByteSize(Sphere,vt));
        LiteralToMemory(Sphere,Sphere.r(p,tfInitialValue),q,vp);
       end;
      ttLiteral:
       begin
        q:=Sphere.r(p,tfEvaluatesTo);
        vtp(q,np);
        inc(np,ByteSize(Sphere,vt));
        LiteralToMemory(Sphere,p,q,vp);
       end;

      ttUnaryOp:
        if Sphere.r(p,tfRight)=0 then
          Sphere.Error(pe,'unary operator without subject')
        else
        if p1=0 then
         begin
          //if "?@@" in constructor then use class type from original call
          if (TStratoToken(Sphere.v(p,tfOperator)) in
            [stOpSizeOf,stQuestionMark])//stOpTypeOf
            and (Sphere.t(Sphere.r(p,tfRight))=ttThis) then
           begin
            //assert Sphere.t(p.Parent)=ttCodeBlock
            i:=stackIndex;
            while (i<>0) and (stack[i-1].p<>Sphere.r(p,tfParent)) do dec(i);
            if i=0 then q:=0 else
             begin
              q:=stack[i-1].p;
              while (q<>0) and (Sphere.t(q)=ttCodeBlock) do
                q:=Sphere.r(q,tfParent);
              if Sphere.t(q)=ttConstructor then
               begin
                //original constructor class type stored right after 'this'
                //see also StratoFnAddOverload
                Move(FMem[stack[i-1].bp+SystemWordSize],r,SystemWordSize);
                //assert Sphere[i].ThingType=ttClass
                if TStratoToken(Sphere.v(p,tfOperator))=stOpSizeOf then
                 begin
                  if r=0 then
                   begin
                    Sphere.Error(pe,'SizeOf object to construct on empty reference');
                    i:=0;//?throw?
                   end
                  else
                    if Sphere.t(r)=ttClass then //here? switch to ByteSize?
                      i:=Sphere.v(r,tfByteSize)
                    else
                      i:=ByteSize(Sphere,r);
                  vtp(TypeDecl_number,np);
                  inc(np,SystemWordSize);
                  Move(i,FMem[vp],SystemWordSize);
                 end
                else //stQuestionMark://stOpTypeOf:
                 begin
                  vtp(TypeDecl_type,np);
                  inc(np,SystemWordSize);
                  Move(r,FMem[vp],SystemWordSize);
                 end;
               end
              else q:=0;
             end;
           end
          else q:=0;
          if q=0 then //nope, evaluate 'right'
           begin
            Push(p,p,0,np);
            p:=Sphere.r(p,tfRight);
            vt:=0;
           end;
         end
        else
        if vt=0 then
          Sphere.Error(pe,'unary operator without value')
        else
          case TStratoToken(Sphere.v(p,tfOperator)) of
            stOpSub,stOpInc,stOpDec:
              if (vt>=TypeDecl_number) and (vt<=TypeDecl_intLast) then
               begin
                i:=ByteSize(Sphere,vt);
                ii:=0;
                Move(FMem[vp],ii,i);
                case TStratoToken(Sphere.v(p,tfOperator)) of
                  stOpSub:ii:=-ii;
                  stOpInc:inc(ii);
                  stOpDec:dec(ii);
                end;
                Move(ii,FMem[vp],i);
                inc(np,SystemWordSize);
                vt0:=0;//silence unused error
               end
              else
                Sphere.Error(pe,'//TODO: more operator stuff');
            stOpNot,stTilde:
              if vt=TypeDecl_bool then
               begin
                Move(FMem[vp],i,SystemWordSize);
                if i=0 then i:=1 else i:=0;
                Move(i,FMem[vp],SystemWordSize);
                vt0:=0;//silence unused error
               end
              else
                Sphere.Error(pe,'//TODO: more operator stuff');
            stOpSizeOf:
             begin
              if vt=TypeDecl_type then
               begin
                Move(FMem[vp],i,SystemWordSize);
                i:=ByteSize(Sphere,i);
               end
              else
                i:=ByteSize(Sphere,vt);
              vtp(TypeDecl_number,np);
              inc(np,SystemWordSize);
              Move(i,FMem[vp],SystemWordSize);
             end;
            stQuestionMark://stOpTypeOf
              if (vt<>0) and (Sphere.t(vt)=ttClass) then
               begin
                //and TypeDecl_object<>0?
                Move(FMem[vp],r,SystemWordSize);
                if r=0 then
                  //hmm, type of null pointer, pass base type?
                  r:=vt
                else
                  //assert _basetype@-SystemWordSize
                  Move(FMem[r-SystemWordSize],r,SystemWordSize);
                q:=Sphere.Add(ttClassRef,
                  [tfParent,Sphere.r(r,tfParent)
                  ,tfByteSize,SystemWordSize
                  ,tfEvaluatesTo,r
                  ]);
                vtp(q,np);
                inc(np,SystemWordSize);
                Move(r,FMem[vp],SystemWordSize);
               end
              else
               begin
                r:=vt;
                vtp(TypeDecl_type,np);
                inc(np,SystemWordSize);
                Move(r,FMem[vp],SystemWordSize);
               end;
            //TODO: more!
            else
              Sphere.Error(pe,'unsupported unary operator');
          end;
      ttBinaryOp:
        if p1=0 then //evaluate left
          if Sphere.r(p,tfLeft)=0 then
            Sphere.Error(pe,'binary operator without left side')
          else
           begin
            Push(p,IndexStep1,0,np);
            p:=Sphere.r(p,tfLeft);
           end
        else
        if p1=IndexStep1 then //evaluate right
          if vt=0 then
            Sphere.Error(pe,'binary operator left side without value')
          else
            if Sphere.r(p,tfRight)=0 then
              Sphere.Error(pe,'binary operator without right side')
            else
             begin
              Push(0,0,vt,vp);//store result left on stack
              vt:=0;
              Push(p,IndexStep2,0,mp);
              p:=Sphere.r(p,tfRight);
             end
        else
        //if p1=IndexStep2 then //perform operator
          if vt=0 then
            Sphere.Error(pe,'binary operator right side without value')
          else
           begin
            Pop(p1,p2,q,xp);//left: q@xp right: vt@vp
            np:=mp;
            vt0:=0;//drop value
            //assert SameType(q,vt
            case TStratoToken(Sphere.v(p,tfOperator)) of
              stOpEQ:
               begin
                i:=0;
                if SameType(Sphere,vt,q) then
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
                if (q>=TypeDecl_number) and (q<=TypeDecl_intLast) and (q=vt) then
                 begin
                  i:=ByteSize(Sphere,vt);
                  ii:=0;
                  Move(FMem[xp],ii,i);
                  jj:=0;
                  Move(FMem[vp],jj,i);
                  ii:=ii+jj;
                  vtp(q,np);
                  inc(np,i);
                  Move(ii,FMem[vp],i);
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
              stOpSub,stOpMul,stOpDiv,stOpMod,stOpShl,stOpShr,stThreeLT,stThreeGT:
               begin
                if (q>=TypeDecl_number) and (q<=TypeDecl_intLast) and (q=vt) then
                 begin
                  i:=ByteSize(Sphere,vt);
                  ii:=0;
                  Move(FMem[xp],ii,i);
                  jj:=0;
                  Move(FMem[vp],jj,i);
                  case TStratoToken(Sphere.v(p,tfOperator)) of
                    //stOpAdd:k:=i+j;//see above
                    stOpSub:ii:=ii-jj;
                    stOpMul:ii:=ii*jj;
                    stOpDiv:ii:=ii div jj;
                    stOpMod:ii:=ii mod jj;
                    //stOpInc:
                    //stOpDec:
                    stOpShl:ii:=ii shl jj;
                    stOpShr:ii:=ii shr jj;
                    stThreeLT:ii:=(ii shl jj) or (ii shr (i*8-j));//roll left
                    stThreeGT:ii:=(ii shr jj) or (ii shl (i*8-j));//roll right
                  end;
                  vtp(q,np);
                  inc(np,i);
                  Move(ii,FMem[vp],i);
                 end
                else
                  Sphere.Error(pe,'//TODO: more operator stuff');
               end;
              stOpNEQ,stOpLT,stOpLTE,stOpGT,stOpGTE:
               begin
                if (q>=TypeDecl_number) and (q<=TypeDecl_intLast) and (q=vt) then
                 begin
                  i:=ByteSize(Sphere,vt);
                  ii:=0;
                  Move(FMem[xp],ii,i);
                  jj:=0;
                  Move(FMem[vp],jj,i);
                  j:=0;
                  case TStratoToken(Sphere.v(p,tfOperator)) of
                    stOpNEQ:if ii<>jj then j:=1;
                    stOpLT: if ii< jj then j:=1;
                    stOpLTE:if ii<=jj then j:=1;
                    stOpGT: if ii> jj then j:=1;
                    stOpGTE:if ii>=jj then j:=1;
                  end;
                  vtp(TypeDecl_bool,np);
                  inc(np,SystemWordSize);
                  Move(j,FMem[vp],SystemWordSize);
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
                  case TStratoToken(Sphere.v(p,tfOperator)) of
                    stOpAnd:if (i<>0) and (j<>0) then k:=1 else k:=0;
                    stOpOr: if (i<>0) or  (j<>0) then k:=1 else k:=0;
                    stOpXor:if (i<>0) xor (j<>0) then k:=1 else k:=0;
                  end;
                  vtp(TypeDecl_bool,np);
                  inc(np,SystemWordSize);
                  Move(k,FMem[vp],SystemWordSize);
                 end;
              stOpTypeIs:
               begin
                if (Sphere.t(q)=ttClass) and (xp<>0) then
                 begin
                  Move(FMem[xp],i,SystemWordSize);//dereference
                  //assert object._baseclass @-SystemWordSize
                  Move(FMem[i-SystemWordSize],q,SystemWordSize);
                 end;
                if (Sphere.t(vt)=ttClassRef) and (vp<>0) then
                  Move(FMem[vp],vt,SystemWordSize);
                if SameType(Sphere,vt,q) then i:=1 else i:=0;
                vtp(TypeDecl_bool,np);
                inc(np,SystemWordSize);
                Move(i,FMem[vp],SystemWordSize);
               end;
              //TODO: more!
              else
                Sphere.Error(pe,'unsupported binary operator');
            end;
           end;
      ttAssign:
        if p1=0 then //evaluate AssignTo
         begin
          //see also stratoLogic IsAssignable
          q:=Sphere.r(p,tfAssignTo);
          if Sphere.r(p,tfValueFrom)=0 then
            Sphere.Error(pe,'assignment without right side')
          else if q=0 then
            Sphere.Error(pe,'assignment without left side')
          else if Sphere.t(q)=ttCast then
           begin
            //'dirty' cast into
            Push(p,Sphere.r(p,tfValueFrom),Sphere.r(q,tfEvaluatesTo),np);
            p:=Sphere.r(q,tfTarget);
           end
          else
           begin
            Push(p,Sphere.r(p,tfValueFrom),0,np);
            p:=Sphere.r(p,tfAssignTo);
           end;
         end
        else
        if p1=Sphere.r(p,tfValueFrom) then //evaluate ValueFrom
          if vt=0 then
            Sphere.Error(pe,'assignment right side without value')
          else
           begin
            if p2<>0 then //AssignTo is ttCast
             begin
              //TODO: check types? zero when larger?
              vt:=p2;
             end;
            Push(0,0,vt,vp);//store address
            vt:=0;
            Push(p,IndexStep1,0,mp);
            p:=Sphere.r(p,tfValueFrom);
           end
        else
        //if p1=IndexStep1
          if vt=0 then
            Sphere.Error(pe,'assignment left side without address')
          else
           begin
            Pop(p1,p2,q,xp);
            if not SameType(Sphere,vt,q) then
              Sphere.Error(pe,'assignment type mismatch')
            else
              case TStratoToken(Sphere.v(p,tfOperator)) of
                stOpAssign:
                  Move(FMem[vp],FMem[xp],ByteSize(Sphere,vt));
                stOpAssignAdd:
                  if (vt>=TypeDecl_number) and (vt<=TypeDecl_intLast) then
                   begin
                    i:=ByteSize(Sphere,vt);
                    ii:=0;
                    Move(FMem[xp],ii,i);
                    jj:=0;
                    Move(FMem[vp],jj,i);
                    ii:=ii+jj;
                    Move(ii,FMem[xp],i);
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
                stOpAssignOr,  //"||="
                stOpAssignAnd: //"$$="
                  if vt=TypeDecl_number then
                   begin
                    i:=ByteSize(Sphere,vt);
                    ii:=0;
                    Move(FMem[xp],ii,i);
                    jj:=0;
                    Move(FMem[vp],jj,i);
                    case TStratoToken(Sphere.v(p,tfOperator)) of
                      stOpAssignSub:ii:=ii-jj;
                      stOpAssignMul:ii:=ii*jj;
                      stOpAssignDiv:ii:=ii div jj;
                      stOpAssignMod:ii:=ii mod jj;
                      stOpAssignOr: ii:=ii or  jj;
                      stOpAssignAnd:ii:=ii and jj;
                    end;
                    Move(ii,FMem[xp],i);
                   end
                  else
                    Sphere.Error(pe,'//TODO: more operator stuff');
                else
                  Sphere.Error(pe,'//TODO: assignment operators');
              end;
            vt:=0;//vtp(q,xp);
            np:=mp;
           end;
      ttSelection:
       begin
        q:=Sphere.r(p,tfDoIf);
        if (p1=0) and (q<>0) then
         begin
          Push(p,q,0,np);
          p:=q;
         end
        else
         begin
          if q=0 then
            p:=Sphere.r(p,tfDoThen)
          else
           begin
            //assert vt=0 or vt=TypeDecl_bool
            //TODO: reference var check null
            if vt=TypeDecl_bool then
             begin
              Move(FMem[vp],i,SystemWordSize);
              if i<>0 then p:=Sphere.r(p,tfDoThen) else p:=Sphere.r(p,tfDoElse);
             end
            else
              Sphere.Error(pe,'selection criterium value is not boolean');
            vt:=0;
            np:=mp;
           end;
         end;
       end;
      ttIteration:
        if (p1=0) and Sphere.r(p,tfDoFirst,q) then
         begin
          Push(p,p,0,np);
          p:=q;
         end
        else
         begin
          if p1<>0 then np:=mp;
          //evaluate DoIf
          if Sphere.r(p,tfDoIf,q) then
           begin
            i:=0;//Default
            if p1=q then
             begin
              if vt=TypeDecl_bool then
                Move(FMem[vp],i,SystemWordSize)
              else
                Sphere.Error(pe,'iteration criterium value is not boolean');
             end
            else
             begin
              Push(p,q,0,np);
              p:=q;
             end;
           end
          else
            i:=1;
          //perform Body,DoThen
          if (p=p0) and (i<>0) then
           begin
            r:=Sphere.r(p,tfBody);
            q:=Sphere.r(p,tfDoThen);
            if (r<>0) or (q<>0) then
             begin
              Push(p,p,0,np);
              p:=r;
              if q<>0 then Push(q,0,0,np);
             end;
           end;
          vt:=0;
         end;
      ttIterationPE:
        if (p1=0) and Sphere.r(p,tfDoFirst,q) then
         begin
          Push(p,p,0,np);
          p:=q;
         end
        else
        if Sphere.r(p,tfBody,q) and ((p1=0) or (p1=p)) then
         begin
          Push(p,q,0,np);
          p:=q;
          vt:=0;//silence DoFirst/DoThen leftover
         end
        else
         begin
          if p1<>0 then np:=mp;
          //evaluate DoIf
          if Sphere.r(p,tfDoIf,q) then
           begin
            i:=0;//default
            if p1=q then
             begin
              if vt=TypeDecl_bool then
                Move(FMem[vp],i,SystemWordSize)
              else
                Sphere.Error(pe,'iteration criterium value is not boolean');
             end
            else
             begin
              Push(p,q,0,np);
              p:=q;
             end;
           end
          else
            i:=1;
          //perform Body,DoThen
          if (p=p0) and (i<>0) and
            (Sphere.r(p,tfBody,r) or Sphere.r(p,tfDoThen,q)) then
           begin
            Push(p,0,0,mp);
            p:=q;
           end;
          vt:=0;
         end;
      ttCast:
        if p1=0 then
          if Sphere.r(p,tfTarget,q) then
           begin
            Push(p,q,0,np);
            p:=q;
           end
          else
            Sphere.Error(pe,'cast without subject')
        else
          if vt=0 then
            Sphere.Error(pe,'no value to cast')
          else
           begin
            //TODO:
            //see also SameType
            q:=Sphere.r(p,tfEvaluatesTo);
            if (vt=TypeDecl_string) and (q=TypeDecl_number) then
             begin
              Move(FMem[vp],i,SystemWordSize);
              if not TryStrToInt(string(Sphere.GetBinaryData(i)),integer(j)) then
                Sphere.Error(pe,'invalid integer value');//TODO: raise
              vtp(TypeDecl_number,np);
              inc(np,SystemWordSize);
              Move(j,FMem[vp],SystemWordSize);
             end
            else
            if (vt=TypeDecl_number) and (q=TypeDecl_string) then
             begin
              Move(FMem[vp],i,SystemWordSize);
              j:=Sphere.AddBinaryData(UTF8String(IntToStr(i)));
              vtp(TypeDecl_string,np);
              inc(np,SystemWordSize);
              Move(j,FMem[vp],SystemWordSize);
             end
            else
            if (vt=TypeDecl_bool) and (q=TypeDecl_string) then
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
            if (vt=TypeDecl_number) and (Sphere.t(q)=ttEnumeration) then
             begin
              Move(FMem[vp],i,SystemWordSize);
              //check enumeration item?
              vtp(q,np);
              inc(np,SystemWordSize);
              Move(i,FMem[vp],SystemWordSize);
             end
            else
            if (Sphere.t(vt)=ttEnumeration) and (q=TypeDecl_number) then
             begin
              Move(FMem[vp],i,SystemWordSize);
              vtp(q,np);
              inc(np,SystemWordSize);
              Move(i,FMem[vp],SystemWordSize);
             end
            else
            if (vt=TypeDecl_number) and (Sphere.t(q)=TypeDecl_pointer) then
             begin
              Move(FMem[vp],i,SystemWordSize);
              //if i<>0 then //TODO: protect against pointer arith
              vtp(TypeDecl_pointer,np);
              inc(np,SystemWordSize);
              Move(i,FMem[vp],SystemWordSize);
             end
            else
            if (TypeDecl_object<>0) and
              (Sphere.t(vt)=ttClass) and (Sphere.t(q)=ttClass) then
             begin
              //cast to base class?
              if vp=0 then r:=vt else
               begin
                //dereference
                Move(FMem[vp],i,SystemWordSize);
                //assert object._baseclass @-SystemWordSize
                Move(FMem[i-SystemWordSize],r,SystemWordSize);
               end;
              while (r<>0) and (r<>q) do r:=Sphere.r(r,tfInheritsFrom);
              if r<>0 then
               begin
                //TODO: check @@._baseclass!
                vtp(q,vp);
               end
              else
                //TODO: throw
                Sphere.Error(pe,'instance not of this or inherited class');
             end

            //TODO: char
            else
            if (Sphere.v(vt,tfByteSize) in [1..8]) //TODO: and numeric?
              and (q=TypeDecl_string) then
             begin
              ii:=0;
              Move(FMem[vp],ii,Sphere.v(vt,tfByteSize));
              j:=Sphere.AddBinaryData(UTF8String(IntToStr(ii)));
              vtp(TypeDecl_string,np);
              inc(np,SystemWordSize);
              Move(j,FMem[vp],SystemWordSize);
             end
            else
            if (Sphere.v(vt,tfByteSize) in [1..8]) //TODO: and numeric?
              and (Sphere.v(q,tfByteSize) in [1..8]) then //and numeric
             begin
              ii:=0;
              Move(FMem[vp],ii,Sphere.v(vt,tfByteSize));
              vtp(q,np);
              inc(np,Sphere.v(q,tfByteSize));
              Move(ii,FMem[vp],Sphere.v(q,tfByteSize));
             end

            else
              Sphere.Error(pe,'unsupported cast');//TODO: display type
           end;

      ttThrow:
        if not Sphere.r(p,tfTarget,q) then //break
         begin
          BreakLoop;
          vt:=0;
         end
        else
        if (q=0) or (p=p1) then //throw
         begin
          Throw(np,vt,vp);
          vt:=0;
         end
        else //get object to throw
         begin
          Push(p,p,0,np);
          p:=q;
         end;

      ttAddressOf:
        if p1=0 then
         begin
          q:=Sphere.r(p,tfValueFrom);
          if (Sphere.t(q)=ttThis) and
            (Sphere.r(q,tfParent)=Sphere.r(p,tfParent)) then
           begin
            //TODO: move with ttThis above?
            //assert Sphere.t(q.Parent)=ttCodeBlock
            //assert ttThis is first item and Offset=0
            i:=stackIndex;
            while (i<>0) and (stack[i-1].p<>Sphere.r(p,tfParent)) do dec(i);
            if i=0 then
              Sphere.Error(pe,'this unknown')
            else
              vtp(Sphere.r(p,tfEvaluatesTo),stack[i-1].bp);
           end
          else
           begin
            Push(p,p,0,np);
            p:=Sphere.r(p,tfValueFrom);
           end
         end
        else
         begin
          i:=vp;
          vtp(Sphere.r(p,tfEvaluatesTo),np);
          Move(i,FMem[np],SystemWordSize);
          inc(np,SystemWordSize);
         end;
      ttDereference:
        if p1=0 then
         begin
          Push(p,p,0,np);
          p:=Sphere.r(p,tfValueFrom);
         end
        else
         begin
          vtp(Sphere.r(p,tfEvaluatesTo),vp);
          Move(FMem[vp],vp,SystemWordSize);
          //TODO: access violation? bounds check?
         end;
      ttVarByRef:
       begin
        q:=Sphere.r(p,tfParent);//assert code block
        vp:=Sphere.v(p,tfOffset);
        i:=stackIndex;
        while (i<>0) and (stack[i-1].p<>q) do dec(i);
        if i=0 then
          Sphere.Error(pe,'var-by-ref relative to code block not currently in execution')
        else
          inc(vp,stack[i-1].bp);
        Move(FMem[vp],i,SystemWordSize);
        vtp(Sphere.r(p,tfEvaluatesTo),i);
       end;

      ttClass:
       begin
        Move(p,FMem[np],SystemWordSize);
        vtp(TypeDecl_type,np);
        inc(np,SystemWordSize);
       end;

      ttPropCall://TODO: un-copy from ttFnCall?
        if not Sphere.r(p,tfTarget,q) then
          Sphere.Error(pe,'call without function overload')
        else
         begin
          //step 0: determine call target
          if p1=0 then
           begin
            r:=0;
            p2:=0;
            mp:=np;
            case Sphere.t(q) of
              ttPropertyGet,ttPropertySet:
                r:=Sphere.r(q,tfBody);
              ttVar,ttArrayIndex:
               begin
                Push(p,IndexStep1,0,mp);
                p:=Sphere.r(p,tfTarget);
               end;
              ttField:
               begin
                Push(p,IndexStep1,0,mp);
                p:=Sphere.r(q,tfSubject);
               end;
              //TODO: ttInterface? ttClass?
              else
                Sphere.Error(pe,'Unexpected call target');
            end;
           end
          else
            r:=p2;
          //step 1: target evaluated (see above: ttVar,ttField) store ttThis
          if p1=IndexStep1 then
           begin
            q:=0;
            case Sphere.t(q) of
              ttVar,ttArrayIndex:
                if vt=0 then
                  Sphere.Error(pe,'unable to resolve call target variable')
                else
                  case Sphere.t(vt) of
                    ttClassRef:
                     begin
                      Move(FMem[vp],p2,SystemWordSize);
                      q:=StratoFnCallFindVirtual(Sphere,p2,Sphere.r(p,tfTarget));
                     end;
                    else
                      Sphere.Error(pe,'unexpected call target variable type');
                  end;
              ttField:
               begin
                if (vp=0) or (vt=0) then
                  Sphere.Error(pe,'unable to resolve call target')
                else
                  case Sphere.t(vt) of
                    ttClass:
                     begin
                      Move(FMem[vp],vp,SystemWordSize);//dereference first
                      //assert object._baseclass @-SystemWordSize
                      Move(FMem[vp-SystemWordSize],p2,SystemWordSize);
                     end;
                    ttClassRef:
                      Move(FMem[vp],p2,SystemWordSize);
                    else
                      p2:=vt;//more checks? error?
                  end;
                q:=StratoFnCallFindVirtual(Sphere,p2,Sphere.r(q,tfTarget));//!
               end;
              else
                raise Exception.Create('//TODO');
            end;
            if q=0 then r:=0 else r:=Sphere.r(q,tfBody);
            if r=0 then
              Sphere.Error(p,'dynamic implementation not found')
            else
              case Sphere.t(q) of
                ttPropertyGet,ttPropertySet:
                 begin
                  //get address for 'this' ("@@")
                  q:=Sphere.r(r,tfFirstItem);
                  while (q<>0) and (Sphere.t(q)<>ttThis) do
                    q:=Sphere.r(q,tfNext);
                  if q=0 then
                    Sphere.Error(pe,'Could not find "@@"')
                  else
                    //TODO: SameType? vt px.Target.Target
                    //not FMem[vp] here! see ttThis under ttVar below
                    Move(vp,FMem[mp+Sphere.v(q,tfOffset)],SystemWordSize);
                 end;
                else
                  Sphere.Error(pe,'unexpected virtual call target');
              end;
            vt:=0;
           end;
          //found a body, or enumerating arguments?
          if r<>0 then
           begin
            //step 2: evaluate arguments
            if p1<>p then
             begin
              //step 2a: start evaluating arguments
              if (Sphere.t(r)=ttCodeBlock) and (p1<>IndexStep2) then
               begin
                p1:=Sphere.r(p,tfFirstArgument);
                p2:=Sphere.rr(r,[tfParent,tfFirstArgument]);
                if (p1<>0) and (p2=0) then
                  Sphere.Error(pe,'overload without argument values in code block')
                else
                  if p1<>0 then
                   begin
                    //push function's code block (to address variables)
                    Push(r,0,0,np);
                    //start getting the first argument
                    //TODO: //if qx.ThingType<>ttArgByRef then
                    q:=Sphere.r(p1,tfInitialValue);
                    if q<>0 then LiteralToMemory(Sphere,q,
                      Sphere.r(p1,tfEvaluatesTo),np+Sphere.v(p2,tfOffset));
                    Push(p,p1,p2,np);
                    inc(np,Sphere.v(r,tfByteSize));
                    p:=Sphere.r(p1,tfTarget);
                   end;
               end
              else
              //step 2b: store argument value, next argument?
              if p1<>IndexStep2 then
               begin
                //store argument value
                if vt=0 then
                  Sphere.Error(pe,'no value for argument "'+
                    string(Sphere.Dict.Str[Sphere.v(p1,tfName)])+'"')
                else
                 begin
                  if Sphere.t(p2)=ttVarByRef then
                    Move(vp,FMem[mp+Sphere.v(p2,tfOffset)],SystemWordSize)
                  else
                   begin
                    //assert p2.ThingType=ttArgument
                    i:=ByteSize(Sphere,vt);
                    if i<>0 then Move(FMem[vp],FMem[mp+Sphere.v(p2,tfOffset)],i);
                   end;
                  vt:=0;
                 end;
                //next argument
                p1:=Sphere.r(p1,tfNext);
                p2:=Sphere.r(p2,tfNext);//assert sequence of arguments correctly added as vars to codeblock
                np:=mp;
                if p1=0 then
                 begin
                  Pop(r,p1,p2,xp);
                  //calling property setter: evaluate value to set
                  if Sphere.t(Sphere.r(r,tfParent))=ttPropertySet then
                   begin
                    Push(p,IndexStep2,r,mp);
                    np:=mp+Sphere.v(r,tfByteSize);
                    p:=Sphere.r(p,tfEvaluatesTo);
                   end;
                 end
                else
                 begin
                  q:=Sphere.r(p1,tfInitialValue);
                  if q<>0 then LiteralToMemory(Sphere,
                    q,Sphere.r(p1,tfEvaluatesTo),mp+Sphere.v(p2,tfOffset));
                  Push(p,p1,p2,mp);
                  p:=Sphere.r(p1,tfTarget);
                 end;
               end
              //step 2c: calling property setter? store value to set
              else //p1=IndexStep2
               begin
                if vt=0 then
                  Sphere.Error(pe,'no value for property setter "'+
                    string(Sphere.Dict.Str[Sphere.v(p,tfName)])+'"')
                else
                 begin
                  q:=Sphere.Lookup(r,tfFirstItem,
                    Sphere.v(Sphere.rr(r,[tfParent,tfParent]),tfName));//ttMember
                  Move(FMem[vp],FMem[mp+Sphere.v(q,tfOffset)],
                    ByteSize(Sphere,vt));
                 end;
                np:=mp;
                vt:=0;
               end;
              //step 3: jump to target body
              if (p1<>p) and (p=p0) then
               begin
                //push self (see ttThrow with Target=0 for break/abort)
                Push(p,p,r,np);
                p:=r;//ttCodeBlock
               end;
             end
            else
            //step 4: output result value
            if Sphere.t(Sphere.r(r,tfParent))=ttPropertyGet then
             begin
              //assert px.EvaluatesTo<>0
              //assert first value in code block is return value (after this)
              q:=Sphere.r(r,tfFirstItem);
              if Sphere.t(q)=ttThis then
                q:=Sphere.r(q,tfNext);//assert Sphere[px.Target].ThingType=ttField
              vtp(Sphere.r(q,tfEvaluatesTo),mp+Sphere.v(q,tfOffset));
             end;
           end;
         end;

      //TODO: more

      else
       begin
        Sphere.Error(pe,Format('unknown logic item %d:%.4x',[p,Sphere.t(p)]));
        p:=0;
       end;
    end;
    //checks
    if p=p0 then
      p:=0//nothing new? force pop
    else
      p1:=0;//else clear 'indexer' p1 //p2:=0;?
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
  p,q:TStratoIndex;addr:cardinal);
var
  i:int64;
  r:TStratoIndex;
begin
  if Sphere.t(p)<>ttLiteral then
    Sphere.Error(p,'literal expected '+IntToHex(Sphere.t(p),4))
  else
   begin
    r:=Sphere.r(p,tfEvaluatesTo);
    //assert q=0 or Sphere[q].ThingType=ttTypeDecl
    if (r>=TypeDecl_number) and (r<=TypeDecl_intLast) then
     begin
      i:=ParseInteger(string(Sphere.GetBinaryData(Sphere.r(p,tfInitialValue))));
      Move(i,FMem[addr],Sphere.v(q,tfByteSize));
     end
    else
    if r=TypeDecl_string then
     begin
      //TODO: store strings in conceptual mem (see also stratoRunTime)
      i:=Sphere.r(p,tfInitialValue);//ttBinaryData
      Move(i,FMem[addr],SystemWordSize);
     end
    else
    if r=TypeDecl_bool then
     begin
      if Sphere.GetBinaryData(Sphere.r(p,tfInitialValue))='0' then
        i:=0
      else
        i:=1;
      Move(i,FMem[addr],SystemWordSize);
     end
    else
      Sphere.Error(p,'unsupported literal type');
   end;
end;

procedure TStratoMachine.PerformSysCall(Sphere: TStratoSphere;
  Fn: TStratoIndex; Ptr: cardinal);
var
  i,j,k:cardinal;
begin
  case Sphere.v(Fn,tfOperator) of

    stratoSysCall_xinc,stratoSysCall_xdec:
     begin
      Move(FMem[Ptr+SystemWordSize],i,SystemWordSize);
      Move(FMem[i],j,SystemWordSize);
      case Sphere.v(Fn,tfOperator) of
        stratoSysCall_xinc:k:=cardinal(InterlockedIncrement(integer(j)));
        stratoSysCall_xdec:k:=cardinal(InterlockedDecrement(integer(j)));
      end;
      Move(j,FMem[i],SystemWordSize);
      Move(k,FMem[Ptr],SystemWordSize);
     end;

    stratoSysCall_writeln:
     begin
      //assert chain is there: ttFnCall>ttArgument>ttVar with string data
      Move(FMem[Ptr],i,SystemWordSize);
      Writeln(Sphere.GetBinaryData(i));
     end;
    stratoSysCall_malloc:
     begin
      Move(FMem[Ptr+SystemWordSize],i,SystemWordSize);
      j:=FMemAllocIndex;
      inc(FMemAllocIndex,i);
      //alignment?
      if (FMemAllocIndex mod SystemWordSize)<>0 then
        inc(FMemAllocIndex,SystemWordSize-(FMemAllocIndex mod SystemWordSize));
      while FMemAllocIndex>FMemSize do
       begin
        inc(FMemSize,InitialMemSize);//grow
        SetLength(FMem,FMemSize);
        //TODO: out of memory?
       end;
      //TODO: register mem as in use
      Move(j,FMem[Ptr],SystemWordSize);
     end;

    //TODO:
    //stratoSysCall_realloc:
    //stratoSysCall_mfree:

    else
      Sphere.Error(Fn,'unknown system call '+IntToStr(Sphere.v(Fn,tfOperator)));
  end;
end;

end.

