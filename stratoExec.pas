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
  inherited                                     ;
end;

procedure TStratoMachine.Run(Sphere: TStratoSphere);
var
  p:TStratoName;
begin
  if FDebugView<>nil then FDebugView.Show;
  AllocateGlobals(Sphere);
  //TODO: halt on unhandled exception?
  p:=Sphere.Header.FirstInitialization;
  while p<>0 do
   begin
    Perform(Sphere,p);
    p:=Sphere[p].Next;
   end;
  p:=Sphere.Header.FirstFinalization;
  while p<>0 do
   begin
    Perform(Sphere,p);
    p:=Sphere[p].Next;
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
  inc(FMemIndex,Sphere.Header.GlobalByteSize);

  //initialize
  p:=Sphere.Header.FirstGlobalVar;
  while p<>0 do
   begin
    //assert Sphere[p].ThingType=ttGlobal
    q:=Sphere[p].Target;
    //assert Sphere[q].ThingType=ttVar
    r:=Sphere[q].InitialValue;
    if r<>0 then LiteralToMemory(Sphere,r,
      Sphere[q].EvaluatesTo,SphereBasePtr+Sphere[q].Offset);
    p:=Sphere[p].Next;
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
      Push(Sphere[q].Target,0,0,bp);
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
              if (px.DoIf=0) or SameType(Sphere,vt,px.DoIf) then
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
      Push(Sphere[cc].Body,0,0,bp);

      //TODO: proper allocation! since catch var is set post cb.ByteSize, that memory could be in use!!
      //vt:=//TODO: cast to cc.ItemType
      i:=ByteSize(Sphere,vt);
      Move(FMem[vp],FMem[xp+Sphere[Sphere[cc].FirstArgument].Offset],i);

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
        Push(Sphere[q].Target,0,0,bp);
       end;

     end;
  end;

var
  p,q,r,vt,p0,p1,p2,vt0,pe:TStratoIndex;
  px,qx,rx:PStratoThing;
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
    pp:TStratoIndex;
    ppx:PStratoThing;
    pi,ii,jj:cardinal;
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
        li.SubItems.Add(StratoDumpThing(Sphere,p,Sphere[p]));
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
            li.SubItems.Add(StratoDumpThing(Sphere,stack[i].p,Sphere[stack[i].p]));
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
        pi:=BaseMemPtr;
        k:=0;
        ii:=0;
        if Sphere.Header.GlobalByteSize=0 then pp:=0 else
          pp:=Sphere.Header.FirstGlobalVar;
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
              (Sphere[stack[k].p].ThingType=ttCodeBlock) and
              (Sphere[stack[k].p].ByteSize<>0) and
              (stack[k].bp>=i)) do inc(k);
            if (k=stackIndex) or (stack[k].p=0) then
              if (p<>0) and (px.ThingType=ttCodeBlock) then
               begin
                pp:=px.FirstItem;
                pi:=mp;
               end
              else
                pp:=0
            else
             begin
              pp:=Sphere[stack[k].p].FirstItem;
              pi:=stack[k].bp;
             end;
           end;
          if (pp=0) or (pi>i) then
           begin
            li1.SubItems.Add('');
            jj:=4;
           end
          else
           begin
            ppx:=Sphere[pp];
            //TODO: accurately keep pi+.Offset equal to i !!!
            if ppx.ThingType=ttGlobal then
             begin
              li1.SubItems.Add(Format('%d: %s',[pp,
                StratoDumpThing(Sphere,ppx.Target,Sphere[ppx.Target])]));
              jj:=ByteSize(Sphere,Sphere[ppx.Target].EvaluatesTo);
              while (pp<>0) and (pi+Sphere[Sphere[pp].Target].Offset<=i) do
                pp:=Sphere[pp].Next;
             end
            else
             begin
              li1.SubItems.Add(Format('%d:%d: %s',[ppx.Parent,pp,
                StratoDumpThing(Sphere,pp,ppx)]));
              jj:=ByteSize(Sphere,ppx.EvaluatesTo);
              while (pp<>0) and (pi+Sphere[pp].Offset<=i) do
                pp:=Sphere[pp].Next;
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
          [p,StratoDumpThing(Sphere,p,Sphere[p]),mp,np]));
        if p1=0 then
          FDebugView.txtUpNext.Lines.Add('p1')
        else if p1>=IndexStep1 then
          FDebugView.txtUpNext.Lines.Add('p1: '+IntToStr(p1))
        else
          FDebugView.txtUpNext.Lines.Add(Format('p1: %d: %s',
            [p1,StratoDumpThing(Sphere,p1,Sphere[p1])]));
        if p2=0 then
          FDebugView.txtUpNext.Lines.Add('p2')
        else if p2>=IndexStep1 then
          FDebugView.txtUpNext.Lines.Add('p2: '+IntToStr(p2))
        else
          FDebugView.txtUpNext.Lines.Add(Format('p2: %d: %s',
            [p2,StratoDumpThing(Sphere,p2,Sphere[p2])]));
        if vt<>0 then
         begin
          FDebugView.txtUpNext.Lines.Add(Format('vt: %d: %s',
            [vt,StratoDumpThing(Sphere,vt,Sphere[vt])]));
          Move(FMem[vp],j,4);//TODO: ByteSize(Sphere,vt);
          FDebugView.txtUpNext.Lines.Add(Format('vp: @=%d x=%.8x v=%d',[vp,j,j]));
         end;
      finally
        FDebugView.txtUpNext.Lines.EndUpdate;
      end;
      try
        ppx:=Sphere[pe];
        if (pe<>0) and StratoGetSourceFile(Sphere,pe,pp,pi) then
          FDebugView.ShowSource(Sphere,pp,ppx.SrcPos div pi,ppx.SrcPos mod pi)
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
    px:=Sphere[p];
    if px.SrcPos<>0 then pe:=p;//else look up stack?
    //assert q=0 or ((q.ThingType and str__Typed)<>0)

    if RefreshDebugView then
      asm int 3 end;//DebugBreak;//forced breakpoint

    case px.ThingType of

      ttAlias:p:=px.Target;

      ttFnCall:
        if px.Target=0 then
          Sphere.Error(pe,'call without function overload')
        else
         begin
          qx:=Sphere[px.Target];
          //step 0: determine call target
          if p1=0 then
           begin
            r:=0;
            p2:=0;
            mp:=np;
            case qx.ThingType of
              ttOverload:
                r:=qx.Body;
              ttVarIndex:
               begin
                Push(p,IndexStep1,0,mp);
                //if (qx.Target<>0) and (Sphere[qx.Target].ThingType in [ttOverload,ttConstructor] then
                if qx.FirstArgument=0 then
                  p:=qx.Parent
                else
                  p:=px.Target;
               end;
              ttVar:
               begin
                Push(p,IndexStep1,0,mp);
                p:=px.Target;
               end;
              ttConstructor:
               begin
                r:=qx.Body;
                if px.Name<>Name_Inherited then
                 begin
                  //store default nil pointer, base class
                  //see also StratoFnAddOverload
                  i:=0;
                  Move(i,FMem[np],SystemWordSize);
                  p2:=px.EvaluatesTo;//assert ttClass
                  Move(p2,FMem[np+SystemWordSize],SystemWordSize);
                 end
                else
                 begin
                  //constructor calling constructor: copy this and original class
                  //see also StratoFnAddOverload
                  i:=stackIndex;
                  while (i<>0) and (Sphere[stack[i-1].p].ThingType<>ttFnCall) do
                    dec(i);
                  if i=0 then
                    Sphere.Error(pe,'Unable to obtain parent constructor base pointer')//TODO:throw
                  else
                   begin
                    //assert stack[i-1].p].Target.ThingType=ttConstructor
                    Move(FMem[stack[i-1].bp],FMem[np],SystemWordSize*2);
                   end;
                  //assert vp=0
                  p2:=qx.EvaluatesTo;
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
          //step 1: target evaluated (see above: ttVar,ttVarIndex) store ttThis
          if p1=IndexStep1 then
           begin
            q:=0;
            case qx.ThingType of
              ttVarIndex:
               begin
                if (vp=0) or (vt=0) then
                  p2:=vt //this null? assert in constructor
                else
                  case Sphere[vt].ThingType of
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
                if (p2=vt) or (px.Name=Name_Inherited) then
                  q:=qx.Target
                else
                  if TypeDecl_object=0 then
                   begin
                    Sphere.Error(p,'base class for dynamic calls not defined');
                    q:=qx.Target;
                   end
                  else
                    q:=StratoFnCallFindVirtual(Sphere,p2,qx.Target);//!
               end;
              ttVar:
               begin
                q:=0;
                if vt=0 then
                  Sphere.Error(pe,'unable to resolve call target variable')
                else
                  case Sphere[vt].ThingType of
                    ttClassRef:
                     begin
                      Move(FMem[vp],p2,SystemWordSize);
                      q:=StratoFnCallFindVirtual(Sphere,p2,px.Target);
                     end;
                    else
                      Sphere.Error(pe,'unexpected call target variable type');
                  end;
                vt:=0;
               end;
              else
                raise Exception.Create('//TODO');
            end;
            if q=0 then r:=0 else r:=Sphere[q].Body;
            if r=0 then
              Sphere.Error(p,'dynamic implementation not found')
            else
              case Sphere[q].ThingType of
                ttOverload,ttDestructor:
                 begin
                  //get address for 'this' ("@@")
                  q:=Sphere[r].FirstItem;
                  while (q<>0) and (Sphere[q].ThingType<>ttThis) do
                    q:=Sphere[q].Next;
                  if q=0 then
                    Sphere.Error(pe,'Could not find "@@"')
                  else
                    //TODO: SameType? vt px.Target.Target
                    //not FMem[vp] here! see ttThis under ttVar below
                    Move(vp,FMem[mp+Sphere[q].Offset],SystemWordSize);
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
            vt:=0;
           end;
          //found a body, or enumerating arguments?
          if r<>0 then
           begin
            rx:=Sphere[r];
            //step 2: evaluate arguments
            if p1<>p then
             begin
              //step 2a: start evaluating arguments
              if rx.ThingType=ttCodeBlock then
               begin
                p1:=px.FirstArgument;
                p2:=Sphere[rx.Parent].FirstArgument;
                if (p1<>0) and (p2=0) then
                  Sphere.Error(pe,'overload without argument values in code block')
                else
                  if p1<>0 then
                   begin
                    //push function's code block (to address variables)
                    Push(r,0,0,np);
                    //start getting the first argument
                    qx:=Sphere[p1];
                    //TODO: //if qx.ThingType<>ttArgByRef then
                    if qx.InitialValue<>0 then LiteralToMemory(Sphere,
                      qx.InitialValue,qx.EvaluatesTo,np+Sphere[p2].Offset);
                    Push(p,p1,p2,np);
                    inc(np,rx.ByteSize);
                    p:=qx.Target;
                   end;
               end
              else
              //step 2b: store argument value, next argument?
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
                    i:=ByteSize(Sphere,vt);
                    if i<>0 then Move(FMem[vp],FMem[mp+Sphere[p2].Offset],i);
                   end;
                  vt:=0;
                 end;
                //next argument
                p1:=Sphere[p1].Next;
                p2:=Sphere[p2].Next;//assert sequence of arguments correctly added as vars to codeblock
                np:=mp;
                if p1=0 then
                  Pop(r,p1,p2,xp)
                else
                 begin
                  qx:=Sphere[p1];
                  if qx.InitialValue<>0 then LiteralToMemory(Sphere,
                    qx.InitialValue,qx.EvaluatesTo,mp+Sphere[p2].Offset);
                  Push(p,p1,p2,mp);
                  p:=qx.Target;
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
            if Sphere[rx.Parent].ThingType=ttConstructor then
             begin
              xp:=mp;//+Sphere[q].Offset;//assert 0 (see StratoFnAddOverload)
              //called by constructor? cascade 'this'
              if px.Name=Name_Inherited then
               begin
                i:=stackIndex;
                if i<>0 then dec(i);
                while (i<>0) and not((stack[i-1].p<>0) and
                  (Sphere[stack[i-1].p].ThingType=ttFnCall)) do dec(i);
                if i=0 then
                  Sphere.Error(pe,'Could not find current code block for call')
                else
                 begin
                  //assert stack[i-1].p's eventual target is a ttConstructor
                  xp:=stack[i-1].bp;
                  Move(FMem[mp],FMem[xp],SystemWordSize);
                 end;
               end;
              vtp(px.EvaluatesTo,xp);
             end
            else
            if px.EvaluatesTo<>0 then
             begin
              //assert first value in code block is return value (after this)
              q:=rx.FirstItem;
              qx:=Sphere[q];
              if (q<>0) and (qx.ThingType=ttThis) then
               begin
                q:=qx.Next;//assert Sphere[px.Target].ThingType=ttVarIndex
                qx:=Sphere[q];
               end;
              vtp(qx.EvaluatesTo,mp+qx.Offset);
             end;
           end;
         end;

      ttCodeBlock:
       begin
        if p1=0 then //begin block
         begin
          mp:=np;
          if px.FirstStatement<>0 then
           begin
            Push(p,px.FirstStatement,0,mp);
            //initial values
            r:=px.FirstItem;
            while r<>0 do
             begin
              rx:=Sphere[r];
              if rx.InitialValue<>0 then LiteralToMemory(Sphere,
                rx.InitialValue,rx.EvaluatesTo,mp+rx.Offset);
              r:=rx.Next;
             end;
            //first statement
            p:=px.FirstStatement;
           end;
         end
        else
         begin
          //assert p=cb
          q:=Sphere[p1].Next;
          if q<>0 then
           begin
            //next statement
            Push(p,q,p2,mp);
            p:=q;
            vt:=0;
           end
          else
           begin
            //code block done
            if px.EvaluatesTo=0 then vt:=0 else vt0:=0;//silence unused error
            //check any deferred
            q:=px.FirstStatement;
            while (q<>0) and (Sphere[q].ThingType<>ttDeferred) do
              q:=Sphere[q].Next;
            if q<>0 then
             begin
              Push(0,0,vt,vp);//keep result value
              vt:=0;
              Push(p,IndexStep1,0,mp);
              Push(q,q,p2,mp);
              p:=Sphere[q].Target;
             end;
           end;
         end;
        np:=mp+px.ByteSize;
       end;

      ttDeferred://when p1=0: don't run now, see ttThrow,ttCodeBlock
        if p1<>0 then
         begin
          //next deferred command?
          np:=mp;
          q:=Sphere[p1].Next;
          while (q<>0) and (Sphere[q].ThingType<>ttDeferred) do
            q:=Sphere[q].Next;
          if q<>0 then
           begin
            Push(q,q,p2,mp);
            p:=Sphere[q].Target;
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
        ;//if p1<>0 then p:=px.Target;//see ttThrow

      ttSysCall:
        try
          PerformSysCall(Sphere,p,mp);
        except
          on e:Exception do //TODO: in-lang throw
            Sphere.Error(pe,'['+e.ClassName+']'+e.Message);
        end;

      //ttFunction?
      ttOverload:
       begin
        //address of
        q:=Sphere.Add(ttPointer,qx);
        qx.ByteSize:=SystemWordSize;
        qx.EvaluatesTo:=px.Target;//ttSignature
        vtp(q,np);
        Move(p,FMem[np],SystemWordSize);
        inc(np,ByteSize(Sphere,vt));
       end;

      ttVar://vp:=Addr(p);
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

            ttThis:
             begin
              //assert Sphere[qx.Parent].ThingType=ttCodeBlock
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

      ttThis:
       begin
        //assert Sphere[px.Parent].ThingType=ttCodeBlock
        i:=stackIndex;
        while (i<>0) and (stack[i-1].p<>px.Parent) do dec(i);
        if i=0 then
          Sphere.Error(pe,'this unknown')
        else
          vtp(px.EvaluatesTo,stack[i-1].bp+px.Offset);
       end;

      ttVarIndex:
       begin
        if p1=0 then
         begin
          Push(p,IndexStep1,0,mp);
          p:=px.Parent;
         end
        else
        if px.FirstArgument=0 then
          if px.Target=0 then
            Sphere.Error(pe,'ttVarIndex without subject or aguments')
          else
          if vt=0 then
            Sphere.Error(pe,'ttVarIndex could not determine subject')
          else
           begin
            qx:=Sphere[px.Target];
            case qx.ThingType of
              ttVar,ttVarByRef,ttThis: //assert qx.Target=vt
               begin
                //auto-dereference
                case Sphere[vt].ThingType of
                  ttClass:
                    Move(FMem[vp],vp,SystemWordSize);
                  //more?
                end;
                vtp(qx.EvaluatesTo,vp+qx.Offset);
               end;
              ttProperty:
               begin
                //property getter
                Push(px.Target,qx.ValueFrom,0,np);
                vt0:=0;//silence unused error
               end;
              else Sphere.Error(pe,'unexpected ttVarIndex subject '+
                IntToHex(qx.ThingType,4));
            end;
           end
        else
        if p1=IndexStep1 then
         begin
          Push(p,IndexStep2,vt,vp);
          vt:=0;
          p:=px.FirstArgument;
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
          //assert p1=IndexStep2
          q:=p2;xp:=mp;//Pop?
          qx:=Sphere[q];
          if qx.ThingType=ttArray then
           begin
            //TODO: check range? (px.ByteSize)
            Move(FMem[vp],i,SystemWordSize);
            r:=Sphere[q].ElementType;
            inc(xp,i*ByteSize(Sphere,r));
            vtp(r,xp);
           end
          else
            Sphere.Error(pe,'Unexpected index not into array');
         end;
       end;

      ttConstant:
       begin
        vtp(px.EvaluatesTo,np);
        inc(np,ByteSize(Sphere,vt));
        LiteralToMemory(Sphere,px.InitialValue,px.EvaluatesTo,vp);
       end;
      ttLiteral:
       begin
        vtp(px.EvaluatesTo,np);
        inc(np,ByteSize(Sphere,vt));
        LiteralToMemory(Sphere,p,px.EvaluatesTo,vp);
       end;
      ttUnaryOp:
        if px.Right=0 then
          Sphere.Error(pe,'unary operator without subject')
        else
        if p1=0 then
         begin
          //if "?@@" in constructor then use class type from original call
          if (TStratoToken(px.Op) in [stOpSizeOf,stQuestionMark])//stOpTypeOf
            and (Sphere[px.Right].ThingType=ttThis) then
           begin
            //assert Sphere[px.Parent].ThingType=ttCodeBlock
            i:=stackIndex;
            while (i<>0) and (stack[i-1].p<>px.Parent) do dec(i);
            if i=0 then q:=0 else
             begin
              q:=stack[i-1].p;
              while (q<>0) and (Sphere[q].ThingType=ttCodeBlock) do
                q:=Sphere[q].Parent;
              if Sphere[q].ThingType=ttConstructor then
               begin
                //original constructor class type stored right after 'this'
                //see also StratoFnAddOverload
                Move(FMem[stack[i-1].bp+SystemWordSize],i,SystemWordSize);
                //assert Sphere[i].ThingType=ttClass
                if TStratoToken(px.Op)=stOpSizeOf then
                 begin
                  if i=0 then
                    Sphere.Error(pe,'SizeOf object to construct on empty reference')
                  else
                    if Sphere[i].ThingType=ttClass then //here? switch to ByteSize?
                      i:=Sphere[i].ByteSize
                    else
                      i:=ByteSize(Sphere,i);
                  vtp(TypeDecl_number,np);
                 end
                else //stQuestionMark://stOpTypeOf:
                 begin
                  //i:=i;
                  vtp(TypeDecl_type,np);
                 end;
                inc(np,SystemWordSize);
                Move(i,FMem[vp],SystemWordSize);
               end
              else q:=0;
             end;
           end
          else q:=0;
          if q=0 then //nope, evaluate 'right'
           begin
            Push(p,p,0,np);
            p:=px.Right;
            vt:=0;
           end;
         end
        else
        if vt=0 then
          Sphere.Error(pe,'unary operator without value')
        else
          case TStratoToken(px.Op) of
            stOpSub,stOpInc,stOpDec:
              if (vt>=TypeDecl_number) and (vt<=TypeDecl_intLast) then
               begin
                i:=ByteSize(Sphere,vt);
                ii:=0;
                Move(FMem[vp],ii,i);
                case TStratoToken(px.Op) of
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
              if (vt<>0) and (Sphere[vt].ThingType=ttClass) then
               begin
                //and TypeDecl_object<>0?
                Move(FMem[vp],i,SystemWordSize);
                if i=0 then
                  //hmm, type of null pointer, pass base type?
                  i:=vt
                else
                  //assert _basetype@-SystemWordSize
                  Move(FMem[i-SystemWordSize],i,SystemWordSize);
                q:=Sphere.Add(ttClassRef,qx);
                qx.ByteSize:=SystemWordSize;
                qx.EvaluatesTo:=i;
                vtp(q,np);
                inc(np,SystemWordSize);
                Move(i,FMem[vp],SystemWordSize);
               end
              else
               begin
                i:=vt;
                vtp(TypeDecl_type,np);
                inc(np,SystemWordSize);
                Move(i,FMem[vp],SystemWordSize);
               end;
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
              Push(p,p,p,mp);
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
            //assert SameType(q,vt
            case TStratoToken(px.Op) of
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
                  case TStratoToken(px.Op) of
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
                  case TStratoToken(px.Op) of
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
                  case TStratoToken(px.Op) of
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
                if (Sphere[q].ThingType=ttClass) and (xp<>0) then
                 begin
                  Move(FMem[xp],i,SystemWordSize);//dereference
                  //assert object._baseclass @-SystemWordSize
                  Move(FMem[i-SystemWordSize],q,SystemWordSize);
                 end;
                if (Sphere[vt].ThingType=ttClassRef) and (vp<>0) then
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
          q:=px.AssignTo;
          qx:=Sphere[q];
          if px.ValueFrom=0 then
            Sphere.Error(pe,'assignment without right side')
          else if q=0 then
            Sphere.Error(pe,'assignment without left side')
          else if qx.ThingType=ttCast then
           begin
            //'dirty' cast into
            Push(p,px.ValueFrom,qx.EvaluatesTo,np);
            p:=qx.Target;
           end
          else if (qx.ThingType=ttVarIndex) and (qx.Target<>0) and
            (Sphere[qx.Target].ThingType=ttProperty) then
           begin
            //property setter, locate object first
            //TODO: parse into (something more like) ffFnCall
            Push(qx.Target,Sphere[qx.Target].AssignTo,px.ValueFrom,np);
            p:=qx.Parent;
            vt:=0;
           end
          else
           begin
            Push(p,px.ValueFrom,0,np);
            p:=px.AssignTo;
           end;
         end
        else
        if p1=px.ValueFrom then //evaluate ValueFrom
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
            p:=px.ValueFrom;
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
              case TStratoToken(px.Op) of
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
                stOpAssignOr, //"||="
                stOpAssignAnd:
                  if vt=TypeDecl_number then
                   begin
                    i:=ByteSize(Sphere,vt);
                    ii:=0;
                    Move(FMem[xp],ii,i);
                    jj:=0;
                    Move(FMem[vp],jj,i);
                    case TStratoToken(px.Op) of
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
        if (p1=0) and (px.DoElse<>0) then //DoFirst
         begin
          Push(p,p,0,np);
          p:=px.DoElse;
         end
        else
         begin
          if p1<>0 then np:=mp;
          if px.DoIf=0 then i:=1 else
           begin
            i:=0;//default
            if p1=px.DoIf then
             begin
              if vt=TypeDecl_bool then
                Move(FMem[vp],i,SystemWordSize)
              else
                Sphere.Error(pe,'iteration criterium value is not boolean');
             end
            else
             begin
              Push(p,px.DoIf,0,np);
              p:=px.DoIf;
             end;
           end;
          vt:=0;
          //perform Body,DoThen
          if (i<>0) and not((px.Body=0) and (px.DoThen=0)) then
           begin
            Push(p,p,0,np);
            p:=px.Body;
            if px.DoThen<>0 then Push(px.DoThen,0,0,np);
           end;
         end;
      ttIterationPE:
        if (p1=0) and (px.DoElse<>0) then //DoFirst
         begin
          Push(p,p,0,np);
          p:=px.DoElse;
         end
        else
        if (px.Body<>0) and ((p1=0) or (p1=p)) then
         begin
          Push(p,px.Body,0,np);
          p:=px.Body;
          vt:=0;//silence DoFirst/DoThen leftover
         end
        else
         begin
          if p1<>0 then np:=mp;
          if px.DoIf=0 then i:=1 else
           begin
            i:=0;//default
            if p1=px.DoIf then
             begin
              if vt=TypeDecl_bool then
                Move(FMem[vp],i,SystemWordSize)
              else
                Sphere.Error(pe,'iteration criterium value is not boolean');
             end
            else
             begin
              Push(p,px.DoIf,0,np);
              p:=px.DoIf;
             end;
           end;
          vt:=0;
          //perform Body,DoThen
          if (i<>0) and not((px.Body=0) and (px.DoThen=0)) then
           begin
            Push(p,0,0,mp);
            p:=px.DoThen;
           end;
         end;
      ttCast:
        if p1=0 then
          if px.Target=0 then
            Sphere.Error(pe,'cast without subject')
          else
           begin
            Push(p,px.Target,0,np);
            p:=px.Target;
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
            if (vt=TypeDecl_number) and (px.EvaluatesTo=TypeDecl_pointer) then
             begin
              Move(FMem[vp],i,SystemWordSize);
              //if i<>0 then //TODO: protect against pointer arith
              vtp(TypeDecl_pointer,np);
              inc(np,SystemWordSize);
              Move(i,FMem[vp],SystemWordSize);
             end
            else
            if (TypeDecl_object<>0) and
              (Sphere[vt].ThingType=ttClass) and
              (Sphere[px.EvaluatesTo].ThingType=ttClass) then
             begin
              //cast to base class?
              if vp=0 then q:=vt else
               begin
                //dereference
                Move(FMem[vp],i,SystemWordSize);
                //assert object._baseclass @-SystemWordSize
                Move(FMem[i-SystemWordSize],q,SystemWordSize);
               end;
              while (q<>0) and (q<>px.EvaluatesTo) do q:=Sphere[q].InheritsFrom;
              if q<>0 then
               begin
                //TODO: check @@._baseclass!
                vtp(px.EvaluatesTo,vp);
               end
              else
                //TODO: throw
                Sphere.Error(pe,'instance not of this or inherited class');
             end

            //TODO: char
            else
            if (Sphere[vt].ByteSize in [1..8]) //TODO: and numeric?
              and (px.EvaluatesTo=TypeDecl_string) then
             begin
              ii:=0;
              Move(FMem[vp],ii,Sphere[vt].ByteSize);
              j:=Sphere.AddBinaryData(UTF8String(IntToStr(ii)));
              vtp(TypeDecl_string,np);
              inc(np,SystemWordSize);
              Move(j,FMem[vp],SystemWordSize);
             end
            else
            if (Sphere[vt].ByteSize in [1..8]) //TODO: and numeric?
              and (Sphere[px.EvaluatesTo].ByteSize in [1..8]) then //and numeric
             begin
              ii:=0;
              Move(FMem[vp],ii,Sphere[vt].ByteSize);
              vtp(px.EvaluatesTo,np);
              inc(np,Sphere[px.EvaluatesTo].ByteSize);
              Move(ii,FMem[vp],Sphere[px.EvaluatesTo].ByteSize);
             end

            else
              Sphere.Error(pe,'unsupported cast');//TODO: display type
           end;

      ttThrow:
        if px.Target=0 then //break
         begin
          BreakLoop;
          vt:=0;
         end
        else
        if (px.Target=0) or (p=p1) then //throw
         begin
          Throw(np,vt,vp);
          vt:=0;
         end
        else //get object to throw
         begin
          Push(p,p,0,np);
          p:=px.Target;
         end;

      ttAddressOf:
        if p1=0 then
          if (Sphere[px.ValueFrom].ThingType=ttThis) and
            (Sphere[px.ValueFrom].Parent=px.Parent) then
           begin
            //TODO: move with ttThis above?
            //assert Sphere[qx.Parent].ThingType=ttCodeBlock
            //assert ttThis is first item and Offset=0
            i:=stackIndex;
            while (i<>0) and (stack[i-1].p<>px.Parent) do dec(i);
            if i=0 then
              Sphere.Error(pe,'this unknown')
            else
              vtp(px.EvaluatesTo,stack[i-1].bp);
           end
          else
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

      ttClass:
       begin
        Move(p,FMem[np],SystemWordSize);
        vtp(TypeDecl_type,np);
        inc(np,SystemWordSize);
       end;

      ttProperty:
        if p1=0 then
          Sphere.Error(pe,'call of uninstantiated property')
        else
          //TODO: array indexes: px.FirstArgument
          if ((p1=px.ValueFrom) and (p2=0)) or
             ((p1=px.AssignTo) and (p2<>0)) then
            if vt=0 then
              Sphere.Error(pe,'property called without object')
            else
             begin
              //set this value (@@)
              Move(FMem[vp],FMem[np],SystemWordSize);//assert p1.FirstItem.ThingType=ttThis@0
              //virtual implementation?
              if (TypeDecl_object<>0) and (Sphere[vt].ThingType=ttClass) then
               begin
                //dereference
                Move(FMem[vp],i,SystemWordSize);
                //assert object._baseclass @-SystemWordSize
                Move(FMem[i-SystemWordSize],q,SystemWordSize);
                q:=StratoFnCallFindVirtual(Sphere,q,p);
               end
              else
                q:=p;
              //get or set?
              if (p1=px.ValueFrom) then
               begin
                if p<>q then p1:=Sphere[q].ValueFrom;
                Push(p,p1,IndexStep1,np);
                p:=p1;//assert ttCodeBlock
                mp:=np;
               end
              else//p1=px.AssignTo
               begin
                if p<>q then p1:=sphere[q].AssignTo;
                Push(p,p1,0,np);
                p:=p2;//get value to set
                inc(np,Sphere[p1].ByteSize);//assert Sphere[p1].ThingType=ttCodeBlock;
               end;
              vt:=0;
             end
          else
          if p1=px.ValueFrom then //getter called, pass result
            if vt=0 then
             begin
              //from result var
              q:=Sphere[p1].FirstItem;
              if (q<>0) and (Sphere[q].ThingType=ttThis) then q:=Sphere[q].Next;
              vtp(px.EvaluatesTo,mp+Sphere[q].Offset);
             end
            else
             begin
              //from code block result
              //assert vt=px.EvaluatesTo
              vt0:=0;//silence unused error
             end
          else
          if p1=px.AssignTo then //setter called, got value to set
           begin
            if vt=0 then
              Sphere.Error(pe,'property setter called without value to set')
            else
              Move(FMem[vp],FMem[mp+SystemWordSize],ByteSize(Sphere,vt));
            p:=p1;
            np:=mp;
            vt:=0;
           end
          else
            Sphere.Error(pe,'unknown property invocation');

      //TODO: more

      else
       begin
        Sphere.Error(pe,Format('unknown logic item %d:%.4x',[p,px.ThingType]));
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
  px:PStratoThing;
  i:int64;
begin
  px:=Sphere[p];
  if px.ThingType<>ttLiteral then
    Sphere.Error(p,'literal expected '+IntToHex(px.ThingType,4))
  else
    //assert q=0 or Sphere[q].ThingType=ttTypeDecl
    if (px.EvaluatesTo>=TypeDecl_number) and
      (px.EvaluatesTo<=TypeDecl_intLast) then
     begin
      i:=ParseInteger(string(Sphere.GetBinaryData(px.InitialValue)));
      Move(i,FMem[addr],Sphere[q].ByteSize);
     end
    else
    if px.EvaluatesTo=TypeDecl_string then
     begin
      //TODO: store strings in conceptual mem (see also stratoRunTime)
      i:=px.InitialValue;//ttBinaryData
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
  i,j,k:cardinal;
begin
  case Sphere[Fn].Op of

    stratoSysCall_xinc,stratoSysCall_xdec:
     begin
      Move(FMem[Ptr+SystemWordSize],i,SystemWordSize);
      Move(FMem[i],j,SystemWordSize);
      case Sphere[Fn].Op of
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

    else Sphere.Error(Fn,'unknown system call '+IntToStr(Sphere[Fn].Op));
  end;
end;

end.

