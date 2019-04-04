unit stratoExec;

interface

uses SysUtils, ComCtrls, stratoDecl, stratoSphere, stratoDebug, stratoDebugView;

const
  MaxResolveCB=8;

type
  TStratoMachine=class(TObject)
  private
    stackBase,stackTop,//TODO: 'TStratoTread'?
    FMem,FMemPastGlobals,FMemIndex:pointer;
    FMemSize:cardinal;
    FGlobals:array of record Item:rItem; ByteSize:cardinal; end;
    FGlobalsSize,FGlobalsIndex:cardinal;
    FDebugView:TfrmDebugView;
    FDebugCount:integer;
    FData:TObject;
    procedure Perform(Entry:rItem);
    procedure LiteralToMemory(p:rItem;ptr:pointer);
    procedure PerformSysCall(Fn:rItem;Data:pointer);
  protected
    //used by FDebugView
    resolveCB:array[0..MaxResolveCB-1] of record
      cb:rItem;
      cp,cq,_reserved1:pointer;
    end;
    procedure lvStackData(Sender: TObject; li: TListItem);
    procedure lvMemData(Sender: TObject; li: TListItem);
  public
    constructor Create(DoDebug:boolean=false);
    destructor Destroy; override;
    procedure Run;
  end;

implementation

uses Windows, CommCtrl, stratoFn, stratoTokenizer, stratoLogic, stratoParse,
  stratoTools;

const
  InitialMemSize=$100000;//?
  InitialStackSize=$10000;//?

{$IFDEF DEBUG}
type
  TCArr=array[0..$FFFFFF] of cardinal;
  PCArr=^TCArr;
{ add watches (Ctrl+F5):
    PCArr(stackBase)^
    PCArr(@FMem[BaseMemPtr])^
    PCArr(@FMem[FirstAllocMemPtr])^
    (mp-BaseMemPtr) div 4
    (np-BaseMemPtr) div 4
    (vp-BaseMemPtr) div 4
    (xp-BaseMemPtr) div 4
}
{$ENDIF}

procedure pAlign(var p:pointer); inline;
begin
  inc(xValue(p),xValue(p) xor (SystemWordSize-1));
end;

{ TStratoMachine }

constructor TStratoMachine.Create(DoDebug:boolean);
begin
  inherited Create;
  FMemSize:=InitialMemSize;
  FMem:=VirtualAlloc(nil,FMemSize,
    MEM_RESERVE or MEM_COMMIT,PAGE_READWRITE);//MEM_LARGE_PAGES?
  FMemPastGlobals:=FMem;
  FGlobalsSize:=0;
  FGlobalsIndex:=0;
  FDebugCount:=0;
  FData:=TStratoParser.Create(nil,false);
  //TODO: restore previous position
  if DoDebug then
   begin
    FDebugView:=TfrmDebugView.Create(nil);
    FDebugView.lvStack.OnData:=lvStackData;
    FDebugView.lvMem.OnData:=lvMemData;
   end
  else
    FDebugView:=nil;
end;

destructor TStratoMachine.Destroy;
begin
  VirtualFree(FMem,0,MEM_RELEASE);
  FreeAndNil(FDebugView);
  FreeAndNil(FData);
  inherited;
end;

procedure TStratoMachine.Run;
var
  i,bs:cardinal;
  p,p0,p1,p2:rItem;
begin
  //SystemWordSize:=SizeOf(pointer);
  //if Store.SystemWordSize>SystemWordSize then
  if SystemWordSize>SizeOf(pointer) then
    raise Exception.Create('Mismatching system word size');

  if FDebugView<>nil then FDebugView.Show;

  //allocate globals
  FGlobalsIndex:=0;
  for i:=0 to SourceFilesCount-1 do
   begin
    ListFirst(xxr(i * StratoSphereBlockBase),lSourceFile_Globals,p,p0);
    while p.x<>0 do
     begin
      p1:=p.r(iTarget);
      bs:=ByteSize(p1);

      //list for debug
      if FGlobalsIndex=FGlobalsSize then
       begin
        inc(FGlobalsSize,$100);//grow
        SetLength(FGlobals,FGlobalsSize);
       end;
      FGlobals[FGlobalsIndex].Item:=p1;
      FGlobals[FGlobalsIndex].ByteSize:=bs;
      inc(FGlobalsIndex);

      //set offset, value
      p1.s(vOffset,xValue(FMemPastGlobals));
      p2:=p1.r(iValue);
      if p2.x<>0 then LiteralToMemory(p2,FMemPastGlobals); //else zeroes?
      inc(xValue(FMemPastGlobals),bs);
      pAlign(FMemPastGlobals);
      ListNext(p,p0);
     end;
   end;

  //TODO: plus margin?
  FMemIndex:=FMemPastGlobals;

  //TODO: halt on unhandled exception?
  //TODO: re-construct correct order based on dependencies?

  //initialization
  for i:=0 to SourceFilesCount-1 do
   begin
    p.x:=i * StratoSphereBlockBase;
    p:=p.r(iSourceFile_InitializationBlock);
    if p.x<>0 then Perform(p);
   end;

  //finalization
  for i:=SourceFilesCount-1 downto 0 do
   begin
    p.x:=i * StratoSphereBlockBase;
    p:=p.r(iSourceFile_FinalizationBlock);
    if p.x<>0 then Perform(p);
   end;

  if FDebugView<>nil then FDebugView.Done;
end;

procedure TStratoMachine.Perform(Entry:rItem);

  procedure Push(i:cardinal); overload;
  begin
    if xValue(stackTop)>=xValue(stackBase)+InitialStackSize then
      raise Exception.Create('Stack overflow');//TODO: Throw
    Move(i,stackTop^,SystemWordSize);
    inc(xValue(stackTop),SystemWordSize);
  end;
  procedure Push(p:pointer); overload;
  begin
    if xValue(stackTop)>=xValue(stackBase)+InitialStackSize then
      raise Exception.Create('Stack overflow');//TODO: Throw
    Move(p,stackTop^,SystemWordSize);
    inc(xValue(stackTop),SystemWordSize);
  end;
  procedure Push(q:rItem); overload;
  begin
    if xValue(stackTop)+1>=xValue(stackBase)+InitialStackSize then
      raise Exception.Create('Stack overflow');//TODO: Throw
    Move(q,stackTop^,SystemWordSize);
    inc(xValue(stackTop),SystemWordSize);
  end;
  procedure ReserveStack(s:cardinal);
  begin
    //ATTENTION: caller must prevent natural popping from the reserved memory
    if xValue(stackTop)+s>=xValue(stackBase)+InitialStackSize then
      raise Exception.Create('Stack overflow');//TODO: Throw
    ZeroMemory(stackTop,s);//?
    inc(xValue(stackTop),s);
  end;
  procedure Pop(var i:cardinal); overload;
  begin
    if stackTop=stackBase then
      raise Exception.Create('Stack underflow');//TODO: Throw
    dec(xValue(stackTop),SystemWordSize);
    Move(stackTop^,i,SystemWordSize);
  end;
  procedure Pop(var p:pointer); overload;
  begin
    if stackTop=stackBase then
      raise Exception.Create('Stack underflow');//TODO: Throw
    dec(xValue(stackTop),SystemWordSize);
    Move(stackTop^,p,SystemWordSize);
  end;
  procedure Pop(var q:rItem); overload;
  begin
    if stackTop=stackBase then
      raise Exception.Create('Stack underflow');//TODO: Throw
    dec(xValue(stackTop),SystemWordSize);
    Move(stackTop^,q,SystemWordSize);
  end;

  procedure Peek(var x:pointer;var p:rItem); overload;
  begin
    if x=stackBase then
      raise Exception.Create('Stack underflow');//TODO: Throw
    dec(xValue(x),SystemWordSize);
    Move(x^,p,SystemWordSize);
  end;
  procedure Peek(var x:pointer;var p:pointer); overload;
  begin
    if x=stackBase then
      raise Exception.Create('Stack underflow');//TODO: Throw
    dec(xValue(x),SystemWordSize);
    Move(x^,p,SystemWordSize);
  end;

var
  OpCount:cardinal;
  ip,ipNext:rItem;//instruction pointer
  new:boolean;
  cp:pointer;//code block pointer
  ep:pointer;//exception pointer
  vp:pointer;//value pointer
  vt:rItem;

  function Pass:cardinal;
  begin
    if new then Result:=0 else Pop(Result);
  end;
  procedure Next(nPass:cardinal); overload;
  begin
    Push(nPass);
    Push(ip);
  end;
  procedure Next(nPass:cardinal;nNext:rItem); overload;
  begin
    Push(nPass);
    Push(ip);
    if nNext.x<>0 then ipNext:=nNext;
    //TODO: vp:=nil;vt:=0; here?
  end;

  function Volatile(nvt:rItem):pointer;
  begin
    //TODO: separate register?
    vp:=stackTop;
    vt:=nvt;
    //inc(xValue(???),ByteSize(Sphere,vt));?
    Result:=vp;
  end;

  procedure CbStart(cb:rItem;var np:pointer);
  var
    q1,q2:rItem;
  begin
    Push(cp);
    Push(cb);
    np:=stackTop;
    //allocate space for local variables
    ReserveStack(cb.v(vByteSize));
    ListFirst(cb,lItems,q1,q2);
    Push(q2);
    Push(q1);
    ipNext:=q1;
    Push(cb);
  end;

  function RefreshDebugView: boolean;
  var
    li:TListItem;
    p,p0:pointer;
    p1,q1,q2:rItem;
    i,sx,sy:cardinal;
  begin
    //TODO: move this to other unit
    inc(OpCount);
    if (FDebugView<>nil) and (FDebugView.cbKeepTrail.Checked) then
     begin
      FDebugView.lvTrail.Items.BeginUpdate;
      try
        li:=FDebugView.lvTrail.Items.Add;
        li.Caption:=IntToStr(OpCount);
        li.SubItems.Add(ItemToStr(ip));
        li.SubItems.Add(PtrToStr(vp));
        li.SubItems.Add(ItemToStr(vt));
        li.SubItems.Add('');//ItemToStr(bp));
        li.SubItems.Add(PtrToStr(cp));
        li.SubItems.Add(PtrToStr(ep));
        li.SubItems.Add(PtrToStr(stackTop));
        li.SubItems.Add(StratoDumpThing(ip));
        //timestamp?
      finally
        FDebugView.lvTrail.Items.EndUpdate;
      end;
      FDebugView.lvTrail.ItemFocused:=li;
      li.MakeVisible(false);
     end;

    Result:=false;
    if (FDebugView<>nil) and (FDebugView.CheckBreakPoint(ip)) then
     begin

      //fill codeblock trace for local variables display
      i:=0;
      p:=cp;
      while i<MaxResolveCB do
       begin
        if p=nil then
         begin
          resolveCB[i].cb.x:=0;
          resolveCB[i].cp:=nil;
          resolveCB[i].cq:=nil;
         end
        else
         begin
          //assert (q>stackBase) and (q<stackTop)
          resolveCB[i].cp:=p;
          dec(xValue(p),SystemWordSize);
          resolveCB[i].cb.x:=PxValue(p)^;
          if resolveCB[i].cb.NodeType=nCodeBlock then
           begin
            //inc(xValue(resolveCB[i].cp),SystemWordSize*8);//???
            resolveCB[i].cq:=pointer(xValue(resolveCB[i].cp)+resolveCB[i].cb.v(vByteSize));
            dec(xValue(p),SystemWordSize);
            p:=PPointer(p)^;
           end
          else
           begin
            //should not happen: fail? error? message?
            dec(i);//clear this entry
            p:=nil;//stop walking up stack
           end;
         end;
        inc(i);
       end;

      p:=stackTop;
      if vp<>nil then
       begin
        p0:=vp;
        inc(xValue(p0),ByteSize(vt));
        if xValue(p0)>xValue(p) then p:=p0;
       end;

      i:=(xValue(p)+(SystemWordSize-1)-xValue(stackBase)) div SystemWordSize;
      FDebugView.lvStack.Items.Count:=i;
      ListView_EnsureVisible(FDebugView.lvStack.Handle,i-1,false);

      FDebugView.lvMem.Items.Count:=
        (xValue(FMemIndex)+(SystemWordSize-1)-xValue(FMem)) div SystemWordSize;

      FDebugView.txtUpNext.Lines.BeginUpdate;
      try
        FDebugView.txtUpNext.Lines.Clear;
        if new then
          FDebugView.txtUpNext.Lines.Add(Format('>>> %s: %s',
            [ItemToStr(ip),StratoDumpThing(ip)]))
        else
          FDebugView.txtUpNext.Lines.Add(Format('ip: %s: %s',
            [ItemToStr(ip),StratoDumpThing(ip)]));
        if vp=nil then
          FDebugView.txtUpNext.Lines.Add('vp')
        else
          FDebugView.txtUpNext.Lines.Add(Format('vp: $%.8x',
            [xValue(vp)]));//,StratoDumpThing(Sphere,p1)]));
        if vt.x=0 then
          FDebugView.txtUpNext.Lines.Add('vt')
        else
          FDebugView.txtUpNext.Lines.Add(Format('vt: %s: %s',
            [ItemToStr(vt),StratoDumpThing(vt)]));
        FDebugView.txtUpNext.Lines.Add(Format(
          'bp: $%.8x, cp: $%.8x, ep: $%.8x, sp: $%.8x',
          [{xValue(bp)}0,xValue(cp),xValue(ep),xValue(stackTop)]));
{
        if vt<>nil then
         begin
          FDebugView.txtUpNext.Lines.Add(Format('vt: $%.8x: %s',
            [vt,StratoDumpThing(Sphere,vt)]));
          Move(FMem[vp],j,4);//TODO: ByteSize(Sphere,vt);
          FDebugView.txtUpNext.Lines.Add(Format('vp: @=%d x=%.8x v=%d',[vp,j,j]));
         end;
}         
      finally
        FDebugView.txtUpNext.Lines.EndUpdate;
      end;
      try
        if (ip.x<>0) and StratoGetSourceFile(ip,sy,sx,q1,q2) then
          FDebugView.ShowSource(ip,sy,sx)
        else
          FDebugView.txtSourceView.Clear;
      except
        FDebugView.txtSourceView.Text:=#13#10#13#10'?????';
      end;

      if FDebugView.cbStackTrace.Checked then
       begin
        FDebugView.lvStackTrace.Items.BeginUpdate;
        try
          FDebugView.lvStackTrace.Items.Clear;
          p:=cp;
          while p<>nil do
           begin
            li:=FDebugView.lvStackTrace.Items.Add;
            li.Caption:=PtrToStr(p);
            Peek(p,p1);
            li.SubItems.Add(ItemToStr(p1));
            while p1.NodeType=nCodeBlock do
              p1:=p1.r(iParent);
            li.SubItems.Add(ItemToStr(p1));
            li.SubItems.Add(StratoDumpThing(p1));
            li.SubItems.Add(StratoDumpThing(p1.rr(iParent,iParent)));//?
            if StratoGetSourceFile(p1,sy,sx,q1,q2) and (sy<>0) then
             begin
              li.SubItems.Add(UTF8ToString(
                BinaryData(xxr(SourceFiles[rSrc(p1)].FileName))));
              li.SubItems.Add(IntToStr(sx));
              li.SubItems.Add(IntToStr(sy));
             end;
            if p1.x=0 then
             begin
              li.SubItems.Add('');
              li.SubItems.Add('');
              li.SubItems.Add('');
             end;
            Peek(p,p);
           end;
        finally
          FDebugView.lvStackTrace.Items.EndUpdate;
        end;
        FDebugView.lvStackTrace.ItemIndex:=0;
       end;

      if FDebugView.WaitNext then
       begin
        inc(FDebugCount);
        Result:=true;
       end;
     end;
  end;


var
  p1,p2,q1,q2:rItem;
  xp,yp,zp:pointer;

  x,y:cardinal;
  xx,yy:int64;
  ipt:xTypeNr;
begin
  OpCount:=0;
  stackBase:=VirtualAlloc(nil,InitialStackSize,
    MEM_RESERVE or MEM_COMMIT,PAGE_READWRITE);//MEM_LARGE_PAGES?
  try
    stackTop:=stackBase;
    ip:=Entry;
    ep:=nil;
    new:=true;
    cp:=nil;
    vp:=nil;
    vt.x:=0;
    while ip.x<>0 do
     begin

      if RefreshDebugView then
        asm int 3 end;//DebugBreak;//forced breakpoint

      //TODO: check ip with block base pointers
      ipNext.x:=0;
      ipt:=ip.NodeType;
      case ipt of

        nCodeBlock:
          if new then
           begin
            CbStart(ip,cp);
            vp:=nil;
            vt.x:=0;
           end
          else
           begin
            Pop(q1);
            Pop(q2);
            ListNext(q1,q2);
            if q1.x=0 then
             begin
              //resulting value
              if ip.r(iReturnType).x=0 then
               begin
                //if vt<>0 then raise?throw?
                vt.x:=0;
                vp:=nil;
               end
              else
               begin
                if vt.x=0 then
                  raise Exception.Create('CodeBlock with ReturnType didn''t resolve to value');
               end;

{
              //resulting value
              vt:=ip.r(iReturnType);
              if vt.x=0 then
               begin
                vp:=nil;
               end
              else
               begin
                vp:=cp;
                //assert resulting value's nVar.vOffset=0
               end;
}
              //code-block done (see also StartCB)
              stackTop:=cp;//dec(xValue(stackTop),Sphere.n(cb0,vByteSize)^);
              Pop(q1);
              Pop(cp);
             end
            else
             begin
              //next statement
              Push(q2);
              Push(q1);
              Push(ip);
              ipNext:=q1;
              vt.x:=0;
              vp:=nil;
             end;
           end;

        nFCall,//static call (no subject)
        nVCall,//virtual call (subject.target)
        nICall://inherited call (subject.target)
         begin
          if new then
            if ipt=nVCall then x:=0 else x:=1
          else
            x:=Pass;
          case x of
            0://start, evaluate subject
              Next(1,ip.r(iSubject));

            1://subject evaluated, arguments?
             begin
              q2.x:=0;//see below
              case ipt of
                nFCall:
                 begin
                  p1:=ip.r(iTarget);
                  if p1.x=0 then RunError(ip,'F-Call target not defined');
                 end;
                nVCall:
                 begin
                  if vt.x=0 then RunError(ip,'V-Call subject not resolved');
                  p1:=StratoFnCallFindVirtual(ip,vt,vp);
                  if p1.x=0 then
                   begin
                    RunError(ip,'V-Call target implementation not found');
                    q2.x:=0;
                   end
                  else
                   begin
                    Push(p1);
                    if p1.NodeType=nCtor then q2.x:=PxValue(vp)^;
                   end;
                 end;
                nICall:
                 begin
                  p1:=ip.r(iSubject);
                  if p1.x=0 then RunError(ip,'I-Call subject not defined');
                  p1:=ip.r(iTarget);
                  if p1.x=0 then RunError(ip,'I-Call target not defined');
                 end;
              end;

              if p1.x<>0 then
               begin
                Next(3);
                CbStart(p1.r(iBody),xp);//not cp here! see below

                case p1.NodeType of

                  nOverload,nPropGet,nPropSet:
                    if p1.rrr(iBody,lLocals,iNext).NodeType=nThis then
                     begin
                      //assert "@@".vOffset=0
                      //TODO: check SameType(t,ResType("@@"?
                      case vt.NodeType of //auto-dereference
                        nClass:PxValue(xp)^:=q1.x;
                        else PxValue(xp)^:=xItem(vp);
                      end;
                     end;

                  nCtor:
                   begin
                    PxValue(xp)^:=0;//"@@":this default nil until malloc
                    yp:=xp;
                    inc(xValue(yp),SystemWordSize);
                    //calling an inherited constructor?
                    q1.x:=0;
                    if (ipt=nFCall) or (cp=nil) then p2.x:=0 else
                     begin
                      zp:=cp;
                      Peek(zp,p2);
                      q1:=p2.r(iParent);
                      while q1.NodeType=nCodeBlock do
                       begin
                        Peek(zp,zp);
                        if zp=nil then
                         begin
                          p2.x:=0;
                          q1.x:=0;
                         end
                        else
                         begin
                          Peek(zp,p2);
                          q1:=p2.r(iParent);
                         end;
                       end;
                     end;
                    if q1.NodeType=nCtor then
                     begin
                      inc(xValue(zp),SystemWordSize*2); //var "?@@"
                      PxValue(yp)^:=PxValue(zp)^;
                     end
                    else
                     begin
                      //q2 from subject evaluation above!
                      if q2.x=0 then q2:=ip.rrr(iTarget,iSignature,iReturnType);
                      PxValue(yp)^:=q2.x;
                     end;
                   end;

                  nDtor:
                    PxValue(xp)^:=q2.x;//assert "@@".vOffset=0

                  else
                    raise Exception.Create('//TODO:');
                end;

                //Sphere.First(ip,fArguments,p2,);
                p2:=ip.rr(lArguments,iNext);
                if p2.x=0 then
                 begin
                  cp:=xp;
                  //ipNext:= by CbStart above
                 end
                else
                 begin
                  Push(xp);//new cp after arguments
                  if ipt=nVCall then Push(p1);//nOverload
                  Push(p1.r(iFirstArgVar));
                  Push(p2);//nCallArg
                  Next(2,p2.r(iValue));
                 end;
               end;  

              vp:=nil;
              vt.x:=0;
             end;

            2://store argument value
             begin
              Pop(p2);//nCallArg
              Pop(q2);//nVarDecl
              if ipt=nVCall then Pop(p1) else p1:=ip.r(iTarget);
              Pop(xp);//new cp after arguments

              //store value
              yp:=xp;
              inc(xValue(yp),q2.v(vOffset));
              if p2.NodeType=nSigArgByRef then
                PxValue(yp)^:=xItem(vp) //Move(vp,yp^,SystemWordSize)
              else
                Move(vp^,yp^,ByteSize(vt));

              //next argument //TODO: Sphere.Next(q2,);
              if q2.x=p1.r(iFirstArgVar).x then q2.x:=0 else
               begin
                p2:=p2.r(iNext);
                q2:=q2.r(iNext);
               end;
              if q2.x=0 then
               begin
                //all done, do first of body (StartCB already done by Pass=1 above)
                cp:=xp;
                q1:=p1.r(iBody);
                //Sphere.First(q1,fItems,ipNext,);
                ipNext:=q1.rr(lItems,iNext);
               end
              else
               begin
                Push(xp);//new cp after arguments
                if ipt=nVCall then Push(p1);//nOverload
                Push(q2);//nVarDecl
                Push(p2);//nCallArg
                Next(2,p2.r(iValue));
               end;

              vp:=nil;
              vt.x:=0;
             end;

            3://post-block (returned value?)
             begin
              if ipt=nVCall then Pop(p1) else p1:=ip.r(iTarget);

              //inherited constructor? propagate this
              if p1.NodeType=nCtor then
               begin
                xp:=stackTop;//previous 'cp' from freshly ended codeblock of body
                inc(xValue(xp),SystemWordSize*4);
                if ipt=nVCall then inc(xValue(xp),SystemWordSize);
                vp:=xp;
                inc(xValue(xp),SystemWordSize);
                vt.x:=PxValue(xp)^;

                //calling an inherited constructor?
                if cp=nil then p2.x:=0 else
                 begin
                  xp:=cp;
                  Peek(xp,p2);
                  while p2.r(iParent).NodeType=nCodeBlock do
                   begin
                    Peek(xp,xp);
                    if xp=nil then p2.x:=0 else Peek(xp,p2);
                   end;
                 end;
                if p2.r(iParent).NodeType=nCtor then
                 begin
                  inc(xValue(xp),SystemWordSize);
                  PxValue(xp)^:=PxValue(vp)^;
                 end;
               end
              else
               begin
                //check return value
                q2:=p1.rr(iSignature,iReturnType);
                //if q2<>0 and p1.iBody.iReturnType=0 then?
                if q2.x=0 then
                 begin
                  //if vt<>0 then raise?throw?
                  vt.x:=0;
                  vp:=nil;
                 end
                else
                 begin
                  //assert return value first (past this)
                  if vt.x=0 then
                   begin
                    vt:=q2;
                    q1:=p1.rrr(iBody,lLocals,iNext);
                    if q1.NodeType=nThis then q1:=q1.r(iNext);
                    xp:=stackTop;//previous 'cp' from freshly ended codeblock of body
                    inc(xValue(xp),SystemWordSize*4);
                    if ipt=nVCall then inc(xValue(xp),SystemWordSize);
                    inc(xValue(xp),q1.v(vOffset));
                    vp:=xp;
                   end;
                  //else check SameType(Sphere,vt,q2)?
                 end;
               end;
             end;
          end;
         end;

        nSCall://system call
         begin
          if ip.r(lArguments).x<>0 then
            RunError(ip,'interpreter doesn''t support syscalls with arguments');
          PerformSysCall(ip,cp);
         end;

        nClass:
          //TODO: all Sphere.Add(nClass do Sphere.Add(nClassRef onbeforehand?
          PxValue(Volatile((FData as TStratoParser).
            Add(nClassRef,[iTarget,ip.x])))^:=ip.x;

        nVar,nVarReadOnly,nVarByRef,nThis://calculate address
         begin
          //assert vp=nil
          vt:=ip.r(iType);
          vp:=nil;//calulated below
          q1:=ip;
          while q1.x<>0 do
           begin
            q2.x:=0;//next...
            case q1.NodeType of
              nVar,nVarReadOnly,nThis://offset
               begin
                inc(xValue(vp),q1.v(vOffset));
                q2:=q1.r(iParent);
               end;

              nVarByRef:
               begin
                xValue(vp):=PxValue(vp)^;
                q2.x:=0;
               end;

              nNameSpace://was global var
                ;//end loop (assert vOffset set by init)
              nCodeBlock://was local var
               begin
                //but to which code block: go up the chain
                xp:=cp;
                yp:=nil;
                p2.x:=0;
                while (xp<>nil) and (p2.x<>q1.x) do
                 begin
                  yp:=xp;
                  Peek(xp,p2);//codeblock
                  Peek(xp,xp);//previous cp
                 end;
                if yp=nil then
                  RunError(ip,'local block not found on stack')
                else
                  inc(xValue(vp),xValue(yp));
               end;
              //nRecord://TODO: dereference pointer

              nClass,nRecord:;//just calculate offset, assert here via nField...

              else RunError(ip,'invalid relativity chain');
            end;
            q1:=q2;//next!
           end;
         end;

        nConstant:
          LiteralToMemory(ip.r(iValue),Volatile(ip.r(iType)));
        nLiteral:
          LiteralToMemory(ip,Volatile(ip.r(iType)));

        nArrayIndex:
          case Pass of
            0://start, evaluate subject
              Next(1,ip.r(iSubject));
            1://evaluate index
             begin
              if vt.x=0 then
                RunError(ip,'array to index into didn''t resolve');
              Push(vp);
              //TODO: more than one array index
              Next(2,ip.rr(lArguments,iValue));//nCalArg
             end;
            2://combine
             begin
              Pop(xp);

              //assert vt=IntrinsicType(itNumber)
              vt:=ip.r(iType);
              vp:=pointer(xValue(xp)+xValue(vp^)*ByteSize(vt));
             end;
          end;

        nField:
          case Pass of
            0://start, evaluate subject
              Next(1,ip.r(iSubject));
            1://evaluate target
             begin
              if vt.x=0 then
                RunError(ip,'subject didn''t resolve');
              //TODO: check vt and ip.fSubject.fTypeDecl?
              Push(vt);
              Push(vp);
              Next(2,ip.r(iTarget));
             end;
            2://combine
             begin
              Pop(xp);

              //auto-dereferencing
              //TODO: calculate deference count parse-time?
              Pop(p2);//subject type
              while p2.x<>0 do
               begin
                p1:=p2;
                p2.x:=0;//default
                case p1.NodeType of
                  nThis:
                    xp:=pointer(xValue(xp^));//assert p1.r(iType).NodeType=nClass
                  nVar,nVarReadOnly:
                    p2:=p1.r(iType);
                  nVarByRef:
                   begin
                    xp:=pointer(xValue(xp^));
                    p2:=p1.r(iType);
                   end;
                  nPointer:
                   begin
                    xp:=pointer(xValue(xp^));
                    p2:=p1.r(iTarget);
                   end;
                  nClass:
                    xp:=pointer(xValue(xp^));
                end;
               end;

              vp:=pointer(xValue(xp)+xValue(vp));
              //vt:=vt;//assert same as ip.fSubject
             end;
          end;

        {
        nOverload://resolve to address?
         begin
          vp:=pointer(ip);//?
          vt:=Sphere.n(ip,fSignature)^;
         end;
        }


        nUnaryOp:
          case Pass of
            0://evaluate operand
             begin
              x:=0;//default
              case TStratoToken(ip.v(vOperator)) of
                stOpSizeOf,stQuestionMark:
                  if ip.r(iRight).NodeType=nThis then
                   begin
                    if cp=nil then p2.x:=0 else
                     begin
                      xp:=cp;
                      Peek(xp,p2);
                      while p2.r(iParent).NodeType=nCodeBlock do
                       begin
                        Peek(xp,xp);
                        if xp=nil then p2.x:=0 else Peek(xp,p2);
                       end;
                     end;
                    if p2.r(iParent).NodeType=nCtor then
                     begin
                      inc(xValue(xp),SystemWordSize*2); //var "?@@"
                      case TStratoToken(ip.v(vOperator)) of
                        stOpSizeOf:
                          PxValue(Volatile(xxr(IntrinsicTypes[itNumber])))^:=
                            xxr(PxValue(xp)^).v(vByteSize);
                        stQuestionMark:
                          PxValue(Volatile(xxr(IntrinsicTypes[itNumber])))^:=
                            PxValue(xp)^;
                      end;
                      x:=1;//skip Next() below
                     end;
                   end;
              end;
              if x=0 then Next(1,ip.r(iRight));
             end;
            1://TODO: move to runtime/namespaces/intrinsics
              case TStratoToken(ip.v(vOperator)) of
                stOpSub,stOpInc,stOpDec,stTilde:
                  if IsIntrinsicNumeric(vt) then
                   begin
                    x:=ByteSize(vt);
                    xx:=0;
                    Move(vp^,xx,x);
                    case TStratoToken(ip.v(vOperator)) of
                      stOpSub:xx:=-xx;
                      stOpInc:xx:=xx+1;
                      stOpDec:xx:=xx-1;
                      stTilde:xx:=not(xx);//xx:=-(xx+1);?
                    end;
                    Move(xx,vp^,x);
                    vt:=ip.r(iReturnType);
                   end
                  else
                    RunError(ip,'Unknown operation');
                stOpNot:
                  if vt.x=IntrinsicTypes[itBoolean] then
                    if PxValue(vp)^=0 then PxValue(vp)^:=1 else PxValue(vp)^:=0
                  else
                    RunError(ip,'Unknown operation');
                stOpSizeOf:
                 begin
                  if vt.x=IntrinsicTypes[itType] then
                    x:=ByteSize(xxr(PxValue(vp)^))
                  else
                    x:=ByteSize(vt);
                  PxValue(Volatile(xxr(IntrinsicTypes[itNumber])))^:=x;
                 end;
                stQuestionMark://type of
                  if vt.NodeType=nClass then
                   begin
                    //live object? extract base class
                    xp:=pointer(vp^);
                    //inc(xValue(xp),Lookup(itObject,Name('_baseclass')).v(vOffset));
                    dec(xValue(xp),SystemWordSize);//assert object._baseclass offset -4
                    PxValue(Volatile(xxr(IntrinsicTypes[itType])))^:=PxValue(xp)^;
                    //Sphere.Add(nClassRef,[iTarget,PxValue(xp)^]);?
                   end
                  else
                    PxValue(Volatile(xxr(IntrinsicTypes[itType])))^:=vt.x;
                else RunError(ip,'Unknown operator');
              end;
          end;

        nBinaryOp:
          case Pass of
            0://evaluate left
              Next(1,ip.r(iLeft));
            1://evaluate right
             begin
              //stored voliatile on stack? keep it there!
              xp:=stackTop;
              inc(xValue(xp),SystemWordSize*2);
              if vp=xp then
               begin
                x:=ByteSize(vt);
                inc(xValue(stackTop),x+SystemWordSize*2);
                Push(x);
                Push(0);
               end
              else
                Push(vp);
              Push(vt);
              Next(2,ip.r(iRight));
              vp:=nil;
              vt.x:=0;
             end;
            2://
             begin
              Pop(p1);
              Pop(xp);
              if xp=nil then //stored volatile? roll back!
               begin
                Pop(x);
                dec(xValue(stackTop),x+SystemWordSize*2);
                xp:=stackTop;
                inc(xValue(xp),SystemWordSize*2);
               end;
              case TStratoToken(ip.v(vOperator)) of
                stOpEQ,stOpNEQ:
                  if SameType(vt,p1) then
                   begin
                    x:=ByteSize(vt);
                    while (x<>0) and (PByte(vp)^=PByte(xp)^) do
                     begin
                      dec(x);
                      inc(xValue(vp));
                      inc(xValue(xp));
                     end;
                    Volatile(xxr(IntrinsicTypes[itBoolean]));//sets vp
                    case TStratoToken(ip.v(vOperator)) of
                      stOpEQ:
                        if x=0 then PxValue(vp)^:=1 else PxValue(vp)^:=0;
                      stOpNEQ:
                        if x=0 then PxValue(vp)^:=0 else PxValue(vp)^:=1;
                    end;
                   end
                  else
                    RunError(ip,'Unknown operation');
                stOpAdd:
                  if IsIntrinsicNumeric(vt) and IsIntrinsicNumeric(p1) then
                   begin
                    x:=ByteSize(p1);
                    xx:=0;
                    Move(xp^,xx,x);
                    y:=ByteSize(vt);
                    yy:=0;
                    Move(vp^,yy,y);
                    xx:=xx+yy;
                    Move(xx,Volatile(ip.r(iReturnType))^,y);
                   end
                  else
                  if (p1.x=IntrinsicTypes[itString]) and
                     (vt.x=IntrinsicTypes[itString]) then
                   begin
                    //TODO: strings in memory (by runtime?)
                    p2.x:=AddBinaryData((FData as TStratoParser).SrcIndex,
                      BinaryData(xxr(PxValue(xp)^))+
                      BinaryData(xxr(PxValue(vp)^)));
                    PxValue(Volatile(xxr(IntrinsicTypes[itString])))^:=p2.x;
                   end
                  else
                    RunError(ip,'Unknown operation');
                stOpSub,stOpMul,stOpDiv,stOpMod:
                  if IsIntrinsicNumeric(vt) and IsIntrinsicNumeric(p1) then
                   begin
                    x:=ByteSize(p1);
                    xx:=0;
                    Move(xp^,xx,x);
                    y:=ByteSize(vt);
                    yy:=0;
                    Move(vp^,yy,y);
                    case TStratoToken(ip.v(vOperator)) of
                      stOpSub:xx:=xx-yy;
                      stOpMul:xx:=xx*yy;
                      stOpDiv:xx:=xx div yy;
                      stOpMod:xx:=xx mod yy;
                    end;
                    Move(xx,Volatile(ip.r(iReturnType))^,x);
                   end
                  else
                    RunError(ip,'Unknown operation');
                stOpLT,stOpLTE,stOpGT,stOpGTE:
                  if IsIntrinsicNumeric(vt) and IsIntrinsicNumeric(p1) then
                   begin
                    x:=ByteSize(p1);
                    xx:=0;
                    Move(xp^,xx,x);
                    y:=ByteSize(vt);
                    yy:=0;
                    Move(vp^,yy,y);
                    Volatile(xxr(IntrinsicTypes[itBoolean]));//sets vp
                    case TStratoToken(ip.v(vOperator)) of
                      stOpLT: if xx<yy  then PxValue(vp)^:=1 else PxValue(vp)^:=0;
                      stOpLTE:if xx<=yy then PxValue(vp)^:=1 else PxValue(vp)^:=0;
                      stOpGT: if xx>yy  then PxValue(vp)^:=1 else PxValue(vp)^:=0;
                      stOpGTE:if xx>=yy then PxValue(vp)^:=1 else PxValue(vp)^:=0;
                    end;
                   end
                  else
                    RunError(ip,'Unknown operation');
                stOpAnd,stOpOr,stOpXor:
                  if (p1.x=IntrinsicTypes[itBoolean]) and
                     (vt.x=IntrinsicTypes[itBoolean]) then
                   begin
                    x:=PxValue(xp)^;
                    y:=PxValue(vp)^;
                    Volatile(xxr(IntrinsicTypes[itBoolean]));//sets vp
                    case TStratoToken(ip.v(vOperator)) of
                      stOpAnd:if (x<>0) and (y<>0) then PxValue(vp)^:=1 else PxValue(vp)^:=0;
                      stOpOr: if (x<>0) or  (y<>0) then PxValue(vp)^:=1 else PxValue(vp)^:=0;
                      stOpXor:if (x<>0) xor (y<>0) then PxValue(vp)^:=1 else PxValue(vp)^:=0;
                    end;
                   end
                  else
                    RunError(ip,'Unknown operation');
                //TODO: stOpSub,stOpMul,stOpDiv,stOpMod,stOpShl,stOpShr,stThreeLT,stThreeGT:
                //TODO: stOpNEQ,stOpLT,stOpLTE,stOpGT,stOpGTE:
                //TODO: stOpAnd,stOpOr,stOpXor
                //stOpTypeIs:
                else RunError(ip,'Unknown operator');
              end;
             end;
          end;

        nAssign:
          case Pass of
            0://evaluate left
             begin
              p1:=ip.r(iTarget);
              if p1.NodeType=nCast then //'dirty cast'?
                Next(3,p1.r(iSubject))
              else
                Next(1,p1);
             end;
            1://evaluate right
             begin
              //if vp=nil then throw?
              Push(vp);//vt?
              Next(2,ip.r(iValue));
              vp:=nil;
              vt.x:=0;
             end;
            2://copy value
             begin
              Pop(xp);//pt? assert =vt
              if xp=nil then
                raise Exception.Create('nil pointer assignment');//TODO: proper throw
              case TStratoToken(ip.v(vOperator)) of
                stOpAssign:Move(vp^,xp^,ByteSize(vt));
                stOpAssignAdd:
                 begin
                  //TODO: merge with binaryop
                  //if =TypeDecl_number!!
                  x:=ByteSize(vt);//ByteSize(ResType(ip.r(iTarget?
                  xx:=0;
                  Move(xp^,xx,x);
                  yy:=0;
                  Move(vp^,yy,x);
                  xx:=xx+yy;
                  Move(xx,xp^,x);
                 end;
                //TODO: stOpAssignSub,stOpAssignMul,stOpAssignDiv,stOpAssignMod,stOpAssignOr,stOpAssignAnd
                else
                  RunError(ip,'unknown assignment type');
              end;
              vp:=nil;//drop value (!!! by language design)
              vt.x:=0;
             end;
            3://dirty cast, evaluate right
             begin
              Push(vp);//vt?
              Next(4,ip.r(iValue));
              vp:=nil;
              vt.x:=0;
             end;
            4://dirty cast, push value
             begin
              Pop(xp);//pt? assert =vt
              case TStratoToken(ip.v(vOperator)) of
                stOpAssign://plain
                  Move(vp^,xp^,ByteSize(ip.rr(iTarget,iType)));
                stOpAssignAdd:
                 begin
                  //TODO: merge with binaryop
                  //TODO: switch pointer arith (default off!)
                  //if =TypeDecl_number!!
                  x:=ByteSize(vt);//ByteSize(ResType(ip.r(iTarget?
                  xx:=0;
                  Move(xp^,xx,x);
                  yy:=0;
                  Move(vp^,yy,x);
                  xx:=xx+yy;
                  Move(xx,xp^,x);
                 end;
                else
                  RunError(ip,'unknown assignment type');
              end;
              vp:=nil;//drop value (!!! by language design)
              vt.x:=0;
             end;
          end;

        nCast:
          if new then
           begin
            Push(ip);
            ipNext:=ip.r(iSubject);
           end
          else
           begin
            //TODO: move these specifics over to runtime (with intrinsics? syscalls?)
            p1:=ip.r(iType);//TODO: check ByteSize?
            if IsIntrinsicNumeric(vt) and IsIntrinsicNumeric(p1) then
             begin
              //TODO: zero-extend sign-extend
              x:=ByteSize(vt);
              xx:=0;
              Move(vp^,xx,x);
              Move(xx,Volatile(p1)^,ByteSize(p1));
             end
            else
            if (vt.x=IntrinsicTypes[itString]) and IsIntrinsicNumeric(p1) then
             begin
              if not TryStrToInt64(string(
                BinaryData(xxr(PxValue(vp)^))),xx) then
                RunError(ip,'invalid integer value');//TODO: raise
              PxValue(Volatile(p1))^:=xx;//Move(xx,vp^,ByteSize(p1));
             end
            else
            if IsIntrinsicNumeric(vt) and (p1.x=IntrinsicTypes[itString]) then
             begin
              x:=ByteSize(vt);
              xx:=0;
              Move(vp^,xx,x);
              q1.x:=AddBinaryData((FData as TStratoParser).SrcIndex,
                UTF8String(IntToStr(xx)));
              PxValue(Volatile(p1))^:=q1.x;//Move(q1,vp^,ByteSize(p1));
             end
            else
            if (vt.x=IntrinsicTypes[itBoolean]) and (p1.x=IntrinsicTypes[itString]) then
             begin
              x:=PxValue(vp)^;//Move(vp^,x,SystemWordSize);
              if x=0 then
                q1.x:=AddBinaryData((FData as TStratoParser).SrcIndex,'0')
              else
                q1.x:=AddBinaryData((FData as TStratoParser).SrcIndex,'1');
              PxValue(Volatile(p1))^:=q1.x;//Move(q1,vp^,SystemWordSize);
             end
            else
            if (vt.x=IntrinsicTypes[itNumber]) and (p1.NodeType=nEnum) then
             begin
              x:=PxValue(vp)^;//Move(vp^,x,SystemWordSize);
              //check enumeration index in range?
              PxValue(Volatile(p1))^:=x;//Move(x,vp^,SystemWordSize);
             end
            else
            if (vt.NodeType=nEnum) and (p1.x=IntrinsicTypes[itNumber]) then
             begin
              //x:=PxValue(vp)^;//Move(vp^,x,SystemWordSize);
              vt:=p1;
              //PxValue(Volatile(p1))^:=x;//Move(x,vp^,SystemWordSize);
             end
            else
            {//TODO
            if (p1>=TypeDecl_number) and (p1<=TypeDecl_intLast) and
               (vt>=TypeDecl_number) and (vt<=TypeDecl_intLast) then
             begin
              x:=ByteSize(Sphere,vt);
              xx:=0;
              Move(vp^,xx,x);
              Move(xx,Volatile(p1)^,ByteSize(Sphere,p1));
             end
            else
            }
            if (vt.x=IntrinsicTypes[itNumber]) and (p1.x=IntrinsicTypes[itPointer]) then
             begin
              x:=PxValue(vp)^;//Move(vp^,x,SystemWordSize);
              //if i<>x then //TODO: protect against pointer arith
              PxValue(Volatile(p1))^:=x;//Move(x,vp^,SystemWordSize);
             end
            else
            if (IntrinsicTypes[itObject]<>0) and
              (vt.NodeType=nClass) and (p1.NodeType=nClass) then
             begin
              //cast to base class?
              if vp=nil then q1:=vt else
               begin
                //dereference
                xp:=pointer(PxValue(vp)^-SystemWordSize);
                //assert object._baseclass @-SystemWordSize
                q1.x:=PxValue(xp)^;
               end;
              while (q1.x<>0) and (q1.x<>p1.x) do q1:=q1.r(iInheritsFrom);
              if q1.x<>0 then
               begin
                //TODO: check @@._baseclass!
                //vp:=vp;
                vt:=q1;
               end;
              //else?
             end
            else
              RunError(ip,'unsupported cast');
           end;

{
        nAddressOf:
          if new then
           begin
            Push(ip);
            ipNext:=Sphere.n(ip,fSubject)^;
           end
          else
           begin
            //assert p1<>0
            PxValue(Volatile(Sphere.n(ip,fReturnType)^))^:=xItem(vp);
           end;

}
        nSelection:
          case Pass of
            0:
              Next(1,ip.r(iPredicate));
            1:
              if vt.x<>IntrinsicTypes[itBoolean] then
                RunError(ip,'selection predicate didn''t evaluate to boolean')
              else
                if PxValue(vp)^=0 then
                  ipNext:=ip.r(iDoFalse)
                else
                  ipNext:=ip.r(iDoTrue);
          end;

        nIteration:
         begin
          p1:=ip.r(iPredicate);
          if p1.NodeType=nRangeIndex then
            case Pass of
              0://first resolve iterator
                Next(1,p1.r(iLeft));
              1:
               begin
                if vt.x<>IntrinsicTypes[itNumber] then
                  RunError(ip,'iteration currently only supports number ranges');//TODO
                Push(vp);
                Next(2,p1.rr(iRight,iLeft));
               end;
              2:
               begin
                if vt.x<>IntrinsicTypes[itNumber] then
                  RunError(ip,'iteration currently only supports number ranges');//TODO
                Pop(xp);
                PxValue(xp)^:=PxValue(vp)^;
                Push(xp);
                Next(3,p1.rr(iRight,iRight));
               end;
              3:
               begin
                if vt.x<>IntrinsicTypes[itNumber] then
                  RunError(ip,'iteration currently only supports number ranges');//TODO
                Push(PxValue(vp)^);
                Next(4,ip.r(iBody));
               end;
              4:
               begin
                Pop(x);
                Pop(xp);
                inc(PxValue(xp)^);
                if PxValue(xp)^>x then
                 begin
                  //done
                  //TODO: return value
                 end
                else
                 begin
                  Push(xp);
                  Push(x);
                  Next(4,ip.r(iBody));
                 end;
               end;
            end
          else
            case Pass of
              0:
                Next(1,p1);
              1:
                if vt.x<>IntrinsicTypes[itBoolean] then
                  RunError(ip,'iteration predicate didn''t evaluate to boolean')
                else
                  if PxValue(vp)^=0 then
                    //done
                  else
                    Next(0,ip.r(iBody));//TODO: store result value
            end;
         end;

        nIterPostEval:
          case Pass of
            0:
              Next(1,ip.r(iBody));
            1:
              Next(2,ip.r(iPredicate));//TODO: store result value
            2:
              if vt.x<>IntrinsicTypes[itBoolean] then
                RunError(ip,'iteration predicate didn''t evaluate to boolean')
              else
                if PxValue(vp)^=0 then
                  //done
                else
                  Next(1,ip.r(iBody));
          end;

        else
          RunError(ip,'unknown logic item '+ItemToStr(ip)+':'+
            NodeTypeToStr(ipt));
      end;

      {//TODO
      if (vt0<>0) and (vt=vt0) then
       begin
        RunError(ip,'unused resulting value');
        vp:=nil;
        vt:=0;
       end;
      }

      if ipNext.x=0 then
       begin
        if stackTop=stackBase then ip.x:=0 else Pop(ip);
        new:=false;
       end
      else
       begin
        ip:=ipNext;
        new:=true;
       end;

     end;
    //
  finally
    VirtualFree(stackBase,0,MEM_RELEASE);
  end;
end;

procedure TStratoMachine.LiteralToMemory(p:rItem;ptr:pointer);
var
  i:int64;
  r:rItem;
begin
  if p.NodeType<>nLiteral then
    RunError(p,'literal expected '+ItemToStr(p))
  else
   begin
    r:=p.r(iType);
    //assert q=0 or rType(r)=nType
    if SameType(r,xxr(IntrinsicTypes[itString])) then
     begin
      //TODO: store strings in conceptual mem (see also stratoRunTime)
      PxValue(ptr)^:=p.r(iValue).x;
     end
    else
    if SameType(r,xxr(IntrinsicTypes[itBoolean])) then
     begin
      if BinaryData(p)='0' then
        i:=0
      else
        i:=1;
      Move(i,ptr^,SystemWordSize);
     end
    //if SameType(r,xxr(IntrinsicTypes[itNumber])) then
    else
     begin
      //TODO: not ParseInteger here, but store(d) binary?
      i:=ParseInteger(BinaryData(p.r(iValue)));
      Move(i,ptr^,r.v(vByteSize));
     end;
    //else
    //  Sphere.Error(p,'unsupported literal type');
   end;
end;

procedure TStratoMachine.PerformSysCall(Fn: rItem; Data: pointer);
var
  p:pointer;
  q:rItem;
  i:integer;
begin
  q:=Fn.r(iTarget);
  if q.NodeType<>nLiteral then
    raise Exception.Create('SysCall: unexpected target');
  i:=ParseInteger(BinaryData(q.r(iValue)));
  case i of

    1://xinc
     begin
      p:=pointer(Data^);
      inc(xValue(p^));
     end;
    2://xdec
     begin
      p:=pointer(Data^);
      dec(xValue(p^));
     end;

    100://malloc
     begin
      p:=Data;
      inc(xValue(p),SystemWordSize);

      if xValue(FMemIndex)-xValue(FMem)+xValue(p^)>FMemSize then
        RunError(Fn,'Out of memory')//TODO:throw
      else
       begin
        pointer(Data^):=FMemIndex;
        inc(xValue(FMemIndex),xValue(p^));
        //TODO: align?
        //TODO: mark allocated? (see also deallocation)
       end;

     end;

     200://writeln
      begin
       Writeln(string(BinaryData(xxr(PxValue(Data)^))));
      end;

    else
      raise Exception.Create('SysCall: unknown key '+IntToStr(i));
  end;
end;

procedure TStratoMachine.lvStackData(Sender: TObject; li: TListItem);
var
  p:pointer;
  p1,p2,p0:rItem;
  i:integer;
begin
  p:=stackBase;
  inc(xValue(p),xValue(li.Index)*SystemWordSize);

  if xValue(p)>=xValue(stackTop) then
   begin
    li.Caption:=PtrToStr(p)+'^';
    i:=MaxResolveCB;
   end
  else
   begin
    li.Caption:=PtrToStr(p);
    i:=0;
    while (i<MaxResolveCB) and (xValue(p)<xValue(resolveCB[i].cp)) do inc(i);
    if xValue(p)>=xValue(resolveCB[i].cq) then i:=MaxResolveCB;
   end;

  p1.x:=PxValue(p)^;
  li.SubItems.Add(ItemToStrX(p1.x));

  if i=MaxResolveCB then
    if (p1.x<200) or (p1.x>=BlocksCount * StratoSphereBlockBase) then
      li.SubItems.Add('')
    else
      if p1.NodeType=nCodeBlock then
        li.SubItems.Add('{ '+StratoDumpThing(p1.r(iParent)))
      else
        li.SubItems.Add(StratoDumpThing(p1))
  else
   begin
    ListFirst(resolveCB[i].cb,lLocals,p1,p0);
    if p1.x=0 then
      li.SubItems.Add('')
    else
     begin
      p2:=p1;
      while (p1.x<>0) and (xValue(resolveCB[i].cp)+p1.v(vOffset)<=xValue(p)) do
       begin
        p2:=p1;
        ListNext(p1,p0);
       end;
      if xValue(resolveCB[i].cp)+p2.v(vOffset)+SystemWordSize>xValue(p) then
        li.SubItems.Add(#$95' '+StratoDumpThing(p2)+
          ' [#'+IntToStr(i)+']')
      else
        li.SubItems.Add(#$85' '+StratoDumpThing(p2));
     end;
   end;
end;

procedure TStratoMachine.lvMemData(Sender: TObject; li: TListItem);
var
  p,q:pointer;
  i:cardinal;
begin
  p:=FMem;
  inc(xValue(p),xValue(li.Index)*SystemWordSize);
  li.Caption:=PtrToStr(p);
  li.SubItems.Add(ItemToStrX(PxValue(p)^));
  if xValue(p)<xValue(FMemPastGlobals) then
   begin
    li.SubItems.Add('');

    i:=0;
    q:=FMem;
    while (i<FGlobalsIndex) and (xValue(q)<xValue(p)) do
     begin
      inc(xValue(q),FGlobals[i].ByteSize);
      pAlign(q);
      inc(i);
     end;
    if p=q then
      li.SubItems.Add(StratoDumpThing(FGlobals[i].Item))
    else
      li.SubItems.Add('');

   end
  else
   begin
    //TODO: lookup any vars in locals of codeblocks on stack
    li.SubItems.Add('');
    li.SubItems.Add('');
   end;
end;

end.

