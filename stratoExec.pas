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
    FGlobals:array of record Item:xNode; ByteSize:cardinal; end;
    FGlobalsSize,FGlobalsIndex:cardinal;
    FDebugView:TfrmDebugView;
    FDebugCount:integer;
    FData:TObject;
    procedure Perform(Entry:xNode);
    procedure RunError(p:xNode;const Msg:string);
    procedure LiteralToMemory(p:xNode;ptr:pointer);
    procedure PerformSysCall(Fn:xNode;Data:pointer);

    function VtoN(v:cardinal):xNode;
    function NtoV(n:xNode):cardinal;

  protected
    //used by FDebugView
    resolveCB:array[0..MaxResolveCB-1] of record
      cb:xNode;
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

uses Classes, Windows, CommCtrl, stratoFn, stratoTokenizer, stratoLogic,
  stratoParse, stratoTools;

const
  InitialMemSize=$100000;//?
  InitialStackSize=$10000;//?

  NtoV_Margin=$400;

{$IFDEF DEBUG}
type
  TCArr=record s:array[0..$FFFFFF] of cardinal; end;
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
  FData:=TStratoSphere.Create;
  //TODO: restore previous position
  if DoDebug then
   begin
    FDebugView:=TfrmDebugView.Create(nil);
    FDebugView.lvStack.OnData:=lvStackData;
    FDebugView.lvMem.OnData:=lvMemData;
    IncludeDictionaryNodes:=true;
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

function TStratoMachine.VtoN(v:cardinal):xNode;
var
  i,j:cardinal;
begin
  if v<=NtoV_Margin then Result.none else
   begin
    i:=0;
    j:=v-NtoV_Margin;
    while (i<SpheresCount) and (j>=Spheres[i].kCount) do
     begin
      dec(j,Spheres[i].kCount);
      inc(i);
     end;
    if i=SpheresCount then
      if j<(FData as TStratoSphere).kCount then
        Result.s(FData as TStratoSphere,j)
      else
        Result.none //raise Exception.Create('Unexpected node reference')
    else
      Result.s(Spheres[i],j);
   end;
end;

function TStratoMachine.NtoV(n:xNode):cardinal;
var
  i:cardinal;
begin
  if n.IsNone then Result:=NtoV_Margin else
   begin
    if n.sphere=FData then i:=SpheresCount else i:=SphereIndex(n.sphere)-1;
    Result:=NtoV_Margin+n.index;
    while (i<>0) do
     begin
      dec(i);
      inc(Result,Spheres[i].kCount);
     end;
   end;
end;

procedure TStratoMachine.Run;
var
  i,bs:cardinal;
  p,p1,p2:xNode;
begin
  //SystemWordSize:=SizeOf(pointer);
  //if Store.SystemWordSize>SystemWordSize then
  if SystemWordSize>SizeOf(pointer) then
    raise Exception.Create('Mismatching system word size');

  if FDebugView<>nil then FDebugView.Show;

  //allocate globals
  FGlobalsIndex:=0;
  for i:=0 to SpheresCount-1 do
   begin
    p:=Spheres[i].r(0,lSphere_Globals);
    while p.Next(p1) do
     begin
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
      p1.sphere.SetVal(p1.index,vOffset,0,xValue(FMemPastGlobals));//TODO: list here! not in sphere!
      p2:=p1.r(iValue);
      if not(p2.IsNone) then LiteralToMemory(p2,FMemPastGlobals); //else zeroes?
      inc(xValue(FMemPastGlobals),bs);
      pAlign(FMemPastGlobals);
     end;
   end;

  //TODO: plus margin?
  FMemIndex:=FMemPastGlobals;

  //TODO: halt on unhandled exception?
  //TODO: re-construct correct order based on dependencies?

  //initialization
  for i:=0 to SpheresCount-1 do
   begin
    p:=Spheres[i].r(0,iSphere_InitializationBlock);
    if not p.IsNone then Perform(p);
   end;

  //finalization
  for i:=SpheresCount-1 downto 0 do
   begin
    p:=Spheres[i].r(0,iSphere_FinalizationBlock);
    if not p.IsNone then Perform(p);
   end;

  if FDebugView<>nil then FDebugView.Done;
end;

procedure TStratoMachine.Perform(Entry:xNode);

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
  procedure Push(q:xNode); overload;
  var
    v:cardinal;
  begin
    if xValue(stackTop)+1>=xValue(stackBase)+InitialStackSize then
      raise Exception.Create('Stack overflow');//TODO: Throw
    v:=NtoV(q);
    Move(v,stackTop^,SystemWordSize);
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
  procedure Pop(var q:xNode); overload;
  var
    v:cardinal;
  begin
    if stackTop=stackBase then
      raise Exception.Create('Stack underflow');//TODO: Throw
    dec(xValue(stackTop),SystemWordSize);
    Move(stackTop^,v,SystemWordSize);
    q:=VtoN(v);
  end;

  procedure Peek(var x:pointer;var p:pointer); overload;
  begin
    if x=stackBase then
      raise Exception.Create('Stack underflow');//TODO: Throw
    dec(xValue(x),SystemWordSize);
    Move(x^,p,SystemWordSize);
  end;
  procedure Peek(var x:pointer;var p:xNode); overload;
  var
    v:cardinal;
  begin
    if x=stackBase then
      raise Exception.Create('Stack underflow');//TODO: Throw
    dec(xValue(x),SystemWordSize);
    Move(x^,v,SystemWordSize);
    p:=VtoN(v);
  end;

var
  OpCount:cardinal;
  ip,ipNext:xNode;//instruction pointer
  new:boolean;
  cp:pointer;//code block pointer
  ep:pointer;//exception pointer
  vp:pointer;//value pointer
  vt:xNode;

  function Pass:cardinal;
  begin
    if new then Result:=0 else Pop(Result);
  end;
  procedure Next(nPass:cardinal); overload;
  begin
    Push(nPass);
    Push(ip);
  end;
  procedure Next(nPass:cardinal;nNext:xNode); overload;
  begin
    Push(nPass);
    Push(ip);
    if not(nNext.IsNone) then ipNext:=nNext;
    //TODO: vp:=nil;vt:=0; here?
  end;

  function Volatile(nvt:xNode):pointer;
  begin
    //TODO: separate register?
    vp:=stackTop;
    vt:=nvt;
    //inc(xValue(???),ByteSize(Sphere,vt));?
    Result:=vp;
  end;

  procedure CbStart(cb:xNode;var np:pointer);
  var
    q1:xNode;
  begin
    Push(cp);
    Push(cb);
    np:=stackTop;
    //allocate space for local variables
    ReserveStack(cb.v(vByteSize));
    q1.Start(cb,lCodeBlock_Statements);
    q1.Next(ipNext);
    Push(q1);
    Push(cb);
  end;

  function RefreshDebugView: boolean;
  var
    li:TListItem;
    p,p0:pointer;
    p1,q1,q2:xNode;
    i,v,sx,sy:cardinal;
  begin
    //TODO: move this to other unit
    inc(OpCount);
    if (FDebugView<>nil) and (FDebugView.cbKeepTrail.Checked) then
     begin
      FDebugView.lvTrail.Items.BeginUpdate;
      try
        li:=FDebugView.lvTrail.Items.Add;
        li.Caption:=IntToStr(OpCount);
        li.SubItems.Add(ip.AsString);
        li.SubItems.Add(PtrToStr(vp));
        li.SubItems.Add(vt.AsString);
        li.SubItems.Add('');//ItemToStr(bp));
        li.SubItems.Add(PtrToStr(cp));
        li.SubItems.Add(PtrToStr(ep));
        li.SubItems.Add(PtrToStr(stackTop));
        li.SubItems.Add(string(StratoDumpThing(ip)));
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
          resolveCB[i].cb.none;
          resolveCB[i].cp:=nil;
          resolveCB[i].cq:=nil;
         end
        else
         begin
          //assert (q>stackBase) and (q<stackTop)
          resolveCB[i].cp:=p;
          dec(xValue(p),SystemWordSize);
          resolveCB[i].cb:=VtoN(PCardinal(p)^);
          if resolveCB[i].cb.Key=nCodeBlock then
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
            [ip.AsString,StratoDumpThing(ip)]))
        else
          FDebugView.txtUpNext.Lines.Add(Format('ip: %s: %s',
            [ip.AsString,StratoDumpThing(ip)]));
        if vp=nil then
          FDebugView.txtUpNext.Lines.Add('vp')
        else
          FDebugView.txtUpNext.Lines.Add(Format('vp: $%.8x',
            [xValue(vp)]));//,StratoDumpThing(Sphere,p1)]));
        if vt.IsNone then
          FDebugView.txtUpNext.Lines.Add('vt')
        else
          FDebugView.txtUpNext.Lines.Add(Format('vt: %s: %s',
            [vt.AsString,StratoDumpThing(vt)]));
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
        if not(ip.IsNone) and StratoGetSourceFile(ip,sy,sx,q1,q2) then
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
          while p<>stackBase do
           begin
            li:=FDebugView.lvStackTrace.Items.Add;
            li.Caption:=PtrToStr(p);
            Peek(p,pointer(v));
            if v<NtoV_Margin then
             begin
              li.SubItems.Add(IntToStr(v));
              li.SubItems.Add('');
              li.SubItems.Add('');
              li.SubItems.Add('');
              li.SubItems.Add('');
              li.SubItems.Add('');
              li.SubItems.Add('');
             end
            else
             begin
              p1:=VtoN(v);
              li.SubItems.Add(p1.AsString);
              while p1.Key=nCodeBlock do
                p1:=p1.r(iParent);
              li.SubItems.Add(p1.AsString);
              li.SubItems.Add(string(StratoDumpThing(p1)));
              li.SubItems.Add(string(StratoDumpThing(p1.r(iParent).r(iParent))));//?
              if StratoGetSourceFile(p1,sy,sx,q1,q2) and (sy<>0) then
               begin
                li.SubItems.Add(UTF8ToString(
                  p1.sphere.BinaryData(p1.sphere.r(0,iSphere_FileName).index)));
                li.SubItems.Add(IntToStr(sx));
                li.SubItems.Add(IntToStr(sy));
               end
              else
               begin
                li.SubItems.Add('');
                li.SubItems.Add('');
                li.SubItems.Add('');
               end;
             end;
            //Peek(p,p);
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
  p1,p2,q1,q2,r1:xNode;
  xp,yp,zp:pointer;

  x,y:cardinal;
  xx,yy:int64;
  ipt:xKey;
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
    vt.none;

    while not(ip.IsNone) do
     begin

      if RefreshDebugView then
        asm int 3 end;//DebugBreak;//forced breakpoint

      //try

      //TODO: check ip with block base pointers
      ipNext.none;
      ipt:=ip.Key;
      case ipt of

      nCodeBlock:
        if new then
         begin
          CbStart(ip,cp);
          vp:=nil;
          vt.none;
         end
        else
         begin
          Pop(q1);
          q1.Next(q2);
          if q2.IsNone then
           begin
            //resulting value
            if ip.r(iReturnType).IsNone then
             begin
              //if vt<>0 then raise?throw?
              vt.none;
              vp:=nil;
             end
            else
             begin
              if vt.IsNone then
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
            Pop(q1);//=ip (used for debug display)
            Pop(cp);
           end
          else
           begin
            //next statement
            Push(q1);
            Push(ip);
            ipNext:=q2;
            vt.none;
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
            q2.none;//see below
            case ipt of
              nFCall:
               begin
                p1:=ip.r(iTarget);
                if p1.IsNone then RunError(ip,'F-Call target not defined');
               end;
              nVCall:
               begin
                if vt.IsNone then RunError(ip,'V-Call subject not resolved');
                p1:=StratoFnCallFindVirtual(ip,vt,vp);
                if p1.IsNone then
                 begin
                  RunError(ip,'V-Call target implementation not found');
                  q2.none;
                 end
                else
                 begin
                  Push(p1);
                  if p1.Key=nCtor then q2:=VtoN(PCardinal(vp)^);
                 end;
               end;
              nICall:
               begin
                p1:=ip.r(iSubject);
                if p1.IsNone then RunError(ip,'I-Call subject not defined');
                p1:=ip.r(iTarget);
                if p1.IsNone then RunError(ip,'I-Call target not defined');
               end;
            end;

            if not p1.IsNone then
             begin
              Next(3);
              CbStart(p1.r(iBody),xp);//not cp here! see below

              case p1.Key of

                nOverload,nPropGet,nPropSet:
                 begin
                  p2.Start(p1.r(iBody),lCodeBlock_Locals);
                  if p2.Next(p2) and (p2.Key=nThis) then
                   begin
                    //assert "@@".vOffset=0
                    //TODO: check SameType(t,ResType("@@"?
                    case vt.Key of //auto-dereference
                      nClass:PCardinal(xp)^:=NtoV(q1);
                      else PCardinal(xp)^:=cardinal(vp);
                    end;
                   end;
                  end;

                nCtor:
                 begin
                  PCardinal(xp)^:=0;//"@@":this default nil until malloc
                  yp:=xp;
                  inc(xValue(yp),SystemWordSize);
                  //calling an inherited constructor?
                  q1.none;
                  if (ipt=nFCall) or (cp=nil) then p2.none else
                   begin
                    zp:=cp;
                    Peek(zp,p2);
                    q1:=p2.r(iParent);
                    while q1.Key=nCodeBlock do
                     begin
                      Peek(zp,zp);
                      if zp=nil then
                       begin
                        p2.none;
                        q1.none;
                       end
                      else
                       begin
                        Peek(zp,p2);
                        q1:=p2.r(iParent);
                       end;
                     end;
                   end;
                  if q1.Key=nCtor then
                   begin
                    inc(xValue(zp),SystemWordSize*2); //var "?@@"
                    PCardinal(yp)^:=PCardinal(zp)^;
                   end
                  else
                   begin
                    //q2 from subject evaluation above!
                    if q2.IsNone then q2:=ip.r(iTarget).r(iSignature).r(iReturnType);
                    PCardinal(yp)^:=NtoV(q2);
                   end;
                 end;

                nDtor:
                  PCardinal(xp)^:=NToV(q2);//assert "@@".vOffset=0

                else
                  raise Exception.Create('//TODO:');
              end;

              //arguments to evaluate first?
              q1.Start(ip,lArguments);
              if not(q1.Next(q1)) then
               begin
                cp:=xp;
                //ipNext:= by CbStart above
               end
              else
               begin
                Push(xp);//new cp after arguments
                if ipt=nVCall then Push(p1);//nOverload
                Push(ip.r(iTarget).r(iSignature).rl(lArguments));//first sig arg
                Push(p2);//nCallArg
                Next(2,q1.r(iValue));
               end;
             end;

            vp:=nil;
            vt.none;
           end;

          2://store argument value
           begin
            Pop(p2);//nCallArg
            Pop(q2);//nVarDecl
            if ipt=nVCall then Pop(p1) else p1:=ip.r(iTarget);
            Pop(xp);//new cp after arguments

            if q2.Next(r1) then
             begin
              r1:=r1.r(iArgVar);
              if r1.IsNone then
                RunError(ip,'Unable to obtain argument variable');
             end
            else
              RunError(ip,'Unexpectedly ran out of signature arguments');

            //store value
            yp:=xp;
            inc(xValue(yp),r1.v(vOffset));
            if p2.Key=nSigArgByRef then
              PCardinal(yp)^:=cardinal(vp) //Move(vp,yp^,SystemWordSize)
            else
              Move(vp^,yp^,ByteSize(vt));

            //next argument
            p2.Next(q2);
            if q2.IsNone then
             begin
              //all done, do first of body (StartCB already done by Pass=1 above)
              cp:=xp;
              q1:=p1.r(iBody);
              //Sphere.First(q1,fItems,ipNext,);
              ipNext.Start(q1,lCodeBlock_Statements);
              ipNext.Next(ipNext);
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
            vt.none;
           end;

          3://post-block (returned value?)
           begin
            if ipt=nVCall then Pop(p1) else p1:=ip.r(iTarget);

            //inherited constructor? propagate this
            if p1.Key=nCtor then
             begin
              xp:=stackTop;//previous 'cp' from freshly ended codeblock of body
              inc(xValue(xp),SystemWordSize*4);
              if ipt=nVCall then inc(xValue(xp),SystemWordSize);
              vp:=xp;
              inc(xValue(xp),SystemWordSize);
              vt:=VtoN(PCardinal(xp)^);

              //calling an inherited constructor?
              if cp=nil then p2.none else
               begin
                xp:=cp;
                Peek(xp,p2);
                while p2.r(iParent).Key=nCodeBlock do
                 begin
                  Peek(xp,xp);
                  if xp=nil then p2.none else Peek(xp,p2);
                 end;
               end;
              if p2.r(iParent).Key=nCtor then
               begin
                inc(xValue(xp),SystemWordSize);
                PCardinal(xp)^:=PCardinal(vp)^;
               end;
             end
            else
             begin
              //check return value
              q2:=p1.r(iSignature).r(iReturnType);
              //if q2<>0 and p1.iBody.iReturnType=0 then?
              if q2.IsNone then
               begin
                //if vt<>0 then raise?throw?
                vt.none;
                vp:=nil;
               end
              else
               begin
                //assert return value first (past this)
                if vt.IsNone then
                 begin
                  vt:=q2;
                  q1.Start(p1.r(iBody),lCodeBlock_Locals);
                  q1.Next(q2);
                  if q2.Key=nThis then q1.Next(q2);
                  xp:=stackTop;//previous 'cp' from freshly ended codeblock of body
                  inc(xValue(xp),SystemWordSize*4);
                  if ipt=nVCall then inc(xValue(xp),SystemWordSize);
                  inc(xValue(xp),q2.v(vOffset));
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
        q1.Start(ip,lArguments);
        if q1.Next(q2) then
          RunError(ip,'interpreter doesn''t support syscalls with arguments');
        PerformSysCall(ip,cp);
       end;

      nClass:
        //TODO: all Sphere.Add(nClass do Sphere.Add(nClassRef onbeforehand?
       begin
        q1.sphere:=(FData as TStratoSphere);
        q1.index:=q1.sphere.Add(nClassRef,2);
        q1.sphere.SetRef(q1.index,iTarget,ip);
        PCardinal(Volatile(q1))^:=NToV(ip);
       end;

      nVar,nVarReadOnly,nVarByRef,nThis://calculate address
       begin
        //assert vp=nil
        vt:=ip.r(iType);
        vp:=nil;//calulated below
        q1:=ip;
        while not q1.IsNone do
         begin
          q2.none;//next...
          case q1.Key of
            nVar,nVarReadOnly,nThis://offset
             begin
              inc(xValue(vp),q1.v(vOffset));
              q2:=q1.r(iParent);
             end;

            nVarByRef:
             begin
              xValue(vp):=PCardinal(vp)^;
              q2.none;
             end;

            nNameSpace://was global var
              ;//end loop (assert vOffset set by init)
            nCodeBlock://was local var
             begin
              //but to which code block: go up the chain
              xp:=cp;
              yp:=nil;
              p2.none;
              while (xp<>nil) and not(p2.IsSame(q1)) do
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
            if vt.IsNone then
              RunError(ip,'array to index into didn''t resolve');
            Push(vp);
            //TODO: more than one array index
            q1.Start(ip,lArguments);
            q1.Next(q1);
            Next(2,q1.r(iValue));//nCalArg
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
            if vt.IsNone then
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
            while not p2.IsNone do
             begin
              p1:=p2;
              p2.none;//default
              case p1.Key of
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
                if ip.r(iRight).Key=nThis then
                 begin
                  if cp=nil then p2.none else
                   begin
                    xp:=cp;
                    Peek(xp,p2);
                    while p2.r(iParent).Key=nCodeBlock do
                     begin
                      Peek(xp,xp);
                      if xp=nil then p2.none else Peek(xp,p2);
                     end;
                   end;
                  if p2.r(iParent).Key=nCtor then
                   begin
                    inc(xValue(xp),SystemWordSize*2); //var "?@@"
                    case TStratoToken(ip.v(vOperator)) of
                      stOpSizeOf:
                        PCardinal(Volatile(IntrinsicTypes[itNumber]))^:=
                          VtoN(PCardinal(xp)^).v(vByteSize);
                      stQuestionMark:
                        PCardinal(Volatile(IntrinsicTypes[itNumber]))^:=
                          PCardinal(xp)^;
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
                if vt.IsSame(IntrinsicTypes[itBoolean]) then
                  if PCardinal(vp)^=0 then PCardinal(vp)^:=1 else PCardinal(vp)^:=0
                else
                  RunError(ip,'Unknown operation');
              stOpSizeOf:
               begin
                if vt.IsSame(IntrinsicTypes[itType]) then
                  x:=ByteSize(VtoN(PCardinal(vp)^))
                else
                  x:=ByteSize(vt);
                PCardinal(Volatile(IntrinsicTypes[itNumber]))^:=x;
               end;
              stQuestionMark://type of
                if vt.Key=nClass then
                 begin
                  //live object? extract base class
                  xp:=pointer(vp^);
                  //inc(xValue(xp),Lookup(itObject,Name('_baseclass')).v(vOffset));
                  dec(xValue(xp),SystemWordSize);//assert object._baseclass offset -4
                  PCardinal(Volatile(IntrinsicTypes[itType]))^:=PCardinal(xp)^;
                  //Sphere.Add(nClassRef,[iTarget,PxValue(xp)^]);?
                 end
                else
                  PCardinal(Volatile(IntrinsicTypes[itType]))^:=NtoV(vt);
              else RunError(ip,'Unknown operator');
            end;
        end;

      nBinaryOp:
        case Pass of
          0://evaluate left
            Next(1,ip.r(iLeft));
          1://evaluate right
           begin
            //stored volatile on stack? keep it there!
            xp:=stackTop;
            inc(xValue(xp),SystemWordSize*2);
            if cardinal(vp)>=cardinal(xp) then
             begin
              x:=ByteSize(vt);
              Move(vp^,stackTop^,x);
              inc(xValue(stackTop),x+SystemWordSize*2);
              Push(x);
              Push(0);
             end
            else
              Push(vp);
            Push(vt);
            Next(2,ip.r(iRight));
            vp:=nil;
            vt.none;
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
              //inc(xValue(xp),SystemWordSize*2);
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
                  Volatile(IntrinsicTypes[itBoolean]);//sets vp
                  case TStratoToken(ip.v(vOperator)) of
                    stOpEQ:
                      if x=0 then PCardinal(vp)^:=1 else PCardinal(vp)^:=0;
                    stOpNEQ:
                      if x=0 then PCardinal(vp)^:=0 else PCardinal(vp)^:=1;
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
                if p1.IsSame(IntrinsicTypes[itString]) and
                   vt.IsSame(IntrinsicTypes[itString]) then
                 begin
                  //TODO: strings in memory (by runtime?)
                  q1:=VtoN(PCardinal(xp)^);
                  q2:=VtoN(PCardinal(vp)^);

                  p2.sphere:=FData as TStratoSphere;
                  p2.index:=p2.sphere.AddBinaryData(
                    q1.sphere.BinaryData(q1.index)+
                    q2.sphere.BinaryData(q2.index));
                  PCardinal(Volatile(IntrinsicTypes[itString]))^:=NtoV(p2);
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
                  Volatile(IntrinsicTypes[itBoolean]);//sets vp
                  case TStratoToken(ip.v(vOperator)) of
                    stOpLT: if xx<yy  then PCardinal(vp)^:=1 else PCardinal(vp)^:=0;
                    stOpLTE:if xx<=yy then PCardinal(vp)^:=1 else PCardinal(vp)^:=0;
                    stOpGT: if xx>yy  then PCardinal(vp)^:=1 else PCardinal(vp)^:=0;
                    stOpGTE:if xx>=yy then PCardinal(vp)^:=1 else PCardinal(vp)^:=0;
                  end;
                 end
                else
                  RunError(ip,'Unknown operation');
              stOpAnd,stOpOr,stOpXor:
                if p1.IsSame(IntrinsicTypes[itBoolean]) and
                   vt.IsSame(IntrinsicTypes[itBoolean]) then
                 begin
                  x:=PCardinal(xp)^;
                  y:=PCardinal(vp)^;
                  Volatile(IntrinsicTypes[itBoolean]);//sets vp
                  case TStratoToken(ip.v(vOperator)) of
                    stOpAnd:if (x<>0) and (y<>0) then PCardinal(vp)^:=1 else PCardinal(vp)^:=0;
                    stOpOr: if (x<>0) or  (y<>0) then PCardinal(vp)^:=1 else PCardinal(vp)^:=0;
                    stOpXor:if (x<>0) xor (y<>0) then PCardinal(vp)^:=1 else PCardinal(vp)^:=0;
                  end;
                 end
                else
                  RunError(ip,'Unknown operation');
              //TODO: stOpSub,stOpMul,stOpDiv,stOpMod,stOpShl,stOpShr,stThreeLT,stThreeGT:
              //TODO: stOpNEQ,stOpLT,stOpLTE,stOpGT,stOpGTE:
              //TODO: stOpAnd,stOpOr,stOpXor
              //stOpWhatIs: //'type is'
              else RunError(ip,'Unknown operator');
            end;
           end;
        end;

      nAssign:
        case Pass of
          0://evaluate left
           begin
            p1:=ip.r(iTarget);
            if p1.Key=nCast then //'dirty cast'?
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
            vt.none;
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
            vt.none;
           end;
          3://dirty cast, evaluate right
           begin
            Push(vp);//vt?
            Next(4,ip.r(iValue));
            vp:=nil;
            vt.none;
           end;
          4://dirty cast, push value
           begin
            Pop(xp);//pt? assert =vt
            case TStratoToken(ip.v(vOperator)) of
              stOpAssign://plain
                Move(vp^,xp^,ByteSize(ip.r(iTarget).r(iType)));
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
            vt.none;
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
          if vt.IsSame(IntrinsicTypes[itString]) and IsIntrinsicNumeric(p1) then
           begin
            q1:=VtoN(PCardinal(vp)^);
            if not TryStrToInt64(string(
              q1.sphere.BinaryData(q1.index)),xx) then
              RunError(ip,'invalid integer value');//TODO: raise
            PCardinal(Volatile(p1))^:=xx;//Move(xx,vp^,ByteSize(p1));
           end
          else
          if IsIntrinsicNumeric(vt) and p1.IsSame(IntrinsicTypes[itString]) then
           begin
            x:=ByteSize(vt);
            xx:=0;
            Move(vp^,xx,x);
            q1.sphere:=FData as TStratoSphere;
            q1.index:=q1.sphere.AddBinaryData(
              UTF8String(IntToStr(xx)));
            PCardinal(Volatile(p1))^:=NtoV(q1);//Move(q1,vp^,ByteSize(p1));
           end
          else
          if vt.IsSame(IntrinsicTypes[itBoolean]) and
             p1.IsSame(IntrinsicTypes[itString]) then
           begin
            x:=PCardinal(vp)^;//Move(vp^,x,SystemWordSize);
            q1.sphere:=FData as TStratoSphere;
            if x=0 then
              q1.index:=q1.sphere.AddBinaryData('0')
            else
              q1.index:=q1.sphere.AddBinaryData('1');
            PCardinal(Volatile(p1))^:=NtoV(q1);//Move(q1,vp^,SystemWordSize);
           end
          else
          if vt.IsSame(IntrinsicTypes[itNumber]) and (p1.Key=nEnum) then
           begin
            x:=PCardinal(vp)^;//Move(vp^,x,SystemWordSize);
            //check enumeration index in range?
            PCardinal(Volatile(p1))^:=x;//Move(x,vp^,SystemWordSize);
           end
          else
          if (vt.Key=nEnum) and p1.IsSame(IntrinsicTypes[itNumber]) then
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
          if vt.IsSame(IntrinsicTypes[itNumber]) and
             p1.IsSame(IntrinsicTypes[itPointer]) then
           begin
            x:=PCardinal(vp)^;//Move(vp^,x,SystemWordSize);
            //if i<>x then //TODO: protect against pointer arith
            PCardinal(Volatile(p1))^:=x;//Move(x,vp^,SystemWordSize);
           end
          else
          if not(IntrinsicTypes[itObject].IsNone) and
            (vt.Key=nClass) and (p1.Key=nClass) then
           begin
            //cast to base class?
            if vp=nil then q1:=vt else
             begin
              //dereference
              xp:=pointer(PCardinal(vp)^-SystemWordSize);
              //assert object._baseclass @-SystemWordSize
              q1:=VtoN(PCardinal(xp)^);
             end;
            while not(q1.IsNone) and not(q1.IsSame(p1)) do
              q1:=q1.r(iInheritsFrom);
            if not q1.IsNone then
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
            if not vt.IsSame(IntrinsicTypes[itBoolean]) then
              RunError(ip,'selection predicate didn''t evaluate to boolean')
            else
              if PCardinal(vp)^=0 then
                ipNext:=ip.r(iDoFalse)
              else
                ipNext:=ip.r(iDoTrue);
        end;

      nIteration:
       begin
        p1:=ip.r(iPredicate);
        if p1.Key=nRangeIndex then
          case Pass of
            0://first resolve iterator
              Next(1,p1.r(iLeft));
            1://store it on stack, then range start
             begin
              if not vt.IsSame(IntrinsicTypes[itNumber]) then
                RunError(ip,'iteration currently only supports number ranges');//TODO
              Push(vp);
              Next(2,p1.r(iRight).r(iLeft));
             end;
            2://store range start, then range end
             begin
              if not vt.IsSame(IntrinsicTypes[itNumber]) then
                RunError(ip,'iteration currently only supports number ranges');//TODO
              Pop(xp);
              PCardinal(xp)^:=PCardinal(vp)^;
              Push(xp);
              Next(3,p1.r(iRight).r(iRight));
             end;
            3://store range end, then do body
             begin
              if not vt.IsSame(IntrinsicTypes[itNumber]) then
                RunError(ip,'iteration currently only supports number ranges');//TODO
              Push(PCardinal(vp)^);
              Next(4,ip.r(iBody));
             end;
            4://body done, iterate more or is iteration done?
             begin
              Pop(x);
              Pop(xp);
              inc(PCardinal(xp)^);
              if PCardinal(xp)^>x then
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
              if not vt.IsSame(IntrinsicTypes[itBoolean]) then
                RunError(ip,'iteration predicate didn''t evaluate to boolean')
              else
                if PCardinal(vp)^=0 then
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
            if not vt.IsSame(IntrinsicTypes[itBoolean]) then
              RunError(ip,'iteration predicate didn''t evaluate to boolean')
            else
              if PCardinal(vp)^=0 then
                //done
              else
                Next(1,ip.r(iBody));
        end;

      else
        RunError(ip,'unknown logic item '+ip.AsString+':'+
          KeyToStr(ipt));
      end;

      {
      except
        on e:Exception do
         begin
          RunError(ip,'Fatal:['+e.ClassName+']'+e.Message);
          //TODO: proper !!!throw
          //TODO: show on debugview!!!
         end;
      end;
      }

      {//TODO
      if (vt0<>0) and (vt=vt0) then
       begin
        RunError(ip,'unused resulting value');
        vp:=nil;
        vt:=0;
       end;
      }

      if ipNext.IsNone then
       begin
        if stackTop=stackBase then ip.none else Pop(ip);
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
    VirtualFree(stackBase,0,MEM_RELEASE);//TODO: move this to DebugView close?
  end;
end;

procedure TStratoMachine.RunError(p:xNode;const Msg:string);
var
  sx,sy:cardinal;
  p1,p2:xNode;
begin
  if StratoGetSourceFile(p,sx,sy,p1,p2) then
    Writeln(ErrOutput,Format('%s(%d:%d): %s',
      [p.sphere.BinaryData(p.sphere.r(0,iSphere_FileName).index)
      ,sx,sy,Msg
      ]))
  else
    Writeln(ErrOutput,Msg);
  ExitCode:=1;
end;

procedure TStratoMachine.LiteralToMemory(p:xNode;ptr:pointer);
var
  i:int64;
  q,r:xNode;
begin
  if p.Key<>nLiteral then
    RunError(p,'literal expected '+p.AsString)
  else
   begin
    r:=p.r(iType);
    //assert q=0 or rType(r)=nType
    if SameType(r,IntrinsicTypes[itString]) then
     begin
      //TODO: store strings in conceptual mem (see also stratoRunTime)
      PCardinal(ptr)^:=NtoV(p.r(iValue));
     end
    else
    if SameType(r,IntrinsicTypes[itBoolean]) then
     begin
      if p.sphere.BinaryData(p.index)='0' then
        i:=0
      else
        i:=1;
      Move(i,ptr^,SystemWordSize);
     end
    //if SameType(r,xxr(IntrinsicTypes[itNumber])) then
    else
     begin
      //TODO: not ParseInteger here, but store(d) binary?
      q:=p.r(iValue);
      i:=ParseInteger(q.sphere.BinaryData(q.index));
      Move(i,ptr^,r.v(vByteSize));
     end;
    //else
    //  Sphere.Error(p,'unsupported literal type');
   end;
end;

procedure TStratoMachine.PerformSysCall(Fn:xNode;Data:pointer);
var
  p:pointer;
  q,q1:xNode;
  i:integer;
  f:TFileStream;
  s:UTF8String;
const
  UTF8ByteOrderMark:UTF8STring=#$EF#$BB#$BF;
begin
  q:=Fn.r(iTarget);
  if q.Key<>nLiteral then
    raise Exception.Create('SysCall: unexpected target');
  q1:=q.r(iValue);
  i:=ParseInteger(q1.sphere.BinaryData(q1.index));
  case i of

  xSCall_inc://xinc
   begin
    p:=pointer(Data^);
    inc(xValue(p^));
   end;
  xSCall_dec://xdec
   begin
    p:=pointer(Data^);
    dec(xValue(p^));
   end;

  xSCall_malloc://malloc
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

  xSCall_writeln://write line
   begin
    q1:=VtoN(PCardinal(Data)^);
    Writeln(string(q1.sphere.BinaryData(q1.index)));
   end;

  xSCall_filetostr://read a file into a string
   begin
    //TODO: try except throw
    //TODO: added security: limit access to specific folder
    q1:=VtoN(PCardinal(Data)^);
    f:=TFileStream.Create(string(q1.sphere.BinaryData(q1.index)),
      fmOpenRead or fmShareDenyWrite);
    try
      i:=f.Size-3;
      s:=#0#0#0;
      f.Read(s,3);
      if s<>UTF8ByteOrderMark then
        RunError(Fn,'Only UTF8-files supported');
      SetLength(s,i);
      f.Read(s[1],i);
    finally
      f.Free;
    end;
    q1.index:=q1.sphere.AddBinaryData(s);
    PCardinal(Data)^:=NtoV(q1);
   end;

  xSCall_strtofile://write a string to a file
   begin
    //TODO: added security: limit access to specific folder
    q1:=VtoN(PCardinal(Data)^);
    f:=TFileStream.Create(string(q1.sphere.BinaryData(q1.index)),
      fmCreate);
    try
      f.Write(UTF8ByteOrderMark[1],3);
      q1:=VtoN(PCardinal(Data)^);////????
      s:=q1.sphere.BinaryData(q1.index);
      f.Write(s[1],Length(s));
    finally
      f.Free;
    end;
   end;

  xSCall_filetomem://read a file into memory
   begin
    //TODO: try except throw
    //TODO: added security: limit access to specific folder
    q1:=VtoN(PCardinal(Data)^);
    f:=TFileStream.Create(string(q1.sphere.BinaryData(q1.index)),
      fmOpenRead or fmShareDenyWrite);
    try
      i:=f.Size;
      if xValue(FMemIndex)-xValue(FMem)+xValue(i)>FMemSize then
        RunError(Fn,'Out of memory')//TODO:throw
      else
       begin
        p:=FMemIndex;
        inc(xValue(FMemIndex),i);
        //TODO: align?
        //TODO: mark allocated? (see also deallocation)
       end;
      f.Read(p^,i);
    finally
      f.Free;
    end;
    pointer(Data^):=p;
   end;

  xSCall_memtofile://write a block of data to a file
   begin
    //TODO: added security: limit access to specific folder
    p:=Data;
    inc(xValue(p),SystemWordSize);
    i:=PCardinal(p)^;
    inc(xValue(p),SystemWordSize);
    q1:=VtoN(PCardinal(p)^);
    f:=TFileStream.Create(string(q1.sphere.BinaryData(q1.index)),
      fmCreate);
    try
      //TODO: check correct memory, fully allocated
      p:=pointer(Data^);
      f.Write(p,i);
    finally
      f.Free;
    end;
    //PCardinal(Data^):=i;//?
   end;

  else
    raise Exception.Create('SysCall: unknown key '+IntToStr(i));
  end;
end;

procedure TStratoMachine.lvStackData(Sender: TObject; li: TListItem);
var
  p:pointer;
  v:cardinal;
  p1,p2,p0:xNode;
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

  v:=0;
  p1.none;
  try
    v:=PCardinal(p)^;
    li.SubItems.Add(IntToStr(v));
    if v<NtoV_margin then
      li.SubItems.Add('')
    else
    if v>=cardinal(stackBase) then
      li.SubItems.Add(PtrToStr(pointer(v)))
    else
     begin
      p1:=VtoN(v);
      if p1.IsNone then
        li.SubItems.Add('')
      else
        li.SubItems.Add(p1.AsString);
     end;

    if i=MaxResolveCB then
      if p1.IsNone then
        li.SubItems.Add('')
      else
        if p1.Key=nCodeBlock then
          li.SubItems.Add('{ '+string(StratoDumpThing(p1.r(iParent))))
        else
          li.SubItems.Add(string(StratoDumpThing(p1)))
    else
     begin
      p0.Start(resolveCB[i].cb,lCodeBlock_Locals);
      p0.Next(p1);
      if p1.IsNone then
        li.SubItems.Add('')
      else
       begin
        p2:=p1;
        while not(p1.IsNone) and (xValue(resolveCB[i].cp)+p1.v(vOffset)<=xValue(p)) do
         begin
          p2:=p1;
          p0.Next(p1);
         end;
        if xValue(resolveCB[i].cp)+p2.v(vOffset)+SystemWordSize>xValue(p) then
          li.SubItems.Add(#$95' '+string(StratoDumpThing(p2))+
            ' [#'+IntToStr(i)+']')
        else
          li.SubItems.Add(#$85' '+string(StratoDumpThing(p2)));
       end;
     end;

  except
    on e:Exception do
     begin
      if p1.IsNone then
        if v=0 then
          li.SubItems.Add('')
        else
          li.SubItems.Add(IntToStr(v))
      else
        li.SubItems.Add(p1.AsString);
      li.SubItems.Add('!!!['+e.ClassName+']'+e.Message);
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
  li.SubItems.Add(VtoN(PCardinal(p)^).AsString);
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
      li.SubItems.Add(string(StratoDumpThing(FGlobals[i].Item)))
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

