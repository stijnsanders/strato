unit stratoExec;

interface

uses SysUtils, stratoDecl, stratoSphere, stratoDebug, stratoDebugView;

type
  TStratoMachine=class(TObject)
  private
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
  public
    constructor Create(DoDebug:boolean=false);
    destructor Destroy; override;
    procedure Run;
  end;

implementation

uses Windows, stratoFn, stratoTokenizer, ComCtrls, stratoLogic, stratoParse,
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
    FDebugView:=TfrmDebugView.Create(nil)
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
      ListNext(p,p0);
     end;
   end;

  FMemIndex:=FMemPastGlobals;//TODO: plus margin? 

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

type
  qItem=type xItem;

procedure TStratoMachine.Perform(Entry:rItem);
var
  stackBase,stackTop:pointer;
  procedure Push(i:cardinal); overload;
  begin
    if cardinal(stackTop)>=cardinal(stackBase)+InitialStackSize then
      raise Exception.Create('Stack overflow');//TODO: Throw
    Move(i,stackTop^,SystemWordSize);
    inc(cardinal(stackTop),SystemWordSize);
  end;
  procedure Push(p:pointer); overload;
  begin
    if cardinal(stackTop)>=cardinal(stackBase)+InitialStackSize then
      raise Exception.Create('Stack overflow');//TODO: Throw
    Move(p,stackTop^,SystemWordSize);
    inc(cardinal(stackTop),SystemWordSize);
  end;
  procedure Push(q:rItem); overload;
  begin
    if cardinal(stackTop)+1>=cardinal(stackBase)+InitialStackSize then
      raise Exception.Create('Stack overflow');//TODO: Throw
    Move(q,stackTop^,SystemWordSize);
    inc(cardinal(stackTop),SystemWordSize);
  end;
  procedure ReserveStack(s:cardinal);
  begin
    //ATTENTION: caller must prevent natural popping from the reserved memory
    if cardinal(stackTop)+s>=cardinal(stackBase)+InitialStackSize then
      raise Exception.Create('Stack overflow');//TODO: Throw
    ZeroMemory(stackTop,s);//?
    inc(cardinal(stackTop),s);
  end;
  procedure Pop(var i:cardinal); overload;
  begin
    if stackTop=stackBase then
      raise Exception.Create('Stack underflow');//TODO: Throw
    dec(cardinal(stackTop),SystemWordSize);
    Move(stackTop^,i,SystemWordSize);
  end;
  procedure Pop(var p:pointer); overload;
  begin
    if stackTop=stackBase then
      raise Exception.Create('Stack underflow');//TODO: Throw
    dec(cardinal(stackTop),SystemWordSize);
    Move(stackTop^,p,SystemWordSize);
  end;
  procedure Pop(var q:rItem); overload;
  begin
    if stackTop=stackBase then
      raise Exception.Create('Stack underflow');//TODO: Throw
    dec(cardinal(stackTop),SystemWordSize);
    Move(stackTop^,q,SystemWordSize);
  end;

  procedure Peek(var x:pointer;var p:rItem); overload;
  begin
    if x=stackBase then
      raise Exception.Create('Stack underflow');//TODO: Throw
    dec(cardinal(x),SystemWordSize);
    Move(x^,p,SystemWordSize);
  end;
  procedure Peek(var x:pointer;var p:pointer); overload;
  begin
    if x=stackBase then
      raise Exception.Create('Stack underflow');//TODO: Throw
    dec(cardinal(x),SystemWordSize);
    Move(x^,p,SystemWordSize);
  end;

  function PtrToStr(p:pointer):string;
  begin
    Result:=Format('$%.8x',[cardinal(p)]);
  end;
  function ItemToStrX(p:xValue):string;
  begin
    if p<200 then
      Result:=IntToStr(p)
    else
    if p>=BlocksCount * StratoSphereBlockBase then
      Result:=Format('$%.8x',[p])
    else
      Result:=ItemToStr(xxr(p));
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
  const
    MaxResolveCB=8;
  var
    li,li1:TListItem;
    p,q:pointer;
    cp0:array[0..MaxResolveCB-1] of pointer;
    p1,q1,q2,cv,cv0:rItem;
    sx,sy,ii,j,cpX:cardinal;
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
      li:=nil;//default;
      FDebugView.lvStack.Items.BeginUpdate;
      try
        FDebugView.lvStack.Items.Clear;
        //TODO: work up stack to list cb's first, then resolve local var names (new column)
        p:=stackBase;
        cpX:=0;
        for j:=0 to MaxResolveCB-1 do cp0[j]:=nil;
        cv.x:=0;
        while cardinal(p)<cardinal(stackTop) do
         begin
          li:=FDebugView.lvStack.Items.Add;
          li.Caption:=PtrToStr(p);
          p1.x:=PCardinal(p)^;
          li.SubItems.Add(ItemToStrX(p1.x));
          if cv.x=0 then
            if (p1.x<200) or (p1.x>=BlocksCount * StratoSphereBlockBase) then
              li.SubItems.Add('')
            else
             begin
              if p1.NodeType=nCodeBlock then
               begin
                q:=pointer(PxValue(cardinal(p)-SystemWordSize)^);
                j:=0;
                while (j<>MaxResolveCB) and (cp0[j]<>q) do inc(j);
               end
              else
                j:=MaxResolveCB;
              try
                if j<>MaxResolveCB then
                 begin
                  cp0[cpX]:=pointer(cardinal(p)+SystemWordSize);
                  ListFirst(p1,lLocals,cv,cv0);
                  if cv.x=0 then
                   begin
                    li.SubItems.Add(StratoDumpThing(p1));
                    inc(cpX);
                    if cpX=MaxResolveCB then cpX:=0;
                   end
                  else
                    li.SubItems.Add('{ '+StratoDumpThing(p1.r(iParent)));
                 end
                else
                  li.SubItems.Add(StratoDumpThing(p1));
              except
                on e:Exception do
                  li.SubItems.Add('! ['+e.ClassName+']'+e.Message);
              end;
             end
          else
          if cardinal(cp0[cpX])+cv.v(vOffset)>cardinal(p) then
            li.SubItems.Add('')
          else
           begin
            ii:=ByteSize(cv);
            if ii>SystemWordSize*8 then
             begin
              li.SubItems.Add(#$85' '+StratoDumpThing(cv)+
                ' [#'+IntToStr(ii)+']');
              inc(cardinal(p),ii-SystemWordSize);
             end
            else
              li.SubItems.Add(#$95' '+StratoDumpThing(cv));
           end;
          inc(cardinal(p),SystemWordSize);
          if cv.x<>0 then
           begin
            while (cv.x<>0) and (cardinal(cp0[cpX])+cv.v(vOffset)<
              cardinal(p)) do ListNext(cv,cv0);
            if cv.x=0 then
             begin
              inc(cpX);
              if cpX=MaxResolveCB then cpX:=0;
             end;
           end;
         end;

        //freshly evaluated literal?
        if cardinal(vp)=cardinal(stackTop)+SystemWordSize then
          //and ByteSize(t1)=SystemWordSize?
         begin
          inc(cardinal(p),SystemWordSize);
          li1:=FDebugView.lvStack.Items.Add;
          li1.Caption:=PtrToStr(p)+'*';
          li1.SubItems.Add(ItemToStrX(PxValue(p)^));
          li1.SubItems.Add('');
          //li1.SubItems.Add(ItemToStr(p0));
          //li1.SubItems.Add(StratoDumpThing(p1));
         end
        else
          li1:=li;

      finally
        FDebugView.lvStack.Items.EndUpdate;
      end;
      if li1<>nil then li1.MakeVisible(false);
      FDebugView.lvStack.Selected:=li;


      FDebugView.lvMem.Items.BeginUpdate;
      try
        FDebugView.lvMem.Items.Clear;
        ii:=0;
        p:=FMem;
        while (ii<FGlobalsIndex) do
         begin
          li:=FDebugView.lvMem.Items.Add;
          li.Caption:=PtrToStr(p);
          li.SubItems.Add(ItemToStrX(PxValue(p)^));
          li.SubItems.Add('');
          li.SubItems.Add(StratoDumpThing(FGlobals[ii].Item));
          inc(xValue(p),FGlobals[ii].ByteSize);
          inc(ii);
         end;

        while cardinal(p)<cardinal(FMemIndex) do
         begin
          //TODO: lookup any vars in locals of codeblocks on stack
          li:=FDebugView.lvMem.Items.Add;
          li.Caption:=PtrToStr(p);
          li.SubItems.Add(ItemToStrX(PxValue(p)^));
          li.SubItems.Add('');
          li.SubItems.Add('');
          inc(xValue(p),SystemWordSize);
         end;
      finally
        FDebugView.lvMem.Items.EndUpdate;
      end;

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
            [cardinal(vp)]));//,StratoDumpThing(Sphere,p1)]));
        if vt.x=0 then
          FDebugView.txtUpNext.Lines.Add('vt')
        else
          FDebugView.txtUpNext.Lines.Add(Format('vt: %s: %s',
            [ItemToStr(vt),StratoDumpThing(vt)]));
        FDebugView.txtUpNext.Lines.Add(Format(
          'bp: $%.8x, cp: $%.8x, ep: $%.8x, sp: $%.8x',
          [{cardinal(bp)}0,cardinal(cp),cardinal(ep),cardinal(stackTop)]));
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
              li.SubItems.Add(BinaryData(xxr(SourceFiles[rSrc(p1)].FileName)));
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
              stackTop:=cp;//dec(cardinal(stackTop),Sphere.n(cb0,vByteSize)^);
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
                    if xxt(p1.rrr(iBody,lLocals,iNext))=nThis then
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
                  Push(p2);//nArgument
                  Next(2,p2.r(iValue));
                 end;
               end;  

              vp:=nil;
              vt.x:=0;
             end;

            2://store argument value
             begin
              Pop(p2);//nArgument
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
                Push(p2);//nArgument
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
                  while xxt(p2.r(iParent))=nCodeBlock do
                   begin
                    Peek(xp,xp);
                    if xp=nil then p2.x:=0 else Peek(xp,p2);
                   end;
                 end;
                if xxt(p2.r(iParent))=nCtor then
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
              Next(2,ip.rr(lItems,iValue));//nArgument
             end;
            2://combine
             begin
              Pop(xp);

              //assert vt=IntrinsicType(itNumber)
              vt:=ip.r(iType);
              vp:=pointer(cardinal(xp)+cardinal(vp^)*ByteSize(vt));
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
              Push(vp);
              Next(2,ip.r(iTarget));
             end;
            2://combine
             begin
              Pop(xp);

              //TODO: auto-dereference (when? always?)
              vp:=pointer(cardinal(xp^)+cardinal(vp));

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
                  if xxt(ip.r(iRight))=nThis then
                   begin
                    if cp=nil then p2.x:=0 else
                     begin
                      xp:=cp;
                      Peek(xp,p2);
                      while xxt(p2.r(iParent))=nCodeBlock do
                       begin
                        Peek(xp,xp);
                        if xp=nil then p2.x:=0 else Peek(xp,p2);
                       end;
                     end;
                    if xxt(p2.r(iParent))=nCtor then
                     begin
                      inc(xValue(xp),SystemWordSize*2); //var "?@@"
                      case TStratoToken(ip.v(vOperator)) of
                        stOpSizeOf:
                          PxValue(Volatile(xxr(IntrinsicTypes[itNumber])))^:=
                            xxv(xxr(PxValue(xp)^),vByteSize);
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
                  if (vt.x>=IntrinsicTypes[itNumber]) and
                     (vt.x<IntrinsicTypes[itString]) then
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
                  if (vt.x>=IntrinsicTypes[itNumber]) and
                     (vt.x<IntrinsicTypes[itString]) and
                     (p1.x>=IntrinsicTypes[itNumber]) and
                     (p1.x<IntrinsicTypes[itString]) then
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
                  if (vt.x>=IntrinsicTypes[itNumber]) and
                     (vt.x<IntrinsicTypes[itString]) and
                     (p1.x>=IntrinsicTypes[itNumber]) and
                     (p1.x<IntrinsicTypes[itString]) then
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
                  if (vt.x>=IntrinsicTypes[itNumber]) and
                     (vt.x<IntrinsicTypes[itString]) and
                     (p1.x>=IntrinsicTypes[itNumber]) and
                     (p1.x<IntrinsicTypes[itString]) then
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
            if (vt.x=IntrinsicTypes[itString]) and (p1.x=IntrinsicTypes[itNumber]) then
             begin
              if not TryStrToInt(string(
                BinaryData(xxr(PxValue(vp)^))),integer(x)) then
                RunError(ip,'invalid integer value');//TODO: raise
              PxValue(Volatile(p1))^:=x;//Move(x,vp^,SystemWordSize);
             end
            else
            if (vt.x=IntrinsicTypes[itNumber]) and (p1.x=IntrinsicTypes[itString]) then
             begin
              //TODO: int..intLast?
              x:=ByteSize(vt);
              xx:=0;
              Move(vp^,xx,x);
              q1.x:=AddBinaryData((FData as TStratoParser).SrcIndex,
                UTF8String(IntToStr(xx)));
              PxValue(Volatile(p1))^:=q1.x;//Move(q1,vp^,SystemWordSize);
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
      i:=ParseInteger(string(BinaryData(p.r(iValue))));
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
      inc(cardinal(p^));
     end;
    2://xdec
     begin
      p:=pointer(Data^);
      dec(cardinal(p^));
     end;

    100://malloc
     begin
      p:=Data;
      inc(xValue(p),SystemWordSize);

      if cardinal(FMemIndex)-cardinal(FMem)+cardinal(p^)>FMemSize then
        RunError(Fn,'Out of memory')//TODO:throw
      else
       begin
        pointer(Data^):=FMemIndex;
        inc(xValue(FMemIndex),cardinal(p^));
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

end.

