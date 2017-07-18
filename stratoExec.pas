unit stratoExec;

interface

uses SysUtils, stratoDecl, stratoSphere, stratoDebug, stratoDebugView;

type
  TStratoMachine=class(TObject)
  private
    FMem,FMemPastGlobals,FMemIndex:pointer;
    FMemSize:cardinal;
    FGlobals:array of record Item:xItem; ByteSize:cardinal; end;
    FGlobalsSize,FGlobalsIndex:cardinal;
    FDebugView:TfrmDebugView;
    FDebugCount:integer;
    procedure Perform(Sphere:TStratoSphere;Entry:xItem);
    procedure LiteralToMemory(Sphere:TStratoSphere;p:xItem;ptr:pointer);
    procedure PerformSysCall(Sphere:TStratoSphere;Fn:xItem;Data:pointer);
  public
    constructor Create(DoDebug:boolean=false);
    destructor Destroy; override;
    procedure Run(Sphere:TStratoSphere);
  end;

implementation

uses Windows, stratoFn, stratoRunTime, stratoTokenizer, stratoLogic, ComCtrls;

const
  InitialMemSize=$100000;//?
  InitialStackSize=$10000;//?

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

function ItemToStr(x:xItem):string; overload;
begin
  if x=0 then Result:='' else Result:=Format('$%.8x',[cardinal(x)]);
end;

function ItemToStr(x:pointer):string; overload;
begin
  if x=nil then Result:='' else Result:=Format('@%.8x',[cardinal(x)]);
end;

function IntToStr0(x:cardinal):string;
begin
  if x=0 then Result:='' else Result:=Format('%d',[x]);//IntToStr(x);
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
  inherited;
end;

procedure TStratoMachine.Run(Sphere: TStratoSphere);
var
  p,p0,p1,p2:xItem;
  q:array of xItem;
  qi,ql,bs:cardinal;
begin
  qi:=0;
  ql:=0;
  p:=xItem(-1);
  while Sphere.Store.NextModule(p) do
   begin
    if qi=ql then
     begin
      inc(ql,$20);//grow
      SetLength(q,ql);
     end;
    q[qi]:=p;
    inc(qi);
   end;
  ql:=qi;

  if FDebugView<>nil then FDebugView.Show;

  //allocate globals
  FGlobalsIndex:=0;
  qi:=0;
  while qi<ql do
   begin
    Sphere.First(q[qi],fSourceFile_Globals,p,p0);
    while p<>0 do
     begin
      p1:=Sphere.n(p,fVarDecl)^;
      bs:=ByteSize(Sphere,p1);

      //list for debug
      if FGlobalsIndex=FGlobalsSize then
       begin
        inc(FGlobalsSize,$100);//grow
        SetLength(FGlobals,FGlobalsSize);
       end;
      FGlobals[FGlobalsIndex].Item:=p;
      FGlobals[FGlobalsIndex].ByteSize:=bs;
      inc(FGlobalsIndex);

      //set offset, value
      Sphere.n(p1,vOffset)^:=xValue(FMemPastGlobals);
      p2:=Sphere.n(p1,fValue)^;
      if p2<>0 then LiteralToMemory(Sphere,p2,FMemPastGlobals); //else zeroes?
      inc(xPtr(FMemPastGlobals),bs);
      Sphere.Next(p,p0);
     end;
    inc(qi);
   end;

  FMemIndex:=FMemPastGlobals;//TODO: plus margin? 

  //TODO: halt on unhandled exception?
  //TODO: re-construct correct order based on dependencies?

  //initialization
  qi:=ql;
  while qi<>0 do
   begin
    dec(qi);
    p:=Sphere.n(q[qi],fSourceFile_InitializationBlock)^;
    if p<>0 then Perform(Sphere,p);
   end;

  //finalization
  qi:=0;
  while qi<ql do
   begin
    p:=Sphere.n(q[qi],fSourceFile_FinalizationBlock)^;
    if p<>0 then Perform(Sphere,p);
    inc(qi);
   end;

  if FDebugView<>nil then FDebugView.Done;
end;

procedure TStratoMachine.Perform(Sphere:TStratoSphere;Entry:xItem);
var
  stackBase,stackTop:pointer;
  procedure Push(i:cardinal); overload;
  begin
    if cardinal(stacktop)>=cardinal(stackBase)+InitialStackSize then
      raise Exception.Create('Stack overflow');//TODO: Throw
    Move(i,stackTop^,SystemWordSize);
    inc(cardinal(stackTop),SystemWordSize);
  end;
  procedure Push(p:pointer); overload;
  begin
    if cardinal(stacktop)>=cardinal(stackBase)+InitialStackSize then
      raise Exception.Create('Stack overflow');//TODO: Throw
    Move(p,stackTop^,SystemWordSize);
    inc(cardinal(stackTop),SystemWordSize);
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
  procedure Pop(var p:xItem); overload;
  begin
    if stackTop=stackBase then
      raise Exception.Create('Stack underflow');//TODO: Throw
    dec(cardinal(stackTop),SystemWordSize);
    Move(stackTop^,p,SystemWordSize);
  end;

  procedure Peek(var x:pointer;var p:xItem); overload;
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

var
  OpCount:cardinal;
  ip,ipNext:xItem;//instruction pointer
  cp:pointer;//code block pointer
  ep:pointer;//exception pointer
  new:boolean;
  vp:pointer;//value pointer
  vt,p1,p2,q1,q2:xItem;
  xp,yp,zp:pointer;

  function Pass:cardinal;
  begin
    if new then Result:=0 else Pop(Result);
  end;
  procedure Next(nPass:cardinal;nNext:xItem=0);
  begin
    Push(nPass);
    Push(ip);
    if nNext<>0 then ipNext:=nNext;
    //TODO: vp:=nil;vt:=0; here?
  end;

  function Volatile(nvt:xItem):pointer;
  begin
    //TODO: separate register?
    vp:=stackTop;
    vt:=nvt;
    //inc(xPtr(???),ByteSize(Sphere,vt));?
    Result:=vp;
  end;

  procedure CbStart(cb:xItem;var np:pointer);
  var
    s:cardinal;
  begin
    Push(cp);
    Push(cb);
    np:=stackTop;
    //allocate space for local variables
    s:=Sphere.n(cb,vByteSize)^;
    if cardinal(stacktop)+s>=cardinal(stackBase)+InitialStackSize then
      raise Exception.Create('Stack overflow');//TODO: Throw
    ZeroMemory(stackTop,s);//?
    inc(cardinal(stackTop),s);
    //Sphere.First(cb,fItems,ipNext,);
    ipNext:=Sphere.n(Sphere.n(cb,fItems)^,fNext)^;
    Push(ipNext);
    Push(cb);
  end;

  function RefreshDebugView: boolean;
  const
    MaxResolveCB=8;
  var
    li,li1:TListItem;
    p,q:pointer;
    cp0:array[0..MaxResolveCB-1] of pointer;
    i,cv,cv0:xItem;
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
        li.SubItems.Add(ItemToStr(vp));
        li.SubItems.Add(ItemToStr(vt));
        li.SubItems.Add('');//ItemToStr(bp));
        li.SubItems.Add(ItemToStr(cp));
        li.SubItems.Add(ItemToStr(ep));
        li.SubItems.Add(ItemToStr(stackTop));
        li.SubItems.Add(StratoDumpThing(Sphere,ip));
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
        cv:=0;
        while cardinal(p)<cardinal(stackTop) do
         begin
          li:=FDebugView.lvStack.Items.Add;
          li.Caption:=ItemToStr(p);
          i:=xItem(pointer(cardinal(PCardinal(p)^)));
          if i<SizeOf(xSourceFile) then
           begin
            li.SubItems.Add(IntToStr(i));
            i:=0;
           end
          else
            li.SubItems.Add(ItemToStr(i));
          if cv=0 then
            if i=0 then
              li.SubItems.Add('')
            else
             begin
              if Sphere.n(i,vTypeNr)^=nCodeBlock then
               begin
                q:=pointer(PxValue(cardinal(p)-SystemWordSize)^);
                j:=0;
                while (j<>MaxResolveCB) and (cp0[j]<>q) do inc(j);
               end
              else
                j:=MaxResolveCB;
              if j<>MaxResolveCB then
               begin
                cp0[cpX]:=pointer(cardinal(p)+SystemWordSize);
                Sphere.First(i,fVarDecls,cv,cv0);
                if cv=0 then
                 begin
                  li.SubItems.Add(StratoDumpThing(Sphere,i));
                  inc(cpX);
                  if cpX=MaxResolveCB then cpX:=0;
                 end
                else
                  li.SubItems.Add('{ '+StratoDumpThing(
                    Sphere,Sphere.n(i,fParent)^));
               end
              else
                li.SubItems.Add(StratoDumpThing(Sphere,i));
             end
          else
          if cardinal(cp0[cpX])+Sphere.n(cv,vOffset)^>cardinal(p) then
            li.SubItems.Add('')
          else
           begin
            ii:=ByteSize(Sphere,cv);
            if ii>SystemWordSize*8 then
             begin
              li.SubItems.Add(#$85' '+StratoDumpThing(Sphere,cv)+
                ' [#'+IntToStr(ii)+']');
              inc(cardinal(p),ii-SystemWordSize);
             end
            else
              li.SubItems.Add(#$95' '+StratoDumpThing(Sphere,cv));
           end;
          inc(cardinal(p),SystemWordSize);
          if cv<>0 then
           begin
            while (cv<>0) and (cardinal(cp0[cpX])+Sphere.n(cv,vOffset)^<
              cardinal(p)) do Sphere.Next(cv,cv0);
            if cv=0 then
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
          li1.Caption:=ItemToStr(p)+'*';
          i:=xItem(pointer(cardinal(PCardinal(p)^)));
          if i<SizeOf(xSourceFile) then
           begin
            li1.SubItems.Add(IntToStr(i));
            li1.SubItems.Add('');
           end
          else
           begin
            li1.SubItems.Add(ItemToStr(i));
            li1.SubItems.Add(StratoDumpThing(Sphere,i));
           end;
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
          li.Caption:=ItemToStr(p);
          li.SubItems.Add(ItemToStr(PxValue(p)^));
          li.SubItems.Add('');
          li.SubItems.Add(StratoDumpThing(Sphere,FGlobals[ii].Item));
          inc(xPtr(p),FGlobals[ii].ByteSize);
          inc(ii);
         end;

        while cardinal(p)<cardinal(FMemIndex) do
         begin
          //TODO: lookup any vars in locals of codeblocks on stack
          li:=FDebugView.lvMem.Items.Add;
          li.Caption:=ItemToStr(p);
          li.SubItems.Add(ItemToStr(PxValue(p)^));
          li.SubItems.Add('');
          li.SubItems.Add('');
          inc(xPtr(p),SystemWordSize);
         end;
      finally
        FDebugView.lvMem.Items.EndUpdate;
      end;

      FDebugView.txtUpNext.Lines.BeginUpdate;
      try
        FDebugView.txtUpNext.Lines.Clear;
        if new then
          FDebugView.txtUpNext.Lines.Add(Format('>>> $%.8x: %s',
            [cardinal(ip),StratoDumpThing(Sphere,ip)]))
        else
          FDebugView.txtUpNext.Lines.Add(Format('ip: $%.8x: %s',
            [cardinal(ip),StratoDumpThing(Sphere,ip)]));
        if vp=nil then
          FDebugView.txtUpNext.Lines.Add('vp')
        else
          FDebugView.txtUpNext.Lines.Add(Format('vp: @%.8x',
            [cardinal(vp)]));//,StratoDumpThing(Sphere,p1)]));
        if vt=0 then
          FDebugView.txtUpNext.Lines.Add('vt')
        else
          FDebugView.txtUpNext.Lines.Add(Format('vt: $%.8x: %s',
            [cardinal(vt),StratoDumpThing(Sphere,vt)]));
        FDebugView.txtUpNext.Lines.Add(Format(
          'bp: $%.8x, cp: $%.8x, ep: $%.8x, sp: $%.8x',
          [{cardinal(bp)}0,cardinal(cp),cardinal(ep),cardinal(stackTop)]));
{
        if vt<>nil then
         begin
          FDebugView.txtUpNext.Lines.Add(Format('vt: $%.8x: %s',
            [cardinal(vt),StratoDumpThing(Sphere,vt)]));
          Move(FMem[vp],j,4);//TODO: ByteSize(Sphere,vt);
          FDebugView.txtUpNext.Lines.Add(Format('vp: @=%d x=%.8x v=%d',[vp,j,j]));
         end;
}         
      finally
        FDebugView.txtUpNext.Lines.EndUpdate;
      end;
      try
        if (ip<>0) and StratoGetSourceFile(Sphere,ip,sy,sx,q1,q2) then
          FDebugView.ShowSource(Sphere,Sphere.Store.SourceFile(ip),sy,sx)
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
            li.Caption:=ItemToStr(p);
            Peek(p,i);
            li.SubItems.Add(ItemToStr(i));
            while Sphere.n(i,vTypeNr)^=nCodeBlock do
              i:=Sphere.n(i,fParent)^;
            li.SubItems.Add(StratoDumpThing(Sphere,i));
            li.SubItems.Add(StratoDumpThing(Sphere,
              Sphere.n(Sphere.n(i,fParent)^,fParent)^));//?
            if StratoGetSourceFile(Sphere,i,sy,sx,q1,q2) and (sy<>0) then
             begin
              li.SubItems.Add(Sphere.GetBinaryData(
                Sphere.Store.SourceFile(i).FileName));
              li.SubItems.Add(IntToStr(sx));
              li.SubItems.Add(IntToStr(sy));
             end
            else
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
    vt:=0;
    while ip<>0 do
     begin

      if RefreshDebugView then
        asm int 3 end;//DebugBreak;//forced breakpoint

      //TODO: check ip with block base pointers
      ipNext:=0;
      ipt:=Sphere.n(ip,vTypeNr)^;
      case ipt of

        nCodeBlock:
          if new then
           begin
            CbStart(ip,cp);
            vp:=nil;
            vt:=0;
           end
          else
           begin
            Pop(q1);
            //Sphere.Next(q1,);
            if q1=Sphere.n(ip,fItems)^ then
              q1:=0
            else
              q1:=Sphere.n(q1,fNext)^;
            if q1=0 then
             begin
              //resulting value
              if Sphere.n(ip,fReturnType)^=0 then
               begin
                //if vt<>0 then raise?throw?
                vt:=0;
                vp:=nil;
               end
              else
               begin
                if vt=0 then raise Exception.Create('CodeBlock with ReturnType didn''t resolve to value');
               end;

{
              //resulting value
              vt:=Sphere.n(ip,fReturnType)^;
              if vt=0 then
               begin
                vp:=nil;
               end
              else
               begin
                vp:=cp;
                //assert resulting value's nVarDecl.vOffset=0
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
              ipNext:=q1;
              Push(q1);
              Push(ip);
              vp:=nil;
              vt:=0;
             end;
           end;

        nFCall,//'plain' call (no subject)
        nSCall,//static call (subject.target), no dynamic/virtual
        nVCall://virtual call (subject.target), dynamic (by instance class ref)
         begin
          if new then
            if ipt=nFCall then x:=1 else x:=0
          else
            x:=Pass;
          case x of
            0://start, evaluate subject
              Next(1,Sphere.n(ip,fSubject)^);

            1://subject evaluated, arguments?
             begin
              case ipt of
                nFCall:
                 begin
                  p1:=Sphere.n(ip,fTarget)^;
                  if p1=0 then Sphere.Error(ip,'F-Call target not defined');
                 end;
                nSCall:
                 begin
                  if vt=0 then Sphere.Error(ip,'S-Call subject not resolved');
                  q2:=PxValue(vp)^;
                  p1:=Sphere.n(ip,fTarget)^;
                  if p1=0 then Sphere.Error(ip,'S-Call target not defined');
                 end;
                nVCall:
                 begin
                  if vt=0 then Sphere.Error(ip,'V-Call subject not resolved');
                  q2:=PxValue(vp)^;
                  p1:=StratoFnCallFindVirtual(Sphere,ip,vt,vp);
                  if p1=0 then Sphere.Error(ip,'V-Call target implementation not found');
                  Push(p1);
                 end;
              end;

              //push here to handle any return value
              Next(3); //TODO: if ip.fSignature.fReturnType<>0?

              q1:=Sphere.n(p1,fBody)^;
              CbStart(q1,xp);

              if ipt<>nFCall then
                case Sphere.n(p1,vTypeNr)^ of

                  nOverload,nPropertyGet,nPropertySet:
                    if Sphere.n(Sphere.n(Sphere.n(q1,fVarDecls)^,fNext)^,
                      vTypeNr)^=nThis then
                     begin
                      //assert "@@".vOffset=0
                      //TODO: check SameType(t,ResType("@@"?
                      case Sphere.n(vt,vTypeNr)^ of //auto-dereference
                        nClass:PxValue(xp)^:=q2;
                        else PxValue(xp)^:=xItem(vp);
                      end;
                     end;

                  nConstructor:
                   begin
                    PxValue(xp)^:=0;//"@@":this default nil until malloc
                    yp:=xp;
                    inc(xPtr(yp),SystemWordSize);
                    //calling an inherited constructor?
                    if (ipt<>nSCall) or (cp=nil) then p2:=0 else
                     begin
                      zp:=cp;
                      Peek(zp,p2);
                      while Sphere.n(Sphere.n(p2,fParent)^,vTypeNr)^=nCodeBlock do
                       begin
                        Peek(zp,zp);
                        if zp=nil then p2:=0 else Peek(zp,p2);
                       end;
                     end;
                    if Sphere.n(Sphere.n(p2,fParent)^,vTypeNr)^=nConstructor then
                     begin
                      inc(xPtr(zp),SystemWordSize*2); //var "?@@"
                      PxValue(yp)^:=PxValue(zp)^;
                     end
                    else
                      PxValue(yp)^:=q2;
                   end;

                  nDestructor:
                    PxValue(xp)^:=q2;//assert "@@".vOffset=0

                  else
                    raise Exception.Create('//TODO:');
                end;

              //Sphere.First(ip,fArguments,p2,);
              p2:=Sphere.n(Sphere.n(ip,fArguments)^,fNext)^;
              if p2=0 then
               begin
                cp:=xp;
                //ipNext:= by CbStart above
               end
              else
               begin
                Push(xp);//new cp after arguments
                if ipt=nVCall then Push(p1);//nOverload
                Push(Sphere.n(p1,fFirstArgVar)^);//nVarDecl
                Push(p2);//nArgument
                Next(2,Sphere.n(p2,fValue)^);
               end;

              vp:=nil;
              vt:=0;
             end;

            2://store argument value
             begin
              Pop(p2);//nArgument
              Pop(q2);//nVarDecl
              if ipt=nVCall then Pop(p1) else p1:=Sphere.n(ip,fTarget)^;
              Pop(xp);//new cp after arguments

              //store value
              yp:=xp;
              inc(xPtr(yp),Sphere.n(q2,vOffset)^);
              if Sphere.n(p2,vTypeNr)^=nArgByRef then
                PxValue(yp)^:=xItem(vp) //Move(vp,yp^,SystemWordSize)
              else
                Move(vp^,yp^,ByteSize(Sphere,vt));

              //next argument //TODO: Sphere.Next(q2,);
              if q2=Sphere.n(p1,fFirstArgVar)^ then q2:=0 else
               begin
                p2:=Sphere.n(p2,fNext)^;
                q2:=Sphere.n(q2,fNext)^;
               end;
              if q2=0 then
               begin
                //all done, do first of body (StartCB already done by Pass=1 above)
                q1:=Sphere.n(p1,fBody)^;
                //Sphere.First(q1,fItems,ipNext,);
                cp:=xp;
                ipNext:=Sphere.n(Sphere.n(q1,fItems)^,fNext)^;
               end
              else
               begin
                Push(xp);//new cp after arguments
                if ipt=nVCall then Push(p1);//nOverload
                Push(q2);//nVarDecl
                Push(p2);//nArgument
                Next(2,Sphere.n(p2,fValue)^);
               end;

              vp:=nil;
              vt:=0;
             end;

            3://post-block (returned value?)
             begin
              if ipt=nVCall then Pop(p1) else p1:=Sphere.n(ip,fTarget)^;

              //inherited constructor? propagate this
              if Sphere.n(p1,vTypeNr)^=nConstructor then
               begin
                xp:=stackTop;//previous 'cp' from freshly ended codeblock of body
                inc(xPtr(xp),SystemWordSize*4);
                if ipt=nVCall then inc(xPtr(xp),SystemWordSize);
                vp:=xp;
                inc(xPtr(xp),SystemWordSize);
                vt:=PxValue(xp)^;

                //calling an inherited constructor?
                if cp=nil then p2:=0 else
                 begin
                  xp:=cp;
                  Peek(xp,p2);
                  while Sphere.n(Sphere.n(p2,fParent)^,vTypeNr)^=nCodeBlock do
                   begin
                    Peek(xp,xp);
                    if xp=nil then p2:=0 else Peek(xp,p2);
                   end;
                 end;
                if Sphere.n(Sphere.n(p2,fParent)^,vTypeNr)^=nConstructor then
                 begin
                  inc(xPtr(xp),SystemWordSize);
                  PxValue(xp)^:=PxValue(vp)^;
                 end;
               end
              else
               begin
                //check return value
                q2:=Sphere.n(Sphere.n(p1,fSignature)^,fReturnType)^;
                //if q2<>0 and p1.fBody.fReturnType=0 then?
                if q2=0 then
                 begin
                  //if vt<>0 then raise?throw?
                  vt:=0;
                  vp:=nil;
                 end
                else
                 begin
                  //assert return value first (past this)
                  if vt=0 then
                   begin
                    vt:=q2;
                    q1:=Sphere.n(Sphere.n(Sphere.n(p1,fBody)^,fVarDecls)^,fNext)^;
                    if Sphere.n(q1,vTypeNr)^=nThis then q1:=Sphere.n(q1,fNext)^;

                    xp:=stackTop;//previous 'cp' from freshly ended codeblock of body
                    inc(xPtr(xp),SystemWordSize*4);
                    if ipt=nVCall then inc(xPtr(xp),SystemWordSize);
                    inc(xPtr(xp),Sphere.n(q1,vOffset)^);
                    vp:=xp;
                   end;
                  //else check SameType(Sphere,vt,q2)?
                 end;
               end;
             end;
          end;
         end;

        nClass:
          //TODO: all Sphere.Add(nClass do Sphere.Add(nClassRef onbeforehand?
          PxValue(Volatile(Sphere.Add(nClassRef,0,0,[fSubject,ip])))^:=ip;

        nVarDecl,nThis://calculate address
         begin
          //assert vp=nil
          vt:=Sphere.n(ip,fTypeDecl)^;
          vp:=nil;//calulated below
          q1:=ip;
          while q1<>0 do
           begin
            q2:=0;//next...
            case Sphere.n(q1,vTypeNr)^ of
              nVarDecl,nThis://offset
               begin
                inc(xPtr(vp),Sphere.n(q1,vOffset)^);
                q2:=Sphere.n(q1,fParent)^;
               end;
              nNameSpace://was global var
                ;//end loop (assert xVarDecl().Offset set by init)
              nCodeBlock://was local var
               begin
                //but to which code block: go up the chain
                xp:=cp;
                yp:=nil;
                p2:=0;
                while (xp<>nil) and (p2<>q1) do
                 begin
                  yp:=xp;
                  Peek(xp,p2);//codeblock
                  Peek(xp,xp);//previous cp
                 end;
                if yp=nil then
                  Sphere.Error(ip,'local block not found on stack')
                else
                  inc(xPtr(vp),xPtr(yp));
               end;
              //nRecord://TODO: dereference pointer

              nClass,nRecord:;//just calculate offset, assert here via nField...

              else Sphere.Error(ip,'invalid relativity chain');
            end;
            q1:=q2;//next!
           end;
         end;

        nConstant:
          LiteralToMemory(Sphere,Sphere.n(ip,fValue)^,
            Volatile(Sphere.n(ip,fTypeDecl)^));
        nLiteral:
          LiteralToMemory(Sphere,ip,
            Volatile(Sphere.n(ip,fTypeDecl)^));

        nArrayIndex:
          case Pass of
            0://start, evaluate subject
              Next(1,Sphere.n(ip,fSubject)^);
            1://evaluate index
             begin
              if vt=0 then Sphere.Error(ip,'array to index into didn''t resolve');
              Push(vp);
              Next(2,Sphere.n(Sphere.n(ip,fItems)^,fValue)^);//nArgument
             end;
            2://combine
             begin
              Pop(xp);

              //assert vt=TypeDecl_number
              vt:=ResType(Sphere,ip);//element type
              vp:=pointer(cardinal(xp)+cardinal(vp^)*ByteSize(Sphere,vt));
             end;
          end;

        nField:
          case Pass of
            0://start, evaluate subject
              Next(1,Sphere.n(ip,fSubject)^);
            1://evaluate target
             begin
              if vt=0 then Sphere.Error(ip,'subject didn''t resolve');
              //TODO: check vt and ip.fSubject.fTypeDecl?
              Push(vp);
              Next(2,Sphere.n(ip,fTarget)^);
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
              case TStratoToken(Sphere.n(ip,vOperator)^) of
                stOpSizeOf,stQuestionMark:
                  if Sphere.n(Sphere.n(ip,fRight)^,vTypeNr)^=nThis then
                   begin
                    if cp=nil then p2:=0 else
                     begin
                      xp:=cp;
                      Peek(xp,p2);
                      while Sphere.n(Sphere.n(p2,fParent)^,
                        vTypeNr)^=nCodeBlock do
                       begin
                        Peek(xp,xp);
                        if xp=nil then p2:=0 else Peek(xp,p2);
                       end;
                     end;
                    if Sphere.n(Sphere.n(p2,fParent)^,
                      vTypeNr)^=nConstructor then
                     begin
                      inc(xPtr(xp),SystemWordSize*2); //var "?@@"
                      case TStratoToken(Sphere.n(ip,vOperator)^) of
                        stOpSizeOf:
                          PxValue(Volatile(TypeDecl_number))^:=
                            Sphere.n(PxValue(xp)^,vByteSize)^;
                        stQuestionMark:
                          PxValue(Volatile(TypeDecl_type))^:=PxValue(xp)^;
                      end;
                      x:=1;//skip Next() below
                     end;
                   end;
              end;
              if x=0 then Next(1,Sphere.n(ip,fRight)^);
             end;
            1://TODO: move to runtime/namespaces/intrinsics
              case TStratoToken(Sphere.n(ip,vOperator)^) of
                stOpSub,stOpInc,stOpDec,stTilde:
                  if (vt>=TypeDecl_number) and (vt<=TypeDecl_intLast) then
                   begin
                    x:=ByteSize(Sphere,vt);
                    xx:=0;
                    Move(vp^,xx,x);
                    case TStratoToken(Sphere.n(ip,vOperator)^) of
                      stOpSub:xx:=-xx;
                      stOpInc:xx:=xx+1;
                      stOpDec:xx:=xx-1;
                      stTilde:xx:=not(xx);//xx:=-(xx+1);?
                    end;
                    Move(xx,vp^,x);
                    vt:=Sphere.n(ip,fReturnType)^;//assert the same?
                   end
                  else
                    Sphere.Error(ip,'Unknown operation');
                stOpNot:
                  if vt=TypeDecl_bool then
                    if PxValue(vp)^=0 then PxValue(vp)^:=1 else PxValue(vp)^:=0
                  else
                    Sphere.Error(ip,'Unknown operation');
                stOpSizeOf:
                 begin
                  if vt=TypeDecl_type then
                    x:=ByteSize(Sphere,PxValue(vp)^)
                  else
                    x:=ByteSize(Sphere,vt);
                  PxValue(Volatile(TypeDecl_number))^:=x;
                 end;
                stQuestionMark://type of
                  if Sphere.n(vt,vTypeNr)^=nClass then
                   begin
                    //live object? extract base class
                    xp:=pointer(vp^);
                    //inc(xPtr(xp),Sphere.n(Sphere.Lookup(TypeDecl_object,Sphere.Store.Dict['_baseclass']),vOffset)^);
                    dec(xPtr(xp),SystemWordSize);//assert object._baseclass offset -4
                    PxValue(Volatile(TypeDecl_type))^:=PxValue(xp)^;
                    //Sphere.Add(nClassRef,0,0,[fSubject,PxValue(xp)^]);?
                   end
                  else
                    PxValue(Volatile(TypeDecl_type))^:=vt;
                else Sphere.Error(ip,'Unknown operator');
              end;
          end;

        nBinaryOp:
          case Pass of
            0://evaluate left
              Next(1,Sphere.n(ip,fLeft)^);
            1://evaluate right
             begin
              //stored voliatile on stack? keep it there!
              xp:=stackTop;
              inc(xPtr(xp),SystemWordSize*2);
              if vp=xp then
               begin
                x:=ByteSize(Sphere,vt);
                inc(xPtr(stackTop),x+SystemWordSize*2);
                Push(x);
                Push(nil);
               end
              else
                Push(vp);
              Push(vt);
              Next(2,Sphere.n(ip,fRight)^);
              vp:=nil;
              vt:=0;
             end;
            2://
             begin
              Pop(p1);
              Pop(xp);
              if xp=nil then //stored volatile? roll back!
               begin
                Pop(x);
                dec(xPtr(stackTop),x+SystemWordSize*2);
                xp:=stackTop;
                inc(xPtr(xp),SystemWordSize*2);
               end;
              case TStratoToken(Sphere.n(ip,vOperator)^) of
                stOpEQ,stOpNEQ:
                  if SameType(Sphere,vt,p1) then
                   begin
                    x:=ByteSize(Sphere,vt);
                    while (x<>0) and (PByte(vp)^=PByte(xp)^) do
                     begin
                      dec(x);
                      inc(xPtr(vp));
                      inc(xPtr(xp));
                     end;
                    Volatile(TypeDecl_bool);//sets vp
                    case TStratoToken(Sphere.n(ip,vOperator)^) of
                      stOpEQ:
                        if x=0 then PxValue(vp)^:=1 else PxValue(vp)^:=0;
                      stOpNEQ:
                        if x=0 then PxValue(vp)^:=0 else PxValue(vp)^:=1;
                    end;
                   end
                  else
                    Sphere.Error(ip,'Unknown operation');
                stOpAdd:
                  if (p1>=TypeDecl_number) and (p1<=TypeDecl_intLast) and
                     (vt>=TypeDecl_number) and (vt<=TypeDecl_intLast) then
                   begin
                    x:=ByteSize(Sphere,p1);
                    xx:=0;
                    Move(xp^,xx,x);
                    y:=ByteSize(Sphere,vt);
                    yy:=0;
                    Move(vp^,yy,y);
                    xx:=xx+yy;
                    Move(xx,Volatile(Sphere.n(ip,fReturnType)^)^,y);
                   end
                  else
                  if (p1=TypeDecl_string) and (vt=TypeDecl_string) then
                   begin
                    //TODO: strings in memory (by runtime?)
                    p2:=Sphere.AddBinaryData(
                      Sphere.GetBinaryData(PxValue(xp)^)+
                      Sphere.GetBinaryData(PxValue(vp)^));
                    PxValue(Volatile(TypeDecl_string))^:=p2;
                   end
                  else
                    Sphere.Error(ip,'Unknown operation');
                stOpSub,stOpMul,stOpDiv,stOpMod:
                  if (p1>=TypeDecl_number) and (p1<=TypeDecl_intLast) and
                     (vt>=TypeDecl_number) and (vt<=TypeDecl_intLast) then
                   begin
                    x:=ByteSize(Sphere,p1);
                    xx:=0;
                    Move(xp^,xx,x);
                    y:=ByteSize(Sphere,vt);
                    yy:=0;
                    Move(vp^,yy,y);
                    case TStratoToken(Sphere.n(ip,vOperator)^) of
                      stOpSub:xx:=xx-yy;
                      stOpMul:xx:=xx*yy;
                      stOpDiv:xx:=xx div yy;
                      stOpMod:xx:=xx mod yy;
                    end;
                    Move(xx,Volatile(Sphere.n(ip,fReturnType)^)^,x);
                   end
                  else
                    Sphere.Error(ip,'Unknown operation');
                stOpLT,stOpLTE,stOpGT,stOpGTE:
                  if (p1>=TypeDecl_number) and (p1<=TypeDecl_intLast) and
                     (vt>=TypeDecl_number) and (vt<=TypeDecl_intLast) then
                   begin
                    x:=ByteSize(Sphere,p1);
                    xx:=0;
                    Move(xp^,xx,x);
                    y:=ByteSize(Sphere,vt);
                    yy:=0;
                    Move(vp^,yy,y);
                    Volatile(TypeDecl_bool);//sets vp
                    case TStratoToken(Sphere.n(ip,vOperator)^) of
                      stOpLT: if xx<yy  then PxValue(vp)^:=1 else PxValue(vp)^:=0;
                      stOpLTE:if xx<=yy then PxValue(vp)^:=1 else PxValue(vp)^:=0;
                      stOpGT: if xx>yy  then PxValue(vp)^:=1 else PxValue(vp)^:=0;
                      stOpGTE:if xx>=yy then PxValue(vp)^:=1 else PxValue(vp)^:=0;
                    end;
                   end
                  else
                    Sphere.Error(ip,'Unknown operation');
                stOpAnd,stOpOr,stOpXor:
                  if (p1=TypeDecl_bool) and (vt=TypeDecl_bool) then
                   begin
                    x:=PxValue(xp)^;
                    y:=PxValue(vp)^;
                    Volatile(TypeDecl_bool);//sets vp
                    case TStratoToken(Sphere.n(ip,vOperator)^) of
                      stOpAnd:if (x<>0) and (y<>0) then PxValue(vp)^:=1 else PxValue(vp)^:=0;
                      stOpOr: if (x<>0) or  (y<>0) then PxValue(vp)^:=1 else PxValue(vp)^:=0;
                      stOpXor:if (x<>0) xor (y<>0) then PxValue(vp)^:=1 else PxValue(vp)^:=0;
                    end;
                   end
                  else
                    Sphere.Error(ip,'Unknown operation');
                //TODO: stOpSub,stOpMul,stOpDiv,stOpMod,stOpShl,stOpShr,stThreeLT,stThreeGT:
                //TODO: stOpNEQ,stOpLT,stOpLTE,stOpGT,stOpGTE:
                //TODO: stOpAnd,stOpOr,stOpXor
                //stOpTypeIs:
                else Sphere.Error(ip,'Unknown operator');
              end;
             end;
          end;

        nAssign:
          case Pass of
            0://evaluate left
             begin
              p1:=Sphere.n(ip,fTarget)^;
              if Sphere.n(p1,vTypeNr)^=nCast then //'dirty cast'?
                Next(3,Sphere.n(p1,fSubject)^)
              else
                Next(1,p1);
             end;
            1://evaluate right
             begin
              Push(vp);//vt?
              Next(2,Sphere.n(ip,fValue)^);
              vp:=nil;
              vt:=0;
             end;
            2://copy value
             begin
              Pop(xp);//pt? assert =vt
              case TStratoToken(Sphere.n(ip,vOperator)^) of
                stOpAssign:Move(vp^,xp^,ByteSize(Sphere,vt));
                stOpAssignAdd:
                 begin
                  //TODO: merge with binaryop
                  //if =TypeDecl_number!!
                  x:=ByteSize(Sphere,ResType(Sphere,Sphere.n(ip,fTarget)^));
                  xx:=0;
                  Move(xp^,xx,x);
                  yy:=0;
                  Move(vp^,yy,x);
                  xx:=xx+yy;
                  Move(xx,xp^,x);
                 end;
                //TODO: stOpAssignSub,stOpAssignMul,stOpAssignDiv,stOpAssignMod,stOpAssignOr,stOpAssignAnd
                else
                  Sphere.Error(ip,'unknown assignment type');
              end;
              vp:=nil;//drop value (!!! by language design)
              vt:=0;
             end;
            3://dirty cast, evaluate right
             begin
              Push(vp);//vt?
              Next(4,Sphere.n(ip,fValue)^);
              vp:=nil;
              vt:=0;
             end;
            4://dirty cast, push value
             begin
              Pop(xp);//pt? assert =vt
              case TStratoToken(Sphere.n(ip,vOperator)^) of
                stOpAssign://plain
                  Move(vp^,xp^,ByteSize(Sphere,
                    Sphere.n(Sphere.n(ip,fTarget)^,fTypeDecl)^));
                stOpAssignAdd:
                 begin
                  //TODO: merge with binaryop
                  //TODO: switch pointer arith (default off!)
                  //if =TypeDecl_number!!
                  x:=ByteSize(Sphere,ResType(Sphere,Sphere.n(ip,fTarget)^));
                  xx:=0;
                  Move(xp^,xx,x);
                  yy:=0;
                  Move(vp^,yy,x);
                  xx:=xx+yy;
                  Move(xx,xp^,x);
                 end;
                else
                  Sphere.Error(ip,'unknown assignment type');
              end;
              vp:=nil;//drop value (!!! by language design)
              vt:=0;
             end;
          end;

        nCast:
          if new then
           begin
            Push(ip);
            ipNext:=Sphere.n(ip,fSubject)^;
           end
          else
           begin
            //TODO: move these specifics over to runtime (with intrinsics? syscalls?)
            p1:=Sphere.n(ip,fTypeDecl)^;//TODO: check ByteSize?
            if (vt=TypeDecl_string) and (p1=TypeDecl_number) then
             begin
              q1:=PxValue(vp)^;//Move(vp^,q1,SystemWordSize);
              if not TryStrToInt(string(Sphere.GetBinaryData(q1)),integer(x)) then
                Sphere.Error(ip,'invalid integer value');//TODO: raise
              PxValue(Volatile(p1))^:=x;//Move(x,vp^,SystemWordSize);
             end
            else
            if (vt>=TypeDecl_number) and (vt<=TypeDecl_intLast)
              and (p1=TypeDecl_string) then
             begin
              x:=ByteSize(Sphere,vt);
              xx:=0;
              Move(vp^,xx,x);
              q1:=Sphere.AddBinaryData(UTF8String(IntToStr(xx)));
              PxValue(Volatile(p1))^:=q1;//Move(q1,vp^,SystemWordSize);
             end
            else
            if (vt=TypeDecl_bool) and (p1=TypeDecl_string) then
             begin
              x:=PxValue(vp)^;//Move(vp^,x,SystemWordSize);
              if x=0 then
                q1:=Sphere.AddBinaryData('0')
              else
                q1:=Sphere.AddBinaryData('1');
              PxValue(Volatile(p1))^:=q1;//Move(q1,vp^,SystemWordSize);
             end
            else
            if (vt=TypeDecl_number) and (Sphere.n(p1,vTypeNr)^=nEnumeration) then
             begin
              x:=PxValue(vp)^;//Move(vp^,x,SystemWordSize);
              //check enumeration index in range?
              PxValue(Volatile(p1))^:=x;//Move(x,vp^,SystemWordSize);
             end
            else
            if (Sphere.n(vt,vTypeNr)^=nEnumeration) and (p1=TypeDecl_number) then
             begin
              //x:=PxValue(vp)^;//Move(vp^,x,SystemWordSize);
              vt:=p1;
              //PxValue(Volatile(p1))^:=x;//Move(x,vp^,SystemWordSize);
             end
            else
            if (p1>=TypeDecl_number) and (p1<=TypeDecl_intLast) and
               (vt>=TypeDecl_number) and (vt<=TypeDecl_intLast) then
             begin
              x:=ByteSize(Sphere,vt);
              xx:=0;
              Move(vp^,xx,x);
              Move(xx,Volatile(p1)^,ByteSize(Sphere,p1));
             end
            else
            if (vt=TypeDecl_number) and (Sphere.n(p1,vTypeNr)^=TypeDecl_pointer) then
             begin
              x:=PxValue(vp)^;//Move(vp^,x,SystemWordSize);
              //if i<>x then //TODO: protect against pointer arith
              PxValue(Volatile(p1))^:=x;//Move(x,vp^,SystemWordSize);
             end
            else
            if (TypeDecl_object<>0) and
              (Sphere.n(vt,vTypeNr)^=nClass) and (Sphere.n(p1,vTypeNr)^=nClass) then
             begin
              //cast to base class?
              if vp=nil then q1:=vt else
               begin
                //dereference
                xp:=pointer(PxValue(vp)^-SystemWordSize);
                //assert object._baseclass @-SystemWordSize
                q1:=PxValue(xp)^;
               end;
              while (q1<>0) and (q1<>p1) do q1:=Sphere.n(q1,fInheritsFrom)^;
              if q1<>0 then
               begin
                //TODO: check @@._baseclass!
                //vp:=vp;
                vt:=q1;
               end;
              //else?
             end
            else
              Sphere.Error(ip,'unsupported cast');
           end;

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

        nSysCall:
         begin
          PerformSysCall(Sphere,ip,cp);
          vt:=Sphere.n(Sphere.n(ip,fParent)^,fReturnType)^;
          if vt=0 then vp:=nil else vp:=cp;
         end;


        nSelection:
          if new then
           begin
            Push(ip);
            ipNext:=Sphere.n(ip,fDoIf)^;
           end
          else
          if vt<>TypeDecl_bool then
            Sphere.Error(ip,'selection criterium didn''t evaluate to boolean')
          else
            if PxValue(vp)^=0 then
              ipNext:=Sphere.n(ip,fDoElse)^
            else
              ipNext:=Sphere.n(ip,fDoThen)^;


        nIteration:
         begin
          x:=Pass;
          if x=0 then //DoFirst?
           begin
            p1:=Sphere.n(ip,fDoFirst)^;
            if p1=0 then x:=2 else Next(2,p1);
           end;
          if x=5 then //DoThen
           begin
            p1:=Sphere.n(ip,fDoThen)^;
            if p1=0 then x:=2 else Next(2,p1);
           end;
          if x=2 then //DoIf
           begin
            p1:=Sphere.n(ip,fDoIf)^;
            if p1=0 then x:=4 else Next(3,p1);
           end;
          if x=3 then //DoIf result?
           begin
            if vt<>TypeDecl_bool then
              Sphere.Error(ip,'Iteration criterum didn''t result boolean');
            if PxValue(vp)^=0 then
             begin
              //all done
              vt:=0;
              vp:=nil;
             end
            else
              x:=4;
           end;
          if x=4 then //Body
            Next(5,Sphere.n(ip,fBody)^);
         end;

        nIterPostEval:
         begin
          x:=Pass;
          if x=0 then //DoFirst?
           begin
            p1:=Sphere.n(ip,fDoFirst)^;
            if p1=0 then x:=1 else Next(1,p1);
           end;
          if x=1 then //Body
           begin
            p1:=Sphere.n(ip,fBody)^;
            if p1=0 then x:=2 else Next(2,p1);
           end;
          if x=2 then //DoIf
           begin
            //TODO: if vt/vp on stack keep it there??
            p1:=Sphere.n(ip,fDoIf)^;
            if p1=0 then x:=4 else Next(3,p1);
           end;
          if x=3 then //DoIf result?
           begin
            if vt<>TypeDecl_bool then
              Sphere.Error(ip,'Iteration criterium didn''t result boolean');
            if PxValue(vp)^=0 then
             begin
              //all done
              vt:=0;
              vp:=nil;
             end
            else
              x:=4;
           if x=4 then
             Next(1);
           end;
         end;

        //n:

        else
          Sphere.Error(ip,Format('unknown logic item $%.8x %s',
            [cardinal(ip),xDisplay(Sphere,ip)]));
      end;

      {//TODO
      if (vt0<>0) and (vt=vt0) then
       begin
        Sphere.Error(ip,'unused resulting value');
        vp:=nil;
        vt:=0;
       end;
      }

      if ipNext=0 then
       begin
        if stackTop=stackBase then ip:=0 else Pop(ip);
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

procedure TStratoMachine.LiteralToMemory(Sphere:TStratoSphere;p:xItem;
  ptr:pointer);
var
  i:int64;
  r:xItem;
begin
  if Sphere.n(p,vTypeNr)^<>nLiteral then
    Sphere.Error(p,'literal expected: '+xDisplay(Sphere,p))
  else
   begin
    r:=Sphere.n(p,fTypeDecl)^;
    //assert q=0 or Sphere.n(q,fThingType)^=nTypeDecl
    if (r>=TypeDecl_number) and (r<=TypeDecl_intLast) then
     begin
      i:=ParseInteger(string(Sphere.GetBinaryData(Sphere.n(p,fValue)^)));
      Move(i,ptr^,Sphere.n(r,vByteSize)^);
     end
    else
    if r=TypeDecl_string then
     begin
      //TODO: store strings in conceptual mem (see also stratoRunTime)
      PxValue(ptr)^:=Sphere.n(p,fValue)^;//Move(Sphere.n(p,fValue)^,ptr^,SystemWordSize);
     end
    else
    if r=TypeDecl_bool then
     begin
      if Sphere.GetBinaryData(Sphere.n(p,fValue)^)='0' then
        i:=0
      else
        i:=1;
      Move(i,ptr^,SystemWordSize);
     end
    else
      Sphere.Error(p,'unsupported literal type');
   end;
end;

procedure TStratoMachine.PerformSysCall(Sphere: TStratoSphere;
  Fn: xItem; Data: pointer);
var
  p:pointer;
begin
  case Sphere.n(Fn,vOffset)^ of

    stratoSysCall_xinc:
     begin
      p:=pointer(Data^);
      inc(cardinal(p^));
     end;
    stratoSysCall_xdec:
     begin
      p:=pointer(Data^);
      dec(cardinal(p^));
     end;
    stratoSysCall_malloc:
     begin
      p:=Data;
      inc(xPtr(p),SystemWordSize);

      if cardinal(FMemIndex)-cardinal(FMem)+cardinal(p^)>FMemSize then
        Sphere.Error(Fn,'Out of memory')//TODO:throw
      else
       begin
        pointer(Data^):=FMemIndex;
        inc(xPtr(FMemIndex),cardinal(p^));
        //TODO: align?
        //TODO: mark allocated? (see also deallocation)
       end;

     end;

    //stratoSysCall_realloc
    //stratoSysCall_mfree
    stratoSysCall_writeln:
      Writeln(string(Sphere.GetBinaryData(PxValue(Data)^)));


    else Sphere.Error(Fn,'Unknown system call #'+
      IntToHex(Sphere.n(Fn,vOffset)^,4));
  end;
end;

end.

