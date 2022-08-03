unit xsViewMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, Menus, stratoSphere, ExtCtrls, StdCtrls, ImgList,
  stratoDecl, AppEvnts, System.ImageList;

type
  TXsTreeNode=class(TTreeNode)
  private
    Node,JumpNode:xNode;
    JumpedTo:TTreeNode;
  public
    procedure AfterConstruction; override;
  end;

  TfrmXsViewMain = class(TForm)
    MainMenu1: TMainMenu;
    TreeView1: TTreeView;
    File1: TMenuItem;
    Open1: TMenuItem;
    OpenDialog1: TOpenDialog;
    ListBox1: TListBox;
    Splitter1: TSplitter;
    N1: TMenuItem;
    Close1: TMenuItem;
    View1: TMenuItem;
    Clearclicktrack1: TMenuItem;
    panHeader: TPanel;
    txtGoTo: TEdit;
    btnGoTo: TButton;
    Tools1: TMenuItem;
    GoTo1: TMenuItem;
    ImageList1: TImageList;
    Splitter2: TSplitter;
    txtSourceView: TMemo;
    Source1: TMenuItem;
    procedure Open1Click(Sender: TObject);
    procedure TreeView1CreateNodeClass(Sender: TCustomTreeView;
      var NodeClass: TTreeNodeClass);
    procedure TreeView1Expanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure TreeView1DblClick(Sender: TObject);
    procedure AppActivate(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
    procedure Close1Click(Sender: TObject);
    procedure Clearclicktrack1Click(Sender: TObject);
    procedure txtGoToKeyPress(Sender: TObject; var Key: Char);
    procedure GoTo1Click(Sender: TObject);
    procedure btnGoToClick(Sender: TObject);
    procedure TreeView1KeyPress(Sender: TObject; var Key: Char);
    procedure Source1Click(Sender: TObject);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure FormResize(Sender: TObject);
    procedure Splitter2Moved(Sender: TObject);
    procedure TreeView1CustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
  private
    FRoots:array of TXsTreeNode;
    FHighLight1,FHighLight2:xNode;
    FSrcFile:TStratoSphere;
    FSrcPath,FFilePath,FFileSignature:string;
    w1,w2:integer;
    procedure LoadFile(const FilePath:string);
    function BuildNode(Parent:TXsTreeNode;p:xNode):TXsTreeNode;
    procedure JumpTo(p:xNode);
  protected
    procedure DoCreate; override;
    procedure DoShow; override;
    procedure DoDestroy; override;
    procedure WMActivateApp(var Msg:TWMActivateApp); message WM_ACTIVATEAPP;
  end;

var
  frmXsViewMain: TfrmXsViewMain;

implementation

uses
  stratoDebug, stratoTools, stratoTokenizer;

{$R *.dfm}

function FileSignature(const FilePath:string):string;
var
  fh:THandle;
  fd:TWin32FindData;
begin
  fh:=FindFirstFile(PChar(FilePath),fd);
  if fh=INVALID_HANDLE_VALUE then Result:='' else
   begin
    Result:=Format('%.8x%.8x_%d',[
      fd.ftLastWriteTime.dwHighDateTime,
      fd.ftLastWriteTime.dwLowDateTime,
      fd.nFileSizeLow]);//CRC?
    Windows.FindClose(fh);
   end;
end;

{ TXsTreeNode }

const
  iiDefault=0;
  iiList=1;
  iiItem=2;
  iiNameSpace=3;
  iiTypeDecl=4;
  iiEnum=5;
  iiRecord=6;
  iiClass=7;
  iiInterface=8;
  iiArray=9;
  iiSignature=10;
  //iiProperty=11;
  iiFunction=12;
  iiOverload=13;
  iiVar=14;
  iiThis=15;
  iiLiteral=16;
  iiLitVal=17;
  iiCall=18;
  iiBlock=19;
  iiAssign=20;
  iiArg=21;
  iiArgByRef=22;
  iiImport=23;
  iiBinOp=24;
  iiUnOp=25;
  iiCast=26;
  iiSelection=27;
  iiIteration=28;
  iiThrow=29;
  iiSysCall=30;
  iiArrayIndex=31;
  iiInherited=32;
  iiPointer=33;
  iiAddressOf=34;
  iiDereference=35;
  iiConstant=36;
  iiConstructors=37;
  iiConstructor=38;
  iiDestructor=39;
  iiClassRef=40;
  iiPropertyGet=41;
  iiPropertySet=42;
  iiPropCall=43;
  iiPropAssign=44;
  iiField=45;

procedure TXsTreeNode.AfterConstruction;
begin
  inherited;
  //defaults
  Node.none;
  JumpNode.none;
  JumpedTo:=nil;
end;

{ TfrmXsViewMain }

procedure TfrmXsViewMain.DoCreate;
begin
  inherited;
  Application.OnActivate:=AppActivate;
  FSrcPath:='';
  FSrcFile:=nil;
  FFilePath:='';
  FFileSignature:='';
end;

procedure TfrmXsViewMain.DoDestroy;
begin
  inherited;
  //
end;

procedure TfrmXsViewMain.Open1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then LoadFile(OpenDialog1.FileName);
end;

procedure TfrmXsViewMain.LoadFile(const FilePath: string);
var
  i:cardinal;
  s:TStratoSphere;
  t:string;
  p:PxKeyValue;
  n:TXsTreeNode;
begin
  ReadSettings(ExtractFilePath(ParamStr(0))+'strato.ini');//?
  LoadFromFile(FilePath);

  FFilePath:=FilePath;
  FFileSignature:=FileSignature(FilePath);
  Caption:='xsView - '+FilePath;
  Application.Title:=Caption;
  panHeader.Visible:=false;
  txtGoTo.Text:='';

  ListBox1.Items.BeginUpdate;
  try
    ListBox1.Items.Clear;
  finally
    ListBox1.Items.EndUpdate;
  end;

  TreeView1.Items.BeginUpdate;
  try
    TreeView1.Items.Clear;

    SetLength(FRoots,SpheresCount);
    for i:=0 to SpheresCount-1 do
     begin
      s:=Spheres[i];
      t:='$'+IntToStr(i+1)+' #'+IntToStr(s.kCount);

      p:=s.v(0,iSphere_FileName);
      if p<>nil then t:=t+' '+UTF8ToString(s.BinaryData(p.v));
      p:=s.v(0,vSphere_FileSize);
      if p<>nil then t:=t+' ('+IntToStr(p.v)+' B)';

      n:=TreeView1.Items.AddChild(nil,t) as TXsTreeNode;
      n.Node.s(s,0);
      n.ImageIndex:=iiDefault;
      n.SelectedIndex:=iiDefault;
      n.HasChildren:=true;
      FRoots[i]:=n;
     end;

  finally
    TreeView1.Items.EndUpdate;
  end;
end;

procedure TfrmXsViewMain.TreeView1CreateNodeClass(Sender: TCustomTreeView;
  var NodeClass: TTreeNodeClass);
begin
  NodeClass:=TXsTreeNode;
end;

procedure TfrmXsViewMain.TreeView1DblClick(Sender: TObject);
var
  n:TXsTreeNode;
  i:integer;
  s:string;
begin
  n:=TreeView1.Selected as TXsTreeNode;
  if n<>nil then
    if SpheresCount=0 then
      Open1.Click
    else
     begin
      if n.JumpedTo=nil then
       begin
        JumpTo(n.JumpNode);
        n.JumpedTo:=TreeView1.Selected;
       end
      else
        TreeView1.Selected:=n.JumpedTo;
      if n<>TreeView1.Selected then
       begin
        while (n<>nil) and (n.Node.sphere<>nil) do
          n:=n.Parent as TXsTreeNode;
        if n<>nil then
         begin
          //see also ListBox1DblClick
          s:=Format('#%.13d %s',[n.Node.index,n.Text]);
          i:=ListBox1.Items.IndexOf(s);
          if i=-1 then i:=ListBox1.Items.Add(s);
          ListBox1.ItemIndex:=i;
         end;
       end;
     end;
end;

procedure TfrmXsViewMain.JumpTo(p:xNode);
var
  n:TXsTreeNode;
  q:xNode;
  a:array of xNode;
  ai,al:cardinal;
  b:boolean;
  nn:xKey;
begin
  if p.sphere<>nil then
   begin
    al:=0;
    ai:=0;
    q:=p;
    while q.sphere<>nil do
     begin
      if ai=al then
       begin
        inc(al,$400);//grow;
        SetLength(a,al);
       end;
       nn:=q.Key;
       //if nn=xBinaryData then nFindPrev(q);//TODO
       a[ai]:=q;
       inc(ai);
       //try
        if (nn=xBinaryData) or (nn=nLiteral) then
          q.none
        else
          q:=q.r(iParent);
       //except
       //  on EStratoFieldIndexNotFound do q.none;
       //end;
     end;
    if ai=0 then n:=nil else n:=FRoots[SphereIndex(a[ai-1].sphere)-1];
    b:=true;

    //TODO: revise this to resolve each node in path of item first,
    //and less blunt-force expanding like now

    while (ai<>0) and (n<>nil) do
     begin
      dec(ai);
      while (n<>nil) and not((n.Node.sphere=a[ai].sphere) and (n.Node.index=a[ai].index)) do
       begin
        if (n.HasChildren) and (n.GetFirstChild=nil) then
          TreeView1Expanding(nil,n,b);
        n:=n.GetNext as TXsTreeNode;
       end;
      //if n<>nil then TreeView1Expanding(nil,n,b);
     end;
    if n=nil then
     begin
      //make here?
      n:=TreeView1.Selected as TXsTreeNode;
      if n<>nil then
        TreeView1.Selected:=BuildNode(n,n.JumpNode);
     end
    else
     begin
      n.MakeVisible;
      TreeView1.Selected:=n;
     end;
   end;
end;

procedure TfrmXsViewMain.DoShow;
var
  tw:cardinal;
begin
  inherited;
  w1:=ClientWidth;
  w2:=txtSourceView.Width;
  tw:=12;
  if ParamCount<>0 then LoadFile(ParamStr(1));
  txtSourceView.Perform(EM_SETTABSTOPS,1,integer(@tw));
end;

procedure TfrmXsViewMain.AppActivate(Sender: TObject);
begin
  //TODO: check file modified? reload?
end;

procedure TfrmXsViewMain.WMActivateApp(var Msg:TWMActivateApp);
var
  s:string;
begin
  if Msg.Active then
    if FFilePath<>'' then
     begin
      s:=FileSignature(FFilePath);
      if s<>FFileSignature then
       begin
        //TODO: MessageBox?
        //TODO: attempt to restore expanded nodes state, selected/focused
        LoadFile(FFilePath);
        FSrcPath:='';//case reload
        FSrcFile:=nil;
       end;
     end;
end;

function TfrmXsViewMain.BuildNode(Parent:TXsTreeNode;p:xNode):TXsTreeNode;
var
  p1,p2,p0:xNode;
  q:PxKeyValue;
  nn:xKey;
  k,sy,sx:cardinal;
  n:TXsTreeNode;
  v:PxKeyValue;
  s,t:string;
begin
  nn:=p.Key;
  q:=p.sphere[p.index];
  k:=iiDefault;
  p1.none;//set ExpandIndex?
  p2.none;//set JumpNode?
  if nn in [xUnassigned..n_Max] then
   begin
    n:=Parent;
    while (n<>nil) and (n.Node.IsNone) do n:=n.Parent as TXsTreeNode;
    if (n=nil) or (n.Node.sphere<>p.sphere) then s:=p.AsString else s:=IntToStr(p.index);
    s:=s+': '+UTF8ToWideString(StratoDumpThing(p));
    if StratoGetSourceFile(p,sy,sx,p1,p2) then
     begin
      v:=p.sphere.v(0,iSphere_FileName);
      if (v=nil) or (sy=0) then
        s:=s+' []'
      else
        s:=Format('%s  [%s(%d:%d)]',[s
          ,p.sphere.BinaryData(v.v)
          ,sy
          ,sx
          ]);
     end;
   end
  else
   begin
    s:='';
    if nn>n_Max then k:=iiItem;
   end;
  t:='';//see below
  case nn of

  nNameSpace:k:=iiNameSpace;
  nType:k:=iiTypeDecl;
  nRecord:k:=iiRecord;
  nEnum:k:=iiEnum;
  //nImport:k:=iiImport;
  //nTry,nDeferred:
  nThrow:k:=iiThrow;
  nArray:k:=iiArray;
  //nMember:k:=iiFunction;
  nVar,nVarByRef,nVarReadOnly:k:=iiVar;
  nThis:k:=iiThis;
  nConstant:k:=iiConstant;
  nLiteral:k:=iiLiteral;
  nSignature:k:=iiSignature;
  nOverload:k:=iiOverload;
  //nCtors:k:=iiConstructors;
  nCtor:k:=iiConstructor;
  nFCall,nSCall,nVCall,nICall:k:=iiCall;
  nSigArg:k:=iiArg;
  nSigArgByRef:k:=iiArgByRef;
  //nInherited:k:=iiInherited;
  nArrayIndex:k:=iiArrayIndex;
  nField:k:=iiField;
  nCodeBlock:k:=iiBlock;
  nAssign:k:=iiAssign;
  nUnaryOp:k:=iiUnOp;
  nBinaryOp:k:=iiBinOp;
  nCast:k:=iiCast;
  nClass:k:=iiClass;
  nSelection:k:=iiSelection;
  nIteration,nIterPostEval:k:=iiIteration;
  //nCatch:
  nPointer:k:=iiPointer;
  nCallArg:k:=iiArg;
  //nVarByRef:
  nAddressOf:k:=iiAddressOf;
  nDereference:k:=iiDereference;
  nDtor:k:=iiDestructor;
  nInterface:k:=iiInterface;
  nClassRef:k:=iiClassRef;
  nPropGet:k:=iiPropertyGet;
  nPropSet:k:=iiPropertySet;
//      nPropGetCall:k:=iiPropCall;
//      nPropSetCall:k:=iiPropAssign;

  iSphere_FileName:
    s:=':FileName="'+UTF8ToWideString(p.sphere.BinaryData(q.v))+'"';
  vSphere_FileSize:
    s:=':FileSize='+IntToStr(q.v);
  vSphere_FileHash:
    s:=':FileHash='+IntToStr(q.v);//TODO
  vSphere_SrcPosLineIndex:
    s:=':SrcPosLineIndex='+IntToStr(q.v);
  lSphere_Errors:
    s:=':Errors->'+IntToStr(q.v);
  iSphere_Local:
    t:=':Local=';
  lSphere_Globals:
    s:=':Globals->';
  lSphere_Dictionary:
    //s:='';//if ? then
    s:=':Dictionary->';
  iSphere_InitializationBlock:
    t:=':Initialization=';
  iSphere_FinalizationBlock:
    t:=':Finalization=';

  xBinaryData:
   begin
    k:=iiLitVal;
    s:='('+IntToStr(q.i)+')"'+UTF8ToWideString(p.sphere.BinaryData(p.index))+'"';
   end;
  xDictionary_Entry,xDictionary_Tail:
   begin
    k:=iiDefault;
    s:=p.AsString+': '+UTF8ToWideString(StratoDumpThing(p));
   end;

  iParent,vSrcPos,dName:
    s:='';//don't show (assert done by

  iType:
    t:=':Type=';
  vByteSize:
    s:=':ByteSize='+IntToStr(q.v);
  lChildren:
    ;//see TreeView1Expanding
  vOffset:
    s:=':Offset='+IntToStr(integer(q.v));
  vOperator:
    s:=':Operator "'+string(TokenName[TStratoToken(q.v)])+'"';

  iSubject:
    t:=':Subject=';
  iTarget:
    t:=':Target=';
  iSignature:
    t:=':Signature=';
  iValue:
    t:=':Value=';
  iReturnType:
    t:=':ReturnType=';
  iInheritsFrom:
    t:=':InheritsFrom=';

  iBody:
   begin
    //t:=':Body=';
    p1.s(p.sphere,q.v);
    BuildNode(Parent,p1);
   end;
  iLeft:
    t:=':Left=';
  iRight:
    t:=':Right=';
  iArgVar:
    t:=':ArgVar=';
  iPredicate:
    t:=':Predicate=';
  iDoTrue:
    t:=':DoTrue=';
  iDoFalse:
    t:=':DoFalse=';

  lArguments:
    s:=':Arguments->';

  lCodeBlock_Locals:
    s:=':Locals->';
  lCodeBlock_Statements:
    s:='{x}->';
  vCodeBlock_LocalsSize:
    s:=':{#'+IntToStr(q.v)+'}';

  else
    s:=':?'+IntToStr(cardinal(p.sphere[p.index].k))+'='+IntToStr(p.sphere[p.index].v);
  end;
  if t<>'' then
   begin
    if q.i=0 then s:=t+IntToStr(q.v) else
      s:=t+'$'+IntToStr(q.i)+'#'+IntToStr(q.v);
    p1.ss(p.sphere,q.i,q.v);
   end;
  if s<>'' then
   begin
    n:=TreeView1.Items.AddChild(Parent,s) as TXsTreeNode;
    n.Node:=p;
    if p1.sphere<>nil then
     begin
      n.JumpNode:=p1;
      p0:=p1.r(iParent);
      if not(p0.IsNone) and p0.IsSame(p) then n.JumpedTo:=BuildNode(n,p1);
     end;
    case nn of
    xUnassigned..n_Max1,nSphere:
      n.HasChildren:=true;
    lSphere_Errors,lSphere_Globals,lSphere_Dictionary,
    lChildren,lArguments,lCodeBlock_Locals,lCodeBlock_Statements:
     begin
      k:=iiList;
      n.HasChildren:=true;
     end;
    xDictionary_Entry:
      if p.sphere[p.index].v<>0 then
        n.HasChildren:=true;
    iSphere_InitializationBlock,iSphere_FinalizationBlock:
      n.HasChildren:=true;
    end;
    n.ImageIndex:=k;
    n.SelectedIndex:=k;
    Result:=n;
   end
  else
    Result:=nil;
end;

procedure TfrmXsViewMain.TreeView1Expanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
var
  n:TXsTreeNode;
  p,p1,l:xNode;
  q:PxKeyValue;
begin
  if Node.HasChildren and (Node.GetFirstChild=nil) then
   begin
    n:=Node as TXsTreeNode;
    TreeView1.Items.BeginUpdate;
    try
      Node.HasChildren:=false;
      p:=n.Node;
      q:=p.sphere[p.index];
      case q.k of
      xUnassigned:;
      nNameSpace..n_Max:
       begin
        p1.none;
        p.index:=q.n;
        while p.index<>0 do
         begin
          case p.sphere[p.index].k of
          xUnassigned:;
          lChildren:p1:=p;
          else
            BuildNode(n,p);
          end;
          p.index:=p.sphere[p.index].n;
         end;
        while p1.Next(p) do
          BuildNode(n,p);
       end;

      lSphere_Errors,
      lCodeBlock_Locals,lCodeBlock_Statements,
      lArguments,
      lChildren:
       begin
        l:=p;
        while l.Next(p1) do
          BuildNode(n,p1);
       end;
      //TODO: lSphere_Globals: list of jumpnodes

      lSphere_Dictionary,xDictionary_Entry:
       begin
        p1.sphere:=p.sphere;
        p1.index:=q.v;
        while p1.index<>0 do
         begin
          BuildNode(n,p1);
          p1.index:=p1.sphere.k[p1.index].n;
         end;
       end;

      xDictionary_Tail:;

      iSphere_InitializationBlock,iSphere_FinalizationBlock:
        if not n.JumpNode.IsNone then
          BuildNode(n,n.JumpNode);

      ////$IFDEF DEBUG?
      else
        TreeView1.Items.AddChild(n,'?'+IntToStr(cardinal(q.k)));
      end;
    finally
      TreeView1.Items.EndUpdate;
    end;
   end;
  //AllowExpansion:=true;
end;

procedure TfrmXsViewMain.ListBox1DblClick(Sender: TObject);
var
  s:string;
begin
  if ListBox1.ItemIndex<>-1 then
   begin
    s:=ListBox1.Items[ListBox1.ItemIndex];
//    JumpTo(xxr(StrToInt(Copy(s,2,13))));
    if TreeView1.Selected<>nil then TreeView1.SetFocus;
   end;
end;

procedure TfrmXsViewMain.Close1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmXsViewMain.Clearclicktrack1Click(Sender: TObject);
begin
  ListBox1.Items.BeginUpdate;
  try
    ListBox1.Items.Clear;
  finally
    ListBox1.Items.EndUpdate;
  end;
end;

procedure TfrmXsViewMain.txtGoToKeyPress(Sender: TObject; var Key: Char);
begin
  if Key=#13 then btnGoTo.Click;
end;

procedure TfrmXsViewMain.GoTo1Click(Sender: TObject);
begin
  panHeader.Visible:=true;
  txtGoTo.SelectAll;
  txtGoTo.SetFocus;
end;

procedure TfrmXsViewMain.btnGoToClick(Sender: TObject);
var
  s:string;
  p:xNode;
  b:boolean;
  i,l:integer;
begin
  //TODO: input box
  s:=txtGoTo.Text;
  txtGoTo.SelectAll;
  l:=Length(s);
  i:=1;
  while (i<=l) and (AnsiChar(s[i]) in ['0'..'9']) do inc(i);
  if i<=l then
    p.s(Spheres[StrToInt(Copy(s,1,i-1))],StrToInt(Copy(s,i+1,l-i)))
  else
    p.s(Spheres[0],StrToInt(s));
  TreeView1.Selected:=nil;
  b:=true;
  while (p.sphere<>nil) and b do
   begin
    JumpTo(p);//TODO
    if TreeView1.Selected=nil then
     begin
      //if p.Key=xBinaryData then nFindPrev(p);
      p:=p.r(iParent);
     end
    else
     begin
      TreeView1.SetFocus;
      b:=false;
     end;
   end;
end;

procedure TfrmXsViewMain.TreeView1KeyPress(Sender: TObject; var Key: Char);
begin
  if Key=#13 then TreeView1DblClick(Sender);
end;

procedure TfrmXsViewMain.Source1Click(Sender: TObject);
var
  b:boolean;
begin
  b:=not(Source1.Checked);
  Source1.Checked:=b;
  txtSourceView.Visible:=b;
  Splitter2.Visible:=b;
  if b then TreeView1Change(TreeView1,TreeView1.Selected);
end;

procedure TfrmXsViewMain.TreeView1Change(Sender: TObject; Node: TTreeNode);
var
  n:TXsTreeNode;
  q,h1,h2,p1,p2:xNode;
  i,Line,Col:cardinal;
  si:TScrollInfo;
  v:PxKeyValue;
begin
  if txtSourceView.Visible then
   begin
    h1:=FHighLight1;
    h2:=FHighLight2;
    if Node=nil then
     begin
      txtSourceView.Clear;
      FSrcFile:=nil;
      FHighLight1.none;
      FHighLight2.none;
     end
    else
     begin
      n:=Node as TXsTreeNode;
      Line:=0;
      Col:=0;
      FHighLight1:=n.JumpNode;
      FHighLight2:=n.Node;
      q:=FHighLight2;
      try
        if (q.sphere<>nil) then
          if not StratoGetSourceFile(q,Line,Col,p1,p2) then Line:=0;
        if Line=0 then
         begin
          //txtSourceView.Text:=Format('???[%d] %d:%d',[q,Line,Col])
          txtSourceView.SelLength:=0;
         end
        else
         begin
          if n.Node.sphere<>FSrcFile then
           begin
            FSrcFile:=nil;//in case of error
            //TODO: resolve relative path
            //TODO: cache several?
            v:=n.Node.sphere.v(0,iSphere_FileName);
            if v=nil then FSrcPath:='' else
              FSrcPath:=ResolveKnownPath(UTF8ToString(
                n.Node.sphere.BinaryData(v.v)));
            //TODO: check signature/timestamp
            if FSrcPath='' then
              txtSourceView.Lines.Clear
            else
              txtSourceView.Lines.LoadFromFile(FSrcPath);
            FSrcFile:=n.Node.sphere;
           end;
          //lblFileName.Caption:=Format('%s %d:%d',[FSrcPath,Line,Col]);
          if Line>7 then
           begin
            si.cbSize:=SizeOf(TScrollInfo);
            si.fMask:=SIF_POS;
            si.nTrackPos:=SB_VERT;
            if GetScrollInfo(txtSourceView.Handle,SB_VERT,si) then
               txtSourceView.Perform(EM_LINESCROLL,0,(integer(Line)-7)-si.nPos);
           end
          else;
          i:=txtSourceView.Perform(EM_LINEINDEX,Line-1,0)+integer(Col)-1;
          txtSourceView.Perform(EM_SETSEL,i,i+1);
          //txtSourceView.Perform(EM_SCROLLCARET,0,0);
         end;
      except
        //on e:Exception do?
          txtSourceView.Text:=Format('!!![%s] "%s"%d:%d',
            [q.AsString,FSrcPath,Line,Col]);
      end;
     end;
    if not((FHighLight1.sphere=h1.sphere) or (FHighLight1.index=h1.index)) or
      not((FHighLight2.sphere=h2.sphere) or (FHighLight2.index=h2.index)) then
      TreeView1.Invalidate;
   end;
end;

procedure TfrmXsViewMain.FormResize(Sender: TObject);
var
  i:integer;
begin
  i:=ClientWidth*w2 div w1;
  if i<Splitter2.MinSize then i:=Splitter2.MinSize;
  txtSourceView.Width:=i;
end;

procedure TfrmXsViewMain.Splitter2Moved(Sender: TObject);
begin
  w1:=ClientWidth;
  w2:=txtSourceView.Width;
end;

procedure TfrmXsViewMain.TreeView1CustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  p:xNode;
begin
  if ((FHighLight1.sphere<>nil) or (FHighLight2.sphere<>nil))
    and not(cdsSelected in State) then
    if Node is TXsTreeNode then
     begin
      p:=(Node as TXsTreeNode).Node;
      if (FHighLight1.sphere<>nil)
        and (p.sphere=FHighLight1.sphere) and (p.index=FHighLight1.index) then
        Sender.Canvas.Brush.Color:=$00CCFF //gold
      else
      if (FHighLight2.sphere<>nil)
        and ((Node as TXsTreeNode).JumpNode.sphere=FHighLight2.sphere)
        and ((Node as TXsTreeNode).JumpNode.index=FHighLight2.index) then
        Sender.Canvas.Brush.Color:=$00CC00 //green
      else
      if (FHighLight2.sphere<>nil)
        and (p.sphere=FHighLight2.sphere) and (p.index=FHighLight2.index) then
        Sender.Canvas.Brush.Color:=$FFCC99 //skyblue
      ;
     end;
end;

end.
