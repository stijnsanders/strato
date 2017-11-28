unit xsViewMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, Menus, stratoSphere, ExtCtrls, StdCtrls, ImgList,
  stratoDecl, AppEvnts;

type
  TXsTreeNode=class(TTreeNode)
  private
    Index,ExpandIndex,JumpIndex:rItem;
    ExpandSingle:boolean;
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
    FHighLight1,FHighLight2:rItem;
    FSrcFile:PxSourcefile;
    FSrcPath,FFilePath,FFileSignature:string;
    w1,w2:integer;
    FCheckZero:boolean;
    procedure LoadFile(const FilePath:string);
    function JumpNode(n: TXsTreeNode; const prefix: string;
      pp: rItem): TXsTreeNode;
    function ListNode(n: TXsTreeNode; const prefix: string;
      pp: rItem): TXsTreeNode;
    function ShowNode(n: TXsTreeNode; const prefix: string;
      pp: rItem): TXsTreeNode;
    function BuildNode(Parent: TXsTreeNode; p: rItem): TXsTreeNode;
    procedure JumpTo(p:rItem);
    procedure nFindPrev(var p:rItem);
  protected
    procedure DoCreate; override;
    procedure DoShow; override;
    procedure DoDestroy; override;
    procedure WMActivateApp(var Msg: TWMActivateApp); message WM_ACTIVATEAPP;
  end;

var
  frmXsViewMain: TfrmXsViewMain;

implementation

uses
  stratoDebug, stratoTools;

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
  Index.x:=0;
  ExpandIndex.x:=0;
  JumpIndex.x:=0;
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
  src:PxSourceFile;
  s:string;
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

  FCheckZero:=false;
  TreeView1.Items.BeginUpdate;
  try
    TreeView1.Items.Clear;

    SetLength(FRoots,SourceFilesCount);
    for i:=0 to SourceFilesCount-1 do
     begin
      src:=@SourceFiles[i];
      if src.FileName=0 then s:='' else
        s:=BinaryData(xxr(src.FileName));

      n:=TreeView1.Items.AddChild(nil,Format(
        '%s #%d',[s,src.FileSize])) as TXsTreeNode;
      n.Index.x:=0;
      n.ImageIndex:=iiDefault;
      n.SelectedIndex:=iiDefault;
      FRoots[i]:=n;

      //ListNode(n,':Dependencies->',src.Dependencies));
      ListNode(n,':Namespaces->',xxr(src.NameSpaces));
      JumpNode(n,':Local=',xxr(src.Local));
      ListNode(n,':Globals->',xxr(src.Globals));
      //ListNode(n,':Dictionary->',xxr(src.Dictionary));
      ShowNode(n,':Initialization=',xxr(src.InitializationBlock));
      ShowNode(n,':Finalization=',xxr(src.FinalizationBlock));
     end;

  finally
    TreeView1.Items.EndUpdate;
    FCheckZero:=true;
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
    if SourceFilesCount=0 then
      Open1.Click
    else
     begin
      if n.JumpedTo=nil then
       begin
        JumpTo(n.JumpIndex);
        n.JumpedTo:=TreeView1.Selected;
       end
      else
        TreeView1.Selected:=n.JumpedTo;
      if n<>TreeView1.Selected then
       begin
        while (n<>nil) and (n.Index.x=0) do
          n:=n.Parent as TXsTreeNode;
        if n<>nil then
         begin
          //see also ListBox1DblClick
          s:=Format('#%.13d %s',[n.Index.x,n.Text]);
          i:=ListBox1.Items.IndexOf(s);
          if i=-1 then i:=ListBox1.Items.Add(s);
          ListBox1.ItemIndex:=i;
         end;
       end;
     end;
end;

procedure TfrmXsViewMain.JumpTo(p:rItem);
var
  n:TXsTreeNode;
  q:rItem;
  a:array of rItem;
  ai,al:cardinal;
  b:boolean;
  nn:xTypeNr;
begin
  if p.x<>0 then
   begin
    al:=0;
    ai:=0;
    q:=p;
    while q.x<>0 do
     begin
      if ai=al then
       begin
        inc(al,$400);//grow;
        SetLength(a,al);
       end;
       nn:=q.NodeType;
       if nn=n_BinaryData then nFindPrev(q);
       a[ai]:=q;
       inc(ai);
       try
        if (nn=n_BinaryData) or (nn=nLiteral) then
          q.x:=0
        else
          q:=q.r(iParent);
       except
         on EStratoFieldIndexNotFound do q.x:=0;
       end;
     end;
    if ai=0 then n:=nil else n:=FRoots[a[ai-1].x div StratoSphereBlockBase];
    b:=true;
    while (ai<>0) and (n<>nil) do
     begin
      dec(ai);
      while (n<>nil) and (n.Index.x<>a[ai].x) do
       begin
        if (n.HasChildren) and (n.Count=0) and (n.Index.x=0) then
          TreeView1Expanding(nil,n,b);
        n:=n.GetNext as TXsTreeNode;
       end;
      if n<>nil then TreeView1Expanding(nil,n,b);
     end;
    if n=nil then
     begin
      //make here?
      n:=TreeView1.Selected as TXsTreeNode;
      if n<>nil then
        TreeView1.Selected:=BuildNode(n,n.JumpIndex);
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
  if ParamCount<>0 then LoadFile(ParamStr(1));
  w1:=ClientWidth;
  w2:=txtSourceView.Width;
  tw:=12;
  txtSourceView.Perform(EM_SETTABSTOPS,1,integer(@tw));
end;

procedure TfrmXsViewMain.AppActivate(Sender: TObject);
begin
  //TODO: check file modified? reload?
end;

procedure TfrmXsViewMain.WMActivateApp(var Msg: TWMActivateApp);
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

function TfrmXsViewMain.JumpNode(n:TXsTreeNode;
  const prefix:string; pp:rItem):TXsTreeNode;
var
  nn:xTypeNr;
  b:boolean;
  p,q:rItem;
  m:TXsTreeNode;
begin
  if pp.x=0 then
    Result:=nil
  else
   begin
    Result:=TreeView1.Items.AddChild(n,prefix+ItemToStr(pp)) as TXsTreeNode;
    nn:=pp.NodeType;
    if (nn<>n_BinaryData) and (nn<>nLiteral) then
     begin
      b:=false;
      try
        q:=pp.r(iParent);
      except
        on EStratoFieldIndexNotFound do q.x:=0;
      end;
      m:=n;
      while not(b) and (m<>nil) do
       begin
        if m<>nil then
         begin
          p:=m.Index;
          if p.x=pp.x then m:=nil else
            if p.x=q.x then b:=true;
         end;
        if m<>nil then m:=m.Parent as TXsTreeNode;
       end;
     end
    else
      b:=true;
    if b then
     begin
      //BuildNode(Result,i)
      Result.HasChildren:=true;
      Result.ExpandIndex:=pp;
      Result.ExpandSingle:=true;
     end
    else
      Result.JumpIndex:=pp;
    //TODO: if Sphere[i].Parent=Sphere[(n as TXsTreeNode).Index].Parent ?
    Result.ImageIndex:=iiItem;
    Result.SelectedIndex:=iiItem;
   end;
end;

function TfrmXsViewMain.ListNode(n:TXsTreeNode;const prefix:string;
  pp:rItem):TXsTreeNode;
begin
  Result:=TreeView1.Items.AddChild(n,prefix+ItemToStr(pp)) as TXsTreeNode;
  if pp.x<>0 then
   begin
    Result.HasChildren:=true;
    Result.ExpandIndex:=pp;
   end;
  Result.ImageIndex:=iiList;
  Result.SelectedIndex:=iiList;
end;

function TfrmXsViewMain.ShowNode(n:TXsTreeNode;const prefix:string;
  pp:rItem):TXsTreeNode;
begin
  Result:=TreeView1.Items.AddChild(n,prefix+ItemToStr(pp)) as TXsTreeNode;
  if pp.x<>0 then
   begin
    Result.HasChildren:=true;
    Result.ExpandIndex:=pp;
    Result.ExpandSingle:=true;
   end;
  Result.ImageIndex:=iiItem;
  Result.SelectedIndex:=iiItem;
end;

function TfrmXsViewMain.BuildNode(Parent:TXsTreeNode;p:rItem):TXsTreeNode;
var
  p1,p2:rItem;
  nn:xTypeNr;
  k,sy,sx:cardinal;
  n:TXsTreeNode;
  s:string;
  src:PxSourceFile;
begin
  if (p.x=0) and FCheckZero then
    Result:=nil
  else
   begin
    s:=ItemToStr(p)+': '+StratoDumpThing(p);
    nn:=p.NodeType;
    if (nn<>n_BinaryData) and StratoGetSourceFile(p,sy,sx,p1,p2) then
     begin
      src:=@SourceFiles[rSrc(p)];
      if (src.FileName=0) or (sy=0) then
        s:=s+' []'
      else
        s:=Format('%s  [%s(%d:%d)]',[s
          ,BinaryData(xxr(src.FileName))
          ,sy
          ,sx
          ]);
     end;
    n:=TreeView1.Items.AddChild(Parent,s) as TXsTreeNode;
    n.Index:=p;
    k:=iiDefault;
    p1.x:=0;//set ExpandIndex?
    case nn of

      nNameSpace,nType,nRecord,nEnum,nMember,nCtors,nDefer:
        p1:=p.r(lItems);
      nArray:
       begin
        p1:=p.r(lItems);
        n.JumpIndex:=p.r(iType);
       end;
      nLiteral,nConstant,nSigArg,nVar:
       begin
        p1:=p.r(iValue);
        n.ExpandSingle:=true;
        n.JumpIndex:=p.r(iType);
       end;
      nSignature:
       begin
        JumpNode(n,':subject=',p.r(iSubject));
        ListNode(n,':arguments->',p.r(lArguments));
        JumpNode(n,':result=',p.r(iReturnType));
       end;
      nSigArgByRef:
        n.JumpIndex:=p.r(iType);
      nOverload,nCtor,nDtor,nPropGet,nPropSet:
       begin
        if nn<>nDtor then
         begin
          JumpNode(n,':signature=',p.r(iSignature));
          JumpNode(n,':argvars->',p.r(iFirstArgVar));
         end;
        BuildNode(n,p.r(iBody));
       end;
      nPointer:
       begin
        p1:=p.r(lItems);
        n.JumpIndex:=p.r(iTarget);
       end;
      nTypeAlias,nGlobal,nClassRef:
        n.JumpIndex:=p.r(iTarget);

      nClass,nInterface:
       begin
        p1:=p.r(lItems);
        n.JumpIndex:=p.r(iInheritsFrom);
       end;

      nCodeBlock:
       begin
        n.JumpIndex:=p.r(iReturnType);
        ListNode(n,':var->',p.r(lLocals));
        ListNode(n,':cmd->',p.r(lItems));
       end;
      nVarByRef,nThis:
        n.JumpIndex:=p.r(iType);

      nSCall,nFCall:
       begin
        JumpNode(n,':target=',p.r(iTarget));
        ListNode(n,':arguments->',p.r(lArguments));
       end;
      nVCall,nICall:
       begin
        JumpNode(n,':subject=',p.r(iSubject));
        JumpNode(n,':target=',p.r(iTarget));
        ListNode(n,':arguments->',p.r(lArguments));
       end;
      nCallArg:
        n.JumpIndex:=p.r(iValue);

      nCast:
       begin
        n.JumpIndex:=p.r(iType);
        JumpNode(n,':subject=',p.r(iSubject));
       end;
      nAddressOf,nDereference:
       begin
        n.JumpIndex:=p.r(iReturnType);
        JumpNode(n,':subject=',p.r(iSubject));
       end;
      nArrayIndex:
       begin
        JumpNode(n,':array=',p.r(iSubject));
        ListNode(n,':index->',p.r(lItems));
        //TODO: multiple Indexes?
       end;
      nField:
       begin
        JumpNode(n,':x=',p.r(iSubject));
        JumpNode(n,':y=',p.r(iTarget));
       end;
      nAssign:
       begin
        JumpNode(n,':ValueFrom ',p.r(iValue));
        JumpNode(n,':AssignTo ',p.r(iTarget));
       end;
      nUnaryOp:
       begin
        n.JumpIndex:=p.r(iReturnType);
        JumpNode(n,':Right ',p.r(iRight));
       end;
      nBinaryOp:
       begin
        n.JumpIndex:=p.r(iReturnType);
        JumpNode(n,':Left ',p.r(iLeft));
        JumpNode(n,':Right ',p.r(iRight));
       end;
      nSelection:
       begin
        JumpNode(n,':If ',p.r(iPredicate));
        JumpNode(n,':Then ',p.r(iDoTrue));
        JumpNode(n,':Else ',p.r(iDoFalse));
       end;
      nIteration,nIterPostEval:
       begin
        n.JumpIndex:=p.r(iReturnType);
        JumpNode(n,':Pred ',p.r(iPredicate));
        BuildNode(n,p.r(iBody));
       end;
      nRange:
       begin
        n.JumpIndex:=p.r(iReturnType);
        JumpNode(n,':Start ',p.r(iLeft));
        JumpNode(n,':Stop ',p.r(iRight));
       end;
      nRangeIndex:
       begin
        //n.JumpIndex:=p.r(iReturnType);
        JumpNode(n,':Iterator ',p.r(iLeft));
        JumpNode(n,':Range ',p.r(iRight));
       end;

      nThrow:
        BuildNode(n,p.r(iSubject));
      nCatch:
       begin
        ListNode(n,':t->',p.r(lTypes));
        JumpNode(n,':e=',p.r(iTarget));
        BuildNode(n,p.r(iBody));
       end;

    end;
    case nn of
      nNameSpace:k:=iiNameSpace;
      nType:k:=iiTypeDecl;
      nRecord:k:=iiRecord;
      nEnum:k:=iiEnum;
      nGlobal:k:=iiItem;
      //nExternal:k:=iiImport;
      //nTry,nDeferred:
      nThrow:k:=iiThrow;
      nArray:k:=iiArray;
      nMember:k:=iiFunction;
      nVar,nVarByRef,nVarReadOnly:k:=iiVar;
      nThis:k:=iiThis;
      nConstant:k:=iiConstant;
      nLiteral:k:=iiLiteral;
      n_BinaryData:k:=iiLitVal;
      nSignature:k:=iiSignature;
      nOverload:k:=iiOverload;
      nCtors:k:=iiConstructors;
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
    end;
    n.ImageIndex:=k;
    n.SelectedIndex:=k;
    if p1.x<>0 then
     begin
      n.HasChildren:=true;
      n.ExpandIndex:=p1;
     end;
    Result:=n;
   end;
end;

procedure TfrmXsViewMain.TreeView1Expanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
var
  n:TXsTreeNode;
  p,q:rItem;
begin
  if Node.HasChildren and (Node.Count=0) then
   begin
    n:=Node as TXsTreeNode;
    TreeView1.Items.BeginUpdate;
    try
      Node.HasChildren:=false;
      p:=n.ExpandIndex;
      if n.ExpandSingle then
        BuildNode(n,p)
      else
       begin
        q:=p.r(iNext);
        while q.x<>0 do
         begin
          BuildNode(n,q);
          if q.x=p.x then q.x:=0 else q:=q.r(iNext);
         end;
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
    JumpTo(xxr(StrToInt(Copy(s,2,13))));
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
  p:rItem;
  b:boolean;
  i,l:integer;
begin
  //TODO: input box
  s:=txtGoTo.Text;
  l:=Length(s);
  i:=1;
  while (i<=l) and (s[i] in ['0'..'9']) do inc(i);
  if i<=l then
    p.x:=(StrToInt(Copy(s,1,i-1)) * StratoSphereBlockBase)
         +StrToInt(Copy(s,i+1,l-i))
  else
    p.x:=StrToInt(s);
  TreeView1.Selected:=nil;
  b:=true;
  while (p.x<>0) and b do
   begin
    JumpTo(p);//TODO
    if TreeView1.Selected=nil then
     begin
      if p.NodeType=n_BinaryData then nFindPrev(p);
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
  q,h1,h2,p1,p2:rItem;
  i,Line,Col:cardinal;
  si:TScrollInfo;
  src:PxSourcefile;
begin
  if txtSourceView.Visible then
   begin
    h1:=FHighLight1;
    h2:=FHighLight2;
    if Node=nil then
     begin
      txtSourceView.Clear;
      FSrcFile:=nil;
      FHighLight1.x:=0;
      FHighLight2.x:=0;
     end
    else
     begin
      n:=Node as TXsTreeNode;
      Line:=0;
      Col:=0;
      FHighLight1:=n.JumpIndex;
      FHighLight2:=n.Index;
      q:=FHighLight2;
      try
        if (q.x<>0) then
          if not StratoGetSourceFile(q,Line,Col,p1,p2) then Line:=0;
        if Line=0 then
         begin
          //txtSourceView.Text:=Format('???[%d] %d:%d',[q,Line,Col])
          txtSourceView.SelLength:=0;
         end
        else
         begin
          src:=@SourceFiles[rSrc(n.Index)];
          if src<>FSrcFile then
           begin
            FSrcFile:=nil;//in case of error
            //TODO: resolve relative path
            //TODO: cache several?
            FSrcPath:=ResolveKnownPath(BinaryData(xxr(src.FileName)));
            //TODO: check signature/timestamp
            txtSourceView.Lines.LoadFromFile(FSrcPath);
            FSrcFile:=src;
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
         begin
          txtSourceView.Text:=Format('!!![%s] "%s"%d:%d',
            [ItemToStr(q),FSrcPath,Line,Col]);
          FSrcFile:=nil;
         end;
      end;
     end;
    if (FHighLight1.x<>h1.x) or (FHighLight2.x<>h2.x) then TreeView1.Invalidate;
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
  p:rItem;
begin
  if ((FHighLight1.x<>0) or (FHighLight2.x<>0))
    and not(cdsSelected in State) then
    if Node is TXsTreeNode then
     begin
      p:=(Node as TXsTreeNode).Index;
      if (FHighLight1.x<>0) and (p.x=FHighLight1.x) then
        Sender.Canvas.Brush.Color:=$00CCFF //gold
      else
      if (FHighLight2.x<>0)
        and ((Node as TXsTreeNode).JumpIndex.x=FHighLight2.x) then
        Sender.Canvas.Brush.Color:=$00CC00 //green
      else
      if (FHighLight2.x<>0) and (p.x=FHighLight2.x) then
        Sender.Canvas.Brush.Color:=$FFCC99 //skyblue
      ;
     end;
end;

procedure TfrmXsViewMain.nFindPrev(var p:rItem);
var
  i:cardinal;
begin
  //assert v(x,vTypeNr)=n_BinaryData
  //assert (x and StratoSphereDataBlockMask)>0
  i:=1;
  dec(p.x);
  while (i<>10) and ((Blocks
    [p.x div StratoSphereBlockBase]
    [p.x mod StratoSphereBlockBase] shr 24)<>i) do
   begin
    inc(i);
    dec(p.x);
   end;
  if i=10 then p.x:=0;
end;

end.
