unit xsViewMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, Menus, stratoSphere, ExtCtrls, StdCtrls, ImgList,
  stratoDecl, AppEvnts;

type
  TXsTreeNode=class(TTreeNode)
  private
    Index,ExpandIndex,JumpIndex:xItem;
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
    lblDictName: TLabel;
    txtDictLookup: TEdit;
    Dictionarylookup1: TMenuItem;
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
    procedure txtDictLookupKeyPress(Sender: TObject; var Key: Char);
    procedure Dictionarylookup1Click(Sender: TObject);
    procedure Source1Click(Sender: TObject);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure FormResize(Sender: TObject);
    procedure Splitter2Moved(Sender: TObject);
    procedure TreeView1CustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
  private
    FStore:TStratoStore;
    FSphere:TStratoSphere;
    FHighLight1,FHighLight2:xItem;
    FSrcFile:PxSourcefile;
    FSrcPath,FFilePath,FFileSignature:string;
    w1,w2:integer;
    FCheckZero:boolean;
    procedure LoadFile(const FilePath:string);
    function JumpNode(n: TTreeNode; const prefix: string;
      i: xItem): TXsTreeNode;
    function ListNode(n: TTreeNode; const prefix: string;
      i: xItem): TXsTreeNode;
    function ShowNode(n: TTreeNode; const prefix: string;
      i: xItem): TXsTreeNode;
    function BuildNode(Node: TTreeNode; i: xItem): TXsTreeNode;
    procedure JumpTo(x:xItem);
    procedure nFindPrev(var x:xItem);
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
  stratoDebug;

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

function nStr(x:xItem):string; //inline
begin
  Result:=Format('$%.8x',[cardinal(x)]);
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
  Index:=0;
  ExpandIndex:=0;
  JumpIndex:=0;
  JumpedTo:=nil;
end;

{ TfrmXsViewMain }

procedure TfrmXsViewMain.DoCreate;
begin
  inherited;
  FStore:=nil;
  FSphere:=nil;
  Application.OnActivate:=AppActivate;
  FSrcPath:='';
  FSrcFile:=nil;
  FFilePath:='';
  FFileSignature:='';
end;

procedure TfrmXsViewMain.DoDestroy;
begin
  inherited;
  FreeAndNil(FSphere);
  FreeAndNil(FStore);
end;

procedure TfrmXsViewMain.Open1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then LoadFile(OpenDialog1.FileName);
end;

procedure TfrmXsViewMain.LoadFile(const FilePath: string);
var
  p:xItem;
begin
  FreeAndNil(FSphere);
  FreeAndNil(FStore);

  FStore:=TStratoStore.Create;
  FStore.ReadSettings(ExtractFilePath(ParamStr(0))+'strato.ini');//?
  FStore.LoadFromFile(FilePath);
  FSphere:=TStratoSphere.Create(FStore,nil);

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

    p:=xItem(-1);
    while FStore.NextModule(p) do BuildNode(nil,p);

    //ListNode(nil,':Namespaces->',FSphere.Header.FirstNameSpace);
    //ListNode(nil,':GlobalVars->',FSphere.Header.FirstGlobalVar);
    //ListNode(nil,':Initialization->',FSphere.Header.FirstInitialization);
    //ListNode(nil,':Finalization->',FSphere.Header.FirstFinalization);

{
    v:=FSphere.v(pHeader,tf_Version);
    n:=TreeView1.Items.AddChild(nil,Format(':Namespaces [v%d.%d.%d.%d]',[
        v shr 24,
        (v shr 16) and $FF,
        (v shr 8) and $FF,
        v and $FF])) as TXsTreeNode;
    p:=FSphere.r(pHeader,tf_FirstNameSpace);
    while p<>0 do
     begin
      BuildNode(n,p);
      p:=FSphere.r(p,tfNext);
     end;

    n:=TreeView1.Items.AddChild(nil,':GlobalVars #'+
      IntToStr(FSphere.v(pIndexes,tf_GlobalByteSize))) as TXsTreeNode;
    p:=FSphere.r(pIndexes,tf_FirstGlobalVar);
    while p<>0 do
     begin
      BuildNode(n,p);
      p:=FSphere.r(p,tfNext);
     end;

    n:=TreeView1.Items.AddChild(nil,':Initialization') as TXsTreeNode;
    p:=FSphere.r(pIndexes,tf_FirstInitialization);
    while p<>0 do
     begin
      q:=p;
      while (q<>0) and (FSphere.t(q)=ttAlias) do q:=FSphere.r(q,tfTarget);
      BuildNode(n,q);
      p:=FSphere.r(p,tfNext);
     end;

    n:=TreeView1.Items.AddChild(nil,':Finalization') as TXsTreeNode;
    p:=FSphere.r(pIndexes,tf_FirstFinalization);
    while p<>0 do
     begin
      q:=p;
      while (q<>0) and (FSphere.t(q)=ttAlias) do q:=FSphere.r(q,tfTarget);
      BuildNode(n,q);
      p:=FSphere.r(p,tfNext);
     end;
}

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
  n:TTreeNode;
  i:integer;
begin
  n:=TreeView1.Selected;
  if (n<>nil) and (n is TXsTreeNode) then
    if FSphere=nil then
      Open1.Click
    else
     begin
      if (n as TXsTreeNode).JumpedTo=nil then
       begin
        JumpTo((n as TXsTreeNode).JumpIndex);
        (n as TXsTreeNode).JumpedTo:=TreeView1.Selected;
       end
      else
        TreeView1.Selected:=(n as TXsTreeNode).JumpedTo;
      if n<>TreeView1.Selected then
       begin
        while (n<>nil) and not((n is TXsTreeNode)
          and ((n as TXsTreeNode).Index<>0)) do
          n:=n.Parent;
        if n<>nil then
         begin
          i:=ListBox1.Items.IndexOf(n.Text);
          if i=-1 then i:=ListBox1.Items.Add(n.Text);
          ListBox1.ItemIndex:=i;
         end;
       end;
     end;
end;

procedure TfrmXsViewMain.JumpTo(x:xItem);
var
  n:TTreeNode;
  a:array of xItem;
  i:xItem;
  ai,al:cardinal;
  b:boolean;
  tt:xTypeNr;
begin
  if x<>0 then
   begin
    i:=x;
    al:=0;
    ai:=0;
    while i<>0 do
     begin
      if ai=al then
       begin
        inc(al,$400);//grow;
        SetLength(a,al);
       end;
       tt:=FSphere.n(i,vTypeNr)^;
       if tt=n_Invalid then i:=0 else
        begin
         if tt=n_BinaryData then nFindPrev(i);
         a[ai]:=i;
         inc(ai);
         i:=FSphere.n(i,fParent)^;
        end;
     end;
    n:=TreeView1.Items.GetFirstNode;
    b:=true;
    while (ai<>0) and (n<>nil) do
     begin
      dec(ai);
      while (n<>nil) and not((n is TXsTreeNode)
        and ((n as TXsTreeNode).Index=a[ai])) do
       begin
        if (n is TXsTreeNode) and (n.HasChildren) and (n.Count=0)
          and ((n as TXsTreeNode).Index=0) then
          TreeView1Expanding(nil,n,b);
        n:=n.GetNext;
       end;
      if (n<>nil) and (n is TXsTreeNode) then
        TreeView1Expanding(nil,n,b);
     end;
    if n=nil then
     begin
      //make here?
      n:=TreeView1.Selected;
      if n<>nil then
        TreeView1.Selected:=BuildNode(n,(n as TXsTreeNode).JumpIndex);
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

function TfrmXsViewMain.JumpNode(n:TTreeNode;const prefix:string;
  i:xItem):TXsTreeNode;
var
  tt:xTypeNr;
  b:boolean;
  p,q:xItem;
  m:TTreeNode;
begin
  if i=0 then
    Result:=nil
  else
   begin
    Result:=TreeView1.Items.AddChild(n,prefix+nStr(i)) as TXsTreeNode;
    b:=false;
    tt:=FSphere.n(i,vTypeNr)^;
    if (n<>nil) and (n is TXsTreeNode) then
      if (tt<>nVarDecl) and (tt<>nVarByRef) and (tt<>nThis) then
       begin
        q:=FSphere.n(i,fParent)^;
        m:=n;
        while not(b) and (m<>nil) do
         begin
          if (m<>nil) and (m is TXsTreeNode) then
           begin
            p:=(m as TXsTreeNode).Index;
            if p=i then m:=nil else
              if p=q then b:=true;
           end;
          if m<>nil then m:=m.Parent;
         end;
       end;
    if b then
     begin
      //BuildNode(Result,i)
      Result.HasChildren:=true;
      Result.ExpandIndex:=i;
      Result.ExpandSingle:=true;
     end
    else
      Result.JumpIndex:=i;
    //TODO: if FSphere[i].Parent=FSphere[(n as TXsTreeNode).Index].Parent ?
    Result.ImageIndex:=iiItem;
    Result.SelectedIndex:=iiItem;
   end;
end;

function TfrmXsViewMain.ListNode(n:TTreeNode;const prefix:string;
  i:xItem):TXsTreeNode;
begin
  if i=0 then
    //Result:=nil
    Result:=TreeView1.Items.AddChild(n,prefix+nStr(i)) as TXsTreeNode
  else
   begin
    Result:=TreeView1.Items.AddChild(n,prefix+nStr(i)) as TXsTreeNode;
    Result.HasChildren:=true;
    Result.ExpandIndex:=i;
   end;
  Result.ImageIndex:=iiList;
  Result.SelectedIndex:=iiList;
end;

function TfrmXsViewMain.ShowNode(n:TTreeNode;const prefix:string;
  i:xItem):TXsTreeNode;
begin
  if i=0 then
    //Result:=nil
    Result:=TreeView1.Items.AddChild(n,prefix+nStr(i)) as TXsTreeNode
  else
   begin
    Result:=TreeView1.Items.AddChild(n,prefix+nStr(i)) as TXsTreeNode;
    Result.HasChildren:=true;
    Result.ExpandIndex:=i;
    Result.ExpandSingle:=true;
   end;
  Result.ImageIndex:=iiDefault;
  Result.SelectedIndex:=iiDefault;
end;

function TfrmXsViewMain.BuildNode(Node:TTreeNode;i:xItem):TXsTreeNode;
var
  p1,p2,j:xItem;
  tt:xTypeNr;
  k,sy,sx:cardinal;
  n:TXsTreeNode;
  s:string;
  src:PxSourceFile;
begin
  if (i=0) and FCheckZero then
    Result:=nil
  else
   begin
    s:=Format('$%.8x: %s',[cardinal(i),StratoDumpThing(FSphere,i)]);
    tt:=FSphere.n(i,vTypeNr)^;
    if (tt<>nSourceFile) and (tt<>n_BinaryData)
      and StratoGetSourceFile(FSphere,i,sy,sx,p1,p2) then
     begin
      src:=FSphere.Store.SourceFile(i);
      if (src.FileName=0) or (sy=0) then
        s:=s+' []'
      else
        s:=Format('%s  [%s(%d:%d)]',[s
          ,FSphere.GetBinaryData(FSphere.Store.SourceFile(i).FileName)
          ,sy
          ,sx
          ]);
     end;
    n:=TreeView1.Items.AddChild(Node,s) as TXsTreeNode;
    n.Index:=i;
    k:=iiDefault;
    j:=0;//set ExpandIndex?
    case tt of
      nSourceFile:
       begin
        src:=FSphere.Store.SourceFile(i);
        if src.FileName=0 then s:='' else s:=FSphere.GetBinaryData(src.FileName);
        n.Text:=Format('$%.8x: "%s" #%d%s',
          [cardinal(i)
          ,s
          ,src.FileSize
          ,FSphere.Store.ListBlocks(i)
          ]);
        //ListNode(n,':Dependencies->',src.Dependencies));
        ListNode(n,':Namespaces->',src.NameSpaces);
        ListNode(n,':Globals->',src.Globals);
        ShowNode(n,':Initialization=',src.InitializationBlock);
        ShowNode(n,':Finalization=',src.FinalizationBlock);
       end;
      //nDependency:
      //  n.JumpIndex:=FSphere.u(i,fTarget);
      //nTypeDecl:
      //  j:=FSphere.u(i,fItems);
      nNameSpace,nRecord,nEnumeration:
        j:=FSphere.n(i,fItems)^;
      //nAlias
      nGlobal:
        n.JumpIndex:=FSphere.n(i,fVarDecl)^;
      nImport:
        j:=FSphere.n(i,fSubject)^;
      nDeferred:
        j:=FSphere.n(i,fItems)^;
      nThrow:
        BuildNode(n,FSphere.n(i,fExceptionConstructor)^);//n.JumpIndex:=?
      nArray:
        n.JumpIndex:=FSphere.n(i,fTypeDecl)^;
      nMember,nConstructors:
        j:=FSphere.n(i,fItems)^;
      nVarDecl,nVarByRef,nConstant,nLiteral:
       begin
        n.JumpIndex:=FSphere.n(i,fTypeDecl)^;
        BuildNode(n,FSphere.n(i,fValue)^);
       end;
      nSignature:
       begin
        JumpNode(n,':objects=',FSphere.n(i,fSubject)^);
        ListNode(n,':arguments->',FSphere.n(i,fArguments)^);
        JumpNode(n,':result=',FSphere.n(i,fReturnType)^);
       end;
      nOverload,nConstructor,nDestructor,nPropertyGet,nPropertySet:
       begin
        JumpNode(n,':signature=',FSphere.n(i,fSignature)^);
        if tt<>nDestructor then
          JumpNode(n,':arguments->',FSphere.n(i,fFirstArgVar)^);
        BuildNode(n,FSphere.n(i,fBody)^);
       end;
      nFCall:
       begin
        JumpNode(n,':target=',FSphere.n(i,fTarget)^);
        ListNode(n,':arguments->',FSphere.n(i,fArguments)^);
       end;
      nSCall,nVCall:
       begin
        JumpNode(n,':subject=',FSphere.n(i,fSubject)^);
        JumpNode(n,':target=',FSphere.n(i,fTarget)^);
        ListNode(n,':arguments->',FSphere.n(i,fArguments)^);
       end;
      nArgument,nArgByRef://?
       begin
        n.JumpIndex:=FSphere.n(i,fTypeDecl)^;
        if FSphere.n(FSphere.n(i,fParent)^,vTypeNr)^=nSignature then
          JumpNode(n,':default=',FSphere.n(i,fValue)^)
        else
          JumpNode(n,':value=',FSphere.n(i,fValue)^);
       end;
      nThis:
        n.JumpIndex:=FSphere.n(i,fTypeDecl)^;
      nArrayIndex:
       begin
        //n.JumpIndex:=FSphere.n(?,fTypeDecl)^;
        JumpNode(n,':array=',FSphere.n(i,fSubject)^);
        ListNode(n,':index->',FSphere.n(i,fItems)^);
       end;
      nField:
       begin
        JumpNode(n,':x=',FSphere.n(i,fSubject)^);
        JumpNode(n,':y=',FSphere.n(i,fTarget)^);
       end;
      nCodeBlock:
       begin
        n.JumpIndex:=FSphere.n(i,fReturnType)^;
        ListNode(n,':var->',FSphere.n(i,fVarDecls)^);
        ListNode(n,':cmd->',FSphere.n(i,fItems)^);
       end;
      nAssign:
       begin
        JumpNode(n,':ValueFrom ',FSphere.n(i,fValue)^);
        JumpNode(n,':AssignTo ',FSphere.n(i,fTarget)^);
       end;
      nUnaryOp:
       begin
        n.JumpIndex:=FSphere.n(i,fReturnType)^;
        JumpNode(n,':Right ',FSphere.n(i,fRight)^);
       end;
      nBinaryOp:
       begin
        n.JumpIndex:=FSphere.n(i,fReturnType)^;
        JumpNode(n,':Left ',FSphere.n(i,fLeft)^);
        JumpNode(n,':Right ',FSphere.n(i,fRight)^);
       end;
      nCast:
       begin
        n.JumpIndex:=FSphere.n(i,fTypeDecl)^;
        JumpNode(n,':subject=',FSphere.n(i,fSubject)^);
       end;
      nClass,nInterface:
       begin
        n.JumpIndex:=FSphere.n(i,fInheritsFrom)^;
        j:=FSphere.n(i,fItems)^;
       end;
      nSelection:
       begin
        JumpNode(n,':If ',FSphere.n(i,fDoIf)^);
        JumpNode(n,':Then ',FSphere.n(i,fDoThen)^);
        JumpNode(n,':Else ',FSphere.n(i,fDoElse)^);
       end;
      nIteration:
       begin
        JumpNode(n,':First ',FSphere.n(i,fDoFirst)^);
        JumpNode(n,':If ',FSphere.n(i,fDoIf)^);
        JumpNode(n,':Then ',FSphere.n(i,fDoThen)^);
        BuildNode(n,FSphere.n(i,fBody)^);
       end;
      nIterPostEval:
       begin
        BuildNode(n,FSphere.n(i,fBody)^);
        JumpNode(n,':First ',FSphere.n(i,fDoFirst)^);
        JumpNode(n,':If ',FSphere.n(i,fDoIf)^);
        JumpNode(n,':Then ',FSphere.n(i,fDoThen)^);
       end;
      nCatchAll:
        BuildNode(n,FSphere.n(i,fBody)^);
      nCatchTypes:
       begin
        ListNode(n,'->',FSphere.n(i,fItems)^);
        BuildNode(n,FSphere.n(i,fBody)^);
       end;
      nCatchNamed:
       begin
        //n.JumpIndex:=FSphere.n(i,fBody)^
        JumpNode(n,':e=',FSphere.n(i,fTypeDecl)^);
        BuildNode(n,FSphere.n(i,fBody)^);
       end;
      nPointer,nClassRef:
        n.JumpIndex:=FSphere.n(i,fSubject)^;
      nAddressOf,nDereference:
       begin
        n.JumpIndex:=FSphere.n(i,fReturnType)^;
        BuildNode(n,FSphere.n(i,fSubject)^);
       end;
    end;
    case tt of
      nNameSpace:k:=iiNameSpace;
      nTypeDecl:k:=iiTypeDecl;
      nRecord:k:=iiRecord;
      nEnumeration:k:=iiEnum;
      nGlobal:k:=iiItem;
      nImport:k:=iiImport;
      //nTry,nDeferred:
      nThrow:k:=iiThrow;
      nArray:k:=iiArray;
      nMember:k:=iiFunction;
      nVarDecl:k:=iiVar;
      nConstant:k:=iiConstant;
      nLiteral:k:=iiLiteral;
      n_BinaryData:k:=iiLitVal;
      nSignature:k:=iiSignature;
      nOverload:k:=iiOverload;
      nConstructors:k:=iiConstructors;
      nConstructor:k:=iiConstructor;
      nFCall,nSCall,nVCall:k:=iiCall;
      nArgument:k:=iiArg;
      nThis:k:=iiThis;
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
      nArgByRef:k:=iiArgByRef;
      //nVarByRef:
      nAddressOf:k:=iiAddressOf;
      nDereference:k:=iiDereference;
      nDestructor:k:=iiDestructor;
      nInterface:k:=iiInterface;
      nClassRef:k:=iiClassRef;
      nPropertyGet:k:=iiPropertyGet;
      nPropertySet:k:=iiPropertySet;
//      nPropGetCall:k:=iiPropCall;
//      nPropSetCall:k:=iiPropAssign;
    end;
    n.ImageIndex:=k;
    n.SelectedIndex:=k;
    if j<>0 then
     begin
      n.HasChildren:=true;
      n.ExpandIndex:=j;
     end;
    Result:=n;
   end;
end;

procedure TfrmXsViewMain.TreeView1Expanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
var
  i,j:xItem;
begin
  if (Node is TXsTreeNode) and Node.HasChildren and (Node.Count=0) then
   begin
    TreeView1.Items.BeginUpdate;
    try
      Node.HasChildren:=false;
      i:=(Node as TXsTreeNode).ExpandIndex;
      if (Node as TXsTreeNode).ExpandSingle then
        BuildNode(Node,i)
      else
       begin
        j:=FSphere.n(i,fNext)^;
        while j<>0 do
         begin
          BuildNode(Node,j);
          if j=i then j:=0 else j:=FSphere.n(j,fNext)^;
         end;
       end;
    finally

      TreeView1.Items.EndUpdate;
    end;
   end;
  //AllowExpansion:=true;
end;

procedure TfrmXsViewMain.ListBox1DblClick(Sender: TObject);
begin
  if ListBox1.ItemIndex<>-1 then
   begin
    JumpTo(xItem(StrToInt(Copy(ListBox1.Items[ListBox1.ItemIndex],1,9))));
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
  p:xItem;
  b:boolean;
begin
  TreeView1.Selected:=nil;
  s:=txtGoTo.Text;
  if (s='') or (s[1]<>'$') then s:='$'+s;
  p:=xItem(StrToInt(s));
  b:=true;
  while (p<>0) and b do
   begin
    JumpTo(p);
    if TreeView1.Selected=nil then
     begin
      if FSphere.n(p,vTypeNr)^=n_BinaryData then nFindPrev(p);
      p:=FSphere.n(p,fParent)^;
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

procedure TfrmXsViewMain.txtDictLookupKeyPress(Sender: TObject;
  var Key: Char);
var
  i:integer;
begin
  if Key=#13 then //btnDictLookup.Click;
   begin
    txtDictLookup.SelectAll;
    if TryStrToInt(txtDictLookup.Text,i) then
      lblDictName.Caption:=FStore.Dict[i]
    else
      lblDictName.Caption:=Format('%d/%d',
        [FStore.Dict.StrIdx(txtDictLookup.Text),FStore.Dict.StrCount]);
   end;
end;

procedure TfrmXsViewMain.Dictionarylookup1Click(Sender: TObject);
begin
  panHeader.Visible:=true;
  txtDictLookup.SelectAll;
  txtDictLookup.SetFocus;
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
  q,h1,h2,p1,p2:xItem;
  i,Line,Col:cardinal;
  si:TScrollInfo;
  src:PxSourcefile;
begin
  if txtSourceView.Visible then
   begin
    h1:=FHighLight1;
    h2:=FHighLight2;
    if (Node=nil) or not(Node is TXsTreeNode) then
     begin
      txtSourceView.Clear;
      FSrcFile:=nil;
      FHighLight1:=0;
      FHighLight2:=0;
     end
    else
     begin
      Line:=0;
      Col:=0;
      FHighLight1:=(Node as TXsTreeNode).JumpIndex;
      FHighLight2:=(Node as TXsTreeNode).Index;
      q:=FHighLight2;
      try
        if (q<>0) then
          if not StratoGetSourceFile(FSphere,q,Line,Col,p1,p2) then Line:=0;
        if Line=0 then
         begin
          //txtSourceView.Text:=Format('???[%d] %d:%d',[q,Line,Col])
          txtSourceView.SelLength:=0;
         end
        else
         begin
          src:=FSphere.Store.SourceFile(q);
          if src<>FSrcFile then
           begin
            FSrcFile:=nil;//in case of error
            //TODO: resolve relative path
            //TODO: cache several?
            FSrcPath:=FSphere.Store.ResolvePath(FSphere.GetBinaryData(src.FileName));
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
          txtSourceView.Text:=Format('!!![$%.8x] "%s"%d:%d',[cardinal(q),FSrcPath,Line,Col]);
          FSrcFile:=nil;
         end;
      end;
     end;
    if (FHighLight1<>h1) or (FHighLight2<>h2) then TreeView1.Invalidate;
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
  p:xItem;
begin
  if ((FHighLight1<>0) or (FHighLight2<>0)) and not(cdsSelected in State) then
    if Node is TXsTreeNode then
     begin
      p:=(Node as TXsTreeNode).Index;
      if (FHighLight1<>0) and (p=FHighLight1) then
        Sender.Canvas.Brush.Color:=$00CCFF //gold
      else
      if (FHighLight2<>0) and ((Node as TXsTreeNode).JumpIndex=FHighLight2) then
        Sender.Canvas.Brush.Color:=$00CC00 //green
      else
      if (FHighLight2<>0) and (p=FHighLight2) then
        Sender.Canvas.Brush.Color:=$FFCC99 //skyblue
      ;
     end;
end;

procedure TfrmXsViewMain.nFindPrev(var x: xItem);
var
  i:cardinal;
begin
  //assert v(x,vTypeNr)=n_BinaryData
  //assert (x and StratoSphereDataBlockMask)>0
  i:=1;
  dec(x,SizeOf(xItem));
  while (i<>10) and ((FSphere.n(x,vTypeNr)^ shr 24)<>i) do
   begin
    inc(i);
    dec(x,SizeOf(xItem));
   end;
  if i=10 then x:=0;
end;

end.
