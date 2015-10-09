unit xsViewMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, Menus, stratoSphere, ExtCtrls, StdCtrls, ImgList,
  stratoDecl, AppEvnts;

type
  TXsTreeNode=class(TTreeNode)
  private
    Index,ExpandIndex,JumpIndex:cardinal;
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
    FSphere:TStratoSphere;
    FSrcFile,FHighLight:TStratoIndex;
    FSrcPath,FFilePath,FSignature:string;
    w1,w2:integer;
    procedure LoadFile(const FilePath:string);
    function JumpNode(n: TTreeNode; const prefix: string;
      i: cardinal): TXsTreeNode;
    function ListNode(n: TTreeNode; const prefix: string;
      i: cardinal): TXsTreeNode;
    function BuildNode(Node: TTreeNode; i: cardinal): TXsTreeNode;
    procedure JumpTo(x:cardinal);
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
  FSphere:=nil;
  Application.OnActivate:=AppActivate;
  FSrcPath:='';
  FSrcFile:=0;
  FFilePath:='';
  FSignature:='';
end;

procedure TfrmXsViewMain.DoDestroy;
begin
  inherited;
  FreeAndNil(FSphere);
end;

procedure TfrmXsViewMain.Open1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then LoadFile(OpenDialog1.FileName);
end;

procedure TfrmXsViewMain.LoadFile(const FilePath: string);
var
  n:TTreeNode;
  p,q:TStratoIndex;
begin
  if FSphere<>nil then FSphere.Free;
  FSphere:=TStratoSphere.Create;
  FSphere.LoadFromFile(FilePath);

  FFilePath:=FilePath;
  FSignature:=FileSignature(FilePath);
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

    //ListNode(nil,':Namespaces->',FSphere.Header.FirstNameSpace);
    //ListNode(nil,':GlobalVars->',FSphere.Header.FirstGlobalVar);
    //ListNode(nil,':Initialization->',FSphere.Header.FirstInitialization);
    //ListNode(nil,':Finalization->',FSphere.Header.FirstFinalization);

    n:=TreeView1.Items.AddChild(nil,Format(':Namespaces [v%d.%d.%d.%d]',[
       FSphere.Header.Version shr 24,
       (FSphere.Header.Version shr 16) and $FF,
       (FSphere.Header.Version shr 8) and $FF,
       FSphere.Header.Version and $FF])) as TXsTreeNode;
    p:=FSphere.Header.FirstNameSpace;
    while p<>0 do
     begin
      BuildNode(n,p);
      p:=FSphere[p].Next;
     end;

    n:=TreeView1.Items.AddChild(nil,':GlobalVars #'+
      IntToStr(FSphere.Header.GlobalByteSize)) as TXsTreeNode;
    p:=FSphere.Header.FirstGlobalVar;
    while p<>0 do
     begin
      {
      q:=p;
      if (q<>0) and (FSphere[q].ThingType=ttGlobal) then q:=FSphere[q].Target;
      BuildNode(n,q);
      }
      BuildNode(n,p);
      p:=FSphere[p].Next;
     end;

    n:=TreeView1.Items.AddChild(nil,':Initialization') as TXsTreeNode;
    p:=FSphere.Header.FirstInitialization;
    while p<>0 do
     begin
      q:=p;
      while (q<>0) and (FSphere[q].ThingType=ttAlias) do q:=FSphere[q].Target;
      BuildNode(n,q);
      p:=FSphere[p].Next;
     end;

    n:=TreeView1.Items.AddChild(nil,':Finalization') as TXsTreeNode;
    p:=FSphere.Header.FirstFinalization;
    while p<>0 do
     begin
      q:=p;
      while (q<>0) and (FSphere[q].ThingType=ttAlias) do q:=FSphere[q].Target;
      BuildNode(n,q);
      p:=FSphere[p].Next;
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

procedure TfrmXsViewMain.JumpTo(x:cardinal);
var
  n:TTreeNode;
  a:array of cardinal;
  i,ai,al:cardinal;
  b:boolean;
  px:PStratoThing;
begin
  if x<>0 then
   begin
    i:=x;
    al:=0;
    ai:=0;
    while i<>0 do
     begin
      //if i>=FSphere.NodeCount then i:=0;
      if ai=al then
       begin
        inc(al,$400);//grow;
        SetLength(a,al);
       end;
       px:=FSphere[i];
       if px=nil then i:=0 else
        begin
         if px.ThingType=ttBinaryData then
          begin
           dec(i);
           px:=FSphere[i];
          end;
         a[ai]:=i;
         inc(ai);
         i:=px.Parent;
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
      if s<>FSignature then
       begin
        //TODO: MessageBox?
        //TODO: attempt to restore expanded nodes state, selected/focused
        LoadFile(FFilePath);
        FSrcPath:='';//case reload
        FSrcFile:=0;
       end;
     end;
end;

function TfrmXsViewMain.JumpNode(n:TTreeNode;const prefix:string;
  i:cardinal):TXsTreeNode;
var
  tt:TStratoThingType;
  b:boolean;
  p,q:TStratoIndex;
  m:TTreeNode;
begin
  if i=0 then
    Result:=nil
  else
   begin
    Result:=TreeView1.Items.AddChild(n,prefix+IntToStr(i)) as TXsTreeNode;
    b:=false;
    tt:=FSphere[i].ThingType;
    if (n<>nil) and (n is TXsTreeNode) then
      if not(tt in [ttVar,ttVarByRef,ttThis]) then
       begin
        q:=FSphere[i].Parent;
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
  i:cardinal):TXsTreeNode;
begin
  if i=0 then
    //Result:=nil
    Result:=TreeView1.Items.AddChild(n,prefix+IntToStr(i)) as TXsTreeNode
  else
   begin
    Result:=TreeView1.Items.AddChild(n,prefix+IntToStr(i)) as TXsTreeNode;
    Result.HasChildren:=true;
    Result.ExpandIndex:=i;
   end;
  Result.ImageIndex:=iiList;
  Result.SelectedIndex:=iiList;
end;

function TfrmXsViewMain.BuildNode(Node:TTreeNode;i:cardinal):TXsTreeNode;
var
  p:PStratoThing;
  q:TStratoIndex;
  j,k:cardinal;
  n:TXsTreeNode;
  s:string;
begin
  if i=0 then
    Result:=nil
  else
   begin
    p:=FSphere[i];
    s:=IntToStr(i)+': '+StratoDumpThing(FSphere,i,p);
    if (p.ThingType<>ttSourceFile) and (p.ThingType<>ttBinaryData)
      and (p.SrcPos<>0) and StratoGetSourceFile(FSphere,i,q,j) then
      s:=Format('%s  [%s(%d:%d)]',[s
        ,FSphere.GetBinaryData(PStratoSourceFile(FSphere[q]).FileName)
        ,p.SrcPos div j
        ,p.SrcPos mod j
        ]);
    n:=TreeView1.Items.AddChild(Node,s) as TXsTreeNode;
    n.Index:=i;
    k:=iiDefault;
    j:=0;//set ExpandIndex?
    case p.ThingType of
      ttNameSpace,ttTypeDecl,ttRecord,ttEnumeration:
        j:=p.FirstItem;
      ttAlias,ttGlobal,ttImport,ttTry,ttDeferred,ttThrow:
       begin
        n.JumpIndex:=p.Target;
        if (p.Target<>0) and (FSphere[p.Target].ThingType=ttField) then
          BuildNode(n,p.Target);
       end;
      ttArray:
        n.JumpIndex:=p.Subject;
      ttMember,ttConstructors:
        j:=p.FirstItem;
      ttVar,ttConstant,ttLiteral:
       begin
        n.JumpIndex:=p.EvaluatesTo;
        BuildNode(n,p.InitialValue);
       end;
      ttSignature:
       begin
        JumpNode(n,':obj=',p.Target);
        ListNode(n,':arg->',p.FirstArgument);
        JumpNode(n,':res=',p.EvaluatesTo);
       end;
      ttOverload,ttConstructor,ttPropertyGet,ttPropertySet:
       begin
        JumpNode(n,':sig=',p.Target);
        JumpNode(n,':arg->',p.FirstArgument);
        BuildNode(n,p.Body);
       end;
      ttFnCall:
       begin
        JumpNode(n,':trg=',p.Target);
        ListNode(n,':arg->',p.FirstArgument);
        //if Target is ttConstructor?
        if p.EvaluatesTo<>0 then JumpNode(n,':res->',p.EvaluatesTo);
       end;
      ttArgument:
       begin
        n.JumpIndex:=p.EvaluatesTo;
        JumpNode(n,':dft=',p.InitialValue);
        JumpNode(n,':val=',p.Target);
       end;
      ttThis://,ttInherited:
        n.JumpIndex:=p.EvaluatesTo;
      ttArrayIndex:
       begin
        n.JumpIndex:=p.EvaluatesTo;
        JumpNode(n,':arr=',p.Target);
        ListNode(n,':idx->',p.FirstArgument);
       end;
      ttField:
       begin
        n.JumpIndex:=p.EvaluatesTo;
        JumpNode(n,':sub=',p.Subject);
        JumpNode(n,':mem=',p.Target);
       end;
      ttCodeBlock:
       begin
        n.JumpIndex:=p.EvaluatesTo;
        ListNode(n,':var->',p.FirstItem);
        ListNode(n,':cmd->',p.FirstStatement);
       end;
      ttAssign:
       begin
        JumpNode(n,':ValueFrom ',p.ValueFrom);
        JumpNode(n,':AssignTo ',p.AssignTo);
       end;
      ttUnaryOp:
       begin
        n.JumpIndex:=p.EvaluatesTo;
        JumpNode(n,':Right ',p.Right);
       end;
      ttBinaryOp:
       begin
        n.JumpIndex:=p.EvaluatesTo;
        JumpNode(n,':Left ',p.Left);
        JumpNode(n,':Right ',p.Right);
       end;
      ttCast:
       begin
        n.JumpIndex:=p.EvaluatesTo;
        JumpNode(n,':sub=',p.Target);
       end;
      ttClass,ttInterface:
       begin
        n.JumpIndex:=p.InheritsFrom;
        j:=p.FirstItem;
       end;
      ttSelection:
       begin
        JumpNode(n,':If ',p.DoIf);
        ListNode(n,':Then ',p.DoThen);
        ListNode(n,':Else ',p.DoElse);
       end;
      ttIteration:
       begin
        ListNode(n,':First ',p.DoElse);
        JumpNode(n,':If ',p.DoIf);
        ListNode(n,':Then ',p.DoThen);
        BuildNode(n,p.Body);
       end;
      ttIterationPE:
       begin
        BuildNode(n,p.Body);
        ListNode(n,':First ',p.DoElse);
        JumpNode(n,':If ',p.DoIf);
        ListNode(n,':Then ',p.DoThen);
       end;
      ttCatch:
       begin
        n.JumpIndex:=p.DoIf;
        JumpNode(n,':v=',p.FirstItem);
        BuildNode(n,p.Body);
       end;
      ttPointer,ttArgByRef,ttVarByRef,ttClassRef:
        n.JumpIndex:=p.EvaluatesTo;
      ttAddressOf,ttDereference:
       begin
        n.JumpIndex:=p.EvaluatesTo;
        BuildNode(n,p.ValueFrom);
       end;
      ttDestructor:
       begin
        JumpNode(n,':sig=',p.Target);
        BuildNode(n,p.Body);
       end;
      ttPropCall:
       begin
        JumpNode(n,':trg=',p.Target);
        ListNode(n,':arg->',p.FirstArgument);
        if p.EvaluatesTo<>0 then JumpNode(n,':val=',p.EvaluatesTo);
       end;
    end;
    case p.ThingType of
      ttNameSpace:k:=iiNameSpace;
      ttTypeDecl:k:=iiTypeDecl;
      ttRecord:k:=iiRecord;
      ttEnumeration:k:=iiEnum;
      ttAlias,ttGlobal:k:=iiItem;
      ttImport:k:=iiImport;
      //ttTry,ttDeferred:
      ttThrow:k:=iiThrow;
      ttArray:k:=iiArray;
      ttMember:k:=iiFunction;
      ttVar:k:=iiVar;
      ttConstant:k:=iiConstant;
      ttLiteral:k:=iiLiteral;
      ttBinaryData:k:=iiLitVal;
      ttSignature:k:=iiSignature;
      ttOverload:k:=iiOverload;
      ttConstructors:k:=iiConstructors;
      ttConstructor:k:=iiConstructor;
      ttFnCall:k:=iiCall;
      ttArgument:k:=iiArg;
      ttThis:k:=iiThis;
      //ttInherited:k:=iiInherited;
      ttArrayIndex:k:=iiArrayIndex;
      ttField:k:=iiField;
      ttCodeBlock:k:=iiBlock;
      ttAssign:k:=iiAssign;
      ttUnaryOp:k:=iiUnOp;
      ttBinaryOp:k:=iiBinOp;
      ttCast:k:=iiCast;
      ttClass:k:=iiClass;
      ttSelection:k:=iiSelection;
      ttIteration,ttIterationPE:k:=iiIteration;
      //ttCatch:
      ttPointer:k:=iiPointer;
      ttArgByRef:k:=iiArgByRef;
      //ttVarByRef:
      ttAddressOf:k:=iiAddressOf;
      ttDereference:k:=iiDereference;
      ttDestructor:k:=iiDestructor;
      ttInterface:k:=iiInterface;
      ttClassRef:k:=iiClassRef;
      ttPropertyGet:k:=iiPropertyGet;
      ttPropertySet:k:=iiPropertySet;
      ttPropCall:
        if p.Op=0 then k:=iiPropCall else k:=iiPropAssign;
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
  i:cardinal;
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
        while i<>0 do
         begin
          BuildNode(Node,i);
          i:=FSphere[i].Next;
         end;
    finally
      TreeView1.Items.EndUpdate;
    end;
   end;
  //AllowExpansion:=true;
end;

procedure TfrmXsViewMain.ListBox1DblClick(Sender: TObject);
var
  i,j,l:integer;
  s:string;
begin
  if ListBox1.ItemIndex<>-1 then
   begin
    s:=ListBox1.Items[ListBox1.ItemIndex];
    l:=Length(s);
    i:=1;
    j:=0;
    while (i<l) and (s[i]<>':') do
     begin
      j:=j*10+(byte(s[i]) and $F);
      inc(i);
     end;
    JumpTo(j);
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
  p:TStratoIndex;
  b:boolean;
begin
  TreeView1.Selected:=nil;
  p:=StrToInt(txtGoTo.Text);
  b:=true;
  while (p<>0) and b do
   begin
    JumpTo(p);
    if TreeView1.Selected=nil then
     begin
      if FSphere[p].ThingType=ttBinaryData then dec(p);
      p:=FSphere[p].Parent;
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
      lblDictName.Caption:=FSphere.Dict[i]
    else
      lblDictName.Caption:=Format('%d/%d',[FSphere.Dict.StrIdx(txtDictLookup.Text),FSphere.Dict.StrCount]);
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
  p,q,h1:TStratoIndex;
  qx:PStratoThing;
  pi,Line,Col:cardinal;
  si:TScrollInfo;
begin
  if txtSourceView.Visible then
   begin
    h1:=FHighLight;
    if (Node=nil) or not(Node is TXsTreeNode) then
     begin
      txtSourceView.Clear;
      FSrcFile:=0;
      FHighLight:=0;
     end
    else
     begin
      Line:=0;
      Col:=0;
      q:=(Node as TXsTreeNode).Index;
      FHighLight:=(Node as TXsTreeNode).JumpIndex;
      try
        if (q<>0) then
         begin
          qx:=FSphere[q];
          if StratoGetSourceFile(FSphere,q,p,pi) then
           begin
            Line:=qx.SrcPos div pi;
            Col:=qx.SrcPos mod pi;
           end
          else
            p:=0;
         end;
        if (p=0) or (Line=0) then
         begin
          //txtSourceView.Text:=Format('???[%d] %d:%d',[q,Line,Col])
          txtSourceView.SelLength:=0;
         end
        else
         begin
          if p<>FSrcFile then
           begin
            FSrcFile:=0;//in case of error
            //TODO: resolve relative path
            //TODO: cache several?
            FSrcPath:=FSphere.GetBinaryData(PStratoSourceFile(FSphere[p]).FileName);
            //TODO: check signature/timestamp
            txtSourceView.Lines.LoadFromFile(FSrcPath);
            FSrcFile:=p;
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
          pi:=txtSourceView.Perform(EM_LINEINDEX,Line-1,0)+integer(Col)-1;
          txtSourceView.Perform(EM_SETSEL,pi,pi+1);
          //txtSourceView.Perform(EM_SCROLLCARET,0,0);
         end;

      except
        //on e:Exception do?
         begin
          txtSourceView.Text:=Format('!!![%d] "%s"%d:%d',[q,FSrcPath,Line,Col]);
          FSrcFile:=0;
         end;
      end;
     end;
    if FHighLight<>h1 then TreeView1.Invalidate;
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
begin
  if FHighLight<>0 then
    if Node is TXsTreeNode then
      if (Node as TXsTreeNode).Index=FHighLight then
        Sender.Canvas.Brush.Color:=$00CC00;
end;

end.
