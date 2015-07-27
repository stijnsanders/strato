unit xsViewMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, Menus, stratoSphere, ExtCtrls, StdCtrls, ImgList;

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
  private
    FSphere:TStratoSphere;
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
  end;

var
  frmXsViewMain: TfrmXsViewMain;

implementation

uses
  stratoDecl, stratoDebug;

{$R *.dfm}

{ TfrmXsViewMain }

procedure TfrmXsViewMain.DoCreate;
begin
  inherited;
  FSphere:=nil;
  Application.OnActivate:=AppActivate;
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
      q:=p;
      if (q<>0) and (FSphere[q].ThingType=ttGlobal) then q:=FSphere[q].Target;
      BuildNode(n,q);
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
begin
  inherited;
  if ParamCount<>0 then LoadFile(ParamStr(1));
end;

procedure TfrmXsViewMain.AppActivate(Sender: TObject);
begin
  //TODO: check file modified? reload?
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
  iiProperty=11;
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
  iiVarIndex=31;
  iiInherited=32;
  iiPointer=33;
  iiAddressOf=34;
  iiDereference=35;
  iiConstant=36;
  iiConstructor=37;
  iiDestructor=38;

procedure TXsTreeNode.AfterConstruction;
begin
  inherited;
  //defaults
  Index:=0;
  ExpandIndex:=0;
  JumpIndex:=0;
  JumpedTo:=nil;
end;

function TfrmXsViewMain.JumpNode(n:TTreeNode;const prefix:string;
  i:cardinal):TXsTreeNode;
var
  p:TStratoIndex;
  m:TTreeNode;
begin
  if i=0 then
    Result:=nil
  else
   begin
    Result:=TreeView1.Items.AddChild(n,prefix+IntToStr(i)) as TXsTreeNode;
    if (n<>nil) and (n is TXsTreeNode) then
     begin
      m:=n;
      while (m<>nil) and (m is TXsTreeNode)
        and (((m as TXsTreeNode).Index=0)
        or (FSphere[(m as TXsTreeNode).Index].ThingType=ttVarIndex))
        do m:=m.Parent;
      if (m as TXsTreeNode).Index=0 then p:=0 else
        p:=FSphere[(m as TXsTreeNode).Index].Parent;
     end
    else p:=0;
    if (p<>0) and ((FSphere[i].ThingType=ttVarIndex)
      or ((FSphere[i].Parent=p)
      and not(FSphere[i].ThingType in [ttVar,ttVarByRef,ttThis])))
    then
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
        n.JumpIndex:=p.Target;
      ttArray:
        n.JumpIndex:=p.ElementType;
      ttFunction:
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
      ttOverload,ttConstructor:
       begin
        JumpNode(n,':sig=',p.Target);
        ListNode(n,':arg->',p.FirstArgument);
        BuildNode(n,p.Body);
       end;
      ttFnCall:
       begin
        JumpNode(n,':sub=',p.Target);
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
      ttThis,ttInherited:
        n.JumpIndex:=p.EvaluatesTo;
      ttVarIndex:
       begin
        n.JumpIndex:=p.EvaluatesTo;
        JumpNode(n,':par=',p.Parent);
        JumpNode(n,':sub=',p.Target);
        ListNode(n,':arg->',p.FirstArgument);
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
      ttClass:
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
      ttPointer,ttArgByRef,ttVarByRef:
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
      ttInterface:
       begin
        JumpNode(n,':InheritsFrom ',p.InheritsFrom);
        ListNode(n,'->',p.FirstItem);
       end;
      ttProperty:
       begin
        n.JumpIndex:=p.EvaluatesTo;
        JumpNode(n,':ValueFrom ',p.ValueFrom);
        JumpNode(n,':AssignTo ',p.AssignTo);
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
      ttFunction:k:=iiFunction;
      ttVar:k:=iiVar;
      ttConstant:k:=iiConstant;
      ttLiteral:k:=iiLiteral;
      ttBinaryData:k:=iiLitVal;
      ttSignature:k:=iiSignature;
      ttOverload:k:=iiOverload;
      ttConstructor:k:=iiConstructor;
      ttFnCall:k:=iiCall;
      ttArgument:k:=iiArg;
      ttThis:k:=iiThis;
      ttInherited:k:=iiInherited;
      ttVarIndex:k:=iiVarIndex;
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
      ttProperty:k:=iiProperty;
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
begin
  if Key=#13 then //btnDictLookup.Click;
   begin
    txtDictLookup.SelectAll;
    lblDictName.Caption:=FSphere.Dict[StrToInt(txtDictLookup.Text)];
   end;
end;

procedure TfrmXsViewMain.Dictionarylookup1Click(Sender: TObject);
begin
  panHeader.Visible:=true;
  txtDictLookup.SelectAll;
  txtDictLookup.SetFocus;
end;

end.
