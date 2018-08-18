unit stratoDebugView;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, stratoDecl, stratoSphere, ActnList,
  System.Actions;

type
  TfrmDebugView = class(TForm)
    ActionList1: TActionList;
    actNext: TAction;
    actRunTo: TAction;
    actRunToFocus: TAction;
    PageControl1: TPageControl;
    txContext: TTabSheet;
    tsTrail: TTabSheet;
    lblUpNext: TLabel;
    lblFileName: TLabel;
    btnNext: TButton;
    Panel1: TPanel;
    Splitter1: TSplitter;
    lvMem: TListView;
    lvStack: TListView;
    txtUpNext: TMemo;
    btnRunTo: TButton;
    txtBreakPoints: TEdit;
    txtSourceView: TMemo;
    btnBreak: TButton;
    lvTrail: TListView;
    cbKeepTrail: TCheckBox;
    actCopy: TAction;
    tsStackTrace: TTabSheet;
    lvStackTrace: TListView;
    actRefresh: TAction;
    cbStackTrace: TCheckBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnNextClick(Sender: TObject);
    procedure btnRunToClick(Sender: TObject);
    procedure btnBreakClick(Sender: TObject);
    procedure txtBreakPointsEnter(Sender: TObject);
    procedure txtBreakPointsExit(Sender: TObject);
    procedure actRunToFocusExecute(Sender: TObject);
    procedure actCopyExecute(Sender: TObject);
    procedure lvStackDblClick(Sender: TObject);
    procedure Splitter1Moved(Sender: TObject);
    procedure Panel1Resize(Sender: TObject);
  private
    FDoNext,y1,y2:integer;
    FBreakAt:array of rItem;
    FSrc:cardinal;
    FSrcPath:string;
    FSrcData:TStringList;
    FTrailSuspended:boolean;
  protected
    procedure DoCreate; override;
    procedure DoDestroy; override;
    procedure DoShow; override;
  public
    function WaitNext:boolean;
    function CheckBreakPoint(p:rItem):boolean;
    procedure ShowSource(p:rItem;Line,Col:cardinal);
    procedure Done;
  end;

function PtrToStr(p:pointer):string;
function ItemToStrX(p:xValue):string;

implementation

uses Clipbrd, stratoTools;

{$R *.dfm}

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

{ TfrmDebugView }

procedure TfrmDebugView.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action:=caMinimize;
end;

function TfrmDebugView.WaitNext:boolean;
begin
  Result:=false;//default
  Screen.Cursor:=crDefault;
  if Visible then
   begin
    SetLength(FBreakAt,0);//store?
    FDoNext:=0;
    if FTrailSuspended then
     begin
      FTrailSuspended:=false;
      lvTrail.Items.EndUpdate;
     end;
    while FDoNext=0 do Application.HandleMessage;
    Result:=FDoNext<>1;
   end;
end;

procedure TfrmDebugView.btnNextClick(Sender: TObject);
begin
  FDoNext:=1;
end;

procedure TfrmDebugView.btnBreakClick(Sender: TObject);
begin
  FDoNext:=2;//see WaitNext call
end;

procedure TfrmDebugView.btnRunToClick(Sender: TObject);
var
  s:string;
  i,j,l:integer;
  k1,k2:cardinal;
const
  MaxBreakPoints=$100;
begin
  s:=txtBreakPoints.Text;
  l:=Length(s);
  if l=0 then
    txtBreakPoints.SetFocus
  else
   begin
    i:=1;
    j:=0;
    SetLength(FBreakAt,MaxBreakPoints);
    while (i<=l) do
     begin
      k1:=0;
      while (i<=l) and (AnsiChar(s[i]) in ['0'..'9']) do
       begin
        k1:=k1*10+(byte(s[i]) and $F);
        inc(i);
       end;
      if (i<l) and (s[i]<>' ') and (AnsiChar(s[i+1]) in ['0'..'9']) then
       begin
        //assert s[i]=ItemToStrMask[3]
        inc(i);
        k2:=0;
        while (i<=l) and (AnsiChar(s[i]) in ['0'..'9']) do
         begin
          k2:=k2*10+(byte(s[i]) and $F);
          inc(i);
         end;
       end
      else
       begin
        k2:=k1;
        k1:=0;//ip div StratoSphereBlockBase;
       end;
      if j=MaxBreakPoints then
        raise Exception.Create('Maximum breakpoints exceeded');
      FBreakAt[j]:=xxr(k1*StratoSphereBlockBase+k2);
      inc(j);
      while (i<=l) and not(AnsiChar(s[i]) in ['0'..'9']) do inc(i);
     end;
    SetLength(FBreakAt,j);
    if j=0 then
      raise Exception.Create('No breakpoints defined')
    else
     begin
      FTrailSuspended:=true;
      lvTrail.Items.BeginUpdate;
      FDoNext:=1;
      Screen.Cursor:=crHourGlass;
     end;
   end;
end;

function TfrmDebugView.CheckBreakPoint(p: rItem): boolean;
var
  i,l:integer;
begin
  l:=Length(FBreakAt);
  if l=0 then
    Result:=true //not RunTo: break always
  else
   begin
    i:=0;
    while (i<l) and (FBreakAt[i].x<>p.x) do inc(i);
    Result:=i<>l;
   end;
end;

procedure TfrmDebugView.DoCreate;
begin
  inherited;
  FSrcData:=TStringList.Create;
  FSrcPath:='';
  FSrc:=cardinal(-1);
  FTrailSuspended:=false;
end;

procedure TfrmDebugView.ShowSource(p:rItem;Line,Col:cardinal);
var
  Src,i,j,k:cardinal;
  ss:string;
begin
  try
    if Line=0 then
     begin
      lblFileName.Caption:=Format('[?] %d:%d',[Line,Col]);
      txtSourceView.Text:=#13#10#13#10'?????';
     end
    else
     begin
      Src:=rSrc(p);
      if (Src<>FSrc) then
       begin
        FSrc:=cardinal(-1);//in case of error
        //TODO: resolve relative path
        //TODO: cache several?
        FSrcPath:=ResolveKnownPath(UTF8ToString(
          BinaryData(xxr(SourceFiles[Src].FileName))));
        //TODO: check signature/timestamp
        FSrcData.LoadFromFile(FSrcPath);
        FSrc:=Src;
       end;
      lblFileName.Caption:=Format('%s %d:%d',[FSrcPath,Line,Col]);
      while txtSourceView.Lines.Count<5 do txtSourceView.Lines.Add('//');
      i:=Line-2;
      j:=0;
      k:=0;
      while j<5 do
       begin
        if (i<1) or (i>=cardinal(FSrcData.Count)) then
         begin
          txtSourceView.Lines[j]:='//???pastEOF???//';
          if j<2 then inc(k,7);
         end
        else
         begin
          ss:=FSrcData[i-1];
          txtSourceView.Lines[j]:=ss;
          if j<2 then inc(k,Length(ss)+2);
         end;
        inc(i);
        inc(j);
       end;
      txtSourceView.SelStart:=integer(k+Col)-1;
      txtSourceView.SelLength:=1;//?
     end;
  except
    //on e:Exception do?
    lblFileName.Caption:=Format('%s %d:%d',[FSrcPath,Line,Col]);
    txtSourceView.Text:=#13#10#13#10'!!!!!';
  end;
end;

procedure TfrmDebugView.DoDestroy;
begin
  inherited;
  FSrcData.Free;
end;

procedure TfrmDebugView.txtBreakPointsEnter(Sender: TObject);
begin
  btnRunTo.Default:=true;
end;

procedure TfrmDebugView.txtBreakPointsExit(Sender: TObject);
begin
  btnRunTo.Default:=false;
end;

procedure TfrmDebugView.Done;
begin
  Screen.Cursor:=crDefault;
  if Visible then
   begin
    txtUpNext.Lines.Text:='(Done)';
    btnNext.Caption:='(Done)';
    btnNext.Enabled:=false;
    btnRunTo.Enabled:=false;
    btnBreak.Enabled:=false;
    if FTrailSuspended then
     begin
      FTrailSuspended:=false;
      lvTrail.Items.EndUpdate;
     end;
    while WindowState=wsNormal do //while not closed
      Application.ProcessMessages;
   end;
end;

procedure TfrmDebugView.actRunToFocusExecute(Sender: TObject);
begin
  txtBreakPoints.SetFocus;
end;

procedure TfrmDebugView.actCopyExecute(Sender: TObject);
var
  s:string;
  i,j:integer;
  li:TListItem;
begin
  if ActiveControl is TCustomEdit then
    (ActiveControl as TCustomEdit).CopyToClipboard
  else
  if ActiveControl=lvTrail then
   begin
    s:='';//TODO: headers?
    for i:=0 to lvTrail.Items.Count-1 do
     begin
      li:=lvTrail.Items[i];
      if li.Selected then
       begin
        s:=s+li.Caption;
        for j:=0 to li.SubItems.Count-1 do
          s:=s+#9+li.SubItems[j];
        s:=s+#13#10;
       end;
     end;
    //Clipboard.Clear;//?
    Clipboard.AsText:=s;
   end;
end;

procedure TfrmDebugView.lvStackDblClick(Sender: TObject);
var
  li:TListItem;
begin
  li:=lvStack.ItemFocused;//ItemSelected?
  if li<>nil then
   begin
    txtBreakPoints.Text:=li.SubItems[0];
    btnRunTo.Click;
   end;
end;

procedure TfrmDebugView.DoShow;
begin
  inherited;
  y1:=lvStack.Height;
  y2:=Panel1.Height;
end;

procedure TfrmDebugView.Splitter1Moved(Sender: TObject);
begin
  y1:=lvStack.Height;
  y2:=Panel1.Height;
end;

procedure TfrmDebugView.Panel1Resize(Sender: TObject);
begin
  if y2>8 then lvStack.Height:=y1*Panel1.Height div y2;
end;

end.
