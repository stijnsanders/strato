unit stratoDebugView;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, stratoDecl, stratoSphere, ActnList;

type
  TfrmDebugView = class(TForm)
    btnNext: TButton;
    lblUpNext: TLabel;
    Panel1: TPanel;
    ListView2: TListView;
    Splitter1: TSplitter;
    ListView1: TListView;
    Memo1: TMemo;
    btnRunTo: TButton;
    txtBreakPoints: TEdit;
    Memo2: TMemo;
    lblFileName: TLabel;
    btnBreak: TButton;
    ActionList1: TActionList;
    actNext: TAction;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnNextClick(Sender: TObject);
    procedure btnRunToClick(Sender: TObject);
    procedure btnBreakClick(Sender: TObject);
    procedure txtBreakPointsEnter(Sender: TObject);
    procedure txtBreakPointsExit(Sender: TObject);
  private
    FDoNext:integer;
    FBreakAt:array of TStratoIndex;
    FSrcFile:TStratoIndex;
    FSrcPath:string;
    FSrcData:TStringList;
  protected
    procedure DoCreate; override;
    procedure DoDestroy; override;
  public
    function WaitNext:boolean;
    function CheckBreakPoint(p:TStratoIndex):boolean;
    procedure ShowSource(s:TStratoSphere;p:TStratoIndex;Line,Col:cardinal);
  end;

implementation

{$R *.dfm}

procedure TfrmDebugView.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action:=caMinimize;
end;

function TfrmDebugView.WaitNext:boolean;
begin
  Result:=false;//default
  if Visible then
   begin
    SetLength(FBreakAt,0);//store?
    FDoNext:=0;
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
  k:TStratoIndex;
const
  MaxBreakPoints=$100;
begin
  //
  s:=txtBreakPoints.Text;
  l:=Length(s);
  i:=1;
  j:=0;
  SetLength(FBreakAt,MaxBreakPoints);
  while (i<=l) do
   begin
    k:=0;
    while (i<=l) and (s[i] in ['0'..'9']) do
     begin
      k:=k*10+(byte(s[i]) and $F);
      inc(i);
     end;
    if j=MaxBreakPoints then
      raise Exception.Create('Maximum breakpoints exceeded');
    FBreakAt[j]:=k;
    inc(j);
    while (i<=l) and not(s[i] in ['0'..'9']) do inc(i);
   end;
  SetLength(FBreakAt,j);
  if j=0 then
    raise Exception.Create('No breakpoints defined')
  else
    FDoNext:=1;
end;

function TfrmDebugView.CheckBreakPoint(p: TStratoIndex): boolean;
var
  i,l:integer;
begin
  l:=Length(FBreakAt);
  if l=0 then
    Result:=true //not RunTo: break always
  else
   begin
    i:=0;
    while (i<l) and (FBreakAt[i]<>p) do inc(i);
    Result:=i<>l;
   end;
end;

procedure TfrmDebugView.DoCreate;
begin
  inherited;
  FSrcData:=TStringList.Create;
  FSrcPath:='';
  FSrcFile:=0;
end;

procedure TfrmDebugView.ShowSource(s: TStratoSphere; p: TStratoIndex; Line,
  Col: cardinal);
var
  i,j,k:integer;
  ss:string;
begin
  ///xxxxxxx
  try
    if (p=0) or (Line=0) then
     begin
      lblFileName.Caption:=Format('[%d] %d:%d',[p,Line,Col]);
      Memo2.Text:=#13#10#13#10'?????';
     end
    else
     begin
      if (p<>FSrcFile) then
       begin
        FSrcFile:=0;//in case of error
        //TODO: resolve relative path
        //TODO: cache several?
        FSrcPath:=s.GetBinaryData(PStratoSourceFile(s[p]).FileName);
        FSrcData.LoadFromFile(FSrcPath);
        FSrcFile:=p;
       end;
      lblFileName.Caption:=Format('%s %d:%d',[FSrcPath,Line,Col]);
      while Memo2.Lines.Count<5 do Memo2.Lines.Add('//');
      i:=Line-2;
      j:=0;
      k:=0;
      while j<5 do
       begin
        if (i<1) or (i>=FSrcData.Count) then
         begin
          Memo2.Lines[j]:='/////';
          if j<2 then inc(k,7);
         end
        else
         begin
          ss:=FSrcData[i-1];
          Memo2.Lines[j]:=ss;
          if j<2 then inc(k,Length(ss)+2);
         end;
        inc(i);
        inc(j);
       end;
      Memo2.SelStart:=k+integer(Col)-1;
      Memo2.SelLength:=1;//?
     end;
  except
    //on e:Exception do?
    lblFileName.Caption:=Format('%s %d:%d',[FSrcPath,Line,Col]);
    Memo2.Text:=#13#10#13#10'!!!!!';
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

end.
