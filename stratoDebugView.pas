unit stratoDebugView;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, stratoDecl, stratoSphere;

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
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnNextClick(Sender: TObject);
    procedure btnRunToClick(Sender: TObject);
    procedure btnBreakClick(Sender: TObject);
  private
    FDoNext:boolean;
    FBreakAt:array of TStratoIndex;
    FSrcFile:TStratoIndex;
    FSrcPath:string;
    FSrcData:TStringList;
  protected
    procedure DoCreate; override;
    procedure DoDestroy; override;
  public
    procedure WaitNext;
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

procedure TfrmDebugView.WaitNext;
begin
  if Visible then
   begin
    SetLength(FBreakAt,0);//store?
    FDoNext:=false;
    while not FDoNext do Application.HandleMessage;
   end;
end;

procedure TfrmDebugView.btnBreakClick(Sender: TObject);
begin
  {
  try
    raise Exception.Create('!!!');
  except
  end;
  }
  FDoNext:=true;//set a breakpoint here!
end;

procedure TfrmDebugView.btnNextClick(Sender: TObject);
begin
  FDoNext:=true;
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
    FDoNext:=true;
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
    while (i<=l) and (FBreakAt[i]<>p) do inc(i);
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

end.
