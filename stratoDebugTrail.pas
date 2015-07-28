unit stratoDebugTrail;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ActnList;

type
  TfrmDebugTrail = class(TForm)
    ListView1: TListView;
    ActionList1: TActionList;
    actCopy: TAction;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure actCopyExecute(Sender: TObject);
  end;

implementation

uses ClipBrd;

{$R *.dfm}

procedure TfrmDebugTrail.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action:=caMinimize;
end;

procedure TfrmDebugTrail.actCopyExecute(Sender: TObject);
var
  i,j:integer;
  s:string;
  li:TListItem;
begin
  s:='';//TODO: header?
  for i:=0 to ListView1.Items.Count-1 do
   begin
    li:=ListView1.Items[i];
    if li.Selected then
     begin
      s:=s+Caption;
      for j:=0 to li.SubItems.Count-1 do
        s:=s+#9+li.SubItems[j];
      s:=s+#13#10;
     end;
   end;
  Clipboard.AsText:=s;
end;

end.
 