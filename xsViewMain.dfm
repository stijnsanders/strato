object Form1: TForm1
  Left = 192
  Top = 124
  Width = 557
  Height = 332
  Caption = 'xsView'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Verdana'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poDefault
  PixelsPerInch = 96
  TextHeight = 14
  object TreeView1: TTreeView
    Left = 0
    Top = 0
    Width = 541
    Height = 274
    Align = alClient
    HideSelection = False
    Indent = 19
    ReadOnly = True
    TabOrder = 0
    OnCreateNodeClass = TreeView1CreateNodeClass
    OnDblClick = TreeView1DblClick
    OnExpanding = TreeView1Expanding
    Items.Data = {
      01000000290000000000000000000000FFFFFFFFFFFFFFFF0000000000000000
      10286E6F2066696C65206F70656E656429}
  end
  object MainMenu1: TMainMenu
    Left = 8
    Top = 8
    object File1: TMenuItem
      Caption = '&File'
      object Open1: TMenuItem
        Caption = '&Open...'
        ShortCut = 16463
        OnClick = Open1Click
      end
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'xsu'
    Filter = 'xsv file (*.xsu)|*.xsu|All files (*.*)|*.*'
    InitialDir = '.'
    Left = 40
    Top = 8
  end
end
