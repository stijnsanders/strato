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
  object Splitter1: TSplitter
    Left = 0
    Top = 213
    Width = 541
    Height = 3
    Cursor = crVSplit
    Align = alBottom
  end
  object TreeView1: TTreeView
    Left = 0
    Top = 41
    Width = 541
    Height = 172
    Align = alClient
    HideSelection = False
    Indent = 19
    ReadOnly = True
    TabOrder = 0
    OnCreateNodeClass = TreeView1CreateNodeClass
    OnDblClick = TreeView1DblClick
    OnExpanding = TreeView1Expanding
    OnKeyPress = TreeView1KeyPress
    Items.Data = {
      01000000290000000000000000000000FFFFFFFFFFFFFFFF0000000000000000
      10286E6F2066696C65206F70656E656429}
  end
  object ListBox1: TListBox
    Left = 0
    Top = 216
    Width = 541
    Height = 58
    Align = alBottom
    ItemHeight = 14
    TabOrder = 1
    OnDblClick = ListBox1DblClick
  end
  object panHeader: TPanel
    Left = 0
    Top = 0
    Width = 541
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    Visible = False
    object txtGoTo: TEdit
      Left = 8
      Top = 8
      Width = 57
      Height = 25
      AutoSize = False
      TabOrder = 0
      OnKeyPress = txtGoToKeyPress
    end
    object btnGoTo: TButton
      Left = 72
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Go To'
      TabOrder = 1
      OnClick = btnGoToClick
    end
  end
  object MainMenu1: TMainMenu
    Left = 8
    Top = 32
    object File1: TMenuItem
      Caption = '&File'
      object Open1: TMenuItem
        Caption = '&Open...'
        ShortCut = 16463
        OnClick = Open1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Close1: TMenuItem
        Caption = '&Close'
        OnClick = Close1Click
      end
    end
    object View1: TMenuItem
      Caption = '&View'
      object Clearclicktrack1: TMenuItem
        Caption = '&Clear click track'
        OnClick = Clearclicktrack1Click
      end
    end
    object Tools1: TMenuItem
      Caption = '&Tools'
      object GoTo1: TMenuItem
        Caption = '&Go To...'
        ShortCut = 16455
        OnClick = GoTo1Click
      end
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'xsu'
    Filter = 'xsv file (*.xsu)|*.xsu|All files (*.*)|*.*'
    InitialDir = '.'
    Left = 40
    Top = 32
  end
end
