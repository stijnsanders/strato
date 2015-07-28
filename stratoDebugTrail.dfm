object frmDebugTrail: TfrmDebugTrail
  Left = 192
  Top = 124
  Width = 641
  Height = 567
  Caption = 'Strato Debug Trail'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Lucida Console'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefaultPosOnly
  OnClose = FormClose
  DesignSize = (
    625
    529)
  PixelsPerInch = 96
  TextHeight = 13
  object ListView1: TListView
    Left = 8
    Top = 8
    Width = 609
    Height = 513
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = '#'
        Width = -1
        WidthType = (
          -1)
      end
      item
        Alignment = taRightJustify
        Caption = 'p'
        Width = -1
        WidthType = (
          -1)
      end
      item
        Alignment = taRightJustify
        Caption = 'p1'
        Width = -1
        WidthType = (
          -1)
      end
      item
        Alignment = taRightJustify
        Caption = 'p2'
        Width = -1
        WidthType = (
          -1)
      end
      item
        Alignment = taRightJustify
        Caption = 'bp'
        Width = -1
        WidthType = (
          -1)
      end
      item
        Caption = 'What'
        Width = -1
        WidthType = (
          -1)
      end
      item
        Alignment = taRightJustify
        Caption = 'vt'
        Width = -1
        WidthType = (
          -1)
      end
      item
        Alignment = taRightJustify
        Caption = 'vp'
        Width = -1
        WidthType = (
          -1)
      end>
    HideSelection = False
    MultiSelect = True
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
  end
  object ActionList1: TActionList
    Left = 16
    Top = 40
    object actCopy: TAction
      Caption = '&Copy'
      ShortCut = 16451
      OnExecute = actCopyExecute
    end
  end
end
