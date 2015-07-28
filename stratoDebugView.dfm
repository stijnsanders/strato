object frmDebugView: TfrmDebugView
  Left = 192
  Top = 124
  Width = 641
  Height = 567
  Caption = 'Strato Debug View'
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
  object lblUpNext: TLabel
    Left = 8
    Top = 271
    Width = 56
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Up next'
  end
  object lblFileName: TLabel
    Left = 8
    Top = 383
    Width = 64
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = '[Source]'
  end
  object btnNext: TButton
    Left = 8
    Top = 496
    Width = 81
    Height = 25
    Action = actNext
    Anchors = [akLeft, akBottom]
    TabOrder = 2
  end
  object Panel1: TPanel
    Left = 8
    Top = 8
    Width = 609
    Height = 257
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 0
    object Splitter1: TSplitter
      Left = 0
      Top = 145
      Width = 609
      Height = 4
      Cursor = crVSplit
      Align = alTop
    end
    object ListView2: TListView
      Left = 0
      Top = 149
      Width = 609
      Height = 108
      Align = alClient
      Columns = <
        item
          Caption = '@'
          Width = -1
          WidthType = (
            -1)
        end
        item
          Caption = 'x'
          Width = -1
          WidthType = (
            -1)
        end
        item
          Alignment = taRightJustify
          Caption = 'v'
          Width = -1
          WidthType = (
            -1)
        end>
      HideSelection = False
      ReadOnly = True
      RowSelect = True
      TabOrder = 1
      ViewStyle = vsReport
    end
    object ListView1: TListView
      Left = 0
      Top = 0
      Width = 609
      Height = 145
      Align = alTop
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
        end>
      HideSelection = False
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
    end
  end
  object Memo1: TMemo
    Left = 8
    Top = 288
    Width = 609
    Height = 89
    Anchors = [akLeft, akRight, akBottom]
    HideSelection = False
    Lines.Strings = (
      'p'
      'p1'
      'p2'
      'vt'
      'vp')
    ReadOnly = True
    ScrollBars = ssHorizontal
    TabOrder = 1
    WordWrap = False
  end
  object btnRunTo: TButton
    Left = 96
    Top = 496
    Width = 81
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Run to:'
    TabOrder = 3
    OnClick = btnRunToClick
  end
  object txtBreakPoints: TEdit
    Left = 184
    Top = 496
    Width = 401
    Height = 21
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 4
    OnEnter = txtBreakPointsEnter
    OnExit = txtBreakPointsExit
  end
  object Memo2: TMemo
    Left = 8
    Top = 400
    Width = 609
    Height = 89
    Anchors = [akLeft, akRight, akBottom]
    HideSelection = False
    ReadOnly = True
    ScrollBars = ssHorizontal
    TabOrder = 5
    WordWrap = False
  end
  object btnBreak: TButton
    Left = 592
    Top = 496
    Width = 25
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '!'
    TabOrder = 6
    OnClick = btnBreakClick
  end
  object ActionList1: TActionList
    OnChange = btnNextClick
    Left = 16
    Top = 32
    object actNext: TAction
      Caption = '&Next'
      ShortCut = 119
      OnExecute = btnNextClick
    end
    object actRunTo: TAction
      Caption = 'Run to:'
      ShortCut = 16466
      OnExecute = btnRunToClick
    end
  end
end
