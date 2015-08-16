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
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 625
    Height = 529
    ActivePage = txContext
    Align = alClient
    TabOrder = 0
    object txContext: TTabSheet
      BorderWidth = 4
      Caption = 'Context'
      DesignSize = (
        609
        493)
      object lblUpNext: TLabel
        Left = 0
        Top = 248
        Width = 56
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = 'Up next'
      end
      object lblFileName: TLabel
        Left = 0
        Top = 360
        Width = 64
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = '[Source]'
      end
      object btnNext: TButton
        Left = 0
        Top = 468
        Width = 81
        Height = 25
        Action = actNext
        Anchors = [akLeft, akBottom]
        TabOrder = 0
      end
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 609
        Height = 241
        Anchors = [akLeft, akTop, akRight, akBottom]
        BevelOuter = bvNone
        TabOrder = 4
        object Splitter1: TSplitter
          Left = 0
          Top = 145
          Width = 609
          Height = 4
          Cursor = crVSplit
          Align = alTop
        end
        object lvMem: TListView
          Left = 0
          Top = 149
          Width = 609
          Height = 92
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
          TabOrder = 1
          ViewStyle = vsReport
        end
        object lvStack: TListView
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
      object txtUpNext: TMemo
        Left = 0
        Top = 263
        Width = 609
        Height = 90
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
        TabOrder = 6
        WordWrap = False
      end
      object btnRunTo: TButton
        Left = 88
        Top = 468
        Width = 81
        Height = 25
        Action = actRunTo
        Anchors = [akLeft, akBottom]
        TabOrder = 1
      end
      object txtBreakPoints: TEdit
        Left = 176
        Top = 470
        Width = 401
        Height = 21
        Anchors = [akLeft, akRight, akBottom]
        TabOrder = 2
        OnEnter = txtBreakPointsEnter
        OnExit = txtBreakPointsExit
      end
      object txtSourceView: TMemo
        Left = 0
        Top = 375
        Width = 609
        Height = 90
        Anchors = [akLeft, akRight, akBottom]
        HideSelection = False
        ReadOnly = True
        ScrollBars = ssHorizontal
        TabOrder = 7
        WordWrap = False
      end
      object btnBreak: TButton
        Left = 583
        Top = 468
        Width = 25
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = '!'
        TabOrder = 3
        OnClick = btnBreakClick
      end
      object cbKeepTrail: TCheckBox
        Left = 72
        Top = 248
        Width = 121
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = 'Keep Trail'
        TabOrder = 5
      end
    end
    object tsTrail: TTabSheet
      BorderWidth = 4
      Caption = 'Trail'
      ImageIndex = 1
      object lvTrail: TListView
        Left = 0
        Top = 0
        Width = 609
        Height = 493
        Align = alClient
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
            Caption = 'mp'
            Width = -1
            WidthType = (
              -1)
          end
          item
            Alignment = taRightJustify
            Caption = 'np'
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
        MultiSelect = True
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
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
      ShortCut = 120
      OnExecute = btnRunToClick
    end
    object actRunToFocus: TAction
      Caption = 'Run to:'
      ShortCut = 16466
      OnExecute = actRunToFocusExecute
    end
    object actCopy: TAction
      Caption = 'Copy'
      ShortCut = 16451
      OnExecute = actCopyExecute
    end
  end
end
