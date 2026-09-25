object frmDebugView: TfrmDebugView
  Left = 337
  Top = 165
  ActiveControl = PageControl1
  Caption = 'Strato Debug View'
  ClientHeight = 528
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Consolas'
  Font.Style = []
  OnClose = FormClose
  TextHeight = 14
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 624
    Height = 528
    ActivePage = tsContext
    Align = alClient
    TabOrder = 0
    object tsContext: TTabSheet
      BorderWidth = 4
      Caption = 'Context'
      DesignSize = (
        608
        491)
      object lblUpNext: TLabel
        Left = 0
        Top = 248
        Width = 49
        Height = 14
        Anchors = [akLeft, akBottom]
        Caption = 'Up next'
        ExplicitTop = 247
      end
      object lblFileName: TLabel
        Left = 0
        Top = 360
        Width = 56
        Height = 14
        Anchors = [akLeft, akBottom]
        Caption = '[Source]'
      end
      object btnNext: TButton
        Left = 0
        Top = 470
        Width = 81
        Height = 21
        Action = actNext
        Anchors = [akLeft, akBottom]
        TabOrder = 5
      end
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 608
        Height = 241
        Anchors = [akLeft, akTop, akRight, akBottom]
        BevelOuter = bvNone
        TabOrder = 0
        OnResize = Panel1Resize
        object Splitter1: TSplitter
          Left = 0
          Top = 120
          Width = 608
          Height = 4
          Cursor = crVSplit
          Align = alTop
          ResizeStyle = rsUpdate
          OnMoved = Splitter1Moved
        end
        object lvMem: TListView
          Left = 0
          Top = 124
          Width = 608
          Height = 117
          Align = alClient
          Columns = <
            item
              Caption = '@'
              Width = 100
            end
            item
              Caption = 'x'
              Width = 100
            end
            item
              Alignment = taRightJustify
              Caption = 'v'
              Width = 100
            end
            item
              Caption = 'What'
              Width = 800
            end>
          HideSelection = False
          OwnerData = True
          ReadOnly = True
          RowSelect = True
          TabOrder = 1
          ViewStyle = vsReport
        end
        object lvStack: TListView
          Left = 0
          Top = 0
          Width = 608
          Height = 120
          Align = alTop
          Columns = <
            item
              Caption = '#'
              Width = 100
            end
            item
              Alignment = taRightJustify
              Caption = 'v'
              Width = 100
            end
            item
              Alignment = taRightJustify
              Caption = 'ip'
              Width = 100
            end
            item
              Caption = 'What'
              Width = 800
            end>
          HideSelection = False
          OwnerData = True
          ReadOnly = True
          RowSelect = True
          TabOrder = 0
          ViewStyle = vsReport
          OnDblClick = lvStackDblClick
        end
      end
      object txtUpNext: TMemo
        Left = 0
        Top = 263
        Width = 608
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
        TabOrder = 3
        WordWrap = False
      end
      object btnRunTo: TButton
        Left = 88
        Top = 470
        Width = 81
        Height = 21
        Action = actRunTo
        Anchors = [akLeft, akBottom]
        TabOrder = 6
      end
      object txtBreakPoints: TEdit
        Left = 176
        Top = 470
        Width = 400
        Height = 22
        Anchors = [akLeft, akRight, akBottom]
        TabOrder = 7
        OnEnter = txtBreakPointsEnter
        OnExit = txtBreakPointsExit
      end
      object txtSourceView: TMemo
        Left = 0
        Top = 375
        Width = 608
        Height = 90
        Anchors = [akLeft, akRight, akBottom]
        HideSelection = False
        ReadOnly = True
        ScrollBars = ssHorizontal
        TabOrder = 4
        WordWrap = False
      end
      object btnBreak: TButton
        Left = 582
        Top = 470
        Width = 25
        Height = 21
        Anchors = [akRight, akBottom]
        Caption = '!'
        TabOrder = 8
        OnClick = btnBreakClick
      end
      object cbKeepTrail: TCheckBox
        Left = 200
        Top = 248
        Width = 113
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = 'Keep Trail'
        TabOrder = 2
      end
      object cbStackTrace: TCheckBox
        Left = 80
        Top = 248
        Width = 113
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = 'Trace Stack'
        TabOrder = 1
      end
    end
    object tsStackTrace: TTabSheet
      BorderWidth = 4
      Caption = 'Trace'
      ImageIndex = 2
      object lvStackTrace: TListView
        Left = 0
        Top = 0
        Width = 608
        Height = 491
        Align = alClient
        Columns = <
          item
            Caption = 'sp'
            Width = -1
            WidthType = (
              -1)
          end
          item
            Alignment = taRightJustify
            Caption = 'ip'
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
            Caption = 'Parent'
            Width = -1
            WidthType = (
              -1)
          end
          item
            Caption = 'Source'
            Width = -1
            WidthType = (
              -1)
          end
          item
            Alignment = taRightJustify
            Caption = 'x'
            Width = -1
            WidthType = (
              -1)
          end
          item
            Alignment = taRightJustify
            Caption = 'y'
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
    object tsTrail: TTabSheet
      BorderWidth = 4
      Caption = 'Trail'
      ImageIndex = 1
      object lvTrail: TListView
        Left = 0
        Top = 0
        Width = 608
        Height = 491
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
            Caption = 'ip'
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
            Caption = 't1'
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
            Alignment = taRightJustify
            Caption = 'cp'
            Width = -1
            WidthType = (
              -1)
          end
          item
            Caption = 'ep'
            Width = -1
            WidthType = (
              -1)
          end
          item
            Caption = 'sp'
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
        MultiSelect = True
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
    object tsInspect: TTabSheet
      BorderWidth = 4
      Caption = 'Inspect'
      ImageIndex = 3
      DesignSize = (
        608
        491)
      object txtInpectNr: TEdit
        Left = 351
        Top = 2
        Width = 121
        Height = 22
        TabOrder = 0
        OnKeyPress = txtInpectNrKeyPress
      end
      object btnInspect: TButton
        Left = 478
        Top = 2
        Width = 25
        Height = 22
        TabOrder = 1
        OnClick = btnInspectClick
      end
      object txtInspect: TMemo
        Left = 0
        Top = 30
        Width = 621
        Height = 458
        Anchors = [akLeft, akTop, akRight, akBottom]
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 2
        WordWrap = False
        ExplicitWidth = 605
      end
      object cbInspectSphere: TComboBox
        Left = 0
        Top = 2
        Width = 345
        Height = 22
        Style = csDropDownList
        TabOrder = 3
      end
    end
  end
  object ActionList1: TActionList
    OnChange = btnNextClick
    Left = 40
    Top = 72
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
    object actRefresh: TAction
      Caption = 'Refresh'
      ShortCut = 116
    end
  end
end
