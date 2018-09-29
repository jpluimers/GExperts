object fmProcedureList: TfmProcedureList
  Left = 264
  Top = 199
  ActiveControl = edtMethods
  AutoScroll = False
  BorderIcons = [biSystemMenu]
  Caption = 'Procedure List'
  ClientHeight = 287
  ClientWidth = 742
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnClose = FormClose
  OnKeyPress = edtMethodsKeyPress
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 14
  object splSeparator: TSplitter
    Left = 517
    Top = 0
    Width = 3
    Height = 268
    Cursor = crHSplit
    Align = alRight
    OnCanResize = splSeparatorCanResize
  end
  object pnlFuncHolder: TPanel
    Left = 0
    Top = 0
    Width = 517
    Height = 268
    Align = alClient
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 0
    object pnHolder: TPanel
      Left = 0
      Top = 58
      Width = 517
      Height = 210
      Align = alClient
      BevelOuter = bvNone
      FullRepaint = False
      TabOrder = 0
      object lvProcs: TListView
        Left = 0
        Top = 0
        Width = 517
        Height = 210
        Align = alClient
        Columns = <
          item
            Width = 20
          end
          item
            Caption = 'Procedure'
            Width = 313
          end
          item
            Caption = 'Type'
            Width = 110
          end
          item
            Caption = 'Line'
            Width = 68
          end>
        HideSelection = False
        ReadOnly = True
        RowSelect = True
        SmallImages = dmSharedImages.Images
        SortType = stData
        TabOrder = 0
        ViewStyle = vsReport
        OnChange = lvProcsChange
        OnColumnClick = lvProcsColumnClick
        OnCompare = lvProcsCompare
        OnDblClick = actViewGotoExecute
        OnResize = lvProcsResize
      end
    end
    object pnlHeader: TPanel
      Left = 0
      Top = 22
      Width = 517
      Height = 36
      Align = alTop
      BevelOuter = bvNone
      FullRepaint = False
      ParentColor = True
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnResize = pnlHeaderResize
      object pnlHeaderLeft: TPanel
        Left = 0
        Top = 0
        Width = 252
        Height = 36
        Align = alLeft
        BevelOuter = bvNone
        FullRepaint = False
        TabOrder = 0
        object lblMethods: TLabel
          Left = 14
          Top = 10
          Width = 37
          Height = 14
          Alignment = taRightJustify
          Caption = '&Search'
          FocusControl = edtMethods
        end
        object edtMethods: TEdit
          Left = 56
          Top = 6
          Width = 194
          Height = 22
          TabOrder = 0
          OnChange = edtMethodsChange
          OnKeyDown = edtMethodsKeyDown
          OnKeyPress = edtMethodsKeyPress
        end
      end
      object pnlHeaderRight: TPanel
        Left = 252
        Top = 0
        Width = 265
        Height = 36
        Align = alClient
        BevelOuter = bvNone
        FullRepaint = False
        TabOrder = 1
        object lblObjects: TLabel
          Left = 14
          Top = 8
          Width = 42
          Height = 14
          Alignment = taRightJustify
          Caption = '&Objects'
          FocusControl = cbxObjects
        end
        object cbxObjects: TComboBox
          Left = 62
          Top = 4
          Width = 198
          Height = 22
          Style = csDropDownList
          DropDownCount = 16
          ItemHeight = 14
          Sorted = True
          TabOrder = 0
          OnChange = cbxObjectsChange
          OnKeyPress = edtMethodsKeyPress
        end
      end
    end
    object ToolBar: TToolBar
      Left = 0
      Top = 0
      Width = 517
      Height = 22
      AutoSize = True
      DisabledImages = dmSharedImages.DisabledImages
      Flat = True
      Images = dmSharedImages.Images
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      Wrapable = False
      object tbnCopy: TToolButton
        Left = 0
        Top = 0
        Action = actEditCopy
      end
      object tbnSep1: TToolButton
        Left = 23
        Top = 0
        Width = 8
        ImageIndex = 47
        Style = tbsSeparator
      end
      object tbnMatchClass: TToolButton
        Left = 31
        Top = 0
        Action = actMatchClass
        Grouped = True
        Style = tbsCheck
      end
      object tbnMatchProc: TToolButton
        Left = 54
        Top = 0
        Action = actMatchMethod
        Grouped = True
        Style = tbsCheck
      end
      object tbnSep2: TToolButton
        Left = 77
        Top = 0
        Width = 8
        ImageIndex = 1
        Style = tbsSeparator
      end
      object tbnStart: TToolButton
        Left = 85
        Top = 0
        Action = actViewStart
        Grouped = True
        Style = tbsCheck
      end
      object tbnAny: TToolButton
        Left = 108
        Top = 0
        Action = actViewAny
        Grouped = True
        Style = tbsCheck
      end
      object tbnSep3: TToolButton
        Left = 131
        Top = 0
        Width = 8
        ImageIndex = 4
        Style = tbsSeparator
      end
      object tbnGoto: TToolButton
        Left = 139
        Top = 0
        Action = actViewGoto
      end
      object tbnSep4: TToolButton
        Left = 162
        Top = 0
        Width = 8
        ImageIndex = 5
        Style = tbsSeparator
      end
      object tbnShowFunctionCode: TToolButton
        Left = 170
        Top = 0
        Hint = 'Show procedure code'
        ImageIndex = 58
        OnClick = tbnShowFunctionCodeClick
      end
      object tbnSep5: TToolButton
        Left = 193
        Top = 0
        Width = 8
        ImageIndex = 1
        Style = tbsSeparator
      end
      object tbnOptions: TToolButton
        Left = 201
        Top = 0
        Hint = 'Options'
        Action = actOptions
      end
      object tbnSep6: TToolButton
        Left = 224
        Top = 0
        Width = 8
        ImageIndex = 1
        Style = tbsSeparator
      end
      object tbnHelp: TToolButton
        Left = 232
        Top = 0
        Action = actHelpHelp
      end
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 268
    Width = 742
    Height = 19
    Panels = <
      item
        Width = 420
      end
      item
        Text = '999/999'
        Width = 50
      end>
    ParentFont = True
    SimplePanel = False
    UseSystemFont = False
  end
  object pnlFunctionBody: TPanel
    Left = 520
    Top = 0
    Width = 222
    Height = 268
    Align = alRight
    BevelOuter = bvLowered
    Caption = 'Editor Created at Runtime'
    TabOrder = 1
    Visible = False
  end
  object dlgProcFont: TFontDialog
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    MinFontSize = 0
    MaxFontSize = 0
    Left = 24
    Top = 88
  end
  object Actions: TActionList
    Images = dmSharedImages.Images
    OnUpdate = ActionsUpdate
    Left = 80
    Top = 88
    object actEditCopy: TAction
      Category = 'Edit'
      Caption = '&Edit'
      Hint = 'Copy procedures to clipboard'
      ImageIndex = 6
      ShortCut = 16451
      OnExecute = actEditCopyExecute
    end
    object actOptionsFont: TAction
      Category = 'Options'
      Caption = '&Font..'
      Hint = 'Configure font'
      ImageIndex = 26
      OnExecute = actOptionsFontExecute
    end
    object actViewStart: TAction
      Category = 'View'
      Caption = 'S&tart'
      Hint = 'Match only from the start'
      ImageIndex = 24
      OnExecute = actViewStartExecute
    end
    object actViewAny: TAction
      Category = 'View'
      Caption = '&Any'
      Hint = 'Match anywhere'
      ImageIndex = 25
      OnExecute = actViewAnyExecute
    end
    object actViewGoto: TAction
      Category = 'View'
      Caption = '&Goto'
      Hint = 'Goto implementation'
      ImageIndex = 27
      ShortCut = 13
      OnExecute = actViewGotoExecute
    end
    object actHelpHelp: TAction
      Category = 'Help'
      Caption = '&Help'
      Hint = 'Help'
      ImageIndex = 0
      OnExecute = actHelpHelpExecute
    end
    object actOptions: TAction
      Category = 'Options'
      Caption = 'Options'
      ImageIndex = 17
      OnExecute = actOptionsExecute
    end
    object actMatchClass: TAction
      Category = 'Match'
      Caption = 'Class'
      Hint = 'Match class and method names'
      ImageIndex = 70
      OnExecute = actMatchClassExecute
    end
    object actMatchMethod: TAction
      Category = 'Match'
      Caption = 'Method'
      Hint = 'Match method names only'
      ImageIndex = 69
      OnExecute = actMatchMethodExecute
    end
  end
  object tmrFilter: TTimer
    Enabled = False
    Interval = 10
    OnTimer = tmrFilterTimer
    Left = 136
    Top = 88
  end
end
