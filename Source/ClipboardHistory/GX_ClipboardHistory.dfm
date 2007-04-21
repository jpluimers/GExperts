inherited fmClipboardHistory: TfmClipboardHistory
  Left = 428
  Top = 141
  Width = 477
  Height = 357
  ActiveControl = lvClip
  Caption = 'Clipboard History'
  KeyPreview = True
  Menu = MainMenu
  Position = poScreenCenter
  OnClose = FormClose
  OnKeyDown = FormKeyDown
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter: TSplitter
    Left = 0
    Top = 158
    Width = 469
    Height = 4
    Cursor = crVSplit
    Align = alBottom
    OnMoved = SplitterMoved
  end
  object mmoClipText: TMemo
    Left = 0
    Top = 162
    Width = 469
    Height = 146
    Align = alBottom
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object lvClip: TListView
    Left = 0
    Top = 22
    Width = 469
    Height = 136
    Align = alClient
    Columns = <
      item
        Caption = 'Time'
        Width = -1
        WidthType = (
          -1)
      end
      item
        Caption = 'First Line'
        Width = -1
        WidthType = (
          -1)
      end>
    ColumnClick = False
    HideSelection = False
    MultiSelect = True
    ReadOnly = True
    RowSelect = True
    TabOrder = 1
    ViewStyle = vsReport
    OnChange = lvClipChange
    OnDblClick = lvClipDblClick
    OnKeyPress = lvClipKeyPress
  end
  object ToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 469
    Height = 22
    AutoSize = True
    DisabledImages = dmSharedImages.DisabledImages
    EdgeBorders = []
    Flat = True
    Images = dmSharedImages.Images
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    Wrapable = False
    object tbnClear: TToolButton
      Left = 0
      Top = 0
      Action = actEditClear
    end
    object tbnDelete: TToolButton
      Left = 23
      Top = 0
      Action = actDelete
    end
    object tbnSep1: TToolButton
      Left = 46
      Top = 0
      Width = 8
      ImageIndex = 1
      Style = tbsSeparator
    end
    object tbnCopy: TToolButton
      Left = 54
      Top = 0
      Action = actEditCopy
    end
    object tbnPaste: TToolButton
      Left = 77
      Top = 0
      Action = actEditPasteToIde
    end
    object tbnSep2: TToolButton
      Left = 100
      Top = 0
      Width = 8
      ImageIndex = 3
      Style = tbsSeparator
    end
    object btnOptions: TToolButton
      Left = 108
      Top = 0
      Action = actViewOptions
    end
    object tbnSep3: TToolButton
      Left = 131
      Top = 0
      Width = 8
      ImageIndex = 1
      Style = tbsSeparator
    end
    object tbnHelp: TToolButton
      Left = 139
      Top = 0
      Action = actHelpHelp
    end
  end
  object MainMenu: TMainMenu
    Images = dmSharedImages.Images
    Left = 34
    Top = 48
    object mitFile: TMenuItem
      Caption = '&File'
      object mitFileRehookClipboard: TMenuItem
        Action = actRehookClipboard
      end
      object mitFileExit: TMenuItem
        Action = actFileExit
      end
    end
    object mitEdit: TMenuItem
      Caption = '&Edit'
      object mitEditDelete: TMenuItem
        Action = actDelete
      end
      object mitEditClear: TMenuItem
        Action = actEditClear
      end
      object mitEditSep1: TMenuItem
        Caption = '-'
      end
      object mitEditCopy: TMenuItem
        Action = actEditCopy
      end
      object mitEditPasteToIde: TMenuItem
        Action = actEditPasteToIde
      end
    end
    object mitView: TMenuItem
      Caption = 'View'
      object mitViewToolBar: TMenuItem
        Action = actViewToolBar
      end
      object mitViewOptions: TMenuItem
        Action = actViewOptions
      end
    end
    object mitHelp: TMenuItem
      Caption = '&Help'
      object mitHelpHelp: TMenuItem
        Action = actHelpHelp
      end
      object mitHelpContents: TMenuItem
        Action = actHelpContents
      end
      object mitHelpSep1: TMenuItem
        Caption = '-'
      end
      object mitHelpAbout: TMenuItem
        Action = actHelpAbout
      end
    end
  end
  object Actions: TActionList
    Images = dmSharedImages.Images
    OnUpdate = ActionsUpdate
    Left = 96
    Top = 48
    object actRehookClipboard: TAction
      Category = 'File'
      Caption = 'Rehook Clipboard'
      OnExecute = actRehookClipboardExecute
    end
    object actFileExit: TAction
      Category = 'File'
      Caption = 'E&xit'
      Hint = 'Exit'
      ImageIndex = 8
      ShortCut = 32883
      OnExecute = actFileExitExecute
    end
    object actEditCopy: TAction
      Category = 'Edit'
      Caption = '&Copy'
      Hint = 'Copy selected'
      ImageIndex = 6
      ShortCut = 16451
      OnExecute = actEditCopyExecute
    end
    object actEditClear: TAction
      Category = 'Edit'
      Caption = 'C&lear'
      Hint = 'Clear entries'
      ImageIndex = 44
      ShortCut = 16460
      OnExecute = actEditClearExecute
    end
    object actHelpHelp: TAction
      Category = 'Help'
      Caption = '&Help'
      Hint = 'Help'
      ImageIndex = 0
      ShortCut = 112
      OnExecute = actHelpHelpExecute
    end
    object actHelpContents: TAction
      Category = 'Help'
      Caption = '&Contents'
      Hint = 'Help contents'
      OnExecute = actHelpContentsExecute
    end
    object actHelpAbout: TAction
      Category = 'Help'
      Caption = '&About...'
      Hint = 'About...'
      ImageIndex = 16
      OnExecute = actHelpAboutExecute
    end
    object actEditPasteToIde: TAction
      Category = 'Edit'
      Caption = 'Paste into IDE'
      Hint = 'Paste into IDE'
      ImageIndex = 20
      OnExecute = actEditPasteToIdeExecute
    end
    object actViewToolBar: TAction
      Category = 'View'
      Caption = 'Show Toolbar'
      Checked = True
      Hint = 'Show or hide the toolbar'
      OnExecute = actViewToolBarExecute
    end
    object actViewOptions: TAction
      Category = 'View'
      Caption = 'Options...'
      Hint = 'Options...'
      ImageIndex = 17
      OnExecute = actViewOptionsExecute
    end
    object actDelete: TAction
      Category = 'Edit'
      Caption = 'Delete'
      Hint = 'Delete'
      ImageIndex = 11
      ShortCut = 16430
      OnExecute = actDeleteExecute
    end
  end
end
