inherited fmClipboardHistory: TfmClipboardHistory
  Left = -737
  Top = 210
  ActiveControl = lvClip
  Caption = 'Clipboard History'
  ClientHeight = 428
  ClientWidth = 550
  KeyPreview = True
  Menu = MainMenu
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 14
  object Splitter: TSplitter
    Left = 0
    Top = 278
    Width = 550
    Height = 4
    Cursor = crVSplit
    Align = alBottom
    OnMoved = SplitterMoved
  end
  object mmoClipText: TMemo
    Left = 0
    Top = 282
    Width = 550
    Height = 146
    Align = alBottom
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object lvClip: TListView
    Left = 0
    Top = 63
    Width = 550
    Height = 215
    Align = alClient
    Columns = <
      item
        Caption = 'Time'
        Width = -1
        WidthType = (
          -1)
      end
      item
        Caption = 'Lines'
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
    PopupMenu = pmListMenu
    TabOrder = 1
    ViewStyle = vsReport
    OnChange = lvClipChange
    OnDblClick = lvClipDblClick
    OnKeyPress = lvClipKeyPress
  end
  object ToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 550
    Height = 26
    AutoSize = True
    DisabledImages = dmSharedImages.DisabledImages
    Images = dmSharedImages.Images
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    Wrapable = False
    object tbnClear: TToolButton
      Left = 0
      Top = 2
      Action = actEditClear
    end
    object tbnDelete: TToolButton
      Left = 23
      Top = 2
      Action = actDelete
    end
    object tbnSep1: TToolButton
      Left = 46
      Top = 2
      Width = 8
      ImageIndex = 1
      Style = tbsSeparator
    end
    object tbnCopy: TToolButton
      Left = 54
      Top = 2
      Action = actEditCopy
    end
    object tbnPaste: TToolButton
      Left = 77
      Top = 2
      Action = actEditPasteToIde
    end
    object tbnPasteAsPascal: TToolButton
      Left = 100
      Top = 2
      Action = actEditPasteAsPascalString
    end
    object tbnSep2: TToolButton
      Left = 123
      Top = 2
      Width = 8
      ImageIndex = 3
      Style = tbsSeparator
    end
    object tbnViewPasteAs: TToolButton
      Left = 131
      Top = 2
      Action = actViewPasteAsOptions
    end
    object tbnSep3: TToolButton
      Left = 154
      Top = 2
      Width = 8
      ImageIndex = 1
      Style = tbsSeparator
    end
    object btnOptions: TToolButton
      Left = 162
      Top = 2
      Action = actViewOptions
    end
    object tbnSep4: TToolButton
      Left = 185
      Top = 2
      Width = 8
      Caption = 'tbnSep4'
      ImageIndex = 1
      Style = tbsSeparator
    end
    object tbnHelp: TToolButton
      Left = 193
      Top = 2
      Action = actHelpHelp
    end
  end
  object pnlPasteAsOptions: TPanel
    Left = 0
    Top = 26
    Width = 550
    Height = 37
    Align = alTop
    BevelOuter = bvLowered
    TabOrder = 3
    object lblMaxEntries: TLabel
      Left = 14
      Top = 10
      Width = 78
      Height = 14
      Alignment = taRightJustify
      Caption = 'Paste as type:'
    end
    object cbPasteAsType: TComboBox
      Left = 100
      Top = 7
      Width = 170
      Height = 22
      Style = csDropDownList
      ItemHeight = 14
      TabOrder = 0
    end
    object chkCreateQuotedStrings: TCheckBox
      Left = 285
      Top = 1
      Width = 250
      Height = 25
      Caption = 'Create quoted strings'
      TabOrder = 1
    end
    object chkAddExtraSpaceAtTheEnd: TCheckBox
      Left = 285
      Top = 20
      Width = 250
      Height = 17
      Caption = 'Add extra space char at the end'
      TabOrder = 2
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
      object mitEditCopyfromPascalstring: TMenuItem
        Action = actEditCopyFromPascalString
      end
      object mitEditPasteToIde: TMenuItem
        Action = actEditPasteToIde
      end
      object mitEditPasteAsPascalString: TMenuItem
        Action = actEditPasteAsPascalString
      end
      object mitEditReplaceasPascalstring: TMenuItem
        Action = actEditReplaceAsPascalString
      end
    end
    object mitView: TMenuItem
      Caption = 'View'
      object mitViewToolBar: TMenuItem
        Action = actViewToolBar
      end
      object ShowPasteAsoptions1: TMenuItem
        Action = actViewPasteAsOptions
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
    object actEditPasteAsPascalString: TAction
      Category = 'Edit'
      Caption = 'Paste as Pascal string'
      Hint = 'Paste as Pascal string'
      ImageIndex = 78
      OnExecute = actEditPasteAsPascalStringExecute
    end
    object actViewPasteAsOptions: TAction
      Category = 'View'
      Caption = 'Show PasteAs options'
      Checked = True
      Hint = 'Show or hide the PasteAs options panel'
      ImageIndex = 27
      OnExecute = actViewPasteAsOptionsExecute
    end
    object actEditCopyFromPascalString: TAction
      Category = 'Edit'
      Caption = 'Copy from Pascal string'
      Hint = 'Copy from Pascal string'
      ImageIndex = 6
      OnExecute = actEditCopyExecute
    end
    object actEditReplaceAsPascalString: TAction
      Category = 'Edit'
      Caption = 'Replace as Pascal string'
      Hint = 'Replace as Pascal string'
      OnExecute = actEditPasteAsPascalStringExecute
    end
  end
  object pmListMenu: TPopupMenu
    Left = 208
    Top = 56
    object mitListCopy: TMenuItem
      Action = actEditCopy
    end
    object mitListPasteIntoIDE: TMenuItem
      Action = actEditPasteToIde
    end
    object mitListSep1: TMenuItem
      Caption = '-'
    end
    object mitListPasteAsPascalString: TMenuItem
      Action = actEditPasteAsPascalString
    end
    object mitListCopyfromPascalstring: TMenuItem
      Action = actEditCopyFromPascalString
    end
    object mitReplaceasPascalstring: TMenuItem
      Action = actEditReplaceAsPascalString
    end
    object mitListSep2: TMenuItem
      Caption = '-'
    end
    object mitListDelete: TMenuItem
      Action = actDelete
    end
  end
end
