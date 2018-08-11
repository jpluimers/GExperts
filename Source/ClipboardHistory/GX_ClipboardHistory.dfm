inherited fmClipboardHistory: TfmClipboardHistory
  Left = 337
  Top = 220
  ActiveControl = lvClip
  Caption = 'Clipboard History'
  ClientHeight = 428
  ClientWidth = 550
  KeyPreview = True
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
    Top = 61
    Width = 550
    Height = 217
    Align = alClient
    Columns = <
      item
        Caption = 'Time'
      end
      item
        Caption = 'Lines'
      end
      item
        Caption = 'First Line'
        Width = 100
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
    OnResize = lvClipResize
  end
  object ToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 550
    Height = 24
    AutoSize = True
    DisabledImages = dmSharedImages.DisabledImages
    Flat = True
    Images = dmSharedImages.Images
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    Wrapable = False
    object tbnHamburgerMenu: TToolButton
      Left = 0
      Top = 0
      Action = actHamburgerMenu
    end
    object tbnClear: TToolButton
      Left = 23
      Top = 0
      Action = actEditClear
    end
    object tbnDelete: TToolButton
      Left = 46
      Top = 0
      Action = actDelete
    end
    object tbnSep1: TToolButton
      Left = 69
      Top = 0
      Width = 8
      ImageIndex = 1
      Style = tbsSeparator
    end
    object tbnCopy: TToolButton
      Left = 77
      Top = 0
      Action = actEditCopy
    end
    object tbnPaste: TToolButton
      Left = 100
      Top = 0
      Action = actEditPasteToIde
      ImageIndex = 7
    end
    object tbnPasteAsPascal: TToolButton
      Left = 123
      Top = 0
      Action = actEditPasteAsPascalString
      ImageIndex = 20
    end
    object tbnSep2: TToolButton
      Left = 146
      Top = 0
      Width = 8
      ImageIndex = 3
      Style = tbsSeparator
    end
    object tbnViewPasteAs: TToolButton
      Left = 154
      Top = 0
      Action = actViewPasteAsOptions
    end
    object tbnSep3: TToolButton
      Left = 177
      Top = 0
      Width = 8
      ImageIndex = 1
      Style = tbsSeparator
    end
    object btnOptions: TToolButton
      Left = 185
      Top = 0
      Action = actViewOptions
    end
    object tbnSep4: TToolButton
      Left = 208
      Top = 0
      Width = 8
      Caption = 'tbnSep4'
      ImageIndex = 1
      Style = tbsSeparator
    end
    object tbnHelp: TToolButton
      Left = 216
      Top = 0
      Action = actHelpHelp
    end
  end
  object pnlPasteAsOptions: TPanel
    Left = 0
    Top = 22
    Width = 550
    Height = 37
    Align = alTop
    BevelOuter = bvLowered
    TabOrder = 3
    object lblMaxEntries: TLabel
      Left = 19
      Top = 11
      Width = 78
      Height = 14
      Alignment = taRightJustify
      Caption = 'Paste as type:'
    end
    object cbPasteAsType: TComboBox
      Left = 105
      Top = 7
      Width = 170
      Height = 22
      Style = csDropDownList
      ItemHeight = 14
      TabOrder = 0
    end
    object chkCreateQuotedStrings: TCheckBox
      Left = 292
      Top = 1
      Width = 250
      Height = 25
      Caption = 'Create quoted strings'
      TabOrder = 1
    end
    object chkAddExtraSpaceAtTheEnd: TCheckBox
      Left = 292
      Top = 20
      Width = 250
      Height = 17
      Caption = 'Add extra space at the end'
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
        Action = actFileRehookClipboard
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
    object actFileRehookClipboard: TAction
      Category = 'File'
      Caption = 'Rehook Clipboard'
      OnExecute = actFileRehookClipboardExecute
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
      Caption = 'Paste as Pascal String'
      Hint = 'Paste as Pascal string'
      ImageIndex = 78
      OnExecute = actEditPasteAsPascalStringExecute
    end
    object actViewPasteAsOptions: TAction
      Category = 'View'
      Caption = 'Show PasteAs Options'
      Checked = True
      Hint = 'Show or hide the PasteAs options panel'
      ImageIndex = 27
      OnExecute = actViewPasteAsOptionsExecute
    end
    object actEditCopyFromPascalString: TAction
      Category = 'Edit'
      Caption = 'Copy from Pascal String'
      Hint = 'Copy from Pascal string'
      ImageIndex = 6
      OnExecute = actEditCopyExecute
    end
    object actEditReplaceAsPascalString: TAction
      Category = 'Edit'
      Caption = 'Replace as Pascal String'
      Hint = 'Replace as Pascal string'
      OnExecute = actEditPasteAsPascalStringExecute
    end
    object actHamburgerMenu: TAction
      Caption = 'Menu'
      ImageIndex = 89
      OnExecute = actHamburgerMenuExecute
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
  object pmHamburgerMenu: TPopupMenu
    Left = 144
    Top = 120
    object File1: TMenuItem
      Caption = '&File'
      object RehookClipboard1: TMenuItem
        Action = actFileRehookClipboard
      end
    end
    object Edit1: TMenuItem
      Caption = '&Edit'
      object Delete1: TMenuItem
        Action = actDelete
      end
      object Clear1: TMenuItem
        Action = actEditClear
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Copy1: TMenuItem
        Action = actEditCopy
      end
      object CopyfromPascalString1: TMenuItem
        Action = actEditCopyFromPascalString
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object PasteasPascalString1: TMenuItem
        Action = actEditPasteToIde
      end
      object PasteasPascalString2: TMenuItem
        Action = actEditPasteAsPascalString
      end
      object ReplaceasPascalString1: TMenuItem
        Action = actEditReplaceAsPascalString
      end
    end
    object View1: TMenuItem
      Caption = '&View'
      object ShowToolbar1: TMenuItem
        Action = actViewToolBar
      end
      object ShowPasteAsOptions2: TMenuItem
        Action = actViewPasteAsOptions
      end
      object Options1: TMenuItem
        Action = actViewOptions
      end
    end
    object Help1: TMenuItem
      Caption = '&Help'
      object Help2: TMenuItem
        Action = actHelpHelp
      end
      object Contents1: TMenuItem
        Action = actHelpContents
      end
    end
  end
end
