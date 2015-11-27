inherited fmGrepResults: TfmGrepResults
  Left = 324
  Top = 243
  ActiveControl = lbResults
  Caption = 'Grep Results'
  ClientHeight = 689
  ClientWidth = 540
  DefaultMonitor = dmDesktop
  KeyPreview = True
  Menu = MainMenu
  Position = poScreenCenter
  ShowHint = True
  OnKeyPress = FormKeyPress
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object pnlMain: TPanel
    Left = 0
    Top = 0
    Width = 540
    Height = 670
    Align = alClient
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 0
    object SplitterContext: TSplitter
      Left = 0
      Top = 574
      Width = 540
      Height = 8
      Cursor = crVSplit
      Align = alBottom
      AutoSnap = False
      Beveled = True
      MinSize = 20
    end
    object SplitterFoundList: TSplitter
      Left = 81
      Top = 26
      Width = 8
      Height = 548
      Cursor = crHSplit
      AutoSnap = False
      Beveled = True
      MinSize = 20
    end
    object lbResults: TListBox
      Left = 89
      Top = 26
      Width = 451
      Height = 548
      Style = lbOwnerDrawFixed
      Align = alClient
      ItemHeight = 17
      TabOrder = 0
      OnClick = lbResultsClick
      OnDblClick = actListGotoSelectedExecute
      OnDrawItem = lbResultsDrawItem
      OnKeyDown = lbResultsKeyDown
      OnKeyPress = lbResultsKeyPress
      OnMouseDown = lbResultsMouseDown
      OnMouseMove = lbResultsMouseMove
      OnMouseUp = lbResultsMouseUp
    end
    object ToolBar: TToolBar
      Left = 0
      Top = 0
      Width = 540
      Height = 26
      AutoSize = True
      DisabledImages = dmSharedImages.DisabledImages
      Images = dmSharedImages.Images
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      Wrapable = False
      object tbnSearch: TToolButton
        Left = 0
        Top = 2
        Action = actFileSearch
      end
      object tbnRefresh: TToolButton
        Left = 23
        Top = 2
        Action = actFileRefresh
      end
      object tbnSep1: TToolButton
        Left = 46
        Top = 2
        Width = 8
        ImageIndex = 7
        Style = tbsSeparator
      end
      object tbnAbort: TToolButton
        Left = 54
        Top = 2
        Action = actFileAbort
      end
      object tbnSep2: TToolButton
        Left = 77
        Top = 2
        Width = 8
        ImageIndex = 3
        Style = tbsSeparator
      end
      object tbnGoto: TToolButton
        Left = 85
        Top = 2
        Action = actListGotoSelected
      end
      object tbnSep3: TToolButton
        Left = 108
        Top = 2
        Width = 8
        ImageIndex = 4
        Style = tbsSeparator
      end
      object tbnPrint: TToolButton
        Left = 116
        Top = 2
        Action = actFilePrint
      end
      object tbnSep4: TToolButton
        Left = 139
        Top = 2
        Width = 8
        ImageIndex = 5
        Style = tbsSeparator
      end
      object tbnContract: TToolButton
        Left = 147
        Top = 2
        Action = actListContract
      end
      object tbnExpand: TToolButton
        Left = 170
        Top = 2
        Action = actListExpand
      end
      object tbnSep5: TToolButton
        Left = 193
        Top = 2
        Width = 8
        ImageIndex = 8
        Style = tbsSeparator
      end
      object tbnReplaceSelected: TToolButton
        Left = 201
        Top = 2
        Action = actReplaceSelected
      end
      object tbnReplaceAll: TToolButton
        Left = 224
        Top = 2
        Action = actReplaceAll
      end
      object tbnSep6: TToolButton
        Left = 247
        Top = 2
        Width = 8
        ImageIndex = 9
        Style = tbsSeparator
      end
      object tbnStayOnTop: TToolButton
        Left = 255
        Top = 2
        Action = actViewStayOnTop
      end
      object tbnSep7: TToolButton
        Left = 278
        Top = 2
        Width = 8
        ImageIndex = 2
        Style = tbsSeparator
      end
      object tbnShowFullFilename: TToolButton
        Left = 286
        Top = 2
        Action = actViewShowFullFilename
        Style = tbsCheck
      end
      object tbnSep8: TToolButton
        Left = 309
        Top = 2
        Width = 8
        Caption = 'tbnSep8'
        ImageIndex = 1
        Style = tbsSeparator
      end
      object tbnHelp: TToolButton
        Left = 317
        Top = 2
        Action = actHelpHelp
      end
    end
    object reContext: TRichEdit
      Left = 0
      Top = 582
      Width = 540
      Height = 88
      Align = alBottom
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      PopupMenu = pmContextMenu
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 1
      WordWrap = False
    end
    object lbFoundList: TListBox
      Left = 0
      Top = 26
      Width = 81
      Height = 548
      Style = lbVirtualOwnerDraw
      Align = alLeft
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ItemHeight = 25
      ParentFont = False
      PopupMenu = pmFoundMenu
      TabOrder = 3
      OnContextPopup = lbFoundListContextPopup
      OnData = lbFoundListData
      OnDblClick = lbFoundListDblClick
      OnDrawItem = lbFoundListDrawItem
      OnMouseUp = lbFoundListMouseUp
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 670
    Width = 540
    Height = 19
    Panels = <
      item
        Width = 250
      end
      item
        Width = 60
      end>
    ParentFont = True
    SimplePanel = False
    UseSystemFont = False
  end
  object MainMenu: TMainMenu
    Images = dmSharedImages.Images
    Left = 120
    Top = 32
    object mitFile: TMenuItem
      Caption = '&File'
      Left = 522
      Top = 351
      object mitFileSearch: TMenuItem
        Action = actFileSearch
        Left = 532
        Top = 361
      end
      object mitFileRefresh: TMenuItem
        Action = actFileRefresh
        Left = 542
        Top = 371
      end
      object mitFileAbort: TMenuItem
        Action = actFileAbort
        Left = 552
        Top = 381
      end
      object mitFileSep1: TMenuItem
        Caption = '-'
      end
      object mitFilePrint: TMenuItem
        Action = actFilePrint
        Left = 562
        Top = 391
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object Open1: TMenuItem
        Action = actFileOpen
      end
      object N9: TMenuItem
        Caption = '-'
      end
      object mitFileSave: TMenuItem
        Action = actFileSave
      end
      object miFilePrintToFile: TMenuItem
        Action = actFilePrintToFile
      end
      object miFileSavePrint: TMenuItem
        Action = actFileSavePrint
      end
      object N7: TMenuItem
        Caption = '-'
      end
      object miFileRefreshAll: TMenuItem
        Action = actFoundRefreshAll
      end
      object N11: TMenuItem
        Caption = '-'
      end
      object miFileSaveAll: TMenuItem
        Action = actFoundSaveAll
      end
      object miFilePrintAllToFile: TMenuItem
        Action = actFoundPrintAllToFile
      end
      object miFileSavePrintAllToFile: TMenuItem
        Action = actFoundSavePrintAll
      end
      object mitFileSep3: TMenuItem
        Caption = '-'
      end
      object mitFileExit: TMenuItem
        Action = actFileExit
        Left = 592
        Top = 421
      end
    end
    object mitList: TMenuItem
      Caption = '&List'
      Left = 602
      Top = 422
      object mitListGotoSelected: TMenuItem
        Action = actListGotoSelected
        Left = 764
        Top = 422
      end
      object GotoSelectedandClose1: TMenuItem
        Action = actListGotoSelectedAndClose
      end
      object mitListSep1: TMenuItem
        Caption = '-'
      end
      object mitFileCopy: TMenuItem
        Action = actFileCopy
      end
      object mitListSep2: TMenuItem
        Caption = '-'
      end
      object mitListContract: TMenuItem
        Action = actListContract
        Left = 764
        Top = 422
      end
      object mitListExpand: TMenuItem
        Action = actListExpand
        Left = 764
        Top = 422
      end
    end
    object mitView: TMenuItem
      Caption = '&View'
      object mitViewOptions: TMenuItem
        Action = actViewOptions
      end
      object mitViewSep1: TMenuItem
        Caption = '-'
      end
      object mitViewToolBar: TMenuItem
        Action = actViewToolBar
      end
      object miViewShowMatchContext: TMenuItem
        Action = actViewShowContext
      end
      object miViewShowfoundlist: TMenuItem
        Action = actViewShowFoundList
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object miViewShowfullfilename: TMenuItem
        Action = actViewShowFullFilename
      end
      object mitViewSep2: TMenuItem
        Caption = '-'
      end
      object mitViewStayOnTop: TMenuItem
        Action = actViewStayOnTop
        Left = 572
        Top = 401
      end
    end
    object mitReplace: TMenuItem
      Caption = 'Replace'
      object mitReplaceReplaceAll: TMenuItem
        Action = actReplaceAll
      end
      object mitReplaceSelected: TMenuItem
        Action = actReplaceSelected
      end
    end
    object mitHelp: TMenuItem
      Caption = '&Help'
      Left = 764
      Top = 422
      object mitHelpHelp: TMenuItem
        Action = actHelpHelp
        Left = 764
        Top = 422
      end
      object mitHelpContents: TMenuItem
        Action = actHelpContents
      end
      object mitHelpSep1: TMenuItem
        Caption = '-'
      end
      object mitHelpAbout: TMenuItem
        Action = actHelpAbout
        Left = 764
        Top = 422
      end
    end
  end
  object Actions: TActionList
    Images = dmSharedImages.Images
    OnUpdate = ActionsUpdate
    Left = 184
    Top = 32
    object actReplaceSelected: TAction
      Category = 'Replace'
      Caption = 'Replace Selected Item...'
      Hint = 'Replace selected item...'
      ImageIndex = 60
      ShortCut = 24659
      OnExecute = actReplaceSelectedExecute
    end
    object actFileSearch: TAction
      Category = 'File'
      Caption = '&Search...'
      Hint = 'New search...'
      ImageIndex = 14
      ShortCut = 16467
      OnExecute = actFileSearchExecute
    end
    object actFileRefresh: TAction
      Category = 'File'
      Caption = '&Refresh'
      Hint = 'Refresh search'
      ImageIndex = 34
      ShortCut = 116
      OnExecute = actFileRefreshExecute
    end
    object actFileAbort: TAction
      Category = 'File'
      Caption = '&Abort'
      Hint = 'Abort search'
      ImageIndex = 32
      ShortCut = 16449
      OnExecute = actFileAbortExecute
    end
    object actFilePrint: TAction
      Category = 'File'
      Caption = '&Print'
      Hint = 'Print results'
      ImageIndex = 3
      ShortCut = 16464
      OnExecute = actFilePrintExecute
    end
    object actViewStayOnTop: TAction
      Category = 'View'
      Caption = 'Stay on Top'
      Hint = 'Stay on top'
      ImageIndex = 33
      OnExecute = actViewStayOnTopExecute
    end
    object actFileExit: TAction
      Category = 'File'
      Caption = 'E&xit'
      Hint = 'Exit'
      ImageIndex = 8
      ShortCut = 32883
      OnExecute = actFileExitExecute
    end
    object actListGotoSelected: TAction
      Category = 'List'
      Caption = 'Goto Selected'
      Hint = 'Goto match'
      ImageIndex = 27
      OnExecute = actListGotoSelectedExecute
    end
    object actListContract: TAction
      Category = 'List'
      Caption = '&Contract'
      Hint = 'Contract all'
      ImageIndex = 13
      ShortCut = 16462
      OnExecute = actListContractExecute
    end
    object actListExpand: TAction
      Category = 'List'
      Caption = '&Expand'
      Hint = 'Expand all'
      ImageIndex = 12
      ShortCut = 16453
      OnExecute = actListExpandExecute
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
      Hint = 'Contents'
      OnExecute = actHelpContentsExecute
    end
    object actHelpAbout: TAction
      Category = 'Help'
      Caption = '&About...'
      Hint = 'About...'
      ImageIndex = 16
      OnExecute = actHelpAboutExecute
    end
    object actViewShowContext: TAction
      Category = 'View'
      Caption = 'Show Match Context'
      Checked = True
      Hint = 'Show Match Context'
      ShortCut = 16468
      OnExecute = actShowMatchContextExecute
    end
    object actFileSave: TAction
      Category = 'File'
      Caption = '&Save...'
      Hint = 'Save loadable results to file...'
      ImageIndex = 31
      ShortCut = 49235
      OnExecute = actFileSaveExecute
    end
    object actFilePrintToFile: TAction
      Category = 'File'
      Caption = 'Print to file..'
      Hint = 'Print results to file...'
      ImageIndex = 31
      OnExecute = actFileSaveExecute
    end
    object actFileSavePrint: TAction
      Category = 'File'
      Caption = 'Save && Print...'
      Hint = 'Print results + Save loadable results to file...'
      ImageIndex = 31
      OnExecute = actFileSaveExecute
    end
    object actFileCopy: TAction
      Category = 'File'
      Caption = '&Copy'
      Hint = 'Copy results to clipboard'
      ImageIndex = 6
      ShortCut = 16451
      OnExecute = actFileCopyExecute
    end
    object actViewToolBar: TAction
      Category = 'View'
      Caption = 'Toolbar'
      Checked = True
      OnExecute = actViewToolBarExecute
    end
    object actViewOptions: TAction
      Category = 'View'
      Caption = 'Options...'
      ImageIndex = 17
      ShortCut = 16460
      OnExecute = actViewOptionsExecute
    end
    object actReplaceAll: TAction
      Category = 'Replace'
      Caption = 'Replace All Items...'
      Hint = 'Replace all items...'
      ImageIndex = 61
      ShortCut = 24641
      OnExecute = actReplaceAllExecute
    end
    object actListGotoSelectedAndClose: TAction
      Category = 'List'
      Caption = 'Goto Selected and Close'
      Hint = 'Goto match'
      ImageIndex = 27
      ShortCut = 16397
      OnExecute = actListGotoSelectedAndCloseExecute
    end
    object actFoundView: TAction
      Category = 'Found'
      Caption = 'View'
      OnExecute = actFoundViewExecute
    end
    object actFoundDelete: TAction
      Category = 'Found'
      Caption = 'Delete'
      ImageIndex = 11
      OnExecute = actFoundDeleteExecute
    end
    object actFoundRefresh: TAction
      Category = 'Found'
      Caption = 'Refresh'
      ImageIndex = 34
      OnExecute = actFoundRefreshExecute
    end
    object actFoundSearch: TAction
      Category = 'Found'
      Caption = 'Search...'
      OnExecute = actFoundRefreshExecute
    end
    object actFoundModifySearchOptions: TAction
      Category = 'Found'
      Caption = 'Modify search options...'
      OnExecute = actFoundRefreshExecute
    end
    object actFoundSave: TAction
      Category = 'Found'
      Caption = 'Save...'
      ImageIndex = 31
      OnExecute = actFoundSaveExecute
    end
    object actViewShowFoundList: TAction
      Category = 'View'
      Caption = 'Show found list'
      Checked = True
      Hint = 'Show found list'
      OnExecute = actViewShowFoundListExecute
    end
    object actViewShowFullFilename: TAction
      Category = 'View'
      Caption = 'Show full filename'
      Hint = 'Show full or relative filename'
      OnExecute = actViewShowFullFilenameExecute
    end
    object actFoundPrintToFile: TAction
      Category = 'Found'
      Caption = 'Print to file...'
      ImageIndex = 3
      OnExecute = actFoundSaveExecute
    end
    object actFoundSavePrint: TAction
      Category = 'Found'
      Caption = 'Save && Print...'
      OnExecute = actFoundSaveExecute
    end
    object actFoundDeleteAll: TAction
      Category = 'Found'
      Caption = 'Delete all'
      ImageIndex = 11
      OnExecute = actFoundDeleteAllExecute
    end
    object actFoundSaveAll: TAction
      Category = 'Found'
      Caption = 'Save all...'
      ImageIndex = 31
      OnExecute = actFoundSaveAllExecute
    end
    object actContextSelSearch: TAction
      Category = 'Context'
      Caption = 'Search selected text...'
      OnExecute = actContextSelSearchExecute
    end
    object actFoundPrintAllToFile: TAction
      Category = 'Found'
      Caption = 'Print all to file...'
      ImageIndex = 3
      OnExecute = actFoundSaveAllExecute
    end
    object actFoundSavePrintAll: TAction
      Category = 'Found'
      Caption = 'Save && Print all to file...'
      OnExecute = actFoundSaveAllExecute
    end
    object actFileOpen: TAction
      Category = 'File'
      Caption = 'Open...'
      Hint = 'Open a saved result'
      ImageIndex = 1
      OnExecute = actFileOpenExecute
    end
    object actFoundRefreshAll: TAction
      Category = 'Found'
      Caption = 'Refresh all'
      ImageIndex = 34
      OnExecute = actFoundRefreshAllExecute
    end
  end
  object pmFoundMenu: TPopupMenu
    Left = 24
    Top = 160
    object miFoundItemName: TMenuItem
      Caption = '[Itemname]'
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object miFoundView: TMenuItem
      Action = actFoundView
    end
    object miFoundRefresh: TMenuItem
      Action = actFoundRefresh
    end
    object miFoundSearch: TMenuItem
      Action = actFoundSearch
    end
    object miFoundModifySearchOptions: TMenuItem
      Action = actFoundModifySearchOptions
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object miFoundDelete: TMenuItem
      Action = actFoundDelete
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object miFoundSave: TMenuItem
      Action = actFoundSave
    end
    object miFoundPrintToFile: TMenuItem
      Action = actFoundPrintToFile
    end
    object miFoundSavePrint: TMenuItem
      Action = actFoundSavePrint
    end
    object N5: TMenuItem
      Caption = '-'
    end
    object miFoundDeleteAll: TMenuItem
      Action = actFoundDeleteAll
    end
    object N8: TMenuItem
      Caption = '-'
    end
    object miFoundSaveAll: TMenuItem
      Action = actFoundSaveAll
    end
    object miFoundPrintAllToTile: TMenuItem
      Action = actFoundPrintAllToFile
    end
    object miFoundSavePrintAllToTile: TMenuItem
      Action = actFoundSavePrintAll
    end
  end
  object pmContextMenu: TPopupMenu
    Left = 152
    Top = 552
    object miContextSearchSelectedText: TMenuItem
      Action = actContextSelSearch
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'txt'
    Filter = 'Text Files (*.txt, *.log)|*.txt;*.log|All Files (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 120
    Top = 224
  end
end
