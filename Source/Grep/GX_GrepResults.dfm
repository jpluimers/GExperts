inherited fmGrepResults: TfmGrepResults
  Left = 324
  Top = 243
  ActiveControl = lbResults
  Caption = 'Grep Results'
  ClientHeight = 689
  ClientWidth = 623
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
    Width = 623
    Height = 670
    Align = alClient
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 0
    object SplitterContext: TSplitter
      Left = 0
      Top = 574
      Width = 623
      Height = 8
      Cursor = crVSplit
      Align = alBottom
      AutoSnap = False
      Beveled = True
      MinSize = 20
    end
    object SplitterHistoryList: TSplitter
      Left = 139
      Top = 22
      Width = 8
      Height = 552
      Cursor = crHSplit
      AutoSnap = False
      Beveled = True
      MinSize = 20
      Visible = False
    end
    object lbResults: TListBox
      Left = 147
      Top = 22
      Width = 476
      Height = 552
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
      Width = 623
      Height = 22
      AutoSize = True
      DisabledImages = dmSharedImages.DisabledImages
      Flat = True
      Images = dmSharedImages.Images
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      Wrapable = False
      object tbnSearch: TToolButton
        Left = 0
        Top = 0
        Action = actFileSearch
      end
      object tbnRefresh: TToolButton
        Left = 23
        Top = 0
        Action = actFileRefresh
      end
      object tbnSep1: TToolButton
        Left = 46
        Top = 0
        Width = 8
        ImageIndex = 7
        Style = tbsSeparator
      end
      object tbnAbort: TToolButton
        Left = 54
        Top = 0
        Action = actFileAbort
      end
      object tbnSep2: TToolButton
        Left = 77
        Top = 0
        Width = 8
        ImageIndex = 3
        Style = tbsSeparator
      end
      object tbnGoto: TToolButton
        Left = 85
        Top = 0
        Action = actListGotoSelected
      end
      object tbnSep3: TToolButton
        Left = 108
        Top = 0
        Width = 8
        ImageIndex = 4
        Style = tbsSeparator
      end
      object tbnPrint: TToolButton
        Left = 116
        Top = 0
        Action = actFilePrint
      end
      object tbnSep4: TToolButton
        Left = 139
        Top = 0
        Width = 8
        ImageIndex = 5
        Style = tbsSeparator
      end
      object tbnContract: TToolButton
        Left = 147
        Top = 0
        Action = actListContract
      end
      object tbnExpand: TToolButton
        Left = 170
        Top = 0
        Action = actListExpand
      end
      object tbnSep5: TToolButton
        Left = 193
        Top = 0
        Width = 8
        ImageIndex = 8
        Style = tbsSeparator
      end
      object tbnReplaceSelected: TToolButton
        Left = 201
        Top = 0
        Action = actReplaceSelected
      end
      object tbnReplaceAll: TToolButton
        Left = 224
        Top = 0
        Action = actReplaceAll
      end
      object tbnSep6: TToolButton
        Left = 247
        Top = 0
        Width = 8
        ImageIndex = 9
        Style = tbsSeparator
      end
      object tbnStayOnTop: TToolButton
        Left = 255
        Top = 0
        Action = actViewStayOnTop
      end
      object tbnSep7: TToolButton
        Left = 278
        Top = 0
        Width = 8
        ImageIndex = 2
        Style = tbsSeparator
      end
      object tbnShowFullFilename: TToolButton
        Left = 286
        Top = 0
        Action = actViewShowFullFilename
        Style = tbsCheck
      end
      object tbnSep8: TToolButton
        Left = 309
        Top = 0
        Width = 8
        Caption = ''
        ImageIndex = 1
        Style = tbsSeparator
      end
      object tbnHelp: TToolButton
        Left = 317
        Top = 0
        Action = actHelpHelp
      end
    end
    object reContext: TRichEdit
      Left = 0
      Top = 582
      Width = 623
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
    object lbHistoryList: TListBox
      Left = 0
      Top = 22
      Width = 139
      Height = 552
      Style = lbVirtualOwnerDraw
      Align = alLeft
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ItemHeight = 25
      ParentFont = False
      PopupMenu = pmHistoryMenu
      TabOrder = 3
      Visible = False
      OnContextPopup = lbHistoryListContextPopup
      OnData = lbHistoryListData
      OnDblClick = lbHistoryListDblClick
      OnDrawItem = lbHistoryListDrawItem
      OnMouseUp = lbHistoryListMouseUp
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 670
    Width = 623
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
    Left = 16
    Top = 40
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
      object mitFileSep2: TMenuItem
        Caption = '-'
      end
      object mitFileOpen: TMenuItem
        Action = actFileOpen
      end
      object mitFileSep3: TMenuItem
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
      object mitFileSep4: TMenuItem
        Caption = '-'
      end
      object miFileRefreshAll: TMenuItem
        Action = actHistoryRefreshAll
      end
      object mitFileSep5: TMenuItem
        Caption = '-'
      end
      object miFileSaveAll: TMenuItem
        Action = actHistorySaveAll
      end
      object miFilePrintAllToFile: TMenuItem
        Action = actHistoryPrintAllToFile
      end
      object miFileSavePrintAllToFile: TMenuItem
        Action = actHistorySavePrintAll
      end
      object mitFileSep6: TMenuItem
        Caption = '-'
      end
      object mitFileDeleteAll: TMenuItem
        Action = actHistoryDeleteAll
      end
      object mitFileSep7: TMenuItem
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
      object miViewShowHistoryList: TMenuItem
        Action = actViewShowHistoryList
      end
      object mitViewSep2: TMenuItem
        Caption = '-'
      end
      object miViewShowFullFilename: TMenuItem
        Action = actViewShowFullFilename
      end
      object mitViewSep3: TMenuItem
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
      Caption = 'Print to file...'
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
    object actHistoryView: TAction
      Category = 'History'
      Caption = 'View'
      OnExecute = actHistoryViewExecute
    end
    object actHistoryDelete: TAction
      Category = 'History'
      Caption = 'Delete'
      ImageIndex = 11
      OnExecute = actHistoryDeleteExecute
    end
    object actHistoryRefresh: TAction
      Category = 'History'
      Caption = 'Refresh'
      ImageIndex = 34
      OnExecute = actHistoryRefreshExecute
    end
    object actHistorySearch: TAction
      Category = 'History'
      Caption = 'Search...'
      OnExecute = actHistoryRefreshExecute
    end
    object actHistoryModifySearchOptions: TAction
      Category = 'History'
      Caption = 'Modify search options...'
      OnExecute = actHistoryRefreshExecute
    end
    object actHistorySave: TAction
      Category = 'History'
      Caption = 'Save...'
      ImageIndex = 31
      OnExecute = actHistorySaveExecute
    end
    object actViewShowHistoryList: TAction
      Category = 'View'
      Caption = 'Show history list'
      Hint = 'Show history list'
      OnExecute = actViewShowHistoryListExecute
    end
    object actViewShowFullFilename: TAction
      Category = 'View'
      Caption = 'Show full filename'
      Hint = 'Show full or relative filename'
      OnExecute = actViewShowFullFilenameExecute
    end
    object actHistoryPrintToFile: TAction
      Category = 'History'
      Caption = 'Print to file...'
      ImageIndex = 3
      OnExecute = actHistorySaveExecute
    end
    object actHistorySavePrint: TAction
      Category = 'History'
      Caption = 'Save && Print...'
      OnExecute = actHistorySaveExecute
    end
    object actHistoryDeleteAll: TAction
      Category = 'History'
      Caption = 'Delete all'
      ImageIndex = 11
      OnExecute = actHistoryDeleteAllExecute
    end
    object actHistorySaveAll: TAction
      Category = 'History'
      Caption = 'Save all...'
      ImageIndex = 31
      OnExecute = actHistorySaveAllExecute
    end
    object actContextSelSearch: TAction
      Category = 'Context'
      Caption = 'Search selected text...'
      OnExecute = actContextSelSearchExecute
    end
    object actHistoryPrintAllToFile: TAction
      Category = 'History'
      Caption = 'Print all to file...'
      ImageIndex = 3
      OnExecute = actHistorySaveAllExecute
    end
    object actHistorySavePrintAll: TAction
      Category = 'History'
      Caption = 'Save && Print all to file...'
      OnExecute = actHistorySaveAllExecute
    end
    object actFileOpen: TAction
      Category = 'File'
      Caption = 'Open...'
      Hint = 'Open a saved result'
      ImageIndex = 1
      OnExecute = actFileOpenExecute
    end
    object actHistoryRefreshAll: TAction
      Category = 'History'
      Caption = 'Refresh all'
      ImageIndex = 34
      OnExecute = actHistoryRefreshAllExecute
    end
  end
  object pmHistoryMenu: TPopupMenu
    Left = 16
    Top = 160
    object miHistoryItemName: TMenuItem
      Caption = '[Itemname]'
    end
    object mitHistorySep1: TMenuItem
      Caption = '-'
    end
    object miHistoryView: TMenuItem
      Action = actHistoryView
    end
    object miHistoryRefresh: TMenuItem
      Action = actHistoryRefresh
    end
    object miHistorySearch: TMenuItem
      Action = actHistorySearch
    end
    object miHistoryModifySearchOptions: TMenuItem
      Action = actHistoryModifySearchOptions
    end
    object mitHistorySep2: TMenuItem
      Caption = '-'
    end
    object miHistoryDelete: TMenuItem
      Action = actHistoryDelete
    end
    object mitHistorySep3: TMenuItem
      Caption = '-'
    end
    object miHistorySave: TMenuItem
      Action = actHistorySave
    end
    object miHistoryPrintToFile: TMenuItem
      Action = actHistoryPrintToFile
    end
    object miHistorySavePrint: TMenuItem
      Action = actHistorySavePrint
    end
  end
  object pmContextMenu: TPopupMenu
    Left = 16
    Top = 592
    object miContextSearchSelectedText: TMenuItem
      Action = actContextSelSearch
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'txt'
    Filter = 'Text Files (*.txt, *.log)|*.txt;*.log|All Files (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 360
    Top = 32
  end
end
