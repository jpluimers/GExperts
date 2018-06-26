inherited fmGrepResults: TfmGrepResults
  Left = 324
  Top = 243
  ActiveControl = lbResults
  Caption = 'Grep Results'
  ClientHeight = 337
  ClientWidth = 396
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
    Width = 396
    Height = 318
    Align = alClient
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 0
    object SplitterContext: TSplitter
      Left = 0
      Top = 222
      Width = 396
      Height = 8
      Cursor = crVSplit
      Align = alBottom
      AutoSnap = False
      Beveled = True
      MinSize = 20
    end
    object SplitterHistoryList: TSplitter
      Left = 147
      Top = 22
      Width = 8
      Height = 200
      Cursor = crHSplit
      AutoSnap = False
      Beveled = True
      MinSize = 20
      OnMoved = SplitterHistoryListMoved
    end
    object lbResults: TListBox
      Left = 155
      Top = 22
      Width = 241
      Height = 200
      Style = lbOwnerDrawFixed
      Align = alClient
      ItemHeight = 17
      TabOrder = 2
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
      Width = 396
      Height = 22
      AutoSize = True
      DisabledImages = dmSharedImages.DisabledImages
      Flat = True
      Images = dmSharedImages.Images
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
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
      object tbnSearchInHistory: TToolButton
        Left = 85
        Top = 0
        Action = actHistorySearchInHistory
      end
      object tbnSep9: TToolButton
        Left = 108
        Top = 0
        Width = 8
        ImageIndex = 1
        Style = tbsSeparator
      end
      object tbnGoto: TToolButton
        Left = 116
        Top = 0
        Action = actListGotoSelected
      end
      object tbnSep3: TToolButton
        Left = 139
        Top = 0
        Width = 8
        ImageIndex = 4
        Style = tbsSeparator
      end
      object tbnPrint: TToolButton
        Left = 147
        Top = 0
        Action = actFilePrint
      end
      object tbnSep4: TToolButton
        Left = 170
        Top = 0
        Width = 8
        ImageIndex = 5
        Style = tbsSeparator
      end
      object tbnContract: TToolButton
        Left = 178
        Top = 0
        Action = actListContract
      end
      object tbnExpand: TToolButton
        Left = 201
        Top = 0
        Action = actListExpand
      end
      object tbnSep5: TToolButton
        Left = 224
        Top = 0
        Width = 8
        ImageIndex = 8
        Style = tbsSeparator
      end
      object tbnShowFullFilename: TToolButton
        Left = 232
        Top = 0
        Action = actViewShowFullFilename
      end
      object tbnShowLineIndent: TToolButton
        Left = 255
        Top = 0
        Action = actViewShowIndent
      end
      object tbnSep8: TToolButton
        Left = 278
        Top = 0
        Width = 8
        ImageIndex = 1
        Style = tbsSeparator
      end
      object tbnReplaceSelected: TToolButton
        Left = 286
        Top = 0
        Action = actReplaceSelected
      end
      object tbnReplaceAll: TToolButton
        Left = 309
        Top = 0
        Action = actReplaceAll
      end
      object tbnSep6: TToolButton
        Left = 332
        Top = 0
        Width = 8
        ImageIndex = 9
        Style = tbsSeparator
      end
      object tbnStayOnTop: TToolButton
        Left = 340
        Top = 0
        Action = actViewStayOnTop
      end
      object tbnSep7: TToolButton
        Left = 363
        Top = 0
        Width = 8
        ImageIndex = 2
        Style = tbsSeparator
      end
      object tbnHelp: TToolButton
        Left = 371
        Top = 0
        Action = actHelpHelp
      end
    end
    object reContext: TRichEdit
      Left = 0
      Top = 230
      Width = 396
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
      TabOrder = 3
      WordWrap = False
      OnContextPopup = reContextContextPopup
    end
    object tcHistoryListPage: TTabControl
      Left = 0
      Top = 22
      Width = 147
      Height = 200
      Align = alLeft
      TabOrder = 1
      Tabs.Strings = (
        'Results'
        'Params'
        'All'
        'Search')
      TabIndex = 0
      OnChange = tcHistoryListPageChange
      object lbHistoryList: TListBox
        Left = 4
        Top = 25
        Width = 139
        Height = 171
        Style = lbVirtualOwnerDraw
        Align = alClient
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ItemHeight = 26
        ParentFont = False
        PopupMenu = pmHistoryMenu
        TabOrder = 0
        OnContextPopup = lbHistoryListContextPopup
        OnData = lbHistoryListData
        OnDblClick = lbHistoryListDblClick
        OnDrawItem = lbHistoryListDrawItem
        OnKeyDown = lbHistoryListKeyDown
        OnMouseDown = lbHistoryListMouseDown
        OnMouseUp = lbHistoryListMouseUp
      end
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 318
    Width = 396
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
    Left = 248
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
      object miFileRefreshSelected: TMenuItem
        Action = actHistoryRefreshSelected
      end
      object mitFileSep5: TMenuItem
        Caption = '-'
      end
      object mitFileDeleteSelected: TMenuItem
        Action = actHistoryDeleteSelected
      end
      object mitFileSep6: TMenuItem
        Caption = '-'
      end
      object mitFileModifySaveOptions: TMenuItem
        Action = actHistoryModifySaveOptions
      end
      object mitFileSearhInHistory: TMenuItem
        Action = actHistorySearchInHistory
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
      object mitListSelectNext: TMenuItem
        Action = actListSelectNext
      end
      object mitListSelectPrevious: TMenuItem
        Action = actListSelectPrevious
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
      object miViewShowIndent: TMenuItem
        Action = actViewShowIndent
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
      Tag = 1
      Category = 'File'
      Caption = '&Refresh'
      Hint = 'Refresh search'
      ImageIndex = 34
      ShortCut = 116
      OnExecute = actFileRefreshExecute
      OnUpdate = actHistoryUpdate
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
      OnUpdate = actHistoryUpdate
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
      Tag = 1
      Category = 'File'
      Caption = '&Save...'
      Hint = 'Save loadable results to file...'
      ImageIndex = 31
      ShortCut = 49235
      OnExecute = actFileSaveExecute
      OnUpdate = actHistoryUpdate
    end
    object actFilePrintToFile: TAction
      Category = 'File'
      Caption = 'Print to File...'
      Hint = 'Print results to file...'
      ImageIndex = 31
      OnExecute = actFileSaveExecute
      OnUpdate = actHistoryUpdate
    end
    object actFileSavePrint: TAction
      Category = 'File'
      Caption = 'Save && Print...'
      Hint = 'Print results + Save loadable results to file...'
      ImageIndex = 31
      OnExecute = actFileSaveExecute
      OnUpdate = actHistoryUpdate
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
      OnUpdate = actHistoryUpdate
    end
    object actHistoryDelete: TAction
      Tag = 1
      Category = 'History'
      Caption = 'Delete'
      ImageIndex = 11
      ShortCut = 46
      OnExecute = actHistoryDeleteExecute
      OnUpdate = actHistoryUpdate
    end
    object actHistoryRefresh: TAction
      Tag = 1
      Category = 'History'
      Caption = 'Refresh'
      ImageIndex = 34
      OnExecute = actHistoryRefreshExecute
      OnUpdate = actHistoryUpdate
    end
    object actHistorySearch: TAction
      Tag = 1
      Category = 'History'
      Caption = 'Search...'
      OnExecute = actHistoryRefreshExecute
      OnUpdate = actHistoryUpdate
    end
    object actHistoryModifySearchSettings: TAction
      Category = 'History'
      Caption = 'Modify Search Parameters...'
      OnExecute = actHistoryRefreshExecute
      OnUpdate = actHistoryUpdate
    end
    object actViewShowHistoryList: TAction
      Category = 'View'
      Caption = 'Show History List'
      Hint = 'Show history list'
      OnExecute = actViewShowHistoryListExecute
    end
    object actViewShowFullFilename: TAction
      Category = 'View'
      Caption = 'Show Full Filename'
      Hint = 'Show full or relative filename'
      ImageIndex = 79
      OnExecute = actViewShowFullFilenameExecute
    end
    object actHistoryDeleteSelected: TAction
      Tag = 1
      Category = 'History'
      Caption = 'Delete Items'
      ImageIndex = 11
      OnExecute = actHistoryDeleteSelectedExecute
      OnUpdate = actHistoryUpdate
    end
    object actContextSelSearch: TAction
      Category = 'Context'
      Caption = 'Search Selected Text...'
      ImageIndex = 34
      OnExecute = actContextSelSearchExecute
    end
    object actFileOpen: TAction
      Category = 'File'
      Caption = 'Open...'
      Hint = 'Open a saved result'
      ImageIndex = 1
      OnExecute = actFileOpenExecute
    end
    object actHistoryRefreshSelected: TAction
      Tag = 1
      Category = 'History'
      Caption = 'Refresh Items'
      ImageIndex = 34
      Visible = False
      OnExecute = actHistoryRefreshSelectedExecute
      OnUpdate = actHistoryUpdate
    end
    object actHistoryModifySaveOptions: TAction
      Category = 'History'
      Caption = 'Modify Save Options'
      OnExecute = actHistoryModifySaveOptionsExecute
      OnUpdate = actHistoryUpdate
    end
    object actHistorySort: TAction
      Tag = 1
      Category = 'History'
      Caption = 'Sort'
      OnExecute = actHistorySortExecute
      OnUpdate = actHistoryUpdate
    end
    object actViewShowIndent: TAction
      Category = 'View'
      Caption = 'Show Indent'
      Hint = 'Show line indent in results list'
      ImageIndex = 80
      OnExecute = actViewShowIndentExecute
    end
    object actHistorySearchInHistory: TAction
      Category = 'History'
      Caption = 'Search in History'
      Hint = 'Search in the history list'
      ImageIndex = 81
      OnExecute = actHistorySearchInHistoryExecute
    end
    object actListSelectNext: TAction
      Category = 'List'
      Caption = 'Select Next'
      ImageIndex = 87
      SecondaryShortCuts.Strings = (
        'SHIFT+ALT+F8')
      OnExecute = actListSelectNextExecute
    end
    object actListSelectPrevious: TAction
      Category = 'List'
      Caption = 'Select Previous'
      ImageIndex = 88
      SecondaryShortCuts.Strings = (
        'SHIFT+ALT+F7')
      OnExecute = actListSelectPreviousExecute
    end
  end
  object pmHistoryMenu: TPopupMenu
    Images = dmSharedImages.Images
    Left = 40
    Top = 160
    object miHistoryItemName: TMenuItem
      Caption = 'Search Text'
      OnClick = miHistoryItemNameClick
    end
    object miHistoryLastSearchTime: TMenuItem
      Caption = 'Search Time'
    end
    object miHistorySettings: TMenuItem
      Caption = 'Search Parameters'
      ImageIndex = 76
      object miSettingsSaveOption: TMenuItem
        Caption = 'Save History'
      end
      object miSettingsSep1: TMenuItem
        Caption = '-'
      end
      object miSettingsCurrentFile: TMenuItem
        Caption = 'Current File'
      end
      object miSettingsAllFilesInProjectGroup: TMenuItem
        Caption = 'All Files in Project Group'
      end
      object miSettingsAllFilesInProject: TMenuItem
        Caption = 'All Files in Project'
      end
      object miSettingsOpenProjectFiles: TMenuItem
        Caption = 'Open Project Files'
      end
      object miSettingsDirectories: TMenuItem
        Caption = 'Directories'
      end
      object miSettingsPreviousSearchResultFiles: TMenuItem
        Caption = 'Previous Search Result Files'
      end
      object miSettingsSepDir: TMenuItem
        Caption = '-'
      end
      object miSettingsDirectoriesData: TMenuItem
        Caption = 'Directories'
      end
      object miSettingsExcludeDirs: TMenuItem
        Caption = 'Exclude Dirs'
      end
      object miSettingsFileMasks: TMenuItem
        Caption = 'File Masks'
      end
      object miSettingsSearchSubDirectories: TMenuItem
        Caption = 'Search Subdirectories'
      end
      object miSettingsSep2: TMenuItem
        Caption = '-'
      end
      object miSettingsCaseSensitive: TMenuItem
        Caption = 'Case Sensitive'
      end
      object miSettingsWholeWord: TMenuItem
        Caption = 'Whole Word'
      end
      object miSettingsSearchFormFiles: TMenuItem
        Caption = 'Search Form Files'
      end
      object miSettingsSearchSQLFiles: TMenuItem
        Caption = 'Search SQL Files'
      end
      object miSettingsRegularExpression: TMenuItem
        Caption = 'Regular Expression'
      end
      object miSettingsSep3: TMenuItem
        Caption = '-'
      end
      object miSettingsGrepCode: TMenuItem
        Caption = 'Delphi Code'
      end
      object miSettingsGrepStrings: TMenuItem
        Caption = 'Delphi Strings'
      end
      object miSettingsGrepComments: TMenuItem
        Caption = 'Delphi Comments'
      end
      object miSettingsSep4: TMenuItem
        Caption = '-'
      end
      object miSettingsSectionInterface: TMenuItem
        Caption = 'Delphi Interface'
      end
      object miSettingsSectionImplementation: TMenuItem
        Caption = 'Delphi Implementation'
      end
      object miSettingsSectionInitialization: TMenuItem
        Caption = 'Delphi Initialization'
      end
      object miSettingsSectionFinalization: TMenuItem
        Caption = 'Delphi Finalization'
      end
    end
    object mitHistorySep1: TMenuItem
      Caption = '-'
    end
    object miHistoryView: TMenuItem
      Action = actHistoryView
    end
    object miHistoryRefresh: TMenuItem
      Action = actHistoryRefresh
      Default = True
    end
    object miHistorySearch: TMenuItem
      Action = actHistorySearch
    end
    object miHistoryModifySearchSettings: TMenuItem
      Action = actHistoryModifySearchSettings
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
    object miHistorySort: TMenuItem
      Action = actHistorySort
    end
  end
  object pmContextMenu: TPopupMenu
    Images = dmSharedImages.Images
    Left = 32
    Top = 248
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
