object fmCodeLib: TfmCodeLib
  Left = 344
  Top = 191
  AutoScroll = False
  Caption = 'Code Librarian'
  ClientHeight = 369
  ClientWidth = 536
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Menu = MainMenu
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnHide = FormHide
  OnKeyPress = FormKeyPress
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object Splitter: TSplitter
    Left = 169
    Top = 24
    Width = 3
    Height = 325
    Cursor = crHSplit
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 349
    Width = 536
    Height = 20
    Panels = <
      item
        Width = 380
      end
      item
        Width = 70
      end
      item
        Width = 70
      end>
    ParentFont = True
    SimplePanel = False
    UseSystemFont = False
    OnResize = StatusBarResize
  end
  object pnlView: TPanel
    Left = 172
    Top = 24
    Width = 364
    Height = 325
    Align = alClient
    BevelOuter = bvLowered
    Caption = 'This editor is created at runtime'
    FullRepaint = False
    TabOrder = 1
  end
  object tvTopics: TTreeView
    Left = 0
    Top = 24
    Width = 169
    Height = 325
    Align = alLeft
    DragMode = dmAutomatic
    HideSelection = False
    Images = dmSharedImages.Images
    Indent = 19
    PopupMenu = pmTopics
    RightClickSelect = True
    SortType = stText
    TabOrder = 2
    ToolTips = False
    OnChange = tvTopicsChange
    OnChanging = tvTopicsChanging
    OnCompare = tvTopicsCompare
    OnDblClick = tvTopicsDblClick
    OnDragDrop = tvTopicsDragDrop
    OnDragOver = tvTopicsDragOver
    OnEdited = tvTopicsEdited
    OnEndDrag = tvTopicsEndDrag
    OnKeyUp = tvTopicsKeyUp
    OnMouseDown = tvTopicsMouseDown
    OnStartDrag = tvTopicsStartDrag
  end
  object ToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 536
    Height = 24
    AutoSize = True
    DisabledImages = dmSharedImages.DisabledImages
    EdgeBorders = [ebBottom]
    Flat = True
    Images = dmSharedImages.Images
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    Wrapable = False
    object tbnNewFolder: TToolButton
      Left = 0
      Top = 0
      Action = actNewFolder
    end
    object tbnNewSnippet: TToolButton
      Left = 23
      Top = 0
      Action = actNewSnippet
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
      ImageIndex = 4
      Style = tbsSeparator
    end
    object tbnCut: TToolButton
      Left = 77
      Top = 0
      Action = actEditCut
    end
    object tbnCopy: TToolButton
      Left = 100
      Top = 0
      Action = actEditCopy
    end
    object tbnPaste: TToolButton
      Left = 123
      Top = 0
      Action = actEditPaste
    end
    object tbnSep2: TToolButton
      Left = 146
      Top = 0
      Width = 8
      ImageIndex = 6
      Style = tbsSeparator
    end
    object tbnCopyIde: TToolButton
      Left = 154
      Top = 0
      Action = actEditCopyFromIde
    end
    object tbnPasteIde: TToolButton
      Left = 177
      Top = 0
      Action = actEditPasteToIde
    end
    object tbnSep3: TToolButton
      Left = 200
      Top = 0
      Width = 8
      ImageIndex = 9
      Style = tbsSeparator
    end
    object tbnExpandAll: TToolButton
      Left = 208
      Top = 0
      Action = actExpandAll
    end
    object tbnContractAll: TToolButton
      Left = 231
      Top = 0
      Action = actContractAll
    end
    object tbnSep4: TToolButton
      Left = 254
      Top = 0
      Width = 8
      ImageIndex = 11
      Style = tbsSeparator
    end
    object tbnFind: TToolButton
      Left = 262
      Top = 0
      Action = actEditFind
    end
    object tbnFindNext: TToolButton
      Left = 285
      Top = 0
      Action = actEditFindNext
    end
  end
  object MainMenu: TMainMenu
    Images = dmSharedImages.Images
    Left = 80
    Top = 40
    object mitFile: TMenuItem
      Caption = '&File'
      object mitFileNew: TMenuItem
        Caption = '&New'
        object mitFileNewRootFolder: TMenuItem
          Action = actNewRootFolder
        end
        object mitFileNewFolder: TMenuItem
          Action = actNewFolder
        end
        object mitFileNewSnippet: TMenuItem
          Action = actNewSnippet
        end
      end
      object mitFileDelete: TMenuItem
        Action = actDelete
      end
      object mitFileSep1: TMenuItem
        Caption = '-'
      end
      object CompactStorage1: TMenuItem
        Action = actCompactStorage
      end
      object mitFileSep2: TMenuItem
        Caption = '-'
      end
      object mitFilePrinterSetup: TMenuItem
        Action = actPrinterSetup
      end
      object mitFilePrint: TMenuItem
        Action = actPrint
      end
      object mitFileSep3: TMenuItem
        Caption = '-'
      end
      object mitFileExit: TMenuItem
        Action = actExit
      end
    end
    object mitEdit: TMenuItem
      Caption = '&Edit'
      object mitEditCut: TMenuItem
        Action = actEditCut
      end
      object mitEditCopy: TMenuItem
        Action = actEditCopy
      end
      object mitEditPaste: TMenuItem
        Action = actEditPaste
      end
      object mitEditSep1: TMenuItem
        Caption = '-'
      end
      object mitEditCopyFromIde: TMenuItem
        Action = actEditCopyFromIde
      end
      object mitEditPasteFromIde: TMenuItem
        Action = actEditPasteToIde
      end
      object mitEditSep2: TMenuItem
        Caption = '-'
      end
      object mitEditFind: TMenuItem
        Action = actEditFind
      end
      object mitEditFindNext: TMenuItem
        Action = actEditFindNext
      end
      object mitEditSep3: TMenuItem
        Caption = '-'
      end
      object mitEditExpandAll: TMenuItem
        Action = actExpandAll
      end
      object mitEditContractAll: TMenuItem
        Action = actContractAll
      end
    end
    object mitOptions: TMenuItem
      Caption = '&Options'
      object mitOptionsOptions: TMenuItem
        Action = actOptions
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
  object dlgPrinterSetup: TPrinterSetupDialog
    Left = 80
    Top = 88
  end
  object pmTopics: TPopupMenu
    Images = dmSharedImages.Images
    Left = 16
    Top = 40
    object mitTreeNew: TMenuItem
      Caption = '&New'
      ImageIndex = 10
      object mitTreeNewSnippet: TMenuItem
        Action = actNewSnippet
      end
      object mitTreeNewFolder: TMenuItem
        Action = actNewFolder
      end
      object mitTreeNewRootFolder: TMenuItem
        Action = actNewRootFolder
      end
    end
    object mitTreeRename: TMenuItem
      Action = actEditRename
    end
    object mitTreeDelete: TMenuItem
      Action = actDelete
    end
    object mitTreeMakeRoot: TMenuItem
      Action = actMakeRoot
    end
  end
  object pmCode: TPopupMenu
    Images = dmSharedImages.Images
    Left = 264
    Top = 40
    object mitEditorCut: TMenuItem
      Action = actEditCut
    end
    object mitEditorCopy: TMenuItem
      Action = actEditCopy
    end
    object mitEditorPaste: TMenuItem
      Action = actEditPaste
    end
    object mitEditorSep1: TMenuItem
      Caption = '-'
    end
    object mitEditorCopyFromDelphi: TMenuItem
      Action = actEditCopyFromIde
    end
    object mitEditorPasteIntoDelphi: TMenuItem
      Action = actEditPasteToIde
    end
    object mitEditorSep2: TMenuItem
      Caption = '-'
    end
    object mitEditorHighlighting: TMenuItem
      Caption = 'Syntax &Highlighting'
      ImageIndex = 18
    end
  end
  object Actions: TActionList
    Images = dmSharedImages.Images
    OnUpdate = ActionsUpdate
    Left = 184
    Top = 40
    object actDelete: TAction
      Category = 'File'
      Caption = 'Delete'
      Hint = 'Delete'
      ImageIndex = 11
      ShortCut = 16430
      OnExecute = DeleteExecute
    end
    object actNewRootFolder: TAction
      Category = 'File'
      Caption = 'New Root Folder'
      Hint = 'New Root Folder'
      ShortCut = 16466
      OnExecute = NewRootFolderExecute
    end
    object actNewFolder: TAction
      Category = 'File'
      Caption = 'New Folder'
      Hint = 'New Folder'
      ImageIndex = 9
      ShortCut = 16460
      OnExecute = NewFolderExecute
    end
    object actNewSnippet: TAction
      Category = 'File'
      Caption = 'New Snippet'
      Hint = 'New Snippet'
      ImageIndex = 10
      ShortCut = 16467
      OnExecute = NewSnippetExecute
    end
    object actMakeRoot: TAction
      Category = 'File'
      Caption = 'Move Folder to Root'
      Hint = 'Move Folder to Root'
      ShortCut = 24658
      OnExecute = MakeRootExecute
    end
    object actPrinterSetup: TAction
      Category = 'File'
      Caption = 'Printer Setup...'
      Hint = 'Printer Setup...'
      ImageIndex = 4
      OnExecute = PrinterSetupExecute
    end
    object actPrint: TAction
      Category = 'File'
      Caption = 'Print Snippet'
      Hint = 'Print Snippet'
      ImageIndex = 3
      ShortCut = 16464
      OnExecute = PrintExecute
    end
    object actExit: TAction
      Category = 'File'
      Caption = 'Exit'
      Hint = 'Exit'
      ImageIndex = 8
      ShortCut = 32883
      OnExecute = ExitExecute
    end
    object actEditCut: TEditCut
      Category = 'Edit'
      Caption = 'Cu&t'
      Hint = 'Cut'
      ImageIndex = 5
      ShortCut = 16472
      OnExecute = CutExecute
    end
    object actEditCopy: TEditCopy
      Category = 'Edit'
      Caption = '&Copy'
      Hint = 'Copy'
      ImageIndex = 6
      ShortCut = 16451
      OnExecute = CopyExecute
    end
    object actEditPaste: TEditPaste
      Category = 'Edit'
      Caption = '&Paste'
      Hint = 'Paste'
      ImageIndex = 7
      ShortCut = 16470
      OnExecute = PasteExecute
    end
    object actEditCopyFromIde: TAction
      Category = 'Edit'
      Caption = 'Copy from IDE Editor'
      Hint = 'Copy from IDE Editor'
      ImageIndex = 19
      ShortCut = 24643
      OnExecute = CopyFromIdeExecute
    end
    object actEditPasteToIde: TAction
      Category = 'Edit'
      Caption = 'Paste to IDE Editor'
      Hint = 'Paste to IDE Editor'
      ImageIndex = 20
      ShortCut = 24662
      OnExecute = PasteToIdeExecute
    end
    object actEditFind: TAction
      Category = 'Edit'
      Caption = 'Find...'
      Hint = 'Find...'
      ImageIndex = 14
      ShortCut = 16454
      OnExecute = FindExecute
    end
    object actEditFindNext: TAction
      Category = 'Edit'
      Caption = 'Find Next'
      Hint = 'Find Next'
      ImageIndex = 15
      ShortCut = 16498
      OnExecute = FindNextExecute
    end
    object actExpandAll: TAction
      Category = 'Edit'
      Caption = 'Expand All'
      Hint = 'Expand All'
      ImageIndex = 12
      ShortCut = 16453
      OnExecute = ExpandAllExecute
    end
    object actContractAll: TAction
      Category = 'Edit'
      Caption = 'Contract All'
      Hint = 'Contract All'
      ImageIndex = 13
      ShortCut = 16468
      OnExecute = ContractAllExecute
    end
    object actOptions: TAction
      Category = 'Options'
      Caption = 'Options'
      Hint = 'Options'
      ImageIndex = 17
      ShortCut = 16463
      OnExecute = OptionsExecute
    end
    object actHelpAbout: TAction
      Category = 'Help'
      Caption = 'About GExperts...'
      Hint = 'About GExperts...'
      ImageIndex = 16
      OnExecute = HelpAboutExecute
    end
    object actHelpContents: TAction
      Category = 'Help'
      Caption = 'Contents'
      Hint = 'Contents'
      OnExecute = HelpContentsExecute
    end
    object actHelpHelp: TAction
      Category = 'Help'
      Caption = 'Help'
      Hint = 'Help'
      ImageIndex = 0
      ShortCut = 112
      OnExecute = HelpExecute
    end
    object actEditRename: TAction
      Category = 'Edit'
      Caption = 'Rename'
      Hint = 'Rename folder/snippet'
      ShortCut = 113
      OnExecute = actEditRenameExecute
    end
    object actCompactStorage: TAction
      Category = 'File'
      Caption = 'Compact Storage'
      OnExecute = actCompactStorageExecute
    end
  end
end
