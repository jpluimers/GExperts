inherited fmPeInformation: TfmPeInformation
  Left = 375
  Top = 189
  Caption = 'PE Information'
  ClientHeight = 459
  ClientWidth = 489
  KeyPreview = True
  Menu = MainMenu
  OnActivate = FormActivate
  OnKeyDown = FormKeyDown
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 14
  object pcMain: TPageControl
    Left = 0
    Top = 22
    Width = 489
    Height = 437
    ActivePage = tshMSDOS
    Align = alClient
    HotTrack = True
    MultiLine = True
    TabIndex = 0
    TabOrder = 0
    OnChange = pcMainChange
    object tshMSDOS: TTabSheet
      Caption = 'MS-DOS Header'
      object lvMSDOS: TListView
        Left = 0
        Top = 0
        Width = 481
        Height = 408
        Align = alClient
        Columns = <
          item
            Caption = 'Field'
            Width = 200
          end
          item
            Caption = 'Value'
            Width = 210
          end>
        ColumnClick = False
        OwnerData = True
        ReadOnly = True
        RowSelect = True
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        ViewStyle = vsReport
        OnData = lvMSDOSData
      end
    end
    object tshPEHEader: TTabSheet
      Caption = 'PE Header'
      object lvPEHeader: TListView
        Left = 0
        Top = 0
        Width = 481
        Height = 408
        Align = alClient
        Columns = <
          item
            Caption = 'Field'
            Width = 200
          end
          item
            Caption = 'Value'
            Width = 210
          end>
        ColumnClick = False
        OwnerData = True
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        OnData = lvPEHeaderData
      end
    end
    object tshPEOptional: TTabSheet
      Caption = 'PE Optional Header'
      object lvPEOptionalHeader: TListView
        Left = 0
        Top = 0
        Width = 481
        Height = 408
        Align = alClient
        Columns = <
          item
            Caption = 'Field'
            Width = 200
          end
          item
            Caption = 'Value'
            Width = 210
          end>
        ColumnClick = False
        OwnerData = True
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        OnData = lvPEOptionalHeaderData
      end
    end
    object tshImport: TTabSheet
      Caption = 'Imports'
      object splImport: TSplitter
        Left = 135
        Top = 0
        Width = 3
        Height = 408
        Cursor = crHSplit
      end
      object lvImports: TListView
        Left = 0
        Top = 0
        Width = 135
        Height = 408
        Align = alLeft
        Columns = <
          item
            AutoSize = True
            Caption = 'Import File'
          end>
        ColumnClick = False
        HideSelection = False
        OwnerData = True
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        OnChange = lvImportsChange
        OnData = lvImportsData
      end
      object lvImportFunctions: TListView
        Left = 138
        Top = 0
        Width = 343
        Height = 408
        Align = alClient
        Columns = <
          item
            Caption = 'Name'
            Width = 200
          end
          item
            Caption = 'Ordinal'
            Width = 120
          end>
        ColumnClick = False
        OwnerData = True
        ReadOnly = True
        RowSelect = True
        TabOrder = 1
        ViewStyle = vsReport
        OnData = lvImportFunctionsData
      end
    end
    object tshExports: TTabSheet
      Caption = 'Exports'
      object lvExportFunctions: TListView
        Left = 0
        Top = 0
        Width = 481
        Height = 408
        Align = alClient
        Columns = <
          item
            Caption = 'Name'
            Width = 200
          end
          item
            Caption = 'Ordinal'
            Width = 120
          end
          item
            Caption = 'Entry Point'
            Width = 100
          end>
        ColumnClick = False
        OwnerData = True
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        OnData = lvExportFunctionsData
      end
    end
    object tshVersionInfo: TTabSheet
      Caption = 'Version Info'
      ImageIndex = 5
      object lvVersionInfo: TListView
        Left = 0
        Top = 0
        Width = 481
        Height = 408
        Align = alClient
        Columns = <
          item
            Caption = 'Name'
            Width = 150
          end
          item
            Caption = 'Value'
            Width = 350
          end>
        ColumnClick = False
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
    object tshPackageInfo: TTabSheet
      Caption = 'Package Info'
      ImageIndex = 6
      object splPackageInfo: TSplitter
        Left = 142
        Top = 0
        Height = 408
      end
      object lbPackageInfoType: TListBox
        Left = 0
        Top = 0
        Width = 142
        Height = 408
        Align = alLeft
        ItemHeight = 14
        TabOrder = 0
        OnClick = lbPackageInfoTypeClick
      end
      object lbPackageInfo: TListBox
        Left = 145
        Top = 0
        Width = 336
        Height = 389
        Align = alClient
        ItemHeight = 14
        TabOrder = 1
      end
    end
  end
  object ToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 489
    Height = 22
    AutoSize = True
    DisabledImages = dmSharedImages.DisabledImages
    EdgeBorders = []
    Flat = True
    Images = dmSharedImages.Images
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    Wrapable = False
    object tbnOpen: TToolButton
      Left = 0
      Top = 0
      Action = actFileOpen
    end
    object tbnPrint: TToolButton
      Left = 23
      Top = 0
      Action = actFilePrint
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
    object tbnSep2: TToolButton
      Left = 77
      Top = 0
      Width = 8
      ImageIndex = 1
      Style = tbsSeparator
    end
    object tbnHelp: TToolButton
      Left = 85
      Top = 0
      Action = actHelpHelp
    end
  end
  object MainMenu: TMainMenu
    Images = dmSharedImages.Images
    Left = 32
    Top = 76
    object mitFile: TMenuItem
      Caption = '&File'
      ShortCut = 16467
      object mitFileOpen: TMenuItem
        Action = actFileOpen
      end
      object mitFileSep1: TMenuItem
        Caption = '-'
      end
      object mitFilePrinterSetup: TMenuItem
        Action = actFilePrinterSetup
      end
      object mitFilePrint: TMenuItem
        Action = actFilePrint
      end
      object mitFileSep2: TMenuItem
        Caption = '-'
      end
      object mitFileExit: TMenuItem
        Action = actFileExit
      end
    end
    object mitEdit: TMenuItem
      Caption = '&Edit'
      object mitEditCopy: TMenuItem
        Action = actEditCopy
      end
    end
    object mitOptions: TMenuItem
      Caption = '&Options'
      object mitOptionsDecimal: TMenuItem
        Action = actOptionsDecimal
        GroupIndex = 1
        RadioItem = True
      end
      object mitOptionsHex: TMenuItem
        Tag = 1
        Action = actOptionsHex
        GroupIndex = 1
        RadioItem = True
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
    Left = 128
    Top = 132
  end
  object Actions: TActionList
    Images = dmSharedImages.Images
    OnUpdate = ActionsUpdate
    Left = 84
    Top = 192
    object actFileOpen: TAction
      Category = 'File'
      Caption = '&Open...'
      Hint = 'Open file'
      ImageIndex = 1
      ShortCut = 16463
      OnExecute = actFileOpenExecute
    end
    object actFilePrinterSetup: TAction
      Category = 'File'
      Caption = 'Printer &Setup...'
      Hint = 'Printer setup...'
      ImageIndex = 4
      OnExecute = actFilePrinterSetupExecute
    end
    object actFilePrint: TAction
      Category = 'File'
      Caption = '&Print'
      Hint = 'Print'
      ImageIndex = 3
      ShortCut = 16464
      OnExecute = actFilePrintExecute
    end
    object actFileExit: TAction
      Category = 'File'
      Caption = 'E&xit'
      Hint = 'Exit'
      ImageIndex = 8
      ShortCut = 32883
      OnExecute = actFileExitExecute
    end
    object actOptionsDecimal: TAction
      Category = 'Options'
      Caption = '&Decimal Numbers'
      Hint = 'Decimal numbers'
      ShortCut = 16452
      OnExecute = actOptionsDecimalExecute
    end
    object actOptionsHex: TAction
      Category = 'Options'
      Caption = '&Hex Numbers'
      Hint = 'Hex numbers'
      ShortCut = 16456
      OnExecute = actOptionsHexExecute
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
      OnExecute = actHelpAboutExecute
    end
    object actEditCopy: TAction
      Category = 'Edit'
      Caption = '&Copy'
      Hint = 'Copy tab information'
      ImageIndex = 6
      ShortCut = 16451
      OnExecute = actEditCopyExecute
    end
  end
end
