object fmProjDepend: TfmProjDepend
  Left = 282
  Top = 149
  Width = 683
  Height = 568
  Caption = 'Project Dependencies'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  Menu = MainMenu
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnActivate = FormActivate
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter: TSplitter
    Left = 213
    Top = 22
    Width = 3
    Height = 471
    Cursor = crHSplit
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 493
    Width = 667
    Height = 19
    Panels = <>
    SimplePanel = False
  end
  object tvUnits: TTreeView
    Left = 0
    Top = 22
    Width = 213
    Height = 471
    Align = alLeft
    HideSelection = False
    Images = dmSharedImages.Images
    Indent = 19
    PopupMenu = pmTreeview
    RightClickSelect = True
    TabOrder = 1
    ToolTips = False
    OnChange = tvUnitsChange
    OnEditing = tvUnitsEditing
    OnExpanding = tvUnitsExpanding
    OnKeyDown = tvUnitsKeyDown
  end
  object pnlPageControlHost: TPanel
    Left = 216
    Top = 22
    Width = 451
    Height = 471
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    OnResize = pnlPageControlHostResize
    object pcData: TPageControl
      Left = 0
      Top = 0
      Width = 451
      Height = 471
      ActivePage = tshUnitUses
      Align = alClient
      TabIndex = 0
      TabOrder = 0
      OnChange = pcDataChange
      object tshUnitUses: TTabSheet
        Caption = 'Unit &Uses'
        object lvUnitUses: TListView
          Left = 0
          Top = 0
          Width = 443
          Height = 443
          Align = alClient
          Columns = <
            item
              Caption = 'Unit'
              Width = 120
            end
            item
              Caption = 'Location'
              Width = 120
            end>
          HideSelection = False
          ReadOnly = True
          RowSelect = True
          PopupMenu = pmList
          TabOrder = 0
          ViewStyle = vsReport
          OnColumnClick = lvColumnClick
          OnCompare = lvIndirectCompare
          OnDblClick = actOpenUnitListExecute
        end
      end
      object tshUsedBy: TTabSheet
        Caption = 'Unit is Used &By'
        object lvUsedBy: TListView
          Left = 0
          Top = 0
          Width = 443
          Height = 443
          Align = alClient
          Columns = <
            item
              Caption = 'Unit'
              Width = 120
            end
            item
              Caption = 'Location'
              Width = 120
            end>
          HideSelection = False
          ReadOnly = True
          RowSelect = True
          PopupMenu = pmList
          TabOrder = 0
          ViewStyle = vsReport
          OnColumnClick = lvColumnClick
          OnCompare = lvIndirectCompare
          OnDblClick = actOpenUnitListExecute
        end
      end
      object tshIndirect: TTabSheet
        Caption = 'Indirect &Dependencies'
        object lvIndirect: TListView
          Left = 0
          Top = 0
          Width = 443
          Height = 443
          Align = alClient
          Columns = <
            item
              Caption = 'Unit'
              Width = 120
            end
            item
              Caption = 'Source Files'
              Width = 120
            end>
          HideSelection = False
          ReadOnly = True
          RowSelect = True
          PopupMenu = pmList
          SortType = stData
          TabOrder = 0
          ViewStyle = vsReport
          OnColumnClick = lvColumnClick
          OnCompare = lvIndirectCompare
          OnDblClick = actViewIndirectUnitPropertiesExecute
        end
      end
    end
  end
  object ToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 667
    Height = 22
    AutoSize = True
    DisabledImages = dmSharedImages.DisabledImages
    EdgeBorders = []
    Flat = True
    Images = dmSharedImages.Images
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
    Wrapable = False
    object tbnRefresh: TToolButton
      Left = 0
      Top = 0
      Action = actFileRefresh
    end
    object tbnAbort: TToolButton
      Left = 23
      Top = 0
      Action = actFileAbort
    end
    object tbnSep1: TToolButton
      Left = 46
      Top = 0
      Width = 8
      ImageIndex = 2
      Style = tbsSeparator
    end
    object tbnExport: TToolButton
      Left = 54
      Top = 0
      Action = actFileExport
    end
    object tbnSep2: TToolButton
      Left = 77
      Top = 0
      Width = 8
      ImageIndex = 1
      Style = tbsSeparator
    end
    object tbnFilter: TToolButton
      Left = 85
      Top = 0
      Action = actFileFilter
    end
    object tbnSep3: TToolButton
      Left = 108
      Top = 0
      Width = 8
      ImageIndex = 2
      Style = tbsSeparator
    end
    object tbnHelp: TToolButton
      Left = 116
      Top = 0
      Action = actHelpHelp
    end
  end
  object pmTreeview: TPopupMenu
    Images = dmSharedImages.Images
    Left = 80
    Top = 40
    object mitOpenUnitTree: TMenuItem
      Action = actOpenUnitTree
    end
  end
  object pmList: TPopupMenu
    Images = dmSharedImages.Images
    Left = 208
    Top = 72
    object mitOpenUnitList: TMenuItem
      Action = actOpenUnitList
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object mitIndirectUnitProperties: TMenuItem
      Action = actViewIndirectUnitProperties
    end
  end
  object MainMenu: TMainMenu
    Images = dmSharedImages.Images
    Left = 16
    Top = 40
    object mitFile: TMenuItem
      Caption = '&File'
      object mitFileRefresh: TMenuItem
        Action = actFileRefresh
      end
      object mitFileAbort: TMenuItem
        Action = actFileAbort
      end
      object mitFileSep1: TMenuItem
        Caption = '-'
      end
      object mitFileExport: TMenuItem
        Action = actFileExport
      end
      object mitFileFilter: TMenuItem
        Action = actFileFilter
      end
      object mitFileSep2: TMenuItem
        Caption = '-'
      end
      object mitFileExit: TMenuItem
        Action = actFileExit
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
    Left = 48
    Top = 40
    object actFileRefresh: TAction
      Category = 'File'
      Caption = '&Refresh'
      Hint = 'Refresh'
      ImageIndex = 39
      ShortCut = 116
      OnExecute = actFileRefreshExecute
    end
    object actFileAbort: TAction
      Category = 'File'
      Caption = '&Abort'
      Hint = 'Abort'
      ImageIndex = 32
      OnExecute = actFileAbortExecute
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
    object actFileExit: TAction
      Category = 'File'
      Caption = 'E&xit'
      Hint = 'Exit'
      ImageIndex = 8
      ShortCut = 32883
      OnExecute = actFileExitExecute
    end
    object actOpenUnitTree: TAction
      Category = 'Tree'
      Caption = '&Open Unit'
      Hint = 'Open unit'
      ImageIndex = 1
      OnExecute = actOpenUnitTreeExecute
    end
    object actOpenUnitList: TAction
      Category = 'List'
      Caption = 'Open Unit'
      ImageIndex = 1
      OnExecute = actOpenUnitListExecute
    end
    object actViewIndirectUnitProperties: TAction
      Category = 'List'
      Caption = 'Indirect Properties...'
      Hint = 'View indirect unit properties'
      ImageIndex = 35
      OnExecute = actViewIndirectUnitPropertiesExecute
    end
    object actFileExport: TAction
      Category = 'File'
      Caption = '&Export Used Units...'
      Hint = 'Export Used Units...'
      ImageIndex = 31
      ShortCut = 16453
      OnExecute = actFileExportExecute
    end
    object actFileFilter: TAction
      Category = 'File'
      Caption = '&Filter Used Units...'
      Hint = 'Filter Used Units...'
      ImageIndex = 62
      ShortCut = 16454
      OnExecute = actFileFilterExecute
    end
  end
  object dlgSave: TSaveDialog
    DefaultExt = 'csv'
    Filter = 'CSV Files (*.csv)|*.csv|TXT Files (*.txt)|*.txt'
    Title = 'Export Indirect Dependencies'
    Left = 112
    Top = 40
  end
end
