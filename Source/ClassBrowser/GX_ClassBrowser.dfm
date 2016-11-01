object fmClassBrowser: TfmClassBrowser
  Left = 241
  Top = 191
  AutoScroll = False
  Caption = 'Class Browser'
  ClientHeight = 463
  ClientWidth = 620
  Color = clBtnFace
  DefaultMonitor = dmDesktop
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
  OnActivate = FormActivate
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 14
  object Splitter1: TSplitter
    Left = 175
    Top = 30
    Width = 3
    Height = 414
    Cursor = crHSplit
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 444
    Width = 620
    Height = 19
    Panels = <>
    ParentFont = True
    SimplePanel = True
    UseSystemFont = False
  end
  object pnlData: TPanel
    Left = 178
    Top = 30
    Width = 442
    Height = 414
    Align = alClient
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 2
    OnResize = pnlDataResize
    object pcMain: TPageControl
      Left = 0
      Top = 0
      Width = 442
      Height = 414
      ActivePage = tshMembers
      Align = alClient
      TabIndex = 0
      TabOrder = 0
      OnChange = pcMainChange
      object tshMembers: TTabSheet
        Caption = '&Members'
        object Splitter2: TSplitter
          Left = 0
          Top = 321
          Width = 434
          Height = 4
          Cursor = crVSplit
          Align = alBottom
        end
        object pnlMethod: TPanel
          Left = 0
          Top = 325
          Width = 434
          Height = 60
          Align = alBottom
          BevelOuter = bvNone
          Caption = 'This editor is created at runtime'
          FullRepaint = False
          TabOrder = 0
          TabStop = True
        end
        object lvInfo: TListView
          Left = 0
          Top = 0
          Width = 434
          Height = 321
          Align = alClient
          Columns = <
            item
              Caption = 'Vi'
              Width = 28
            end
            item
              Caption = 'Ty'
              Width = 28
            end
            item
              Caption = 'Di'
              Width = 28
            end
            item
              Caption = 'Ab'
              Width = 28
            end
            item
              Caption = 'Ol'
              Width = 28
            end
            item
              Caption = 'Name'
              Width = 250
            end>
          ColumnClick = False
          HideSelection = False
          ReadOnly = True
          RowSelect = True
          ParentShowHint = False
          PopupMenu = pmInfo
          ShowHint = True
          SmallImages = dmSharedImages.Images
          TabOrder = 1
          ViewStyle = vsReport
          OnChange = lvInfoChange
          OnDblClick = actEditGotoMemberExecute
          OnMouseMove = lvInfoMouseMove
        end
      end
      object tshInherit: TTabSheet
        Caption = '&Inheritance'
        object scInherit: TScrollBox
          Left = 0
          Top = 0
          Width = 434
          Height = 385
          Align = alClient
          Color = clWindow
          ParentColor = False
          TabOrder = 0
        end
      end
      object tshCode: TTabSheet
        Caption = '&Code'
      end
    end
  end
  object tvBrowse: TTreeView
    Left = 0
    Top = 30
    Width = 175
    Height = 414
    Align = alLeft
    HideSelection = False
    Images = dmSharedImages.Images
    Indent = 19
    PopupMenu = pmBrowser
    ReadOnly = True
    RightClickSelect = True
    TabOrder = 1
    ToolTips = False
    OnChange = tvBrowseChange
    OnDblClick = tvBrowseDblClick
    OnMouseDown = tvBrowseMouseDown
  end
  object ControlBar: TControlBar
    Left = 0
    Top = 0
    Width = 620
    Height = 30
    Align = alTop
    AutoSize = True
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
    object tbMain: TToolBar
      Left = 11
      Top = 2
      Width = 110
      Height = 22
      AutoSize = True
      DisabledImages = dmSharedImages.DisabledImages
      EdgeBorders = []
      Flat = True
      Images = dmSharedImages.Images
      TabOrder = 0
      Wrapable = False
      object tbnAdd: TToolButton
        Left = 0
        Top = 0
        Action = actFileAdd
      end
      object tbnRemove: TToolButton
        Left = 23
        Top = 0
        Action = actFileRemove
      end
      object tbnSep1: TToolButton
        Left = 46
        Top = 0
        Width = 8
        ImageIndex = 2
        Style = tbsSeparator
      end
      object tbnRefresh: TToolButton
        Left = 54
        Top = 0
        Action = actFileRefresh
      end
      object tbnSep2: TToolButton
        Left = 77
        Top = 0
        Width = 8
        ImageIndex = 3
        Style = tbsSeparator
      end
      object tbnFind: TToolButton
        Left = 85
        Top = 0
        Action = actEditFind
      end
    end
    object tbKinds: TToolBar
      Left = 134
      Top = 2
      Width = 121
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
      object tbnConstants: TToolButton
        Left = 0
        Top = 0
        Action = actViewConstants
      end
      object tbnMethods: TToolButton
        Left = 23
        Top = 0
        Action = actViewMethods
      end
      object tbnTypes: TToolButton
        Left = 46
        Top = 0
        Action = actViewTypes
      end
      object tbnVariables: TToolButton
        Left = 69
        Top = 0
        Action = actViewVariables
      end
      object tbnProperties: TToolButton
        Left = 92
        Top = 0
        Action = actViewProperties
      end
    end
    object tbVisibility: TToolBar
      Left = 268
      Top = 2
      Width = 97
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
      object tbnPrivate: TToolButton
        Left = 0
        Top = 0
        Action = actViewPrivate
      end
      object tbnProtected: TToolButton
        Left = 23
        Top = 0
        Action = actViewProtected
      end
      object tbnPublic: TToolButton
        Left = 46
        Top = 0
        Action = actViewPublic
      end
      object tbnPublished: TToolButton
        Left = 69
        Top = 0
        Action = actViewPublished
      end
    end
  end
  object MainMenu: TMainMenu
    Images = dmSharedImages.Images
    Left = 16
    Top = 40
    object mitFile: TMenuItem
      Caption = '&File'
      object mitFileAdd: TMenuItem
        Action = actFileAdd
      end
      object mitFileRemove: TMenuItem
        Action = actFileRemove
      end
      object mitFileRefresh: TMenuItem
        Action = actFileRefresh
      end
      object mitFileSep1: TMenuItem
        Caption = '-'
      end
      object mitPrint: TMenuItem
        Caption = '&Print'
        object mitFilePrintClassReport: TMenuItem
          Action = actFilePrintClassReport
        end
        object mitFilePrintClassHierarchy: TMenuItem
          Action = actFilePrintClassHierarchy
        end
      end
      object mitFilePrinterSetup: TMenuItem
        Action = actFilePrinterSetup
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
      object mitEditFind: TMenuItem
        Action = actEditFind
      end
      object mitEditFindNext: TMenuItem
        Action = actEditFindNext
      end
    end
    object mitView: TMenuItem
      Caption = '&View'
      object mitViewList: TMenuItem
        Action = actViewList
        RadioItem = True
      end
      object mitViewTree: TMenuItem
        Tag = 1
        Action = actViewTree
        RadioItem = True
      end
      object mitViewSep1: TMenuItem
        Caption = '-'
      end
      object mitViewUnitNames: TMenuItem
        Action = actViewUnitNames
      end
      object mitViewSep2: TMenuItem
        Caption = '-'
      end
      object mitViewDetails: TMenuItem
        Action = actViewClassProperties
      end
      object mitViewSep3: TMenuItem
        Caption = '-'
      end
      object mitViewConstants: TMenuItem
        Action = actViewConstants
      end
      object mitViewMethods: TMenuItem
        Action = actViewMethods
      end
      object mitViewTypes: TMenuItem
        Action = actViewTypes
      end
      object mitViewVariables: TMenuItem
        Action = actViewVariables
      end
      object mitViewProperties: TMenuItem
        Action = actViewProperties
      end
      object mitViewSep4: TMenuItem
        Caption = '-'
      end
      object mitViewPrivate: TMenuItem
        Action = actViewPrivate
      end
      object mitViewProtected: TMenuItem
        Action = actViewProtected
      end
      object mitViewPublic: TMenuItem
        Action = actViewPublic
      end
      object mitViewPublished: TMenuItem
        Action = actViewPublished
      end
    end
    object mitOptions: TMenuItem
      Caption = '&Options'
      object mitOptionsOptions: TMenuItem
        Action = actOptionsOptions
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
  object pmBrowser: TPopupMenu
    Images = dmSharedImages.Images
    Left = 72
    Top = 240
    object pmiBrowserGotoClass: TMenuItem
      Action = actEditGotoClass
      Default = True
    end
    object pmiBrowserRefresh: TMenuItem
      Action = actFileRefresh
    end
    object pmiBrowserSep1: TMenuItem
      Caption = '-'
    end
    object pmiBrowserProperties: TMenuItem
      Action = actViewClassProperties
    end
  end
  object dlgPrinterSetup: TPrinterSetupDialog
    Left = 72
    Top = 160
  end
  object pmInfo: TPopupMenu
    Images = dmSharedImages.Images
    Left = 216
    Top = 112
    object pmiDetailsGoto: TMenuItem
      Action = actEditGotoMember
    end
  end
  object Actions: TActionList
    Images = dmSharedImages.Images
    OnUpdate = ActionsUpdate
    Left = 216
    Top = 152
    object actFileAdd: TAction
      Category = 'File'
      Caption = '&Add...'
      Hint = 'Add directory of classes...'
      ImageIndex = 10
      ShortCut = 16449
      OnExecute = actFileAddExecute
    end
    object actEditCopy: TAction
      Category = 'Edit'
      Caption = '&Copy'
      Hint = 'Copy'
      ImageIndex = 6
      ShortCut = 16451
      OnExecute = actEditCopyExecute
    end
    object actEditFind: TAction
      Category = 'Edit'
      Caption = '&Find...'
      Hint = 'Find...'
      ImageIndex = 14
      ShortCut = 16454
      OnExecute = actEditFindExecute
    end
    object actFileRemove: TAction
      Category = 'File'
      Caption = '&Remove'
      Hint = 'Remove classes'
      ImageIndex = 11
      ShortCut = 16430
      OnExecute = actFileRemoveExecute
    end
    object actFilePrintClassReport: TAction
      Category = 'File'
      Caption = '&Class Report...'
      Hint = 'Print class report...'
      OnExecute = actFilePrintClassReportExecute
    end
    object actFilePrintClassHierarchy: TAction
      Category = 'File'
      Caption = 'Class &Hierarchy...'
      Hint = 'Print class hierarchy...'
      OnExecute = actFilePrintClassHierarchyExecute
    end
    object actFilePrinterSetup: TAction
      Category = 'File'
      Caption = 'Printer &Setup...'
      Hint = 'Printer setup...'
      ImageIndex = 4
      OnExecute = actFilePrinterSetupExecute
    end
    object actFileExit: TAction
      Category = 'File'
      Caption = 'E&xit'
      Hint = 'Exit'
      ImageIndex = 8
      ShortCut = 32883
      OnExecute = actFileExitExecute
    end
    object actEditFindNext: TAction
      Category = 'Edit'
      Caption = 'Find Next'
      ImageIndex = 15
      ShortCut = 16498
      OnExecute = actEditFindNextExecute
    end
    object actViewList: TAction
      Category = 'View'
      Caption = '&List'
      Hint = 'Class list view'
      ImageIndex = 40
      OnExecute = actViewListExecute
    end
    object actViewTree: TAction
      Category = 'View'
      Caption = '&Tree'
      Hint = 'Class tree view'
      ImageIndex = 45
      OnExecute = actViewTreeExecute
    end
    object actViewUnitNames: TAction
      Category = 'View'
      Caption = 'Unit Names'
      Hint = 'View unit names'
      OnExecute = actViewUnitNamesExecute
    end
    object actViewClassProperties: TAction
      Category = 'View'
      Caption = '&Properties...'
      Hint = 'View properties'
      OnExecute = actViewClassPropertiesExecute
    end
    object actOptionsOptions: TAction
      Category = 'Options'
      Caption = '&Options...'
      Hint = 'Options...'
      ImageIndex = 17
      ShortCut = 16463
      OnExecute = actOptionsOptionsExecute
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
    object actFileRefresh: TAction
      Category = 'File'
      Caption = 'Refres&h'
      Hint = 'Refresh classes'
      ImageIndex = 39
      ShortCut = 116
      OnExecute = actFileRefreshExecute
    end
    object actEditGotoMember: TAction
      Category = 'Edit'
      Caption = '&Goto Member'
      Hint = 'Goto member'
      ImageIndex = 27
      ShortCut = 16455
      OnExecute = actEditGotoMemberExecute
    end
    object actEditGotoClass: TAction
      Category = 'Edit'
      Caption = 'Goto Class'
      Hint = 'Goto class'
      ImageIndex = 27
      OnExecute = actEditGotoClassExecute
    end
    object actViewConstants: TAction
      Category = 'ViewKind'
      Caption = 'Constants'
      Checked = True
      Hint = 'Constants'
      ImageIndex = 48
      OnExecute = actGenericViewNewFilterExecute
    end
    object actViewMethods: TAction
      Category = 'ViewKind'
      Caption = 'Methods'
      Checked = True
      Hint = 'Methods'
      ImageIndex = 49
      OnExecute = actGenericViewNewFilterExecute
    end
    object actViewTypes: TAction
      Category = 'ViewKind'
      Caption = 'Types'
      Checked = True
      Hint = 'Types'
      ImageIndex = 50
      OnExecute = actGenericViewNewFilterExecute
    end
    object actViewVariables: TAction
      Category = 'ViewKind'
      Caption = 'Variables'
      Checked = True
      Hint = 'Variables'
      ImageIndex = 51
      OnExecute = actGenericViewNewFilterExecute
    end
    object actViewProperties: TAction
      Category = 'ViewKind'
      Caption = 'Properties'
      Checked = True
      Hint = 'Properties'
      ImageIndex = 52
      OnExecute = actGenericViewNewFilterExecute
    end
    object actViewPrivate: TAction
      Category = 'ViewVisibility'
      Caption = 'Private'
      Checked = True
      Hint = 'Private'
      ImageIndex = 53
      OnExecute = actGenericViewNewFilterExecute
    end
    object actViewProtected: TAction
      Category = 'ViewVisibility'
      Caption = 'Protected'
      Checked = True
      Hint = 'Protected'
      ImageIndex = 54
      OnExecute = actGenericViewNewFilterExecute
    end
    object actViewPublic: TAction
      Category = 'ViewVisibility'
      Caption = 'Public'
      Checked = True
      Hint = 'Public'
      ImageIndex = 55
      OnExecute = actGenericViewNewFilterExecute
    end
    object actViewPublished: TAction
      Category = 'ViewVisibility'
      Caption = 'Published'
      Checked = True
      Hint = 'Published'
      ImageIndex = 56
      OnExecute = actGenericViewNewFilterExecute
    end
  end
end
