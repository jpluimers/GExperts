object fmOpenFile: TfmOpenFile
  Left = 297
  Top = 241
  ActiveControl = edtFilter
  Caption = 'Open Unit'
  ClientHeight = 448
  ClientWidth = 555
  Color = clBtnFace
  Constraints.MinHeight = 325
  Constraints.MinWidth = 350
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlUnits: TPanel
    Left = 0
    Top = 27
    Width = 555
    Height = 383
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 2
    FullRepaint = False
    TabOrder = 1
    object pcUnits: TPageControl
      Left = 2
      Top = 35
      Width = 551
      Height = 346
      ActivePage = tabSearchPath
      Align = alClient
      TabOrder = 1
      OnChange = pcUnitsChange
      OnResize = pcUnitsResize
      object tabSearchPath: TTabSheet
        Caption = '&Search Path'
        ImageIndex = 3
        OnShow = tabSearchPathShow
        object pnlSearchPathFooter: TPanel
          Left = 0
          Top = 285
          Width = 543
          Height = 33
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 1
          object btnSearchAddToFavorites: TButton
            Left = 4
            Top = 3
            Width = 125
            Height = 25
            Action = actAddToFavorites
            TabOrder = 0
          end
        end
        object pnlSearchPath: TPanel
          Left = 0
          Top = 0
          Width = 543
          Height = 285
          Align = alClient
          BevelOuter = bvNone
          BorderWidth = 3
          FullRepaint = False
          TabOrder = 0
          object lvSearchPath: TListView
            Left = 3
            Top = 3
            Width = 537
            Height = 279
            Align = alClient
            Columns = <
              item
                Caption = 'File'
                Width = 170
              end
              item
                AutoSize = True
                Caption = 'Path'
              end
              item
                Caption = 'Extension'
                Width = 63
              end>
            DragMode = dmAutomatic
            Enabled = False
            HideSelection = False
            MultiSelect = True
            ReadOnly = True
            RowSelect = True
            TabOrder = 0
            ViewStyle = vsReport
            OnDblClick = FileListDoubleClick
          end
        end
      end
      object tabProject: TTabSheet
        Caption = '&Project'
        OnShow = tabProjectShow
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object pnlProject: TPanel
          Left = 0
          Top = 0
          Width = 543
          Height = 284
          Align = alClient
          BevelOuter = bvNone
          BorderWidth = 3
          FullRepaint = False
          TabOrder = 0
          object lvProjects: TListView
            Left = 3
            Top = 3
            Width = 537
            Height = 278
            Align = alClient
            Columns = <>
            DragMode = dmAutomatic
            HideSelection = False
            MultiSelect = True
            ReadOnly = True
            RowSelect = True
            TabOrder = 0
            ViewStyle = vsReport
            OnDblClick = FileListDoubleClick
          end
        end
        object pnlProjFooter: TPanel
          Left = 0
          Top = 284
          Width = 543
          Height = 33
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 1
          object btnProjectAddToFavorites: TButton
            Left = 4
            Top = 3
            Width = 125
            Height = 25
            Action = actAddToFavorites
            TabOrder = 0
          end
        end
      end
      object tabCommon: TTabSheet
        Caption = '&VCL/RTL'
        ImageIndex = 1
        OnShow = tabCommonShow
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object pnlCommon: TPanel
          Left = 0
          Top = 0
          Width = 543
          Height = 284
          Align = alClient
          BevelOuter = bvNone
          BorderWidth = 3
          FullRepaint = False
          TabOrder = 0
          object lvCommon: TListView
            Left = 3
            Top = 3
            Width = 537
            Height = 278
            Align = alClient
            Columns = <>
            DragMode = dmAutomatic
            HideSelection = False
            MultiSelect = True
            ReadOnly = True
            RowSelect = True
            TabOrder = 0
            ViewStyle = vsReport
            OnDblClick = FileListDoubleClick
          end
        end
        object pnlCommonFooter: TPanel
          Left = 0
          Top = 284
          Width = 543
          Height = 33
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 1
          object btnCommonAddToFavorites: TButton
            Left = 4
            Top = 3
            Width = 125
            Height = 25
            Action = actAddToFavorites
            TabOrder = 0
          end
        end
      end
      object tabFavorite: TTabSheet
        Caption = 'Fav&orite'
        ImageIndex = 2
        OnShow = tabFavoriteShow
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object pnlFavorite: TPanel
          Left = 0
          Top = 0
          Width = 543
          Height = 285
          Align = alClient
          BevelOuter = bvNone
          BorderWidth = 3
          FullRepaint = False
          TabOrder = 0
          object lvFavorite: TListView
            Left = 3
            Top = 3
            Width = 537
            Height = 279
            Align = alClient
            Columns = <>
            DragMode = dmAutomatic
            HideSelection = False
            MultiSelect = True
            ReadOnly = True
            RowSelect = True
            TabOrder = 0
            ViewStyle = vsReport
            OnDblClick = FileListDoubleClick
          end
        end
        object pnlFavFooter: TPanel
          Left = 0
          Top = 285
          Width = 543
          Height = 33
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 1
          object btnFavoriteDeleteFromFavorites: TButton
            Left = 140
            Top = 3
            Width = 125
            Height = 25
            Action = actFavDeleteFromFavorites
            TabOrder = 0
          end
          object btnFavoriteAddToFavorites: TButton
            Left = 4
            Top = 3
            Width = 125
            Height = 25
            Action = actFavAddToFavorites
            TabOrder = 1
          end
        end
      end
      object tabRecent: TTabSheet
        Caption = '&Recent'
        ImageIndex = 4
        OnShow = tabRecentShow
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object pnlRecentFooter: TPanel
          Left = 0
          Top = 285
          Width = 543
          Height = 33
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 1
          object btnClearRecent: TButton
            Left = 140
            Top = 3
            Width = 125
            Height = 25
            Action = actClearRecentList
            TabOrder = 0
          end
          object btnRecentAddToFavorites: TButton
            Left = 4
            Top = 3
            Width = 125
            Height = 25
            Action = actAddToFavorites
            TabOrder = 1
          end
        end
        object pnlRecent: TPanel
          Left = 0
          Top = 0
          Width = 543
          Height = 285
          Align = alClient
          BevelOuter = bvNone
          BorderWidth = 3
          TabOrder = 0
          object lvRecent: TListView
            Left = 3
            Top = 3
            Width = 537
            Height = 279
            Align = alClient
            Columns = <>
            DragMode = dmAutomatic
            HideSelection = False
            MultiSelect = True
            ReadOnly = True
            RowSelect = True
            TabOrder = 0
            ViewStyle = vsReport
            OnDblClick = FileListDoubleClick
          end
        end
      end
    end
    object pnlAvailableHeader: TPanel
      Left = 2
      Top = 2
      Width = 551
      Height = 33
      Align = alTop
      BevelOuter = bvNone
      FullRepaint = False
      TabOrder = 0
      DesignSize = (
        551
        33)
      object lblFilter: TLabel
        Left = 19
        Top = 10
        Width = 22
        Height = 13
        Alignment = taRightJustify
        Caption = '&Filter'
        FocusControl = edtFilter
      end
      object lblExtension: TLabel
        Left = 388
        Top = 10
        Width = 24
        Height = 13
        Alignment = taRightJustify
        Anchors = [akTop, akRight]
        Caption = '&Type'
        FocusControl = edtFilter
      end
      object edtFilter: TEdit
        Left = 48
        Top = 6
        Width = 323
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnChange = edtFilterChange
        OnKeyDown = edtFilterKeyDown
      end
      object cbxType: TComboBox
        Left = 418
        Top = 6
        Width = 113
        Height = 21
        Style = csDropDownList
        Anchors = [akTop, akRight]
        ItemHeight = 13
        TabOrder = 1
        OnChange = cbxTypeChange
      end
    end
  end
  object pnlOKCancel: TPanel
    Left = 0
    Top = 410
    Width = 555
    Height = 38
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      555
      38)
    object chkDefault: TCheckBox
      Left = 8
      Top = 10
      Width = 177
      Height = 17
      Caption = 'Default'
      TabOrder = 0
    end
    object btnCancel: TButton
      Left = 473
      Top = 7
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 2
    end
    object btnOK: TButton
      Left = 389
      Top = 7
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 1
      OnClick = btnOKClick
    end
  end
  object ToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 555
    Height = 27
    AutoSize = True
    ButtonHeight = 23
    DisabledImages = dmSharedImages.DisabledImages
    EdgeBorders = [ebTop, ebBottom]
    Images = dmSharedImages.Images
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    object tbnMatchFromStart: TToolButton
      Left = 0
      Top = 0
      Action = actMatchPrefix
      Grouped = True
      ParentShowHint = False
      ShowHint = True
      Style = tbsCheck
    end
    object tbnMatchAnywhere: TToolButton
      Left = 23
      Top = 0
      Action = actMatchAnywhere
      Grouped = True
      ParentShowHint = False
      ShowHint = True
      Style = tbsCheck
    end
    object tbnSep1: TToolButton
      Left = 46
      Top = 0
      Width = 8
      ImageIndex = 1
      Style = tbsDivider
    end
    object tbnOpenFile: TToolButton
      Left = 54
      Top = 0
      Action = actOpenFile
    end
    object tbnSep2: TToolButton
      Left = 77
      Top = 0
      Width = 8
      ImageIndex = 1
      Style = tbsDivider
    end
    object tbnConfig: TToolButton
      Left = 85
      Top = 0
      Action = actConfig
    end
    object tbnSep3: TToolButton
      Left = 108
      Top = 0
      Width = 8
      ImageIndex = 1
      Style = tbsDivider
    end
    object tbnHelp: TToolButton
      Left = 116
      Top = 0
      Action = actHelp
    end
  end
  object OpenDialog: TOpenDialog
    Filter = 'All Files (*.*)|*.*'
    Options = [ofAllowMultiSelect, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 16
    Top = 112
  end
  object ActionList: TActionList
    Images = dmSharedImages.Images
    Left = 16
    Top = 144
    object actAddToFavorites: TAction
      Caption = '&Add to Favorites'
      OnExecute = actAddToFavoritesExecute
    end
    object actMatchPrefix: TAction
      Caption = 'Match Prefix'
      GroupIndex = 1
      Hint = 'Match filter only from the start'
      ImageIndex = 24
      OnExecute = actMatchPrefixExecute
    end
    object actMatchAnywhere: TAction
      Caption = 'Match Anywhere'
      Checked = True
      GroupIndex = 1
      Hint = 'Match filter anywhere'
      ImageIndex = 25
      OnExecute = actMatchAnywhereExecute
    end
    object actConfig: TAction
      Caption = 'Configuration...'
      Hint = 'Options...'
      ImageIndex = 17
      OnExecute = actConfigExecute
    end
    object actHelp: TAction
      Caption = 'Help'
      Hint = 'Help'
      ImageIndex = 0
      OnExecute = actHelpExecute
    end
    object actOpenFile: TAction
      Caption = 'Open File...'
      Hint = 'Open File...'
      ImageIndex = 1
      OnExecute = actOpenFileExecute
    end
    object actFavAddToFavorites: TAction
      Caption = '&Add to Favorites...'
      OnExecute = actFavAddToFavoritesExecute
    end
    object actFavDeleteFromFavorites: TAction
      Caption = '&Delete from Favorites'
      OnExecute = actFavDeleteFromFavoritesExecute
    end
    object actClearRecentList: TAction
      Caption = '&Clear Recent List'
      OnExecute = actClearRecentListExecute
    end
  end
  object tmrFilter: TTimer
    Enabled = False
    Interval = 10
    OnTimer = tmrFilterTimer
    Left = 16
    Top = 176
  end
end
