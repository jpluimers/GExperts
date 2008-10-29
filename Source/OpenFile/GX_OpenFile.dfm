object fmOpenFile: TfmOpenFile
  Left = 297
  Top = 241
  ActiveControl = edtFilter
  AutoScroll = False
  Caption = 'Open Unit'
  ClientHeight = 520
  ClientWidth = 621
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
    Width = 621
    Height = 458
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 6
    FullRepaint = False
    TabOrder = 1
    object pcUnits: TPageControl
      Left = 6
      Top = 39
      Width = 609
      Height = 413
      ActivePage = tabSearchPath
      Align = alClient
      TabIndex = 0
      TabOrder = 1
      OnChange = pcUnitsChange
      OnResize = pcUnitsResize
      object tabSearchPath: TTabSheet
        Caption = '&Search Path'
        ImageIndex = 3
        OnShow = tabSearchPathShow
        object pnlSearchPathFooter: TPanel
          Left = 0
          Top = 352
          Width = 601
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
          Width = 601
          Height = 352
          Align = alClient
          BevelOuter = bvNone
          BorderWidth = 3
          FullRepaint = False
          TabOrder = 0
          object lvSearchPath: TListView
            Left = 3
            Top = 3
            Width = 595
            Height = 346
            Align = alClient
            Columns = <
              item
                Caption = 'File'
                MinWidth = 55
                Width = 170
              end
              item
                AutoSize = True
                Caption = 'Path'
                MinWidth = 55
              end
              item
                Caption = 'Extension'
                MinWidth = 55
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
        object pnlProject: TPanel
          Left = 0
          Top = 0
          Width = 601
          Height = 352
          Align = alClient
          BevelOuter = bvNone
          BorderWidth = 3
          FullRepaint = False
          TabOrder = 0
          object lvProjects: TListView
            Left = 3
            Top = 3
            Width = 595
            Height = 346
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
          Top = 352
          Width = 601
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
        object pnlCommon: TPanel
          Left = 0
          Top = 0
          Width = 601
          Height = 352
          Align = alClient
          BevelOuter = bvNone
          BorderWidth = 3
          FullRepaint = False
          TabOrder = 0
          object lvCommon: TListView
            Left = 3
            Top = 3
            Width = 595
            Height = 346
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
          Top = 352
          Width = 601
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
        object pnlFavorite: TPanel
          Left = 0
          Top = 0
          Width = 601
          Height = 352
          Align = alClient
          BevelOuter = bvNone
          BorderWidth = 3
          FullRepaint = False
          TabOrder = 0
          object lvFavorite: TListView
            Left = 3
            Top = 3
            Width = 595
            Height = 346
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
          Top = 352
          Width = 601
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
            TabOrder = 1
          end
          object btnFavoriteAddToFavorites: TButton
            Left = 4
            Top = 3
            Width = 125
            Height = 25
            Action = actFavAddToFavorites
            TabOrder = 0
          end
        end
      end
      object tabRecent: TTabSheet
        Caption = '&Recent'
        ImageIndex = 4
        OnShow = tabRecentShow
        object pnlRecentFooter: TPanel
          Left = 0
          Top = 352
          Width = 601
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
            TabOrder = 1
          end
          object btnRecentAddToFavorites: TButton
            Left = 4
            Top = 3
            Width = 125
            Height = 25
            Action = actAddToFavorites
            TabOrder = 0
          end
        end
        object pnlRecent: TPanel
          Left = 0
          Top = 0
          Width = 601
          Height = 352
          Align = alClient
          BevelOuter = bvNone
          BorderWidth = 3
          TabOrder = 0
          object lvRecent: TListView
            Left = 3
            Top = 3
            Width = 595
            Height = 346
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
      Left = 6
      Top = 6
      Width = 609
      Height = 33
      Align = alTop
      BevelOuter = bvNone
      FullRepaint = False
      TabOrder = 0
      DesignSize = (
        609
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
        Left = 454
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
        Width = 389
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnChange = edtFilterChange
        OnKeyDown = edtFilterKeyDown
      end
      object cbxType: TComboBox
        Left = 484
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
    Top = 485
    Width = 621
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object chkDefault: TCheckBox
      Left = 9
      Top = 8
      Width = 177
      Height = 17
      Caption = 'Default'
      TabOrder = 0
    end
    object pnlButtonsRight: TPanel
      Left = 420
      Top = 0
      Width = 201
      Height = 35
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
      object btnOK: TButton
        Left = 36
        Top = 2
        Width = 75
        Height = 25
        Caption = 'OK'
        Default = True
        ModalResult = 1
        TabOrder = 0
        OnClick = btnOKClick
      end
      object btnCancel: TButton
        Left = 120
        Top = 2
        Width = 75
        Height = 25
        Cancel = True
        Caption = 'Cancel'
        ModalResult = 2
        TabOrder = 1
      end
    end
  end
  object ToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 621
    Height = 27
    AutoSize = True
    ButtonHeight = 23
    DisabledImages = dmSharedImages.DisabledImages
    EdgeBorders = [ebTop, ebBottom]
    Flat = True
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
