object fmUsesManager: TfmUsesManager
  Left = 311
  Top = 202
  Width = 578
  Height = 432
  ActiveControl = edtFilter
  BorderIcons = [biSystemMenu]
  Caption = 'Uses Clause Manager'
  Color = clBtnFace
  Constraints.MinHeight = 240
  Constraints.MinWidth = 560
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter: TSplitter
    Left = 266
    Top = 0
    Width = 3
    Height = 366
    Cursor = crHSplit
  end
  object pnlUnits: TPanel
    Left = 269
    Top = 0
    Width = 301
    Height = 366
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 2
    FullRepaint = False
    TabOrder = 1
    object pcUnits: TPageControl
      Left = 2
      Top = 43
      Width = 297
      Height = 321
      ActivePage = tabSearchPath
      Align = alClient
      TabIndex = 0
      TabOrder = 1
      object tabSearchPath: TTabSheet
        Caption = '&Search Path'
        ImageIndex = 3
        object pnlSearchPathFooter: TPanel
          Left = 0
          Top = 259
          Width = 289
          Height = 34
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 1
          object btnSearchPathAddToIntf: TButton
            Left = 4
            Top = 4
            Width = 125
            Height = 25
            Action = actAvailAddToIntf
            TabOrder = 0
          end
          object btnSearchPathAddToImpl: TButton
            Left = 136
            Top = 4
            Width = 125
            Height = 25
            Action = actAvailAddToImpl
            TabOrder = 1
          end
        end
        object pnlSearchPath: TPanel
          Left = 0
          Top = 0
          Width = 289
          Height = 259
          Align = alClient
          BevelOuter = bvNone
          BorderWidth = 3
          FullRepaint = False
          TabOrder = 0
          object lbxSearchPath: TListBox
            Left = 3
            Top = 3
            Width = 283
            Height = 253
            Align = alClient
            Color = clBtnFace
            DragMode = dmAutomatic
            Enabled = False
            ItemHeight = 13
            MultiSelect = True
            PopupMenu = pmuAvail
            Sorted = True
            TabOrder = 0
            OnDblClick = lbxAvailDblClick
            OnDragDrop = lbxAvailDragDrop
            OnDragOver = lbxAvailDragOver
          end
        end
      end
      object tabProject: TTabSheet
        Caption = '&Project'
        object pnlProject: TPanel
          Left = 0
          Top = 0
          Width = 289
          Height = 259
          Align = alClient
          BevelOuter = bvNone
          BorderWidth = 3
          FullRepaint = False
          TabOrder = 0
          object lbxProject: TListBox
            Left = 3
            Top = 3
            Width = 283
            Height = 253
            Align = alClient
            DragMode = dmAutomatic
            ItemHeight = 13
            MultiSelect = True
            PopupMenu = pmuAvail
            Sorted = True
            TabOrder = 0
            OnDblClick = lbxAvailDblClick
            OnDragDrop = lbxAvailDragDrop
            OnDragOver = lbxAvailDragOver
          end
        end
        object pnlProjFooter: TPanel
          Left = 0
          Top = 259
          Width = 289
          Height = 34
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 1
          object btnProjectAddToInterface: TButton
            Left = 4
            Top = 4
            Width = 125
            Height = 25
            Action = actAvailAddToIntf
            TabOrder = 0
          end
          object btnProjectAddToImplementation: TButton
            Left = 136
            Top = 4
            Width = 125
            Height = 25
            Action = actAvailAddToImpl
            TabOrder = 1
          end
        end
      end
      object tabCommon: TTabSheet
        Caption = '&VCL/RTL'
        ImageIndex = 1
        object pnlCommon: TPanel
          Left = 0
          Top = 0
          Width = 289
          Height = 259
          Align = alClient
          BevelOuter = bvNone
          BorderWidth = 3
          FullRepaint = False
          TabOrder = 0
          object lbxCommon: TListBox
            Left = 3
            Top = 3
            Width = 283
            Height = 253
            Align = alClient
            DragMode = dmAutomatic
            ItemHeight = 13
            MultiSelect = True
            PopupMenu = pmuAvail
            Sorted = True
            TabOrder = 0
            OnDblClick = lbxAvailDblClick
            OnDragDrop = lbxAvailDragDrop
            OnDragOver = lbxAvailDragOver
          end
        end
        object pnlCommonFooter: TPanel
          Left = 0
          Top = 259
          Width = 289
          Height = 34
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 1
          object btnCommonAddToInterface: TButton
            Left = 4
            Top = 4
            Width = 125
            Height = 25
            Action = actAvailAddToIntf
            TabOrder = 0
          end
          object btnCommonAddToImplementation: TButton
            Left = 136
            Top = 4
            Width = 125
            Height = 25
            Action = actAvailAddToImpl
            TabOrder = 1
          end
        end
      end
      object tabFavorite: TTabSheet
        Caption = '&Favorite'
        ImageIndex = 2
        object pnlFavorite: TPanel
          Left = 0
          Top = 0
          Width = 289
          Height = 230
          Align = alClient
          BevelOuter = bvNone
          BorderWidth = 3
          FullRepaint = False
          TabOrder = 0
          object lbxFavorite: TListBox
            Left = 3
            Top = 3
            Width = 283
            Height = 224
            Align = alClient
            DragMode = dmAutomatic
            ItemHeight = 13
            MultiSelect = True
            PopupMenu = pmuAvail
            Sorted = True
            TabOrder = 0
            OnDblClick = lbxAvailDblClick
            OnDragDrop = lbxAvailDragDrop
            OnDragOver = lbxAvailDragOver
          end
        end
        object pnlFavFooter: TPanel
          Left = 0
          Top = 230
          Width = 289
          Height = 63
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 1
          object btnFavoriteAddToInterface: TButton
            Left = 4
            Top = 33
            Width = 125
            Height = 25
            Action = actAvailAddToIntf
            TabOrder = 0
          end
          object btnFavoriteAddToImplementation: TButton
            Left = 136
            Top = 33
            Width = 125
            Height = 25
            Action = actAvailAddToImpl
            TabOrder = 1
          end
          object btnFavoriteAddToFavorites: TButton
            Left = 136
            Top = 2
            Width = 125
            Height = 25
            Action = actFavAdd
            TabOrder = 2
          end
          object btnFavoriteDeleteFromFavorites: TButton
            Left = 4
            Top = 3
            Width = 125
            Height = 25
            Action = actFavDelete
            TabOrder = 3
          end
        end
      end
    end
    object pnlAvailableHeader: TPanel
      Left = 2
      Top = 2
      Width = 297
      Height = 41
      Align = alTop
      BevelOuter = bvNone
      FullRepaint = False
      TabOrder = 0
      DesignSize = (
        297
        41)
      object lblUnits: TLabel
        Left = 0
        Top = 0
        Width = 297
        Height = 13
        Align = alTop
        Caption = 'Available Units'
      end
      object lblFilter: TLabel
        Left = 18
        Top = 20
        Width = 22
        Height = 13
        Caption = 'Filte&r'
        FocusControl = edtFilter
      end
      object edtFilter: TEdit
        Left = 48
        Top = 16
        Width = 245
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnChange = edtFilterChange
        OnKeyDown = edtFilterKeyDown
      end
    end
  end
  object pnlUses: TPanel
    Left = 0
    Top = 0
    Width = 266
    Height = 366
    Align = alLeft
    BevelOuter = bvNone
    BorderWidth = 2
    FullRepaint = False
    TabOrder = 0
    object lblUses: TLabel
      Left = 2
      Top = 2
      Width = 262
      Height = 13
      Align = alTop
      Caption = 'Used Units '
    end
    object pcUses: TPageControl
      Left = 2
      Top = 15
      Width = 262
      Height = 349
      ActivePage = tabInterface
      Align = alClient
      TabIndex = 0
      TabOrder = 0
      object tabInterface: TTabSheet
        Caption = 'I&nterface'
        object pnlInterface: TPanel
          Left = 0
          Top = 0
          Width = 254
          Height = 287
          Align = alClient
          BevelOuter = bvNone
          BorderWidth = 3
          FullRepaint = False
          TabOrder = 0
          object lbxInterface: TListBox
            Left = 3
            Top = 3
            Width = 248
            Height = 281
            Align = alClient
            DragMode = dmAutomatic
            ItemHeight = 13
            MultiSelect = True
            PopupMenu = pmuUses
            Sorted = True
            TabOrder = 0
            OnDblClick = lbxInterfaceDblClick
            OnDragDrop = lbxInterfaceDragDrop
            OnDragOver = lbxUsedDragOver
          end
        end
        object pnlIntfFooter: TPanel
          Left = 0
          Top = 287
          Width = 254
          Height = 34
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 1
          object btnIntfDelete: TButton
            Left = 4
            Top = 4
            Width = 75
            Height = 25
            Action = actUsesDelete
            TabOrder = 0
          end
          object btnIntfMoveToImpl: TButton
            Left = 88
            Top = 4
            Width = 137
            Height = 25
            Action = actIntfMoveToImpl
            TabOrder = 1
          end
        end
      end
      object tabImplementation: TTabSheet
        Caption = 'I&mplementation'
        ImageIndex = 1
        object pnlImplementation: TPanel
          Left = 0
          Top = 0
          Width = 254
          Height = 287
          Align = alClient
          BevelOuter = bvNone
          BorderWidth = 3
          FullRepaint = False
          TabOrder = 0
          object lbxImplementation: TListBox
            Left = 3
            Top = 3
            Width = 248
            Height = 281
            Align = alClient
            DragMode = dmAutomatic
            ItemHeight = 13
            MultiSelect = True
            PopupMenu = pmuUses
            Sorted = True
            TabOrder = 0
            OnDblClick = lbxImplementationDblClick
            OnDragDrop = lbxImplementationDragDrop
            OnDragOver = lbxUsedDragOver
          end
        end
        object pnlImplFooter: TPanel
          Left = 0
          Top = 287
          Width = 254
          Height = 34
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 1
          object btnImplDelete: TButton
            Left = 4
            Top = 4
            Width = 75
            Height = 25
            Action = actUsesDelete
            TabOrder = 0
          end
          object btnImplMoveToIntf: TButton
            Left = 88
            Top = 4
            Width = 137
            Height = 25
            Action = actImplMoveToIntf
            TabOrder = 1
          end
        end
      end
    end
  end
  object pnlFooter: TPanel
    Left = 0
    Top = 366
    Width = 570
    Height = 37
    Align = alBottom
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 2
    DesignSize = (
      570
      37)
    object chkSingleActionMode: TCheckBox
      Left = 8
      Top = 10
      Width = 285
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Single action/quic&k add mode'
      TabOrder = 0
    end
    object btnCancel: TButton
      Left = 486
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 2
    end
    object btnOK: TButton
      Left = 402
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 1
      OnClick = btnOKClick
    end
  end
  object pmuUses: TPopupMenu
    Left = 104
    Top = 56
    object mitInterfaceDelete: TMenuItem
      Action = actUsesDelete
      Default = True
    end
    object mitUsesMoveToInterface: TMenuItem
      Action = actImplMoveToIntf
    end
    object mitUsesMoveToImplementation: TMenuItem
      Action = actIntfMoveToImpl
    end
    object mitIntfSep1: TMenuItem
      Caption = '-'
    end
    object mitUsesAddToFavorites: TMenuItem
      Action = actUsesAddToFavorites
    end
    object mitUsesOpenUnit: TMenuItem
      Action = actUsesOpenUnit
    end
  end
  object pmuAvail: TPopupMenu
    Left = 296
    Top = 80
    object mitAvailAddToIntf: TMenuItem
      Action = actAvailAddToIntf
    end
    object mitAvailAddToImpl: TMenuItem
      Action = actAvailAddToImpl
    end
    object mitAvailSep1: TMenuItem
      Caption = '-'
    end
    object mitAvailOpenUnit: TMenuItem
      Action = actAvailOpenUnit
    end
    object mitAvailSep2: TMenuItem
      Caption = '-'
    end
    object mitAvailAddToFav: TMenuItem
      Action = actFavAdd
    end
    object mitAvailDelFromFav: TMenuItem
      Action = actFavDelete
    end
  end
  object dlgOpen: TOpenDialog
    Filter = 'Delphi Units (*.pas, *.dcu)|*.dcu;*.pas|All Files (*.*)|*.*'
    FilterIndex = 0
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 354
    Top = 80
  end
  object ActionList: TActionList
    OnUpdate = ActionListUpdate
    Left = 42
    Top = 56
    object actUsesDelete: TAction
      Caption = '&Delete'
      ImageIndex = 42
      ShortCut = 46
      OnExecute = acUsesDeleteExecute
    end
    object actIntfMoveToImpl: TAction
      Caption = 'Move to &Implementation'
      ImageIndex = 36
      ShortCut = 16464
      OnExecute = actIntfMoveToImplExecute
    end
    object actImplMoveToIntf: TAction
      Caption = 'Move to In&terface'
      ImageIndex = 36
      ShortCut = 16468
      OnExecute = actImplMoveToIntfExecute
    end
    object actFavDelete: TAction
      Caption = 'D&elete from Favorites'
      ImageIndex = 42
      ShortCut = 46
      OnExecute = actFavDeleteExecute
    end
    object actFavAdd: TAction
      Caption = '&Add to Favorites...'
      ImageIndex = 41
      ShortCut = 45
      OnExecute = actFavAddExecute
    end
    object actAvailAddToIntf: TAction
      Caption = 'Add to Interfa&ce'
      ShortCut = 16468
      OnExecute = actAvailAddToIntfExecute
    end
    object actAvailAddToImpl: TAction
      Caption = 'Add to Imp&lementation'
      ShortCut = 16464
      OnExecute = actAvailAddToImplExecute
    end
    object actUsesOpenUnit: TAction
      Caption = 'Open Unit'
      ImageIndex = 1
      OnExecute = actUsesOpenUnitExecute
    end
    object actAvailOpenUnit: TAction
      Caption = 'Open Unit'
      ImageIndex = 1
      OnExecute = actAvailOpenUnitExecute
    end
    object actUsesAddToFavorites: TAction
      Caption = 'Add to Favorites'
      OnExecute = actUsesAddToFavoritesExecute
    end
  end
end
