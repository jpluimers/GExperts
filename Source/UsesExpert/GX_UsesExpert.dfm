object fmUsesManager: TfmUsesManager
  Left = 311
  Top = 202
  ActiveControl = edtUnitFilter
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Uses Clause Manager'
  ClientHeight = 478
  ClientWidth = 764
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object Splitter: TSplitter
    Left = 297
    Top = 0
    Height = 441
    OnMoved = SplitterMoved
  end
  object pnlUnits: TPanel
    Left = 300
    Top = 0
    Width = 464
    Height = 441
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 6
    FullRepaint = False
    TabOrder = 1
    object pcUnits: TPageControl
      Left = 6
      Top = 47
      Width = 452
      Height = 388
      ActivePage = tabIdentifiers
      Align = alClient
      TabOrder = 1
      OnChange = pcUnitsChange
      OnResize = pcUnitsResize
      object tabSearchPath: TTabSheet
        Caption = '&Search Path'
        ImageIndex = 3
        object pnlSearchPathFooter: TPanel
          Left = 0
          Top = 325
          Width = 444
          Height = 34
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 1
          object btnSearchPathAddToIntf: TButton
            Left = 4
            Top = 4
            Width = 140
            Height = 25
            Action = actAvailAddToIntf
            TabOrder = 0
          end
          object btnSearchPathAddToImpl: TButton
            Left = 152
            Top = 4
            Width = 150
            Height = 25
            Action = actAvailAddToImpl
            TabOrder = 1
          end
        end
        object pnlSearchPath: TPanel
          Left = 0
          Top = 0
          Width = 444
          Height = 325
          Align = alClient
          BevelOuter = bvNone
          BorderWidth = 3
          FullRepaint = False
          TabOrder = 0
          object sg_SearchPath: TStringGrid
            Left = 3
            Top = 3
            Width = 438
            Height = 319
            Align = alClient
            Color = clBtnFace
            ColCount = 1
            DefaultColWidth = 100
            DefaultRowHeight = 16
            Enabled = False
            FixedCols = 0
            RowCount = 1
            FixedRows = 0
            Options = [goFixedVertLine, goFixedHorzLine, goRangeSelect, goDrawFocusSelected]
            PopupMenu = pmuAvail
            TabOrder = 0
            OnDblClick = lbxAvailDblClick
            OnDragDrop = lbxAvailDragDrop
            OnDragOver = lbxAvailDragOver
            OnDrawCell = sg_AvailDrawCell
            OnMouseDown = sg_MouseDownForDragging
          end
        end
      end
      object tabProject: TTabSheet
        Caption = '&Project'
        object pnlProject: TPanel
          Left = 0
          Top = 0
          Width = 444
          Height = 325
          Align = alClient
          BevelOuter = bvNone
          BorderWidth = 3
          FullRepaint = False
          TabOrder = 0
          object sg_Project: TStringGrid
            Left = 3
            Top = 3
            Width = 438
            Height = 319
            Align = alClient
            ColCount = 1
            DefaultColWidth = 100
            DefaultRowHeight = 16
            FixedCols = 0
            RowCount = 1
            FixedRows = 0
            Options = [goFixedVertLine, goFixedHorzLine, goRangeSelect, goDrawFocusSelected]
            PopupMenu = pmuAvail
            TabOrder = 0
            OnDblClick = lbxAvailDblClick
            OnDragDrop = lbxAvailDragDrop
            OnDragOver = lbxAvailDragOver
            OnDrawCell = sg_AvailDrawCell
            OnMouseDown = sg_MouseDownForDragging
          end
        end
        object pnlProjFooter: TPanel
          Left = 0
          Top = 325
          Width = 444
          Height = 34
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 1
          object btnProjectAddToInterface: TButton
            Left = 4
            Top = 4
            Width = 140
            Height = 25
            Action = actAvailAddToIntf
            TabOrder = 0
          end
          object btnProjectAddToImplementation: TButton
            Left = 152
            Top = 4
            Width = 150
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
          Width = 444
          Height = 325
          Align = alClient
          BevelOuter = bvNone
          BorderWidth = 3
          FullRepaint = False
          TabOrder = 0
          object sg_Common: TStringGrid
            Left = 3
            Top = 3
            Width = 438
            Height = 319
            Align = alClient
            ColCount = 1
            DefaultColWidth = 100
            DefaultRowHeight = 16
            FixedCols = 0
            RowCount = 1
            FixedRows = 0
            Options = [goFixedVertLine, goFixedHorzLine, goRangeSelect, goDrawFocusSelected]
            PopupMenu = pmuAvail
            TabOrder = 0
            OnDblClick = lbxAvailDblClick
            OnDragDrop = lbxAvailDragDrop
            OnDragOver = lbxAvailDragOver
            OnDrawCell = sg_AvailDrawCell
            OnMouseDown = sg_MouseDownForDragging
          end
        end
        object pnlCommonFooter: TPanel
          Left = 0
          Top = 325
          Width = 444
          Height = 34
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 1
          object btnCommonAddToInterface: TButton
            Left = 4
            Top = 4
            Width = 140
            Height = 25
            Action = actAvailAddToIntf
            TabOrder = 0
          end
          object btnCommonAddToImplementation: TButton
            Left = 152
            Top = 4
            Width = 150
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
          Width = 444
          Height = 296
          Align = alClient
          BevelOuter = bvNone
          BorderWidth = 3
          FullRepaint = False
          TabOrder = 0
          object sg_Favorite: TStringGrid
            Left = 3
            Top = 3
            Width = 438
            Height = 290
            Align = alClient
            ColCount = 1
            DefaultColWidth = 100
            DefaultRowHeight = 16
            FixedCols = 0
            RowCount = 1
            FixedRows = 0
            Options = [goFixedVertLine, goFixedHorzLine, goRangeSelect, goDrawFocusSelected]
            PopupMenu = pmuAvail
            TabOrder = 0
            OnDblClick = lbxAvailDblClick
            OnDragDrop = lbxAvailDragDrop
            OnDragOver = lbxAvailDragOver
            OnDrawCell = sg_AvailDrawCell
            OnMouseDown = sg_MouseDownForDragging
          end
        end
        object pnlFavFooter: TPanel
          Left = 0
          Top = 296
          Width = 444
          Height = 63
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 1
          object btnFavoriteAddToInterface: TButton
            Left = 4
            Top = 33
            Width = 140
            Height = 25
            Action = actAvailAddToIntf
            TabOrder = 2
          end
          object btnFavoriteAddToImplementation: TButton
            Left = 152
            Top = 33
            Width = 150
            Height = 25
            Action = actAvailAddToImpl
            TabOrder = 3
          end
          object btnFavoriteAddToFavorites: TButton
            Left = 152
            Top = 3
            Width = 150
            Height = 25
            Action = actFavAdd
            TabOrder = 1
          end
          object btnFavoriteDeleteFromFavorites: TButton
            Left = 4
            Top = 3
            Width = 140
            Height = 25
            Action = actFavDelete
            TabOrder = 0
          end
        end
      end
      object tabIdentifiers: TTabSheet
        Caption = 'Identifiers'
        ImageIndex = 4
        object sg_Identifiers: TStringGrid
          Left = 0
          Top = 0
          Width = 444
          Height = 325
          Align = alClient
          ColCount = 2
          DefaultColWidth = 150
          DefaultRowHeight = 16
          FixedCols = 0
          RowCount = 2
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goDrawFocusSelected, goColSizing, goRowSelect]
          TabOrder = 0
          OnDrawCell = sg_AvailDrawCell
          OnMouseDown = sg_MouseDownForDragging
        end
        object pnlIdentifiersFooter: TPanel
          Left = 0
          Top = 325
          Width = 444
          Height = 34
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 1
          object btnIdentifiersAddToIntf: TButton
            Left = 4
            Top = 4
            Width = 140
            Height = 25
            Action = actAvailAddToIntf
            TabOrder = 0
          end
          object btnIdentifiersAddToImpl: TButton
            Left = 152
            Top = 4
            Width = 150
            Height = 25
            Action = actAvailAddToImpl
            TabOrder = 1
          end
        end
      end
    end
    object pnlAvailableHeader: TPanel
      Left = 6
      Top = 6
      Width = 452
      Height = 41
      Align = alTop
      BevelOuter = bvNone
      FullRepaint = False
      TabOrder = 0
      OnResize = pnlAvailableHeaderResize
      DesignSize = (
        452
        41)
      object edtUnitFilter: TEdit
        Left = 200
        Top = 16
        Width = 250
        Height = 22
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        OnChange = edtUnitFilterChange
        OnEnter = edtUnitFilterEnter
        OnExit = edtUnitFilterExit
        OnKeyDown = edtUnitFilterKeyDown
      end
      object lblUnits: TPanel
        Left = 0
        Top = 0
        Width = 452
        Height = 15
        Align = alTop
        BevelOuter = bvNone
        Caption = 'Available Units'
        ParentColor = True
        TabOrder = 0
        object lblFilter: TLabel
          Left = 1
          Top = 0
          Width = 26
          Height = 14
          Caption = 'Filte&r'
          FocusControl = edtUnitFilter
        end
      end
      object edtIdentifierFilter: TEdit
        Left = 0
        Top = 16
        Width = 209
        Height = 22
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
        Visible = False
        OnChange = edtIdentifierFilterChange
        OnEnter = edtIdentifierFilterEnter
        OnExit = edtIdentifierFilterExit
        OnKeyDown = edtIdentifierFilterKeyDown
      end
    end
  end
  object pnlUses: TPanel
    Left = 0
    Top = 0
    Width = 297
    Height = 441
    Align = alLeft
    BevelOuter = bvNone
    BorderWidth = 6
    FullRepaint = False
    TabOrder = 0
    OnResize = pnlUsesResize
    object p_Interface: TPanel
      Left = 6
      Top = 21
      Width = 139
      Height = 347
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 2
      OnResize = pcUsesResize
      object p_InterfaceTitle: TPanel
        Left = 0
        Top = 0
        Width = 139
        Height = 15
        Align = alTop
        BevelOuter = bvNone
        Caption = 'I&nterface'
        ParentColor = True
        TabOrder = 1
      end
      object sg_Interface: TStringGrid
        Left = 0
        Top = 15
        Width = 139
        Height = 332
        Align = alClient
        ColCount = 1
        DefaultColWidth = 100
        DefaultRowHeight = 16
        FixedCols = 0
        RowCount = 1
        FixedRows = 0
        Options = [goFixedVertLine, goFixedHorzLine, goRangeSelect, goDrawFocusSelected]
        PopupMenu = pm_Intf
        TabOrder = 0
        OnDblClick = sg_InterfaceDblClick
        OnDragDrop = sg_InterfaceDragDrop
        OnDragOver = sg_InterfaceDragOver
        OnDrawCell = sg_UsedDrawCell
        OnMouseDown = sg_MouseDownForDragging
      end
    end
    object p_Implementation: TPanel
      Left = 145
      Top = 21
      Width = 146
      Height = 347
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 3
      OnResize = pcUsesResize
      object p_ImplementationTitle: TPanel
        Left = 0
        Top = 0
        Width = 146
        Height = 15
        Align = alTop
        BevelOuter = bvNone
        Caption = 'I&mplementation'
        ParentColor = True
        TabOrder = 1
      end
      object sg_Implementation: TStringGrid
        Left = 0
        Top = 15
        Width = 146
        Height = 332
        Align = alClient
        ColCount = 1
        DefaultColWidth = 100
        DefaultRowHeight = 16
        FixedCols = 0
        RowCount = 1
        FixedRows = 0
        Options = [goFixedVertLine, goFixedHorzLine, goRangeSelect, goDrawFocusSelected]
        PopupMenu = pm_Impl
        TabOrder = 0
        OnDblClick = sg_ImplementationDblClick
        OnDragDrop = sg_ImplementationDragDrop
        OnDragOver = sg_ImplementationDragOver
        OnDrawCell = sg_UsedDrawCell
        OnMouseDown = sg_MouseDownForDragging
      end
    end
    object lblUses: TPanel
      Left = 6
      Top = 6
      Width = 285
      Height = 15
      Align = alTop
      BevelOuter = bvNone
      Caption = 'Used Units '
      ParentColor = True
      TabOrder = 0
    end
    object pnlUsesBottom: TPanel
      Left = 6
      Top = 368
      Width = 285
      Height = 67
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      OnResize = pnlUsesBottomResize
      object btnAddDots: TButton
        Left = 0
        Top = 36
        Width = 137
        Height = 25
        Caption = 'Add Unit Prefixes'
        ParentShowHint = False
        ShowHint = False
        TabOrder = 0
        OnClick = btnAddDotsClick
      end
      object btnRemoveDots: TButton
        Left = 144
        Top = 36
        Width = 137
        Height = 25
        Caption = 'Remove Unit Prefixes'
        ParentShowHint = False
        ShowHint = False
        TabOrder = 1
        OnClick = btnRemoveDotsClick
      end
      object b_DeleteFromIntf: TButton
        Left = 0
        Top = 8
        Width = 65
        Height = 25
        Action = actIntfDelete
        ParentShowHint = False
        ShowHint = False
        TabOrder = 2
      end
      object b_DeleteFromImpl: TButton
        Left = 216
        Top = 8
        Width = 65
        Height = 25
        Action = actImplDelete
        ParentShowHint = False
        ShowHint = False
        TabOrder = 3
      end
      object b_MoveToImpl: TButton
        Left = 72
        Top = 8
        Width = 65
        Height = 25
        Action = actIntfMove
        ParentShowHint = False
        ShowHint = True
        TabOrder = 4
      end
      object b_MoveToIntf: TButton
        Left = 144
        Top = 8
        Width = 65
        Height = 25
        Action = actImplMove
        ParentShowHint = False
        ShowHint = True
        TabOrder = 5
      end
    end
  end
  object pnlFooter: TPanel
    Left = 0
    Top = 441
    Width = 764
    Height = 37
    Align = alBottom
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 2
    DesignSize = (
      764
      37)
    object chkSingleActionMode: TCheckBox
      Left = 8
      Top = 9
      Width = 402
      Height = 17
      Hint = 
        'If enabled, OK will add the currently selected unit on the right' +
        ' hand side to the uses clause shown on the left hand side and cl' +
        'ose the dialog.'
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Single action/quic&k add mode'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
    end
    object pnlButtonsRight: TPanel
      Left = 409
      Top = 0
      Width = 355
      Height = 37
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
      object btnCancel: TButton
        Left = 273
        Top = 4
        Width = 75
        Height = 25
        Cancel = True
        Caption = 'Cancel'
        ModalResult = 2
        TabOrder = 1
      end
      object btnOK: TButton
        Left = 189
        Top = 4
        Width = 75
        Height = 25
        Action = actOK
        Default = True
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
      end
      object btnOpen: TButton
        Left = 104
        Top = 4
        Width = 75
        Height = 25
        Action = actOpenUnit
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
      end
    end
  end
  object pm_Intf: TPopupMenu
    Left = 104
    Top = 56
    object m_IntfDelete: TMenuItem
      Action = actIntfDelete
      Default = True
    end
    object m_IntfMove: TMenuItem
      Action = actIntfMove
    end
    object m_IntfSep1: TMenuItem
      Caption = '-'
    end
    object m_IntfOpenUnit: TMenuItem
      Action = actOpenUnit
    end
    object m_IntfAddToFavorites: TMenuItem
      Action = actIntfAddToFavorites
    end
    object m_IntfSep2: TMenuItem
      Caption = '-'
    end
    object m_IntfUnalias: TMenuItem
      Action = actUnAlias
    end
  end
  object pm_Impl: TPopupMenu
    Left = 200
    Top = 56
    object m_ImplDelete: TMenuItem
      Action = actImplDelete
      Default = True
    end
    object m_ImplMove: TMenuItem
      Action = actImplMove
    end
    object m_ImplSep: TMenuItem
      Caption = '-'
    end
    object m_ImplOpenUnit: TMenuItem
      Action = actOpenUnit
    end
    object m_ImplAddToFavorites: TMenuItem
      Action = actImplAddToFavorites
    end
    object m_ImplSep2: TMenuItem
      Caption = '-'
    end
    object m_ImplUnAlias: TMenuItem
      Action = actUnAlias
    end
  end
  object pmuAvail: TPopupMenu
    Left = 296
    Top = 80
    object mitAvailAddToUses: TMenuItem
      Action = actAvailAddToImpl
      Default = True
    end
    object mitAvailSep1: TMenuItem
      Caption = '-'
    end
    object mitAvailOpenUnit: TMenuItem
      Action = actOpenUnit
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
    object actIntfDelete: TAction
      Category = 'Intf'
      Caption = 'Delete'
      OnExecute = actIntfDeleteExecute
    end
    object actImplDelete: TAction
      Category = 'Impl'
      Caption = 'Delete'
      OnExecute = actImplDeleteExecute
    end
    object actIntfMove: TAction
      Category = 'Intf'
      Caption = 'Move ->'
      OnExecute = actIntfMoveExecute
    end
    object actImplMove: TAction
      Category = 'Impl'
      Caption = '<- Move'
      OnExecute = actImplMoveExecute
    end
    object actFavDelete: TAction
      Category = 'Fav'
      Caption = 'D&elete from Favorites'
      ImageIndex = 42
      ShortCut = 46
      OnExecute = actFavDeleteExecute
    end
    object actFavAdd: TAction
      Category = 'Fav'
      Caption = '&Add to Favorites...'
      ImageIndex = 41
      ShortCut = 45
      OnExecute = actFavAddExecute
    end
    object actAvailAddToIntf: TAction
      Category = 'Avail'
      Caption = 'Add to Interfa&ce'
      OnExecute = actAvailAddToIntfExecute
    end
    object actAvailAddToImpl: TAction
      Category = 'Avail'
      Caption = 'Add to Imp&lementation'
      OnExecute = actAvailAddToImplExecute
    end
    object actOpenUnit: TAction
      Caption = 'Open Unit'
      Hint = 'Open selected unit (Ctrl+Double Click)'
      ImageIndex = 1
      ShortCut = 16463
      OnExecute = actOpenUnitExecute
    end
    object actIntfAddToFavorites: TAction
      Category = 'Intf'
      Caption = 'Add to Favorites'
      OnExecute = actIntfAddToFavoritesExecute
    end
    object actImplAddToFavorites: TAction
      Category = 'Impl'
      Caption = 'Add to Favorites'
      OnExecute = actImplAddToFavoritesExecute
    end
    object actUnAlias: TAction
      Caption = 'Unalias ...'
      OnExecute = actIntfUnAliasExecute
    end
    object actOK: TAction
      Caption = 'OK'
      Hint = 'OK'
      ShortCut = 16397
      OnExecute = actOKExecute
    end
    object actFocusInterface: TAction
      Caption = 'Focus Interface'
      ShortCut = 32846
      OnExecute = actFocusInterfaceExecute
    end
    object actFocusImplementation: TAction
      Caption = 'Focus Imlementation'
      ShortCut = 32845
      OnExecute = actFocusImplementationExecute
    end
  end
  object tim_Progress: TTimer
    Enabled = False
    Interval = 500
    OnTimer = tim_ProgressTimer
    Left = 376
    Top = 248
  end
end
