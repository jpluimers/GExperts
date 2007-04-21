object fmProjDependFilter: TfmProjDependFilter
  Left = 333
  Top = 160
  Width = 221
  Height = 400
  BorderIcons = [biSystemMenu]
  Caption = 'Unit Filter'
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 221
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlFooter: TPanel
    Left = 0
    Top = 339
    Width = 213
    Height = 32
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btnCancel: TButton
      Left = 119
      Top = 0
      Width = 75
      Height = 25
      Anchors = [akTop]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object btnOK: TButton
      Left = 19
      Top = 0
      Width = 75
      Height = 25
      Anchors = [akTop]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
  end
  object pnlFilters: TPanel
    Left = 0
    Top = 0
    Width = 213
    Height = 339
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object lblExcluded: TLabel
      Left = 8
      Top = 6
      Width = 71
      Height = 13
      Caption = 'E&xcluded Units'
      FocusControl = lbFilter
    end
    object btnAdd: TButton
      Left = 157
      Top = 307
      Width = 49
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Add'
      TabOrder = 2
      OnClick = btnAddClick
    end
    object edtUnitName: TEdit
      Left = 8
      Top = 309
      Width = 142
      Height = 21
      Anchors = [akLeft, akRight, akBottom]
      TabOrder = 1
      OnChange = edtUnitNameChange
    end
    object lbFilter: TListBox
      Left = 8
      Top = 22
      Width = 198
      Height = 279
      Anchors = [akLeft, akTop, akRight, akBottom]
      ItemHeight = 13
      MultiSelect = True
      PopupMenu = mnuPopup
      Sorted = True
      TabOrder = 0
    end
  end
  object dlgOpen: TOpenDialog
    Filter = 'Pascal Files (*.pas)|*.pas'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofEnableSizing]
    Title = 'Filter Files'
    Left = 52
    Top = 32
  end
  object mnuPopup: TPopupMenu
    OnPopup = mnuPopupPopup
    Left = 16
    Top = 32
    object mitAddFiles: TMenuItem
      Caption = 'Add Files...'
      ShortCut = 45
      OnClick = mitAddFilesClick
    end
    object mitDeleteFiles: TMenuItem
      Caption = 'Delete Selected...'
      ShortCut = 46
      OnClick = mitDeleteFilesClick
    end
    object mitClear: TMenuItem
      Caption = '&Clear List...'
      ShortCut = 16430
      OnClick = mitClearClick
    end
    object mitSep1: TMenuItem
      Caption = '-'
    end
    object mitSave: TMenuItem
      Caption = 'Save...'
      ShortCut = 16467
      OnClick = mitSaveClick
    end
    object mitLoad: TMenuItem
      Caption = 'Load...'
      ShortCut = 16463
      OnClick = mitLoadClick
    end
  end
  object dlgFilterOpen: TOpenDialog
    DefaultExt = 'flt'
    Filter = 'Filter Files (*.flt)|*.flt'
    Title = 'Load Filter'
    Left = 88
    Top = 32
  end
  object dlgFilterSave: TSaveDialog
    DefaultExt = 'flt'
    Filter = 'Filter Files (*.flt)|*.flt'
    Title = 'Save Filter'
    Left = 124
    Top = 32
  end
end
