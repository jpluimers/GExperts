object fmReplaceCompMapGrpList: TfmReplaceCompMapGrpList
  Left = 279
  Top = 250
  Width = 371
  Height = 322
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Replace Components - Mapping Groups'
  Color = clBtnFace
  Constraints.MinWidth = 265
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlMain: TPanel
    Left = 0
    Top = 0
    Width = 363
    Height = 257
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 0
    object pnlButtons: TPanel
      Left = 269
      Top = 3
      Width = 91
      Height = 251
      Align = alRight
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 1
      object btnAdd: TButton
        Left = 9
        Top = 11
        Width = 75
        Height = 25
        Caption = 'Add...'
        TabOrder = 0
        OnClick = btnAddClick
      end
      object btnEdit: TButton
        Left = 9
        Top = 38
        Width = 75
        Height = 25
        Caption = 'Edit...'
        TabOrder = 1
        OnClick = btnEditClick
      end
      object btnDelete: TButton
        Left = 9
        Top = 65
        Width = 75
        Height = 25
        Caption = 'Delete...'
        TabOrder = 2
        OnClick = btnDeleteClick
      end
      object btnExport: TButton
        Left = 9
        Top = 119
        Width = 75
        Height = 25
        Caption = 'Export...'
        TabOrder = 4
        OnClick = btnExportClick
      end
      object btnImport: TButton
        Left = 9
        Top = 92
        Width = 75
        Height = 25
        Caption = 'Import...'
        TabOrder = 3
        OnClick = btnImportClick
      end
      object btnClear: TButton
        Left = 9
        Top = 146
        Width = 75
        Height = 25
        Caption = 'Clear...'
        TabOrder = 5
        OnClick = btnClearClick
      end
    end
    object pnlList: TPanel
      Left = 3
      Top = 3
      Width = 266
      Height = 251
      Align = alClient
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 0
      object lbxGroups: TListBox
        Left = 3
        Top = 3
        Width = 260
        Height = 245
        Align = alClient
        Constraints.MinHeight = 180
        Constraints.MinWidth = 127
        ItemHeight = 13
        MultiSelect = True
        TabOrder = 0
        OnClick = lbxGroupsClick
        OnDblClick = lbxGroupsDblClick
      end
    end
  end
  object pnlFooter: TPanel
    Left = 0
    Top = 257
    Width = 363
    Height = 36
    Align = alBottom
    BevelOuter = bvNone
    Constraints.MinHeight = 31
    Constraints.MinWidth = 237
    FullRepaint = False
    TabOrder = 1
    object pnlFooterButtons: TPanel
      Left = 99
      Top = 0
      Width = 264
      Height = 36
      Align = alRight
      BevelOuter = bvNone
      FullRepaint = False
      TabOrder = 0
      object btnOk: TButton
        Left = 96
        Top = 4
        Width = 75
        Height = 25
        Caption = 'OK'
        Default = True
        ModalResult = 1
        TabOrder = 0
      end
      object btnCancel: TButton
        Left = 180
        Top = 4
        Width = 75
        Height = 25
        Cancel = True
        Caption = 'Cancel'
        ModalResult = 2
        TabOrder = 1
      end
    end
  end
  object dlgGetImportFile: TOpenDialog
    DefaultExt = 'xml'
    Filter = 'XML Files (*.xml)|*.xml|All Files (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Select an import file'
    Left = 48
    Top = 144
  end
  object dlgGetExportFile: TSaveDialog
    DefaultExt = 'xml'
    Filter = 'XML Files (*.xml)|*.xml|All Files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Title = 'Select a file write the the exported data to'
    Left = 136
    Top = 144
  end
end
