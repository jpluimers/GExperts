object fmBackup: TfmBackup
  Left = 318
  Top = 175
  Width = 481
  Height = 351
  BorderIcons = [biSystemMenu]
  Caption = 'Backup Project'
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 400
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnActivate = FormActivate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pnlButtons: TPanel
    Left = 0
    Top = 281
    Width = 473
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      473
      41)
    object btnBackup: TButton
      Left = 232
      Top = 6
      Width = 75
      Height = 26
      Anchors = [akRight, akBottom]
      Caption = '&Backup'
      Default = True
      TabOrder = 0
      OnClick = btnBackupClick
    end
    object btnCancel: TButton
      Left = 312
      Top = 6
      Width = 75
      Height = 26
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object btnHelp: TButton
      Left = 392
      Top = 6
      Width = 75
      Height = 26
      Anchors = [akRight, akBottom]
      Caption = '&Help'
      TabOrder = 2
      OnClick = btnHelpClick
    end
  end
  object pnlFiles: TPanel
    Left = 0
    Top = 0
    Width = 473
    Height = 281
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 5
    TabOrder = 1
    object gbxFiles: TGroupBox
      Left = 5
      Top = 5
      Width = 463
      Height = 271
      Align = alClient
      Caption = ' &Files to Backup '
      TabOrder = 0
      DesignSize = (
        463
        271)
      object lbFiles: TListBox
        Left = 8
        Top = 16
        Width = 447
        Height = 214
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 13
        MultiSelect = True
        TabOrder = 0
        OnKeyDown = lbFilesKeyDown
      end
      object btnAdd: TButton
        Left = 7
        Top = 236
        Width = 75
        Height = 26
        Anchors = [akLeft, akBottom]
        Caption = '&Add...'
        TabOrder = 1
        OnClick = btnAddClick
      end
      object btnRemove: TButton
        Left = 87
        Top = 236
        Width = 75
        Height = 26
        Anchors = [akLeft, akBottom]
        Caption = '&Remove'
        TabOrder = 2
        OnClick = btnRemoveClick
      end
      object btnOptions: TButton
        Left = 380
        Top = 236
        Width = 75
        Height = 26
        Anchors = [akRight, akBottom]
        Caption = '&Options...'
        TabOrder = 3
        OnClick = btnOptionsClick
      end
    end
  end
  object dlgSave: TSaveDialog
    DefaultExt = 'zip'
    Filter = 'Zip Files (*.zip)|*.zip|All Files (*.*)|*.*'
    Title = 'Backup As'
    Left = 96
    Top = 48
  end
  object dlgOpen: TOpenDialog
    Filter = 
      'Delphi Files (*.pas;*.dfm;*.xfm;*.dpr;*.dpk;*.bpg;*.res)|*.pas;*' +
      '.dfm;*.xfm;*.dpr;*.dpk;*.bpg;*.res|All Files (*.*)|*.*'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofPathMustExist, ofFileMustExist]
    Title = 'Add to Backup'
    Left = 48
    Top = 48
  end
end
