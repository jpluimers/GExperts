object fmBackup: TfmBackup
  Left = 318
  Top = 175
  AutoScroll = False
  BorderIcons = [biSystemMenu]
  Caption = 'Backup Project'
  ClientHeight = 435
  ClientWidth = 600
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 400
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnActivate = FormActivate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 14
  object pnlButtons: TPanel
    Left = 0
    Top = 397
    Width = 600
    Height = 38
    Align = alBottom
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 1
    object pnlButtonsRight: TPanel
      Left = 334
      Top = 0
      Width = 266
      Height = 38
      Align = alRight
      BevelOuter = bvNone
      FullRepaint = False
      TabOrder = 0
      object btnBackup: TButton
        Left = 18
        Top = 3
        Width = 75
        Height = 26
        Caption = '&Backup'
        Default = True
        TabOrder = 0
        OnClick = btnBackupClick
      end
      object btnCancel: TButton
        Left = 100
        Top = 3
        Width = 75
        Height = 26
        Cancel = True
        Caption = 'Cancel'
        ModalResult = 2
        TabOrder = 1
      end
      object btnHelp: TButton
        Left = 182
        Top = 3
        Width = 75
        Height = 26
        Caption = '&Help'
        TabOrder = 2
        OnClick = btnHelpClick
      end
    end
  end
  object pnlFiles: TPanel
    Left = 0
    Top = 0
    Width = 600
    Height = 397
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 6
    FullRepaint = False
    TabOrder = 0
    object gbxFiles: TGroupBox
      Left = 6
      Top = 6
      Width = 588
      Height = 385
      Align = alClient
      Caption = ' &Files to Backup '
      TabOrder = 0
      object pnlFileList: TPanel
        Left = 2
        Top = 16
        Width = 584
        Height = 331
        Align = alClient
        BevelOuter = bvNone
        BorderWidth = 6
        FullRepaint = False
        TabOrder = 0
        object lbFiles: TListBox
          Left = 6
          Top = 6
          Width = 572
          Height = 319
          Align = alClient
          ItemHeight = 14
          MultiSelect = True
          TabOrder = 0
          OnKeyDown = lbFilesKeyDown
        end
      end
      object pnlFileButtons: TPanel
        Left = 2
        Top = 347
        Width = 584
        Height = 36
        Align = alBottom
        BevelOuter = bvNone
        FullRepaint = False
        TabOrder = 1
        DesignSize = (
          584
          36)
        object pnlFileButtonsRight: TPanel
          Left = 488
          Top = 0
          Width = 96
          Height = 36
          Align = alRight
          BevelOuter = bvNone
          FullRepaint = False
          TabOrder = 2
          object btnOptions: TButton
            Left = 14
            Top = 2
            Width = 75
            Height = 26
            Caption = '&Options...'
            TabOrder = 0
            OnClick = btnOptionsClick
          end
        end
        object btnRemove: TButton
          Left = 89
          Top = 2
          Width = 75
          Height = 26
          Anchors = [akLeft, akBottom]
          Caption = '&Remove'
          TabOrder = 1
          OnClick = btnRemoveClick
        end
        object btnAdd: TButton
          Left = 7
          Top = 2
          Width = 75
          Height = 26
          Anchors = [akLeft, akBottom]
          Caption = '&Add...'
          TabOrder = 0
          OnClick = btnAddClick
        end
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
