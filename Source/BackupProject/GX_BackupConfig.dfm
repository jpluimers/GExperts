object fmBackupConfig: TfmBackupConfig
  Left = 482
  Top = 167
  BorderStyle = bsDialog
  Caption = 'Backup Project Configuration'
  ClientHeight = 489
  ClientWidth = 409
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object gbxOptions: TGroupBox
    Left = 8
    Top = 8
    Width = 393
    Height = 433
    Caption = 'Configuration'
    TabOrder = 0
    object lblDirectives: TLabel
      Left = 31
      Top = 38
      Width = 295
      Height = 14
      Caption = '{$I,  {$R, {#BACKUP, and #pragma backup directives'
    end
    object cbBackupInc: TCheckBox
      Left = 12
      Top = 20
      Width = 342
      Height = 17
      Caption = '&Scan pas/dpr/cpp files for files included by the'
      TabOrder = 0
      OnClick = cbBackupIncClick
    end
    object cbIncludeDir: TCheckBox
      Left = 12
      Top = 85
      Width = 342
      Height = 17
      Caption = '&Include directory names in backup'
      TabOrder = 2
    end
    object rgDefaultScope: TRadioGroup
      Left = 12
      Top = 357
      Width = 373
      Height = 65
      Caption = 'Default Backup Sc&ope'
      Items.Strings = (
        'Acti&ve project'
        'Project &group')
      TabOrder = 5
    end
    object gbBackupTarget: TGroupBox
      Left = 12
      Top = 236
      Width = 373
      Height = 113
      Caption = 'Backup &File Name'
      TabOrder = 4
      object lblBackupDir: TLabel
        Left = 25
        Top = 65
        Width = 152
        Height = 14
        Caption = '&Directory and file to save to'
        FocusControl = edBackupDir
      end
      object btnBackupDir: TButton
        Left = 344
        Top = 84
        Width = 21
        Height = 21
        Caption = '...'
        TabOrder = 5
        OnClick = btnBackupDirClick
      end
      object rbBackupAskForFile: TRadioButton
        Left = 8
        Top = 22
        Width = 313
        Height = 17
        Caption = '&Prompt for file name'
        TabOrder = 0
        OnClick = rbGenericBackupTargetClick
      end
      object rbBackupToDirectory: TRadioButton
        Tag = 1
        Left = 8
        Top = 44
        Width = 313
        Height = 17
        Caption = '&Auto-save to directory'
        TabOrder = 1
        OnClick = rbGenericBackupTargetClick
      end
      object edBackupDir: TEdit
        Left = 25
        Top = 83
        Width = 312
        Height = 22
        TabOrder = 4
      end
      object btnDefault: TButton
        Left = 290
        Top = 56
        Width = 75
        Height = 25
        Caption = 'Default'
        TabOrder = 3
        OnClick = btnDefaultClick
      end
      object btnVariables: TButton
        Left = 208
        Top = 56
        Width = 75
        Height = 25
        Caption = '&Variables'
        TabOrder = 2
      end
    end
    object cbSearchOnLibraryPath: TCheckBox
      Left = 28
      Top = 57
      Width = 326
      Height = 17
      Caption = 'Search &library path for included files'
      TabOrder = 1
    end
    object gbDropDirs: TGroupBox
      Left = 12
      Top = 112
      Width = 373
      Height = 113
      Caption = 'When dropping directories'
      TabOrder = 3
      object cbAddRecursively: TCheckBox
        Left = 8
        Top = 46
        Width = 321
        Height = 17
        Caption = 'Recurse into subdirectories'
        TabOrder = 1
        OnClick = cbAddRecursivelyClick
      end
      object cbIgnoreHistoryDir: TCheckBox
        Left = 24
        Top = 66
        Width = 305
        Height = 17
        Caption = 'Ignore __history and __recovery'
        Enabled = False
        TabOrder = 2
      end
      object cbIgnoreScmDirs: TCheckBox
        Left = 24
        Top = 88
        Width = 305
        Height = 17
        Caption = 'Ignore .svn .hg .git'
        Enabled = False
        TabOrder = 3
      end
      object cbIgnoreBackupFiles: TCheckBox
        Left = 8
        Top = 24
        Width = 313
        Height = 17
        Caption = 'Ingore backup files (*.~*)'
        TabOrder = 0
      end
    end
  end
  object btnOK: TButton
    Left = 248
    Top = 456
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object btnCancel: TButton
    Left = 328
    Top = 456
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object btHelp: TButton
    Left = 8
    Top = 455
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Help'
    TabOrder = 1
    OnClick = btHelpClick
  end
  object pmuVariables: TPopupMenu
    Left = 264
    Top = 256
  end
end
