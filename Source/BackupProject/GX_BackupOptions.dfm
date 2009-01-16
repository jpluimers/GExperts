object fmBackupOptions: TfmBackupOptions
  Left = 373
  Top = 167
  BorderStyle = bsDialog
  Caption = 'Backup Project Options'
  ClientHeight = 220
  ClientWidth = 280
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 14
  object gbBackupOptions: TGroupBox
    Left = 8
    Top = 8
    Width = 265
    Height = 172
    Caption = 'Backup Options'
    TabOrder = 0
    object lPassword: TLabel
      Left = 29
      Top = 46
      Width = 51
      Height = 14
      Alignment = taRightJustify
      Caption = '&Password'
      FocusControl = edPassword
    end
    object cbPassword: TCheckBox
      Left = 8
      Top = 23
      Width = 253
      Height = 17
      Caption = '&Encrypt archive'
      TabOrder = 0
      OnClick = cbPasswordClick
    end
    object edPassword: TEdit
      Left = 88
      Top = 42
      Width = 169
      Height = 22
      Color = clBtnFace
      Enabled = False
      TabOrder = 1
    end
    object rgScope: TRadioGroup
      Left = 9
      Top = 100
      Width = 247
      Height = 62
      Caption = 'Sc&ope'
      Items.Strings = (
        'Active project'
        'Project group')
      TabOrder = 3
    end
    object cbSearchLibraryPath: TCheckBox
      Left = 8
      Top = 72
      Width = 253
      Height = 17
      Caption = '&Search library path for included files'
      TabOrder = 2
    end
  end
  object btnOK: TButton
    Left = 113
    Top = 188
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 197
    Top = 188
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
