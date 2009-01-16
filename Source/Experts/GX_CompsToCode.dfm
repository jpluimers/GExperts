object fmCompsToCode: TfmCompsToCode
  Left = 298
  Top = 212
  BorderStyle = bsDialog
  Caption = 'Components to Code'
  ClientHeight = 271
  ClientWidth = 288
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  DesignSize = (
    288
    271)
  PixelsPerInch = 96
  TextHeight = 14
  object btnOK: TButton
    Left = 21
    Top = 239
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object btnCancel: TButton
    Left = 107
    Top = 239
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object rgpBinProps: TRadioGroup
    Left = 7
    Top = 8
    Width = 274
    Height = 81
    Anchors = [akLeft, akTop, akRight]
    Caption = ' Binary Properties '
    ItemIndex = 1
    Items.Strings = (
      '&Skip'
      'Generate &commented code'
      'Generate &uncommented code')
    TabOrder = 0
  end
  object rgpLanguage: TRadioGroup
    Left = 7
    Top = 168
    Width = 274
    Height = 61
    Anchors = [akLeft, akTop, akRight]
    Caption = ' Language '
    ItemIndex = 0
    Items.Strings = (
      '&Delphi'
      '&C++')
    TabOrder = 2
  end
  object gbxGenerated: TGroupBox
    Left = 7
    Top = 96
    Width = 274
    Height = 63
    Anchors = [akLeft, akTop, akRight]
    Caption = ' Generated Source '
    TabOrder = 1
    object chkPrepend: TCheckBox
      Left = 11
      Top = 18
      Width = 259
      Height = 17
      Caption = '&Prepend original component source'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object chkUseDelphiWith: TCheckBox
      Left = 11
      Top = 38
      Width = 261
      Height = 17
      Caption = 'Use Delphi '#39'&with'#39' statement'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
  end
  object btnHelp: TButton
    Left = 193
    Top = 238
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Help'
    TabOrder = 5
    OnClick = btnHelpClick
  end
end
