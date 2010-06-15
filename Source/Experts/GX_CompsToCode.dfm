object fmCompsToCode: TfmCompsToCode
  Left = 298
  Top = 212
  BorderStyle = bsDialog
  Caption = 'Components to Code'
  ClientHeight = 278
  ClientWidth = 298
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
    298
    278)
  PixelsPerInch = 96
  TextHeight = 14
  object btnOK: TButton
    Left = 53
    Top = 245
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object btnCancel: TButton
    Left = 134
    Top = 245
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object rgpBinProps: TRadioGroup
    Left = 8
    Top = 8
    Width = 282
    Height = 81
    Anchors = [akLeft, akTop, akRight]
    Caption = ' Binary Properties '
    ItemIndex = 1
    Items.Strings = (
      '&Skip'
      'Generate c&ommented code'
      'Generate &uncommented code')
    TabOrder = 0
  end
  object rgpLanguage: TRadioGroup
    Left = 8
    Top = 184
    Width = 282
    Height = 54
    Anchors = [akLeft, akRight, akBottom]
    Caption = ' Language '
    Columns = 2
    ItemIndex = 0
    Items.Strings = (
      '&Delphi'
      '&C++')
    TabOrder = 2
  end
  object gbxGenerated: TGroupBox
    Left = 8
    Top = 95
    Width = 282
    Height = 84
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = ' Generated Source '
    TabOrder = 1
    DesignSize = (
      282
      84)
    object chkPrepend: TCheckBox
      Left = 11
      Top = 18
      Width = 262
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Prepend original component source'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object chkUseDelphiWith: TCheckBox
      Left = 11
      Top = 38
      Width = 262
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Use Delphi &with statement'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object chkCreateFreeCode: TCheckBox
      Left = 11
      Top = 58
      Width = 262
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Generate code to &Free components'
      TabOrder = 2
    end
  end
  object btnHelp: TButton
    Left = 215
    Top = 245
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Help'
    TabOrder = 5
    OnClick = btnHelpClick
  end
end
