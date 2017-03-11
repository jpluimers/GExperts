inherited fmEditMacroItem: TfmEditMacroItem
  BorderStyle = bsDialog
  Caption = 'Edit Macro Item'
  ClientHeight = 177
  ClientWidth = 321
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object pnlEditGoesHere: TPanel
    Left = 24
    Top = 24
    Width = 289
    Height = 25
    BevelInner = bvLowered
    BevelOuter = bvLowered
    Caption = 'Edit field goes here'
    TabOrder = 1
  end
  object btnOK: TButton
    Left = 160
    Top = 144
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 7
  end
  object btnCancel: TButton
    Left = 240
    Top = 144
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 8
  end
  object rbText: TRadioButton
    Left = 8
    Top = 8
    Width = 305
    Height = 17
    Caption = '&Text'
    Checked = True
    TabOrder = 0
    TabStop = True
    OnClick = rbTextClick
  end
  object rbSpecial: TRadioButton
    Left = 8
    Top = 64
    Width = 305
    Height = 17
    Caption = 'Special &Key'
    TabOrder = 2
    OnClick = rbSpecialClick
  end
  object cmbSpecialKey: TComboBox
    Left = 24
    Top = 80
    Width = 289
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 3
    OnEnter = cmbSpecialKeyEnter
  end
  object chkModifierShift: TCheckBox
    Left = 112
    Top = 112
    Width = 81
    Height = 17
    Caption = '&Shift'
    TabOrder = 5
  end
  object chkModifierControl: TCheckBox
    Left = 24
    Top = 112
    Width = 81
    Height = 17
    Caption = '&Control'
    TabOrder = 4
  end
  object chkModifierAlt: TCheckBox
    Left = 200
    Top = 112
    Width = 81
    Height = 17
    Caption = '&Alt'
    TabOrder = 6
  end
end
