object fmMacroTemplateEdit: TfmMacroTemplateEdit
  Left = 374
  Top = 371
  BorderStyle = bsDialog
  Caption = 'Macro Template'
  ClientHeight = 172
  ClientWidth = 466
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 14
  object lblName: TLabel
    Left = 38
    Top = 15
    Width = 84
    Height = 14
    Alignment = taRightJustify
    Caption = 'Keyword/&Name'
    FocusControl = edtName
  end
  object lblDescription: TLabel
    Left = 62
    Top = 44
    Width = 60
    Height = 14
    Alignment = taRightJustify
    Caption = '&Description'
    FocusControl = edtDescription
  end
  object lblShortcut: TLabel
    Left = 74
    Top = 76
    Width = 48
    Height = 14
    Alignment = taRightJustify
    Caption = '&Shortcut'
    FocusControl = edtShortCut
  end
  object lblInsertPos: TLabel
    Left = 44
    Top = 108
    Width = 78
    Height = 14
    Alignment = taRightJustify
    Caption = '&Insert Position'
    FocusControl = cbxInsertPos
  end
  object edtName: TEdit
    Left = 132
    Top = 11
    Width = 135
    Height = 22
    TabOrder = 0
  end
  object edtDescription: TEdit
    Left = 132
    Top = 40
    Width = 309
    Height = 22
    TabOrder = 1
  end
  object btnOK: TButton
    Left = 154
    Top = 138
    Width = 70
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 4
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 242
    Top = 138
    Width = 70
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object cbxInsertPos: TComboBox
    Left = 132
    Top = 104
    Width = 135
    Height = 22
    Style = csDropDownList
    ItemHeight = 14
    TabOrder = 3
    Items.Strings = (
      'Cursor Position'
      'Beginning of File'
      'Beginning of Line')
  end
  object edtShortCut: THotKey
    Left = 132
    Top = 72
    Width = 135
    Height = 22
    HotKey = 32833
    InvalidKeys = [hcNone, hcShift]
    Modifiers = [hkAlt]
    TabOrder = 2
  end
end
