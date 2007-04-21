object fmMacroTemplateEdit: TfmMacroTemplateEdit
  Left = 374
  Top = 371
  BorderStyle = bsDialog
  Caption = 'Macro Template'
  ClientHeight = 172
  ClientWidth = 392
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblName: TLabel
    Left = 57
    Top = 15
    Width = 28
    Height = 13
    Alignment = taRightJustify
    Caption = '&Name'
    FocusControl = edtName
  end
  object lblDescription: TLabel
    Left = 32
    Top = 44
    Width = 53
    Height = 13
    Alignment = taRightJustify
    Caption = '&Description'
    FocusControl = edtDescription
  end
  object lblShortcut: TLabel
    Left = 45
    Top = 76
    Width = 40
    Height = 13
    Alignment = taRightJustify
    Caption = '&Shortcut'
  end
  object lblInsertPos: TLabel
    Left = 19
    Top = 108
    Width = 66
    Height = 13
    Alignment = taRightJustify
    Caption = '&Insert Position'
    FocusControl = cbxInsertPos
  end
  object edtName: TEdit
    Left = 95
    Top = 11
    Width = 148
    Height = 21
    TabOrder = 0
  end
  object edtDescription: TEdit
    Left = 95
    Top = 40
    Width = 280
    Height = 21
    TabOrder = 1
  end
  object btnOK: TButton
    Left = 117
    Top = 138
    Width = 70
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 3
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 205
    Top = 138
    Width = 70
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object cbxInsertPos: TComboBox
    Left = 95
    Top = 104
    Width = 148
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 2
    Items.Strings = (
      'Cursor Position'
      'Beginning of File'
      'Beginning of Line')
  end
end
