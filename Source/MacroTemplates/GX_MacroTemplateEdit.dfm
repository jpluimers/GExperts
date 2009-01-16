object fmMacroTemplateEdit: TfmMacroTemplateEdit
  Left = 374
  Top = 371
  BorderStyle = bsDialog
  Caption = 'Macro Template'
  ClientHeight = 172
  ClientWidth = 392
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 14
  object lblName: TLabel
    Left = 58
    Top = 15
    Width = 31
    Height = 14
    Alignment = taRightJustify
    Caption = '&Name'
    FocusControl = edtName
  end
  object lblDescription: TLabel
    Left = 29
    Top = 44
    Width = 60
    Height = 14
    Alignment = taRightJustify
    Caption = '&Description'
    FocusControl = edtDescription
  end
  object lblShortcut: TLabel
    Left = 41
    Top = 76
    Width = 48
    Height = 14
    Alignment = taRightJustify
    Caption = '&Shortcut'
  end
  object lblInsertPos: TLabel
    Left = 11
    Top = 108
    Width = 78
    Height = 14
    Alignment = taRightJustify
    Caption = '&Insert Position'
    FocusControl = cbxInsertPos
  end
  object edtName: TEdit
    Left = 99
    Top = 11
    Width = 148
    Height = 22
    TabOrder = 0
  end
  object edtDescription: TEdit
    Left = 99
    Top = 40
    Width = 280
    Height = 22
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
    Left = 99
    Top = 104
    Width = 148
    Height = 22
    Style = csDropDownList
    ItemHeight = 14
    TabOrder = 2
    Items.Strings = (
      'Cursor Position'
      'Beginning of File'
      'Beginning of Line')
  end
end
