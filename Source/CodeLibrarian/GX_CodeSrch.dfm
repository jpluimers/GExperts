object fmCodeSearch: TfmCodeSearch
  Left = 280
  Top = 201
  BorderStyle = bsDialog
  Caption = 'Code Librarian Search'
  ClientHeight = 143
  ClientWidth = 279
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
  PixelsPerInch = 96
  TextHeight = 14
  object lblFind: TLabel
    Left = 12
    Top = 14
    Width = 66
    Height = 14
    Alignment = taRightJustify
    Caption = '&Text to find'
    FocusControl = edSearch
  end
  object edSearch: TEdit
    Left = 87
    Top = 10
    Width = 186
    Height = 22
    TabOrder = 0
  end
  object gbxOptions: TGroupBox
    Left = 8
    Top = 40
    Width = 265
    Height = 65
    Caption = 'Options'
    TabOrder = 1
    object cbCaseSensitive: TCheckBox
      Left = 8
      Top = 21
      Width = 200
      Height = 17
      Caption = '&Case sensitive'
      TabOrder = 0
    end
    object cbWholeWord: TCheckBox
      Left = 8
      Top = 40
      Width = 200
      Height = 17
      Caption = '&Whole words only'
      TabOrder = 1
    end
  end
  object btnOK: TButton
    Left = 116
    Top = 112
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object btnCancel: TButton
    Left = 198
    Top = 112
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
end
