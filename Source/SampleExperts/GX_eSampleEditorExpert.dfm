object fmSampleEditorExpertConfig: TfmSampleEditorExpertConfig
  Left = 381
  Top = 212
  BorderStyle = bsDialog
  Caption = 'Sample Editor Expert Config'
  ClientHeight = 124
  ClientWidth = 301
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 14
  object lblNote: TLabel
    Left = 8
    Top = 12
    Width = 281
    Height = 16
    Alignment = taCenter
    AutoSize = False
    Caption = 'Well, this is just a sample. Not a lot here...'
  end
  object lblData: TLabel
    Left = 19
    Top = 48
    Width = 154
    Height = 14
    Alignment = taRightJustify
    Caption = 'Data is saved to the registry'
  end
  object btnOK: TButton
    Left = 74
    Top = 84
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object edtData: TEdit
    Left = 188
    Top = 44
    Width = 101
    Height = 22
    TabOrder = 0
  end
  object btnCancel: TButton
    Left = 162
    Top = 84
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
