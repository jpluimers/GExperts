object fmSampleEditorExpertConfig: TfmSampleEditorExpertConfig
  Left = 381
  Top = 212
  BorderStyle = bsDialog
  Caption = 'Sample Editor Expert Config'
  ClientHeight = 124
  ClientWidth = 290
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object lblNote: TLabel
    Left = 44
    Top = 12
    Width = 196
    Height = 13
    Caption = 'Well, this is just a sample. Not a lot here...'
  end
  object lblData: TLabel
    Left = 12
    Top = 48
    Width = 152
    Height = 13
    Caption = 'This data is saved to the registry'
  end
  object btnOK: TButton
    Left = 64
    Top = 84
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object edtData: TEdit
    Left = 172
    Top = 44
    Width = 109
    Height = 21
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 152
    Top = 84
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
