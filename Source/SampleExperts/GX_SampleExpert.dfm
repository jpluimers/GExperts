object fmGxSampleExpertForm: TfmGxSampleExpertForm
  Left = 381
  Top = 212
  BorderStyle = bsDialog
  Caption = 'GExperts Sample Expert'
  ClientHeight = 124
  ClientWidth = 290
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
    Width = 273
    Height = 15
    Alignment = taCenter
    AutoSize = False
    Caption = 'Well, this is just a sample. Not a lot here...'
  end
  object lblData: TLabel
    Left = 13
    Top = 48
    Width = 154
    Height = 14
    Alignment = taRightJustify
    Caption = 'Data is saved to the registry'
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
    Left = 180
    Top = 44
    Width = 93
    Height = 22
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
