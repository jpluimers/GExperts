object fmCodeFormatterDone: TfmCodeFormatterDone
  Left = 264
  Top = 178
  ActiveControl = b_Ok
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Delphi Code Formatter'
  ClientHeight = 89
  ClientWidth = 313
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object l_Blurb: TLabel
    Left = 8
    Top = 8
    Width = 141
    Height = 13
    Caption = 'The file has been reformatted.'
  end
  object l_Twice: TLabel
    Left = 8
    Top = 32
    Width = 278
    Height = 13
    Caption = 'Note: To undo, you need to use the undo function TWICE.'
  end
  object chk_DontShowAgain: TCheckBox
    Left = 8
    Top = 64
    Width = 217
    Height = 17
    Caption = 'Do not show this message again.'
    TabOrder = 0
  end
  object b_Ok: TButton
    Left = 232
    Top = 56
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
end
