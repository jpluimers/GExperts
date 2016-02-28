object fmAutoTodoDone: TfmAutoTodoDone
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Auto TODOs'
  ClientHeight = 89
  ClientWidth = 313
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object l_Blurb: TLabel
    Left = 8
    Top = 16
    Width = 147
    Height = 13
    Caption = '%d todos have been inserted.'
  end
  object b_OK: TButton
    Left = 232
    Top = 56
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object chk_DontShowAgain: TCheckBox
    Left = 8
    Top = 64
    Width = 217
    Height = 17
    Caption = 'Do not show this message again.'
    TabOrder = 1
  end
end
