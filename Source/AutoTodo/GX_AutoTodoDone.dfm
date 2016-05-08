object fmAutoTodoDone: TfmAutoTodoDone
  Left = 341
  Top = 244
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Comment Empty Code Blocks'
  ClientHeight = 80
  ClientWidth = 359
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    359
    80)
  PixelsPerInch = 96
  TextHeight = 13
  object lblMesssage: TLabel
    Left = 8
    Top = 16
    Width = 270
    Height = 13
    Caption = '%d comments have been inserted in empty code blocks.'
  end
  object btnOK: TButton
    Left = 278
    Top = 47
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object chkDontShowAgain: TCheckBox
    Left = 8
    Top = 55
    Width = 263
    Height = 17
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Do not show this message again'
    TabOrder = 1
  end
end
