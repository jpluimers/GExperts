object fmChangeCase: TfmChangeCase
  Left = 418
  Top = 208
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Change Case'
  ClientHeight = 170
  ClientWidth = 173
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object grpCaseSelection: TRadioGroup
    Left = 10
    Top = 7
    Width = 155
    Height = 125
    Caption = 'Change Case'
    Items.Strings = (
      '&lowercase'
      '&UPPERCASE'
      '&Title Case'
      '&Sentence case'
      't&OGGLE cASE')
    TabOrder = 0
  end
  object btnOK: TButton
    Left = 9
    Top = 137
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 89
    Top = 137
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
