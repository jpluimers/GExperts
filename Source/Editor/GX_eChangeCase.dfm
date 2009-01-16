object fmChangeCase: TfmChangeCase
  Left = 418
  Top = 208
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Change Case'
  ClientHeight = 176
  ClientWidth = 181
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 14
  object grpCaseSelection: TRadioGroup
    Left = 11
    Top = 8
    Width = 161
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
    Left = 10
    Top = 141
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 96
    Top = 141
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
