object fmGxMacroLibraryConfig: TfmGxMacroLibraryConfig
  Left = 257
  Top = 200
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Configure Macro Library'
  ClientHeight = 73
  ClientWidth = 218
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 14
  object chk_AutoPrompt: TCheckBox
    Left = 16
    Top = 16
    Width = 193
    Height = 17
    Caption = 'Auto-Prompt for Name'
    TabOrder = 0
  end
  object b_Ok: TButton
    Left = 56
    Top = 40
    Width = 75
    Height = 25
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object b_Cancel: TButton
    Left = 136
    Top = 40
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
