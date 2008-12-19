object fmGrepOptions: TfmGrepOptions
  Left = 294
  Top = 263
  BorderStyle = bsDialog
  Caption = 'Grep Search Options'
  ClientHeight = 104
  ClientWidth = 335
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  DesignSize = (
    335
    104)
  PixelsPerInch = 96
  TextHeight = 13
  object gbxOptions: TGroupBox
    Left = 8
    Top = 8
    Width = 318
    Height = 55
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Grep Options'
    TabOrder = 0
    object chkGrepUseCurrentIdent: TCheckBox
      Left = 12
      Top = 22
      Width = 301
      Height = 17
      Caption = 'Use &current editor identifier as the default search string'
      TabOrder = 0
    end
  end
  object btnOK: TButton
    Left = 168
    Top = 72
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 251
    Top = 72
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
