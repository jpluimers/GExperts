object fmUsesExpertOptions: TfmUsesExpertOptions
  Left = 338
  Top = 241
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Uses Clause Manager Options'
  ClientHeight = 97
  ClientWidth = 249
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    249
    97)
  PixelsPerInch = 96
  TextHeight = 13
  object chkSingleActionMode: TCheckBox
    Left = 8
    Top = 8
    Width = 233
    Height = 17
    Hint = 
      'If enabled, OK will add the currently selected unit on the right' +
      ' hand side to the uses clause shown on the left hand side and cl' +
      'ose the dialog.'
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Single action/quic&k add mode'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
  end
  object chkReplaceFileUnit: TCheckBox
    Left = 8
    Top = 32
    Width = 233
    Height = 17
    Hint = 
      'If enabled, the menu entry File -> Use Unit will call the GExper' +
      'ts Uses Clause Manager'
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Replace IDE File, Use Unit feature'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
  end
  object btnOK: TButton
    Left = 88
    Top = 64
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object btnCancel: TButton
    Left = 168
    Top = 64
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
end
