object fmUsesExpertOptions: TfmUsesExpertOptions
  Left = 0
  Top = 0
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
  Position = poOwnerFormCenter
  DesignSize = (
    249
    97)
  PixelsPerInch = 96
  TextHeight = 13
  object chkSingleActionMode: TCheckBox
    Left = 8
    Top = 8
    Width = 0
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Single action/quic&k add mode'
    TabOrder = 0
  end
  object chkReplaceFileUnit: TCheckBox
    Left = 8
    Top = 32
    Width = 233
    Height = 17
    Caption = 'Replace File->Use Unit'
    TabOrder = 1
  end
  object btnOK: TButton
    Left = 88
    Top = 64
    Width = 75
    Height = 25
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
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object CheckBox1: TCheckBox
    Left = 8
    Top = 8
    Width = 233
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Single action/quic&k add mode'
    TabOrder = 4
  end
end
