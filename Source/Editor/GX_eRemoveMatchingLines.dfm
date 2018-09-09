object fmRemoveMatchingLinesExpertConfig: TfmRemoveMatchingLinesExpertConfig
  Left = 381
  Top = 212
  BorderIcons = [biSystemMenu]
  Caption = 'Remove Matching Lines Expert Config'
  ClientHeight = 257
  ClientWidth = 377
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  DesignSize = (
    377
    257)
  PixelsPerInch = 96
  TextHeight = 14
  object l_Lines: TLabel
    Left = 8
    Top = 7
    Width = 167
    Height = 14
    Caption = 'Lines to remove (one per line)'
  end
  object m_Lines: TMemo
    Left = 8
    Top = 24
    Width = 361
    Height = 169
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    WordWrap = False
  end
  object b_OK: TButton
    Left = 216
    Top = 224
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 3
  end
  object b_Cancel: TButton
    Left = 296
    Top = 224
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object b_Defaults: TButton
    Left = 8
    Top = 224
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Defaults'
    TabOrder = 2
    OnClick = b_DefaultsClick
  end
  object chk_RegEx: TCheckBox
    Left = 8
    Top = 200
    Width = 361
    Height = 17
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Regular Expression'
    TabOrder = 1
  end
end
