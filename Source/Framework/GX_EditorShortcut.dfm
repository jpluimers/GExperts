object fmEditorShortcut: TfmEditorShortcut
  Left = 304
  Top = 219
  BorderStyle = bsDialog
  Caption = 'Define Shortcut'
  ClientHeight = 129
  ClientWidth = 306
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  DesignSize = (
    306
    129)
  PixelsPerInch = 96
  TextHeight = 14
  object gbxShortcut: TGroupBox
    Left = 8
    Top = 8
    Width = 289
    Height = 81
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Editor Expert Name'
    TabOrder = 0
    DesignSize = (
      289
      81)
    object lblShortCut: TLabel
      Left = 9
      Top = 24
      Width = 48
      Height = 14
      Caption = '&Shortcut'
      FocusControl = hkyShortCut
    end
    object hkyShortCut: THotKey
      Left = 9
      Top = 47
      Width = 156
      Height = 22
      Anchors = [akLeft, akTop, akRight]
      HotKey = 32833
      InvalidKeys = [hcNone, hcShift]
      Modifiers = [hkAlt]
      TabOrder = 0
    end
    object btnDefault: TButton
      Left = 174
      Top = 45
      Width = 101
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Default'
      TabOrder = 1
      OnClick = btnDefaultClick
    end
  end
  object btnCancel: TButton
    Left = 224
    Top = 96
    Width = 73
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object btnOK: TButton
    Left = 144
    Top = 96
    Width = 73
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
end
