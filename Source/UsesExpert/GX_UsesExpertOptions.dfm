object fmUsesExpertOptions: TfmUsesExpertOptions
  Left = 338
  Top = 241
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Uses Clause Manager Options'
  ClientHeight = 121
  ClientWidth = 289
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    289
    121)
  PixelsPerInch = 96
  TextHeight = 13
  object chkReadMap: TCheckBox
    Left = 8
    Top = 8
    Width = 273
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Read project units from map file rather than .dpr'
    TabOrder = 0
  end
  object chkReplaceFileUnit: TCheckBox
    Left = 8
    Top = 32
    Width = 273
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
  object chkParseAll: TCheckBox
    Left = 8
    Top = 56
    Width = 273
    Height = 17
    Caption = 'Parse all units, not just the Favorites'
    TabOrder = 2
  end
  object btnOK: TButton
    Left = 128
    Top = 88
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object btnCancel: TButton
    Left = 208
    Top = 88
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
end
