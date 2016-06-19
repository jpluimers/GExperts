object fmAlign: TfmAlign
  Left = 459
  Top = 281
  ActiveControl = lstTokens
  AutoScroll = False
  BorderIcons = [biSystemMenu]
  Caption = 'Align Lines'
  ClientHeight = 273
  ClientWidth = 265
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
    265
    273)
  PixelsPerInch = 96
  TextHeight = 14
  object lblToken: TLabel
    Left = 8
    Top = 8
    Width = 84
    Height = 14
    Caption = '&Align on token:'
    FocusControl = lstTokens
  end
  object btnOK: TButton
    Left = 104
    Top = 240
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object btnCancel: TButton
    Left = 184
    Top = 240
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object lstTokens: TListBox
    Left = 8
    Top = 24
    Width = 249
    Height = 177
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 14
    TabOrder = 0
    OnDblClick = lstTokensDblClick
  end
  object cbxMode: TComboBox
    Left = 8
    Top = 208
    Width = 248
    Height = 22
    Style = csDropDownList
    Anchors = [akLeft, akRight, akBottom]
    ItemHeight = 14
    ItemIndex = 0
    TabOrder = 1
    Text = 'Align at rightmost token'
    Items.Strings = (
      'Align at rightmost token'
      'Align at first token')
  end
  object btnConfig: TButton
    Left = 8
    Top = 240
    Width = 89
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Configure ...'
    TabOrder = 2
    OnClick = btnConfigClick
  end
end
