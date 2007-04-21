object fmGrepReplace: TfmGrepReplace
  Left = 378
  Top = 227
  BorderStyle = bsDialog
  Caption = 'Replace Matches'
  ClientHeight = 129
  ClientWidth = 368
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  DesignSize = (
    368
    129)
  PixelsPerInch = 96
  TextHeight = 13
  object lblWith: TLabel
    Left = 9
    Top = 33
    Width = 22
    Height = 13
    Caption = '&With'
    FocusControl = cbReplace
  end
  object lblIn: TLabel
    Left = 9
    Top = 57
    Width = 9
    Height = 13
    Caption = 'In'
  end
  object lblInString: TLabel
    Left = 61
    Top = 57
    Width = 300
    Height = 40
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    ShowAccelChar = False
    WordWrap = True
  end
  object lblReplace: TLabel
    Left = 9
    Top = 8
    Width = 40
    Height = 13
    Caption = 'Replace'
  end
  object lblReplaceString: TLabel
    Left = 61
    Top = 8
    Width = 3
    Height = 13
    ShowAccelChar = False
  end
  object cbReplace: TComboBox
    Left = 61
    Top = 30
    Width = 300
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 0
  end
  object btnOK: TButton
    Left = 127
    Top = 96
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 206
    Top = 96
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object btnHelp: TButton
    Left = 285
    Top = 96
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Help'
    TabOrder = 3
    OnClick = btnHelpClick
  end
end
