object fmQuoteConfig: TfmQuoteConfig
  Left = 311
  Top = 211
  BorderStyle = bsDialog
  Caption = 'Quote Expert'
  ClientHeight = 119
  ClientWidth = 287
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  DesignSize = (
    287
    119)
  PixelsPerInch = 96
  TextHeight = 13
  object gbxStyle: TGroupBox
    Left = 9
    Top = 9
    Width = 269
    Height = 64
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Style'
    TabOrder = 0
    DesignSize = (
      269
      64)
    object lblEndOfLine: TLabel
      Left = 16
      Top = 27
      Width = 56
      Height = 13
      Caption = 'End Of Line'
    end
    object cbxEndOfLine: TComboBox
      Left = 94
      Top = 24
      Width = 164
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      TabOrder = 0
    end
  end
  object btnOK: TButton
    Left = 122
    Top = 86
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 203
    Top = 86
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
