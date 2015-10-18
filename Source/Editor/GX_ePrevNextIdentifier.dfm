object fmPrevNextConfig: TfmPrevNextConfig
  Left = 311
  Top = 211
  BorderStyle = bsDialog
  Caption = 'Prev/Next Identifier Options'
  ClientHeight = 136
  ClientWidth = 312
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
    312
    136)
  PixelsPerInch = 96
  TextHeight = 14
  object gbxPrevNextOptions: TGroupBox
    Left = 8
    Top = 8
    Width = 297
    Height = 89
    Caption = 'Prev/Next Identifier Options'
    TabOrder = 0
    object rbNewMode: TRadioButton
      Left = 8
      Top = 29
      Width = 287
      Height = 17
      Caption = 'New mode: next identifier always in the middle'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object rbOldMode: TRadioButton
      Left = 8
      Top = 49
      Width = 113
      Height = 17
      Caption = 'Old mode'
      TabOrder = 1
    end
  end
  object btnOK: TButton
    Left = 148
    Top = 103
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 228
    Top = 103
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
