object fmGxMessageBox: TfmGxMessageBox
  Left = 501
  Top = 221
  BorderStyle = bsDialog
  Caption = 'GExperts Message'
  ClientHeight = 240
  ClientWidth = 329
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object bvlFrame: TBevel
    Left = 8
    Top = 8
    Width = 313
    Height = 185
  end
  object chkNeverShowAgain: TCheckBox
    Left = 16
    Top = 168
    Width = 297
    Height = 17
    Caption = '&Never show this message again'
    TabOrder = 1
  end
  object mmoMessage: TMemo
    Left = 16
    Top = 16
    Width = 297
    Height = 145
    Lines.Strings = (
      '')
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
  end
end
