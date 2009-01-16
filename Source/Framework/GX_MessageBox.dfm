object fmGxMessageBox: TfmGxMessageBox
  Left = 501
  Top = 221
  BorderStyle = bsDialog
  Caption = 'GExperts Message'
  ClientHeight = 244
  ClientWidth = 409
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
    409
    244)
  PixelsPerInch = 96
  TextHeight = 14
  object bvlFrame: TBevel
    Left = 8
    Top = 8
    Width = 391
    Height = 189
    Anchors = [akLeft, akTop, akRight, akBottom]
  end
  object chkNeverShowAgain: TCheckBox
    Left = 16
    Top = 172
    Width = 337
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = '&Never show this message again'
    TabOrder = 1
  end
  object mmoMessage: TMemo
    Left = 16
    Top = 16
    Width = 375
    Height = 149
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      '')
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
  end
end
