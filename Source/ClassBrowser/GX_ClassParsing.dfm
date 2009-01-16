object fmClassParsing: TfmClassParsing
  Left = 299
  Top = 253
  BorderStyle = bsDialog
  Caption = 'Parsing classes...'
  ClientHeight = 94
  ClientWidth = 382
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object lblParsing: TLabel
    Left = 91
    Top = 16
    Width = 286
    Height = 33
    AutoSize = False
    Caption = 'Parsing classes, please wait...'
    WordWrap = True
  end
  object aniFlashlight: TAnimate
    Left = 5
    Top = 8
    Width = 80
    Height = 50
    Active = False
    CommonAVI = aviFindFolder
    StopFrame = 29
  end
  object Progress: TProgressBar
    Left = 28
    Top = 64
    Width = 326
    Height = 25
    Min = 0
    Max = 100
    TabOrder = 1
  end
end
