inherited fmGrepProgress: TfmGrepProgress
  Caption = 'Grep progress'
  ClientHeight = 102
  ClientWidth = 340
  FormStyle = fsStayOnTop
  PixelsPerInch = 96
  TextHeight = 13
  object lblProgressDetailText: TLabel
    Left = 8
    Top = 85
    Width = 324
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = '---'
  end
  object lblProgressText: TLabel
    Left = 8
    Top = 38
    Width = 324
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = '---'
  end
  object Progress: TProgressBar
    Left = 8
    Top = 8
    Width = 324
    Height = 25
    Style = pbstMarquee
    MarqueeInterval = 1
    TabOrder = 0
  end
  object ProgressBarDetail: TProgressBar
    Left = 8
    Top = 55
    Width = 324
    Height = 25
    Style = pbstMarquee
    MarqueeInterval = 1
    TabOrder = 1
  end
end
