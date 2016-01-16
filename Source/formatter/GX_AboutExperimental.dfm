inherited fmAboutExperimental: TfmAboutExperimental
  Caption = 'About GExperts - Experimental'
  ClientHeight = 305
  PixelsPerInch = 96
  TextHeight = 14
  inherited lblWebPage: TLabel
    Top = 119
  end
  inherited lblProjectLeader: TLabel
    Top = 139
  end
  inherited lblContributors: TLabel
    Top = 161
  end
  inherited lblErik: TLabel
    Top = 139
    OnClick = nil
  end
  inherited lblWebSite: TLabel
    Top = 119
  end
  inherited lblPreRelease1: TLabel
    Caption = 'Experimental'
    Font.Color = clRed
    ParentFont = False
    Visible = True
  end
  inherited lblPreRelease2: TLabel
    Caption = 'Experimental'
    Font.Color = clRed
    ParentFont = False
    Visible = True
  end
  object l_CreatedBy: TLabel [9]
    Left = 224
    Top = 64
    Width = 385
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'Experimental version created by Thomas M'#252'ller'
  end
  object l_DummzeuchDe: TLabel [10]
    Left = 224
    Top = 79
    Width = 385
    Height = 14
    Cursor = crHandPoint
    Alignment = taCenter
    AutoSize = False
    Caption = 'http://blog.dummzeuch.de/experimental-gexperts-version/'
    OnClick = lblWebPageClick
  end
  object l_Formatter: TLabel [11]
    Left = 224
    Top = 96
    Width = 385
    Height = 14
    Cursor = crHandPoint
    Hint = 'http://www.aew.wur.nl/UK/Delforexp/'
    Alignment = taCenter
    AutoSize = False
    Caption = 'Code formatter based on DelForExp by Egbert van Nes'
    ParentShowHint = False
    ShowHint = True
    OnClick = lblWebPageClick
  end
  inherited mmoBuildDetails: TMemo
    Left = 240
    Top = 224
    Width = 89
    Height = 16
    Lines.Strings = (
      'invisible')
  end
  inherited btnClose: TButton
    Top = 270
  end
  inherited pnlLogo: TPanel
    Top = 26
    inherited imgLogo: TImage
      Left = 2
      Top = 0
    end
  end
  inherited btnEmail: TButton
    Top = 270
  end
  inherited mmoContributors: TMemo
    Top = 160
    Height = 97
  end
end
