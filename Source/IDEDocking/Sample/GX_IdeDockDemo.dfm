inherited DemoDockForm: TDemoDockForm
  Caption = 'DemoDockForm'
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 184
    Top = 0
    Width = 3
    Height = 213
    Cursor = crHSplit
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 184
    Height = 213
    Align = alLeft
    Caption = 'Panel1'
    TabOrder = 0
  end
  object Panel2: TPanel
    Left = 187
    Top = 0
    Width = 125
    Height = 213
    Align = alClient
    Caption = 'Panel2'
    TabOrder = 1
  end
end
