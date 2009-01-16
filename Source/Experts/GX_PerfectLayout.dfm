object fmPerfectLayout: TfmPerfectLayout
  Left = 275
  Top = 163
  BorderStyle = bsDialog
  Caption = 'Perfect Layout Options'
  ClientHeight = 228
  ClientWidth = 386
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 14
  object gbxLayout: TGroupBox
    Left = 8
    Top = 7
    Width = 289
    Height = 214
    Caption = 'Perfect Layout'
    TabOrder = 0
    object pnlLayout1: TPanel
      Left = 8
      Top = 24
      Width = 121
      Height = 97
      BevelOuter = bvLowered
      Color = clWhite
      TabOrder = 0
      object shpMain1: TShape
        Left = 1
        Top = 1
        Width = 119
        Height = 24
        Align = alTop
      end
      object shpOI1: TShape
        Left = 1
        Top = 25
        Width = 25
        Height = 46
        Align = alLeft
      end
      object shpEditor1: TShape
        Left = 26
        Top = 25
        Width = 94
        Height = 46
        Align = alClient
      end
      object shpWatch1: TShape
        Left = 1
        Top = 71
        Width = 119
        Height = 25
        Align = alBottom
      end
      object lblWatch1: TLabel
        Left = 43
        Top = 77
        Width = 36
        Height = 14
        Caption = 'Watch'
        Transparent = True
      end
      object lblMain1: TLabel
        Left = 48
        Top = 7
        Width = 24
        Height = 14
        Caption = 'Main'
      end
      object lblOI1: TLabel
        Left = 8
        Top = 40
        Width = 13
        Height = 14
        Caption = 'OI'
      end
      object lblEditor1: TLabel
        Left = 59
        Top = 40
        Width = 32
        Height = 14
        Caption = 'Editor'
      end
    end
    object pnlLayout2: TPanel
      Left = 150
      Top = 24
      Width = 121
      Height = 97
      BevelOuter = bvLowered
      Color = clWhite
      TabOrder = 1
      object shpMain2: TShape
        Left = 1
        Top = 1
        Width = 119
        Height = 24
        Align = alTop
      end
      object shpOI2: TShape
        Left = 94
        Top = 25
        Width = 26
        Height = 46
        Align = alRight
      end
      object shpEditor2: TShape
        Left = 1
        Top = 25
        Width = 93
        Height = 46
        Align = alClient
      end
      object shpWatch2: TShape
        Left = 1
        Top = 71
        Width = 119
        Height = 25
        Align = alBottom
      end
      object lblWatch2: TLabel
        Left = 44
        Top = 77
        Width = 36
        Height = 14
        Caption = 'Watch'
        Transparent = True
      end
      object lblMain2: TLabel
        Left = 48
        Top = 7
        Width = 24
        Height = 14
        Caption = 'Main'
      end
      object lblOI2: TLabel
        Left = 102
        Top = 40
        Width = 13
        Height = 14
        Caption = 'OI'
      end
      object lblEditor2: TLabel
        Left = 35
        Top = 40
        Width = 32
        Height = 14
        Caption = 'Editor'
      end
    end
    object rbnLayout1: TRadioButton
      Left = 24
      Top = 128
      Width = 113
      Height = 17
      Caption = 'Layout &1'
      TabOrder = 2
    end
    object rbnLayout2: TRadioButton
      Left = 176
      Top = 128
      Width = 105
      Height = 17
      Caption = 'Layout &2'
      TabOrder = 3
    end
    object rbnCustom: TRadioButton
      Left = 107
      Top = 158
      Width = 126
      Height = 17
      Caption = '&Custom'
      TabOrder = 4
    end
    object btnCustom: TButton
      Left = 72
      Top = 180
      Width = 137
      Height = 25
      Caption = '&Save Custom Layout'
      TabOrder = 5
      OnClick = btnCustomClick
    end
  end
  object btnOK: TButton
    Left = 305
    Top = 12
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 305
    Top = 44
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object btnHelp: TButton
    Left = 305
    Top = 76
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Help'
    TabOrder = 3
    OnClick = btnHelpClick
  end
end
