object fmAsciiChart: TfmAsciiChart
  Left = 422
  Top = 177
  AutoScroll = False
  Caption = 'ASCII Chart'
  ClientHeight = 395
  ClientWidth = 542
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  ShowHint = True
  OnClose = FormClose
  OnConstrainedResize = FormConstrainedResize
  OnDeactivate = FormDeactivate
  OnKeyDown = FormKeyDown
  OnMouseMove = FormMouseMove
  OnMouseUp = FormMouseUp
  OnPaint = FormPaint
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 14
  object ToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 542
    Height = 22
    AutoSize = True
    ButtonWidth = 31
    EdgeBorders = []
    Flat = True
    ShowCaptions = True
    TabOrder = 0
    Wrapable = False
    OnResize = ToolBarResize
    object tbnCharLow: TToolButton
      Left = 0
      Top = 0
      Action = actCharLow
      Caption = 'Low'
      Grouped = True
    end
    object tbnCharHigh: TToolButton
      Left = 31
      Top = 0
      Action = actCharHigh
      Caption = 'High'
      Grouped = True
    end
    object tbnSep1: TToolButton
      Left = 62
      Top = 0
      Width = 8
      ImageIndex = 2
      Style = tbsSeparator
    end
    object tbnCharDec: TToolButton
      Left = 70
      Top = 0
      Action = actCharDec
      Caption = 'Dec'
      Grouped = True
    end
    object tbnCharHex: TToolButton
      Left = 101
      Top = 0
      Action = actCharHex
      Caption = 'Hex'
      Grouped = True
    end
    object tbnSep2: TToolButton
      Left = 132
      Top = 0
      Width = 8
      ImageIndex = 2
      Style = tbsSeparator
    end
    object cbxFontName: TComboBox
      Left = 140
      Top = 0
      Width = 175
      Height = 22
      Hint = 'Character Font'
      Style = csDropDownList
      DropDownCount = 20
      ItemHeight = 14
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnChange = cbxFontNameChange
      OnEnter = cbxFontNameEnter
    end
    object edFontSize: TEdit
      Left = 315
      Top = 0
      Width = 25
      Height = 22
      MaxLength = 2
      TabOrder = 1
      Text = '9'
      OnChange = edFontSizeChange
    end
    object updFontSize: TUpDown
      Left = 340
      Top = 0
      Width = 15
      Height = 22
      Associate = edFontSize
      Min = 6
      Max = 20
      Position = 9
      TabOrder = 2
      Wrap = False
      OnClick = updFontSizeClick
    end
    object eChars: TEdit
      Left = 355
      Top = 0
      Width = 157
      Height = 22
      TabOrder = 3
    end
    object btnClear: TButton
      Left = 520
      Top = 0
      Width = 22
      Height = 22
      Caption = 'X'
      TabOrder = 4
      OnClick = btnClearClick
    end
  end
  object pmContext: TPopupMenu
    AutoPopup = False
    Left = 72
    Top = 88
    object mitShowLowCharacters: TMenuItem
      Action = actCharLow
      GroupIndex = 1
      RadioItem = True
    end
    object mitShowHighCharacters: TMenuItem
      Action = actCharHigh
      GroupIndex = 1
      RadioItem = True
    end
    object mitSep1: TMenuItem
      Caption = '-'
      GroupIndex = 1
    end
    object mitCharAsDec: TMenuItem
      Action = actCharDec
      GroupIndex = 2
      RadioItem = True
    end
    object mitCharAsHex: TMenuItem
      Action = actCharHex
      GroupIndex = 2
      RadioItem = True
    end
    object mitSep2: TMenuItem
      Caption = '-'
      GroupIndex = 2
    end
    object mitFontSize8: TMenuItem
      Tag = 8
      Action = actFontSize8
      GroupIndex = 3
      RadioItem = True
    end
    object mitFontSize10: TMenuItem
      Tag = 10
      Action = actFontSize10
      GroupIndex = 3
      RadioItem = True
    end
    object mitFontSize12: TMenuItem
      Tag = 12
      Action = actFontSize12
      GroupIndex = 3
      RadioItem = True
    end
    object mitSep3: TMenuItem
      Caption = '-'
      GroupIndex = 3
    end
    object mitShowHints: TMenuItem
      Action = actShowHints
      GroupIndex = 3
    end
    object mitSep4: TMenuItem
      Caption = '-'
      GroupIndex = 3
    end
    object mitHelp: TMenuItem
      Action = actHelpHelp
      GroupIndex = 3
    end
    object mitAbout: TMenuItem
      Action = actHelpAbout
      GroupIndex = 3
    end
  end
  object HintTimer: TTimer
    Interval = 3000
    OnTimer = HintTimerTimer
    Left = 16
    Top = 88
  end
  object Actions: TActionList
    Left = 128
    Top = 120
    object actCharLow: TAction
      Category = 'HighLow'
      Caption = 'Show Characters &0-127'
      ImageIndex = 3
      ShortCut = 16460
      OnExecute = actCharLowExecute
      OnUpdate = actCharLowUpdate
    end
    object actCharHigh: TAction
      Category = 'HighLow'
      Caption = 'Show Characters &128-255'
      ImageIndex = 2
      ShortCut = 16456
      OnExecute = actCharHighExecute
      OnUpdate = actCharHighUpdate
    end
    object actCharDec: TAction
      Category = 'HexDec'
      Caption = 'Character Values as &Decimal'
      ImageIndex = 0
      ShortCut = 24644
      OnExecute = actCharDecExecute
      OnUpdate = actCharDecUpdate
    end
    object actCharHex: TAction
      Category = 'HexDec'
      Caption = 'Character Values as &Hexadecimal'
      ImageIndex = 1
      ShortCut = 24648
      OnExecute = actCharHexExecute
      OnUpdate = actCharHexUpdate
    end
    object actFontSize8: TAction
      Category = 'FontSize'
      Caption = 'Font Size: 8'
      ShortCut = 16440
      OnExecute = actFontSize8Execute
      OnUpdate = actGenericFontSizeUpdate
    end
    object actFontSize10: TAction
      Category = 'FontSize'
      Caption = 'Font Size: 10'
      ShortCut = 16432
      OnExecute = actFontSize10Execute
      OnUpdate = actGenericFontSizeUpdate
    end
    object actFontSize12: TAction
      Category = 'FontSize'
      Caption = 'Font Size: 12'
      ShortCut = 16434
      OnExecute = actFontSize12Execute
      OnUpdate = actGenericFontSizeUpdate
    end
    object actShowHints: TAction
      Category = 'Options'
      Caption = 'Show Hi&nts'
      OnExecute = actShowHintsExecute
      OnUpdate = actShowHintsUpdate
    end
    object actHelpHelp: TAction
      Category = 'Help'
      Caption = '&Help'
      ImageIndex = 4
      ShortCut = 112
      OnExecute = actHelpHelpExecute
    end
    object actHelpAbout: TAction
      Category = 'Help'
      Caption = '&About...'
      OnExecute = actHelpAboutExecute
    end
  end
end
