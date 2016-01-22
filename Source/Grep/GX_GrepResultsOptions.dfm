object fmGrepResultsOptions: TfmGrepResultsOptions
  Left = 360
  Top = 227
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Grep Results Options'
  ClientHeight = 433
  ClientWidth = 659
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 14
  object gbxMatchList: TGroupBox
    Left = 336
    Top = 8
    Width = 317
    Height = 169
    Caption = 'Match Results List'
    TabOrder = 2
    DesignSize = (
      317
      169)
    object lblExpandIfMatches: TLabel
      Left = 32
      Top = 60
      Width = 49
      Height = 14
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Matches'
      Enabled = False
    end
    object lblExpandIfFiles: TLabel
      Left = 136
      Top = 60
      Width = 33
      Height = 14
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Files'
      Enabled = False
    end
    object lblExpandFewLines: TLabel
      Left = 32
      Top = 100
      Width = 49
      Height = 14
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Lines'
      Enabled = False
    end
    object chkGrepMiddle: TCheckBox
      Left = 8
      Top = 120
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Jump to matches in the &middle of the editor'
      TabOrder = 6
    end
    object chkGrepExpandAll: TCheckBox
      Left = 8
      Top = 16
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Expand all matches after searching'
      TabOrder = 0
      OnClick = chkGrepExpandClick
    end
    object chkGrepAutoHide: TCheckBox
      Left = 8
      Top = 144
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Hide results window when jumping to match'
      TabOrder = 7
    end
    object chkGrepExpandIf: TCheckBox
      Left = 8
      Top = 40
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Expand all if matches/files are less than:'
      TabOrder = 1
      OnClick = chkGrepExpandClick
    end
    object eExpandIfMatches: TEdit
      Left = 88
      Top = 57
      Width = 33
      Height = 22
      Enabled = False
      TabOrder = 2
      Text = '150'
    end
    object eExpandIfFiles: TEdit
      Left = 176
      Top = 57
      Width = 34
      Height = 22
      Enabled = False
      TabOrder = 3
      Text = '25'
    end
    object chkGrepExpandFew: TCheckBox
      Left = 8
      Top = 80
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Expand files if match lines are less than:'
      TabOrder = 4
      OnClick = chkGrepExpandClick
    end
    object eExpandFewLines: TEdit
      Left = 88
      Top = 97
      Width = 33
      Height = 22
      Enabled = False
      TabOrder = 5
      Text = '20'
    end
  end
  object gbxMatchContext: TGroupBox
    Left = 336
    Top = 192
    Width = 317
    Height = 193
    Caption = 'Match Context Display'
    TabOrder = 3
    DesignSize = (
      317
      193)
    object lblContextLines: TLabel
      Left = 48
      Top = 147
      Width = 132
      Height = 14
      Caption = 'Number of context lines'
      FocusControl = edtContextLines
    end
    object pnlContextFont: TPanel
      Left = 48
      Top = 24
      Width = 197
      Height = 33
      BevelWidth = 2
      Caption = 'Context Font...'
      Color = clWindow
      TabOrder = 0
      OnClick = pnlContextFontClick
    end
    object pnlContextMatchFontColor: TPanel
      Left = 48
      Top = 104
      Width = 197
      Height = 33
      BevelWidth = 2
      Caption = 'Match Font Color...'
      Color = clWindow
      TabOrder = 2
      OnClick = pnlContextMatchFontColorClick
    end
    object edtContextLines: TEdit
      Left = 184
      Top = 144
      Width = 46
      Height = 22
      TabOrder = 3
      Text = '1'
    end
    object udContextLines: TUpDown
      Left = 230
      Top = 144
      Width = 12
      Height = 22
      Associate = edtContextLines
      Min = 1
      Position = 1
      TabOrder = 4
      Wrap = False
    end
    object chkSaveContextSize: TCheckBox
      Left = 8
      Top = 168
      Width = 297
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Save context display height'
      TabOrder = 5
    end
    object pnlContextMacthLineFontColor: TPanel
      Left = 48
      Top = 64
      Width = 197
      Height = 33
      BevelWidth = 2
      Caption = 'Match Line Font Color...'
      Color = clWindow
      TabOrder = 1
      OnClick = pnlContextMacthLineFontColorClick
    end
  end
  object btnOK: TButton
    Left = 496
    Top = 400
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 4
  end
  object btnCancel: TButton
    Left = 576
    Top = 400
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object gbxHistoryList: TGroupBox
    Left = 8
    Top = 8
    Width = 317
    Height = 169
    Caption = 'Search History'
    TabOrder = 0
    DesignSize = (
      317
      169)
    object chkSaveHistoryListSize: TCheckBox
      Left = 8
      Top = 62
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Save search history pane width'
      TabOrder = 3
    end
    object chkGrepSaveHistoryListItems: TCheckBox
      Left = 8
      Top = 24
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Save search parameters and results'
      TabOrder = 0
      OnClick = chkGrepSaveHistoryListItemsClick
    end
    object rbSaveToRegistry: TRadioButton
      Left = 153
      Top = 42
      Width = 113
      Height = 17
      Caption = 'To registry'
      Enabled = False
      TabOrder = 2
    end
    object rbSaveToIniFile: TRadioButton
      Left = 33
      Top = 42
      Width = 113
      Height = 17
      Caption = 'To .ini file'
      Checked = True
      Enabled = False
      TabOrder = 1
      TabStop = True
    end
  end
  object gbxListColors: TGroupBox
    Left = 8
    Top = 192
    Width = 317
    Height = 193
    Caption = 'List Display'
    TabOrder = 1
    DesignSize = (
      317
      193)
    object pnlListMatchTextColor: TPanel
      Left = 48
      Top = 96
      Width = 197
      Height = 33
      BevelWidth = 2
      Caption = 'Match Text Color...'
      Color = clWindow
      TabOrder = 2
      OnClick = pnlListMatchTextColorClick
    end
    object pnlListFont: TPanel
      Left = 48
      Top = 24
      Width = 197
      Height = 33
      BevelWidth = 2
      Caption = 'Match List Font...'
      Color = clWindow
      TabOrder = 0
      OnClick = pnlListFontClick
    end
    object chkDefaultListColors: TCheckBox
      Left = 8
      Top = 72
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Use default list colors'
      TabOrder = 1
      OnClick = chkDefaultListColorsClick
    end
    object pnlListMatchBackgroundColor: TPanel
      Left = 48
      Top = 136
      Width = 197
      Height = 33
      BevelWidth = 2
      Caption = 'Match Background Color...'
      Color = clWindow
      TabOrder = 3
      OnClick = pnlListMatchBackgroundColorClick
    end
  end
  object dlgGrepListFont: TFontDialog
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    MinFontSize = 0
    MaxFontSize = 0
    Left = 271
    Top = 220
  end
  object dlgGrepContextFont: TFontDialog
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    MinFontSize = 0
    MaxFontSize = 0
    Left = 602
    Top = 217
  end
  object dlgContextMatchFontColor: TColorDialog
    Ctl3D = True
    Options = [cdSolidColor]
    Left = 601
    Top = 297
  end
  object dlgListMatchTextColor: TColorDialog
    Ctl3D = True
    Options = [cdSolidColor]
    Left = 273
    Top = 289
  end
  object dlgListMatchBackgroundColor: TColorDialog
    Ctl3D = True
    Options = [cdSolidColor]
    Left = 273
    Top = 329
  end
  object dlgContextMatchLineFontColor: TColorDialog
    Ctl3D = True
    Options = [cdSolidColor]
    Left = 601
    Top = 257
  end
end
