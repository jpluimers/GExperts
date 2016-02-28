object fmGrepResultsOptions: TfmGrepResultsOptions
  Left = 360
  Top = 227
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Grep Results Options'
  ClientHeight = 542
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
    Left = 334
    Top = 8
    Width = 317
    Height = 297
    Caption = 'Match Results List'
    TabOrder = 1
    DesignSize = (
      317
      297)
    object lblExpandIfMatches: TLabel
      Left = 32
      Top = 60
      Width = 49
      Height = 14
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Matches'
      Enabled = False
      Visible = False
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
      Visible = False
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
      Visible = False
    end
    object chkGrepMiddle: TCheckBox
      Left = 8
      Top = 136
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
      Top = 160
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
      Visible = False
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
      Visible = False
    end
    object eExpandIfFiles: TEdit
      Left = 176
      Top = 57
      Width = 34
      Height = 22
      Enabled = False
      TabOrder = 3
      Text = '25'
      Visible = False
    end
    object chkGrepExpandFew: TCheckBox
      Left = 8
      Top = 80
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Expand files if match lines are less than:'
      TabOrder = 4
      Visible = False
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
      Visible = False
    end
    object chkMouseWheelMoveItemIndex: TCheckBox
      Left = 8
      Top = 200
      Width = 305
      Height = 17
      Caption = 'Mouse wheel select prev/next match'
      Enabled = False
      TabOrder = 8
    end
  end
  object gbxMatchContext: TGroupBox
    Left = 334
    Top = 311
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
    object chkSaveContextFixedHeight: TCheckBox
      Left = 8
      Top = 168
      Width = 297
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Save context display (fixed) height'
      TabOrder = 5
    end
  end
  object btnOK: TButton
    Left = 496
    Top = 511
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 5
  end
  object btnCancel: TButton
    Left = 576
    Top = 511
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
  end
  object gbxHistoryList: TGroupBox
    Left = 8
    Top = 8
    Width = 317
    Height = 297
    Caption = 'Search history'
    TabOrder = 0
    DesignSize = (
      317
      297)
    object lblSaveOption: TLabel
      Left = 8
      Top = 187
      Width = 162
      Height = 14
      Caption = 'Save option default value for:'
    end
    object lblOnlySaveParamsAction: TLabel
      Left = 8
      Top = 252
      Width = 275
      Height = 14
      Caption = '"Only save parameters" action when click on item:'
    end
    object lblHistoryListDefaultPage: TLabel
      Left = 8
      Top = 142
      Width = 74
      Height = 14
      Caption = 'Default page:'
    end
    object lblSearchSaveOption: TLabel
      Left = 49
      Top = 206
      Width = 41
      Height = 14
      Alignment = taRightJustify
      Caption = 'Search:'
    end
    object lblOpenSaveOption: TLabel
      Left = 56
      Top = 229
      Width = 34
      Height = 14
      Alignment = taRightJustify
      Caption = 'Open:'
    end
    object lblHistoryPagesTabWidth: TLabel
      Left = 199
      Top = 164
      Width = 60
      Height = 14
      Caption = 'Tab width:'
    end
    object lblWhenIDEClosing: TLabel
      Left = 8
      Top = 55
      Width = 247
      Height = 14
      Caption = 'Actions when IDE closing and storing history:'
    end
    object chkGrepSaveHistoryListItems: TCheckBox
      Left = 8
      Top = 16
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Save search parameters and results '
      TabOrder = 0
      OnClick = chkGrepSaveHistoryListItemsClick
    end
    object rbSaveToRegistry: TRadioButton
      Left = 153
      Top = 34
      Width = 113
      Height = 17
      Caption = 'To Registry'
      Enabled = False
      TabOrder = 2
      Visible = False
    end
    object rbSaveToIniFile: TRadioButton
      Left = 33
      Top = 34
      Width = 113
      Height = 17
      Caption = 'To IniFile'
      Checked = True
      Enabled = False
      TabOrder = 1
      TabStop = True
      Visible = False
    end
    object chkFileListDeleteAfterDays: TCheckBox
      Left = 21
      Top = 72
      Width = 250
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Results delete after days (except params):'
      TabOrder = 3
      OnClick = chkFileListDeleteAfterDaysClick
    end
    object eDeleteAfterDays: TEdit
      Left = 273
      Top = 70
      Width = 33
      Height = 22
      TabOrder = 4
      Text = '30'
    end
    object cbxSearchSaveOptionDefaultValue: TComboBox
      Left = 96
      Top = 203
      Width = 202
      Height = 22
      Style = csDropDownList
      ItemIndex = 1
      TabOrder = 8
      Text = 'Only save parameters'
      Items.Strings = (
        'Save parameters and results'
        'Only save parameters'
        'No save (temp)'
        'Last value')
    end
    object cbxOnlySaveParamsAction: TComboBox
      Left = 96
      Top = 269
      Width = 202
      Height = 22
      Style = csDropDownList
      ItemIndex = 1
      TabOrder = 9
      Text = 'Show embedded search'
      Items.Strings = (
        'Show search window'
        'Show embedded search'
        'Auto refresh'
        'Auto refresh when double click'
        'Empty list')
    end
    object cbxHistoryListDefaultPage: TComboBox
      Left = 96
      Top = 139
      Width = 202
      Height = 22
      Style = csDropDownList
      ItemHeight = 14
      TabOrder = 7
      Items.Strings = (
        'Results'
        'Parameters'
        'All'
        'Search'
        'Last page')
    end
    object chkQuickRefreshMode: TCheckBox
      Left = 8
      Top = 116
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Quick refresh mode'
      Enabled = False
      TabOrder = 6
    end
    object chkEmptyMoveToParams: TCheckBox
      Left = 21
      Top = 91
      Width = 250
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'If results is empty, move to Params page'
      TabOrder = 5
      Visible = False
    end
    object cbxOpenSaveOptionDefaultValue: TComboBox
      Left = 96
      Top = 226
      Width = 202
      Height = 22
      Style = csDropDownList
      ItemIndex = 2
      TabOrder = 10
      Text = 'No save (temp)'
      Items.Strings = (
        'Save parameters and results'
        'Only save parameters'
        'No save (temp)'
        'Search settings')
    end
    object chkHistoryPagesTabMultiLine: TCheckBox
      Left = 96
      Top = 164
      Width = 89
      Height = 17
      Caption = 'Multiline'
      TabOrder = 11
      OnClick = chkHistoryPagesTabMultiLineClick
    end
    object eHistoryPagesTabWidth: TEdit
      Left = 265
      Top = 161
      Width = 33
      Height = 22
      TabOrder = 12
      Text = '0'
    end
  end
  object gbxListColors: TGroupBox
    Left = 8
    Top = 311
    Width = 317
    Height = 193
    Caption = 'List Display'
    TabOrder = 2
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
  object chkAdvanced: TCheckBox
    Left = 8
    Top = 515
    Width = 121
    Height = 17
    Caption = 'Advanced mode'
    TabOrder = 4
    OnClick = chkAdvancedClick
  end
  object dlgGrepListFont: TFontDialog
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Left = 271
    Top = 332
  end
  object dlgGrepContextFont: TFontDialog
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Left = 602
    Top = 329
  end
  object dlgContextMatchFontColor: TColorDialog
    Options = [cdSolidColor]
    Left = 601
    Top = 409
  end
  object dlgListMatchTextColor: TColorDialog
    Options = [cdSolidColor]
    Left = 273
    Top = 401
  end
  object dlgListMatchBackgroundColor: TColorDialog
    Options = [cdSolidColor]
    Left = 273
    Top = 441
  end
  object dlgContextMatchLineFontColor: TColorDialog
    Options = [cdSolidColor]
    Left = 601
    Top = 369
  end
end
