inherited fmGrepSelect: TfmGrepSelect
  ActiveControl = eSearch
  Caption = 'Search History Maintenance'
  ClientHeight = 534
  ClientWidth = 790
  ParentFont = False
  Font.Charset = ANSI_CHARSET
  OldCreateOrder = True
  ShowHint = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lvHistoryList: TListView
    Left = 0
    Top = 0
    Width = 790
    Height = 384
    Align = alClient
    Checkboxes = True
    Columns = <
      item
        Caption = 'Serch Text'
        Tag = 2
        Width = 300
      end
      item
        Caption = 'Search Time'
        Tag = 3
        Width = 140
      end
      item
        Caption = 'Files'
        Tag = 12
        Width = 40
      end
      item
        Caption = 'Matches'
        Tag = 13
        Width = 57
      end
      item
        Caption = 'Where'
        Tag = 14
        Width = 75
      end
      item
        Caption = 'Save Option'
        Tag = 11
        Width = 150
      end>
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ReadOnly = True
    RowSelect = True
    ParentFont = False
    SortType = stData
    TabOrder = 0
    ViewStyle = vsReport
    OnAdvancedCustomDrawItem = lvHistoryListAdvancedCustomDrawItem
    OnColumnClick = lvHistoryListColumnClick
    OnCompare = lvHistoryListCompare
    OnMouseDown = lvHistoryListMouseDown
  end
  object pnlTools: TPanel
    Left = 0
    Top = 384
    Width = 790
    Height = 150
    Align = alBottom
    TabOrder = 1
    DesignSize = (
      790
      150)
    object btnOK: TButton
      Left = 693
      Top = 48
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 12
    end
    object btnCancel: TButton
      Left = 693
      Top = 93
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 13
    end
    object btnCheckItems: TButton
      Left = 2
      Top = 2
      Width = 100
      Height = 25
      Hint = 'Check first found items or all items'
      Caption = 'Check All'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnClick = btnSelectClick
    end
    object btnUnCheckItems: TButton
      Left = 106
      Top = 2
      Width = 100
      Height = 25
      Hint = 'Uncheck first found items or all items'
      Caption = 'Uncheck All'
      TabOrder = 1
      OnClick = btnSelectClick
    end
    object pnlDelete: TPanel
      Left = 0
      Top = 90
      Width = 243
      Height = 60
      BevelOuter = bvNone
      TabOrder = 5
      object cbMoveToParamsPage: TCheckBox
        Left = 5
        Top = 16
        Width = 153
        Height = 17
        Caption = 'Move to Params page'
        TabOrder = 0
      end
    end
    object pnlSaveOptionModify: TPanel
      Left = 0
      Top = 90
      Width = 243
      Height = 60
      BevelOuter = bvNone
      TabOrder = 6
      object lblSaveOption: TLabel
        Left = 5
        Top = 8
        Width = 204
        Height = 13
        Caption = 'Save option new value (when IDE closing):'
      end
      object cbxSaveOptionValue: TComboBox
        Left = 34
        Top = 26
        Width = 183
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 3
        TabOrder = 0
        Text = 'No change'
        Items.Strings = (
          'Save parameters and results'
          'Only save parameters'
          'No save (temp)'
          'No change')
      end
    end
    object btnUnsorted: TButton
      Left = 485
      Top = 2
      Width = 90
      Height = 25
      Caption = 'Unsorted'
      TabOrder = 3
      OnClick = SortButtonClick
    end
    object pnlOpen: TPanel
      Left = 300
      Top = 90
      Width = 177
      Height = 60
      BevelOuter = bvNone
      TabOrder = 10
      object cbOpenClear: TCheckBox
        Left = 24
        Top = 3
        Width = 127
        Height = 17
        Caption = 'Clear history list'
        TabOrder = 0
        OnClick = cbOpenClearClick
      end
      object cbOpenOverwrite: TCheckBox
        Left = 24
        Top = 22
        Width = 127
        Height = 17
        Caption = 'Overwrite if exists'
        TabOrder = 1
        OnClick = cbOpenOverwriteClick
      end
      object cbOpenOnlyIfNewer: TCheckBox
        Left = 41
        Top = 38
        Width = 113
        Height = 17
        Caption = 'Only if it newer'
        Enabled = False
        TabOrder = 2
      end
    end
    object btnSortKeyIndex: TButton
      Tag = 1
      Left = 388
      Top = 2
      Width = 90
      Height = 25
      Caption = 'Sort Keyindex'
      TabOrder = 2
      OnClick = SortButtonClick
    end
    object pnlSearch: TPanel
      Left = 0
      Top = 30
      Width = 670
      Height = 60
      Anchors = [akLeft, akRight, akBottom]
      BevelOuter = bvNone
      BorderStyle = bsSingle
      TabOrder = 4
      object lblSearch: TLabel
        Left = 8
        Top = 8
        Width = 37
        Height = 13
        Caption = 'Search:'
        FocusControl = eSearch
      end
      object lblSearchSaveOption: TLabel
        Left = 8
        Top = 33
        Width = 110
        Height = 13
        Caption = 'Save option for search:'
      end
      object eSearch: TEdit
        Left = 73
        Top = 5
        Width = 261
        Height = 21
        TabOrder = 0
        OnChange = eSearchChange
      end
      object cbSearchSaveOption: TComboBox
        Left = 140
        Top = 30
        Width = 194
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 3
        TabOrder = 3
        Text = 'All'
        OnChange = cbSearchSaveOptionChange
        Items.Strings = (
          'Save parameters and results'
          'Only save parameters'
          'No save (temp)'
          'All')
      end
      object cbCheckedMoveToTop: TCheckBox
        Left = 414
        Top = 2
        Width = 163
        Height = 17
        Caption = 'Checked move to top'
        Checked = True
        State = cbChecked
        TabOrder = 4
        OnClick = cbCheckedMoveToTopClick
      end
      object cbSearchInChecked: TCheckBox
        Left = 414
        Top = 20
        Width = 163
        Height = 17
        Caption = 'Search in checked items'
        TabOrder = 5
        OnClick = cbSearchInCheckedClick
      end
      object cbShowNotFound: TCheckBox
        Left = 414
        Top = 37
        Width = 163
        Height = 17
        Caption = 'Show "not found"'
        TabOrder = 6
        OnClick = cbShowNotFoundClick
      end
      object btnClearSearch: TBitBtn
        Left = 344
        Top = 3
        Width = 26
        Height = 25
        Cancel = True
        TabOrder = 1
        OnClick = btnClearSearchClick
        Glyph.Data = {
          DE010000424DDE01000000000000760000002800000024000000120000000100
          0400000000006801000000000000000000001000000000000000000000000000
          80000080000000808000800000008000800080800000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
          333333333333333333333333000033338833333333333333333F333333333333
          0000333911833333983333333388F333333F3333000033391118333911833333
          38F38F333F88F33300003339111183911118333338F338F3F8338F3300003333
          911118111118333338F3338F833338F3000033333911111111833333338F3338
          3333F8330000333333911111183333333338F333333F83330000333333311111
          8333333333338F3333383333000033333339111183333333333338F333833333
          00003333339111118333333333333833338F3333000033333911181118333333
          33338333338F333300003333911183911183333333383338F338F33300003333
          9118333911183333338F33838F338F33000033333913333391113333338FF833
          38F338F300003333333333333919333333388333338FFF830000333333333333
          3333333333333333333888330000333333333333333333333333333333333333
          0000}
        NumGlyphs = 2
      end
      object btnRefreshSearch: TBitBtn
        Left = 372
        Top = 3
        Width = 26
        Height = 25
        TabOrder = 2
        OnClick = btnRefreshSearchClick
        Glyph.Data = {
          DE010000424DDE01000000000000760000002800000024000000120000000100
          0400000000006801000000000000000000001000000000000000000000000000
          80000080000000808000800000008000800080800000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333444444
          33333333333F8888883F33330000324334222222443333388F3833333388F333
          000032244222222222433338F8833FFFFF338F3300003222222AAAAA22243338
          F333F88888F338F30000322222A33333A2224338F33F8333338F338F00003222
          223333333A224338F33833333338F38F00003222222333333A444338FFFF8F33
          3338888300003AAAAAAA33333333333888888833333333330000333333333333
          333333333333333333FFFFFF000033333333333344444433FFFF333333888888
          00003A444333333A22222438888F333338F3333800003A2243333333A2222438
          F38F333333833338000033A224333334422224338338FFFFF8833338000033A2
          22444442222224338F3388888333FF380000333A2222222222AA243338FF3333
          33FF88F800003333AA222222AA33A3333388FFFFFF8833830000333333AAAAAA
          3333333333338888883333330000333333333333333333333333333333333333
          0000}
        NumGlyphs = 2
      end
    end
    object pnlSearchIn: TPanel
      Left = 0
      Top = 90
      Width = 243
      Height = 60
      BevelOuter = bvNone
      TabOrder = 7
      object cbSearchInClearSearchList: TCheckBox
        Left = 40
        Top = 7
        Width = 119
        Height = 17
        Caption = 'Clear search list'
        TabOrder = 0
      end
    end
    object pnlSave: TPanel
      Left = 0
      Top = 90
      Width = 243
      Height = 60
      BevelOuter = bvNone
      TabOrder = 8
      object lblSaveSplitCount: TLabel
        Left = 27
        Top = 30
        Width = 53
        Height = 13
        Caption = 'Split count:'
        Enabled = False
      end
      object cbSaveSplitToMoreFiles: TCheckBox
        Left = 5
        Top = 8
        Width = 153
        Height = 17
        Caption = 'Split to more files'
        TabOrder = 0
        OnClick = cbSaveSplitToMoreFilesClick
      end
      object eSaveSplitCount: TEdit
        Left = 93
        Top = 27
        Width = 25
        Height = 21
        Enabled = False
        TabOrder = 1
      end
    end
    object pnlSortMoves: TPanel
      Left = 0
      Top = 90
      Width = 243
      Height = 60
      BevelOuter = bvNone
      TabOrder = 9
      object btnMoveDown: TButton
        Tag = 1
        Left = 144
        Top = 32
        Width = 90
        Height = 25
        Caption = 'Move Down'
        TabOrder = 2
        OnClick = MoveButtonsClick
      end
      object btnMoveUp: TButton
        Tag = -1
        Left = 144
        Top = 6
        Width = 90
        Height = 25
        Caption = 'Move Up'
        TabOrder = 1
        OnClick = MoveButtonsClick
      end
      object rgMoveWhat: TRadioGroup
        Left = 2
        Top = 0
        Width = 136
        Height = 58
        Caption = 'Move Items:'
        ItemIndex = 0
        Items.Strings = (
          'Checked items'
          'Unchecked items'
          'Found items')
        TabOrder = 0
      end
    end
    object pnlSortButtons: TPanel
      Left = 361
      Top = 82
      Width = 340
      Height = 60
      Anchors = [akRight, akBottom]
      BevelOuter = bvNone
      Color = clBtnShadow
      TabOrder = 11
      object lblQuickSortButtons: TLabel
        Left = 7
        Top = 10
        Width = 41
        Height = 43
        Alignment = taCenter
        AutoSize = False
        Caption = 'Quick Sort Buttons'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindow
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        WordWrap = True
      end
      object btnSortSearchTextAsc: TButton
        Tag = 2
        Left = 56
        Top = 5
        Width = 90
        Height = 25
        Caption = 'Search Text'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ModalResult = 6
        ParentFont = False
        TabOrder = 0
        OnClick = SortButtonsClick
      end
      object btnSortSearchTextDesc: TButton
        Tag = 2
        Left = 56
        Top = 32
        Width = 90
        Height = 25
        Caption = 'Text Desc'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ModalResult = 7
        ParentFont = False
        TabOrder = 3
        OnClick = SortButtonsClick
      end
      object btnSortTimeAsc: TButton
        Tag = 3
        Left = 148
        Top = 5
        Width = 90
        Height = 25
        Caption = 'Search Time'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ModalResult = 6
        ParentFont = False
        TabOrder = 1
        OnClick = SortButtonsClick
      end
      object btnSortTimeDesc: TButton
        Tag = 3
        Left = 148
        Top = 32
        Width = 90
        Height = 25
        Caption = 'Time Desc'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ModalResult = 7
        ParentFont = False
        TabOrder = 4
        OnClick = SortButtonsClick
      end
      object btnSortKeyIndexAsc: TButton
        Tag = 1
        Left = 240
        Top = 5
        Width = 90
        Height = 25
        Caption = 'Key Index'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ModalResult = 6
        ParentFont = False
        TabOrder = 2
        OnClick = SortButtonsClick
      end
      object btnSortKeyIndexDesc: TButton
        Tag = 1
        Left = 240
        Top = 32
        Width = 90
        Height = 25
        Caption = 'Key Idx Desc'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ModalResult = 7
        ParentFont = False
        TabOrder = 5
        OnClick = SortButtonsClick
      end
    end
  end
  object tmrSearchInFilter: TTimer
    Enabled = False
    Interval = 10
    OnTimer = tmrSearchInFilterTimer
    Left = 536
    Top = 144
  end
end
