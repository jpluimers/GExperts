object fmProofreaderConfig: TfmProofreaderConfig
  Left = 279
  Top = 203
  Width = 560
  Height = 451
  BorderIcons = [biSystemMenu]
  Caption = 'Code Proofreader'
  Color = clBtnFace
  Constraints.MinHeight = 400
  Constraints.MinWidth = 450
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  ShowHint = True
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlMain: TPanel
    Left = 0
    Top = 30
    Width = 552
    Height = 392
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 1
    object pnlButtons: TPanel
      Left = 4
      Top = 357
      Width = 544
      Height = 31
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      DesignSize = (
        544
        31)
      object btnOK: TButton
        Left = 297
        Top = 6
        Width = 75
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = 'OK'
        Default = True
        ModalResult = 1
        TabOrder = 0
        OnClick = btnOKClick
      end
      object btnCancel: TButton
        Left = 383
        Top = 6
        Width = 75
        Height = 25
        Anchors = [akRight, akBottom]
        Cancel = True
        Caption = 'Cancel'
        ModalResult = 2
        TabOrder = 1
      end
      object btnHelp: TButton
        Left = 469
        Top = 6
        Width = 75
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = '&Help'
        TabOrder = 2
        OnClick = btnHelpClick
      end
    end
    object Pages: TPageControl
      Tag = 1
      Left = 4
      Top = 4
      Width = 544
      Height = 353
      ActivePage = tabReplacement
      Align = alClient
      MultiLine = True
      TabIndex = 0
      TabOrder = 0
      OnChange = PagesChange
      object tabReplacement: TTabSheet
        Caption = '&AutoCorrect'
        object pnlReplacement: TPanel
          Left = 0
          Top = 0
          Width = 536
          Height = 325
          Align = alClient
          BevelOuter = bvNone
          BorderWidth = 4
          TabOrder = 0
          object lvReplacement: TListView
            Left = 4
            Top = 32
            Width = 528
            Height = 289
            Align = alClient
            Columns = <
              item
                Caption = 'Replace Text'
                Width = 134
              end
              item
                Caption = 'Location'
                Width = 128
              end
              item
                AutoSize = True
                Caption = 'Replace With'
              end>
            ColumnClick = False
            HideSelection = False
            MultiSelect = True
            OwnerData = True
            ReadOnly = True
            RowSelect = True
            PopupMenu = pmList
            TabOrder = 0
            ViewStyle = vsReport
            OnData = lvReplacementData
            OnDblClick = lvReplacementDblClick
          end
          object pnlACHeader: TPanel
            Left = 4
            Top = 4
            Width = 528
            Height = 28
            Align = alTop
            BevelOuter = bvNone
            BorderWidth = 5
            TabOrder = 1
            object tbrReplacement: TToolBar
              Left = 1
              Top = 3
              Width = 69
              Height = 23
              Align = alNone
              ButtonHeight = 23
              DisabledImages = dmSharedImages.DisabledImages
              EdgeBorders = []
              Flat = True
              Images = dmSharedImages.Images
              TabOrder = 0
              object tbnReplacementInsert: TToolButton
                Left = 0
                Top = 0
                Action = actListInsert
                AutoSize = True
              end
              object tbnReplacementEdit: TToolButton
                Left = 23
                Top = 0
                Action = actListEdit
                AutoSize = True
              end
              object tbnReplacementDelete: TToolButton
                Left = 46
                Top = 0
                Action = actListDelete
                AutoSize = True
              end
            end
            object cbReplacerActive: TCheckBox
              Left = 101
              Top = 6
              Width = 171
              Height = 18
              Caption = 'E&nable AutoCorrect'
              Checked = True
              State = cbChecked
              TabOrder = 1
            end
          end
        end
      end
      object tabDictionary: TTabSheet
        Tag = 1
        Caption = '&Dictionary'
        object pnlDictionary: TPanel
          Left = 0
          Top = 0
          Width = 536
          Height = 325
          Align = alClient
          BevelOuter = bvNone
          BorderWidth = 4
          TabOrder = 0
          object gbxWords: TGroupBox
            Left = 4
            Top = 4
            Width = 263
            Height = 317
            Align = alClient
            Caption = 'Dictionary Words'
            TabOrder = 0
            object pnlWords: TPanel
              Left = 2
              Top = 15
              Width = 259
              Height = 300
              Align = alClient
              BevelOuter = bvNone
              BorderWidth = 4
              TabOrder = 0
              object tbrDictionary: TToolBar
                Left = 4
                Top = 4
                Width = 251
                Height = 24
                DisabledImages = dmSharedImages.DisabledImages
                EdgeBorders = []
                Flat = True
                Images = dmSharedImages.Images
                TabOrder = 0
                object tbnDictionaryInsert: TToolButton
                  Left = 0
                  Top = 0
                  Action = actListInsert
                end
                object tbnDictionaryEdit: TToolButton
                  Left = 23
                  Top = 0
                  Action = actListEdit
                end
                object tbnDictionaryDelete: TToolButton
                  Left = 46
                  Top = 0
                  Action = actListDelete
                end
                object tbnDictionarySep: TToolButton
                  Left = 69
                  Top = 0
                  Width = 8
                  ImageIndex = 13
                  Style = tbsSeparator
                end
                object tbnDictionaryExport: TToolButton
                  Left = 77
                  Top = 0
                  Action = actExportWords
                end
                object tbnDictionaryImport: TToolButton
                  Left = 100
                  Top = 0
                  Action = actImportWords
                end
              end
              object lvDictionary: TListView
                Left = 4
                Top = 28
                Width = 251
                Height = 268
                Align = alClient
                Columns = <
                  item
                    AutoSize = True
                    Caption = 'Words'
                  end>
                ColumnClick = False
                HideSelection = False
                MultiSelect = True
                OwnerData = True
                ReadOnly = True
                RowSelect = True
                PopupMenu = pmList
                ShowColumnHeaders = False
                TabOrder = 1
                ViewStyle = vsReport
                OnData = lvDictionaryData
                OnDblClick = lvDictionaryDblClick
              end
            end
          end
          object pnlDictOptions: TPanel
            Left = 267
            Top = 4
            Width = 265
            Height = 317
            Align = alRight
            BevelOuter = bvNone
            TabOrder = 1
            DesignSize = (
              265
              317)
            object gbReplaceIf: TGroupBox
              Left = 8
              Top = 48
              Width = 257
              Height = 147
              Anchors = [akLeft, akTop, akRight]
              Caption = 'Replace When'
              TabOrder = 2
              object cbOneCharIncorrect: TCheckBox
                Left = 8
                Top = 37
                Width = 246
                Height = 17
                Caption = 'One character is wrong'
                TabOrder = 1
                OnClick = cbOneCharIncorrectClick
              end
              object cbAllowOneCharacterMissing: TCheckBox
                Left = 8
                Top = 81
                Width = 246
                Height = 17
                Caption = 'One character is missing'
                TabOrder = 3
              end
              object cbAllowExtraChar: TCheckBox
                Left = 8
                Top = 101
                Width = 246
                Height = 17
                Caption = 'An extra character is inserted'
                TabOrder = 4
              end
              object cbCaseDiffer: TCheckBox
                Left = 8
                Top = 17
                Width = 246
                Height = 17
                Caption = 'Word case is different'
                TabOrder = 0
              end
              object cbAllowSwitchedChars: TCheckBox
                Left = 8
                Top = 121
                Width = 246
                Height = 17
                Caption = 'Two characters are switched'
                TabOrder = 5
              end
              object cbMustBeNearbyLetter: TCheckBox
                Left = 21
                Top = 59
                Width = 232
                Height = 17
                Caption = 'Character must be nearby correct key'
                TabOrder = 2
              end
            end
            object cbFirstCharMustBeCorrect: TCheckBox
              Left = 8
              Top = 202
              Width = 257
              Height = 17
              Anchors = [akLeft, akTop, akRight]
              Caption = 'Don'#39't replace if the first character is different'
              TabOrder = 3
            end
            object cbEnableDictionary: TCheckBox
              Tag = 2
              Left = 8
              Top = 4
              Width = 257
              Height = 17
              Anchors = [akLeft, akTop, akRight]
              Caption = 'Enable dictionary replacement'
              Checked = True
              State = cbChecked
              TabOrder = 0
              OnClick = cbEnableDicitionaryClick
            end
            object cbEnableCompiler: TCheckBox
              Left = 8
              Top = 24
              Width = 257
              Height = 17
              Anchors = [akLeft, akTop, akRight]
              Caption = 'Enable compiler-assisted replacement'
              TabOrder = 1
              OnClick = cbEnableCompilerClick
            end
          end
        end
      end
      object tabHistory: TTabSheet
        Tag = 2
        Caption = '&Correction History'
        object pnlHistory: TPanel
          Left = 0
          Top = 0
          Width = 536
          Height = 325
          Align = alClient
          BevelOuter = bvNone
          BorderWidth = 4
          TabOrder = 0
          object lvHistory: TListView
            Left = 4
            Top = 4
            Width = 528
            Height = 286
            Align = alClient
            Columns = <
              item
                AutoSize = True
                Caption = 'Correction'
              end
              item
                Caption = 'Time'
                Width = 90
              end>
            ColumnClick = False
            HideSelection = False
            ReadOnly = True
            RowSelect = True
            PopupMenu = pmHistory
            TabOrder = 0
            ViewStyle = vsReport
          end
          object pnlHistoryButtons: TPanel
            Left = 4
            Top = 290
            Width = 528
            Height = 31
            Align = alBottom
            BevelOuter = bvNone
            TabOrder = 1
            DesignSize = (
              528
              31)
            object btnDisableRule: TButton
              Left = 362
              Top = 5
              Width = 166
              Height = 26
              Action = actDisableRule
              Anchors = [akRight, akBottom]
              TabOrder = 0
            end
          end
        end
      end
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 552
    Height = 30
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      552
      30)
    object lblRules: TLabel
      Left = 19
      Top = 8
      Width = 42
      Height = 13
      Alignment = taRightJustify
      Caption = '&Rules for'
      FocusControl = cbLanguage
    end
    object cbLanguage: TComboBox
      Left = 68
      Top = 5
      Width = 185
      Height = 21
      Style = csDropDownList
      DropDownCount = 15
      ItemHeight = 13
      TabOrder = 0
      OnChange = cbLanguageChange
    end
    object cbBeep: TCheckBox
      Left = 402
      Top = 8
      Width = 180
      Height = 17
      Anchors = [akTop, akRight]
      Caption = '&Beep when correcting'
      TabOrder = 1
    end
  end
  object dlgGetWordlist: TOpenDialog
    DefaultExt = 'txt'
    Filter = 'Text Files (*.txt)|*.txt|All Files (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Select a word list file (one word per line)'
    Left = 48
    Top = 144
  end
  object dlgPutWordlist: TSaveDialog
    DefaultExt = 'txt'
    Filter = 'Text Files (*.txt)|*.txt|All Files (*.*)|*.*'
    Title = 'Select a file write the word list to'
    Left = 120
    Top = 144
  end
  object Actions: TActionList
    Images = dmSharedImages.Images
    OnUpdate = ActionsUpdate
    Left = 120
    Top = 200
    object actListInsert: TAction
      Category = 'List'
      Caption = '&Insert'
      Hint = 'Insert'
      ImageIndex = 41
      ShortCut = 16429
      OnExecute = actListInsertExecute
    end
    object actListEdit: TAction
      Category = 'List'
      Caption = '&Edit'
      Hint = 'Edit'
      ImageIndex = 38
      ShortCut = 113
      OnExecute = actListEditExecute
    end
    object actListDelete: TAction
      Category = 'List'
      Caption = 'De&lete'
      Hint = 'Delete'
      ImageIndex = 42
      ShortCut = 16430
      OnExecute = actListDeleteExecute
    end
    object actDisableRule: TAction
      Category = 'List'
      Caption = 'Di&sable Correction Rule'
      Hint = 'Disable Correction Rule'
      OnExecute = actDisableRuleExecute
    end
    object actImportWords: TAction
      Category = 'List'
      Caption = 'Import Word List'
      Hint = 'Import Word List'
      ImageIndex = 1
      OnExecute = actImportWordsExecute
    end
    object actExportWords: TAction
      Category = 'List'
      Caption = 'Export Word List'
      Hint = 'Export Word List'
      ImageIndex = 31
      OnExecute = actExportWordsExecute
    end
  end
  object pmList: TPopupMenu
    Images = dmSharedImages.Images
    Left = 48
    Top = 200
    object pmiListInsert: TMenuItem
      Action = actListInsert
    end
    object pmiListEdit: TMenuItem
      Action = actListEdit
    end
    object pmiListDelete: TMenuItem
      Action = actListDelete
    end
  end
  object pmHistory: TPopupMenu
    Left = 186
    Top = 199
    object pmiDisableRule: TMenuItem
      Action = actDisableRule
    end
  end
end
