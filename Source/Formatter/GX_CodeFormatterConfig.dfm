object fmCodeFormatterConfig: TfmCodeFormatterConfig
  Left = 532
  Top = 220
  HelpContext = 100
  Caption = 'Delphi Code Formatter Configuration'
  ClientHeight = 374
  ClientWidth = 478
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  PopupMenu = pm_Extra
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object p_Main: TPanel
    Left = 0
    Top = 0
    Width = 478
    Height = 340
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 7
    TabOrder = 0
    object pc_Main: TPageControl
      Left = 7
      Top = 7
      Width = 464
      Height = 326
    ActivePage = ts_Indent
      Align = alClient
      TabIndex = 0
      TabOrder = 0
      object ts_Indent: TTabSheet
        Caption = 'Indent'
        object l_SpacesPerIndent: TLabel
          Left = 16
          Top = 8
          Width = 88
          Height = 13
          Caption = 'Spaces per Indent'
        end
        object ed_SpacePerIndent: TEdit
          Left = 16
          Top = 24
          Width = 65
          Height = 21
          TabOrder = 0
          Text = '0'
        end
        object ud_SpacePerIndent: TUpDown
          Left = 81
          Top = 24
          Width = 16
          Height = 21
          Associate = ed_SpacePerIndent
          Min = 0
          Max = 10
          Position = 0
          TabOrder = 1
          Wrap = False
        end
        object chk_IndentComments: TCheckBox
          Left = 216
          Top = 103
          Width = 238
          Height = 14
          Caption = 'Indent comments'
          TabOrder = 5
        end
        object chk_IndentCompDirectives: TCheckBox
          Left = 216
          Top = 123
          Width = 238
          Height = 14
          Caption = 'Indent compiler directives'
          TabOrder = 6
        end
        object chk_NoIndentElseIf: TCheckBox
          Left = 216
          Top = 64
          Width = 238
          Height = 14
          Caption = 'Never indent else if'
          TabOrder = 3
        end
        object chk_NoIndentUsesComma: TCheckBox
          Left = 216
          Top = 142
          Width = 238
          Height = 14
          Hint = 
            'Do not ident units in the uses block if the line starts with a c' +
            'omma.'#13#10'Note: This disables Line Breaks after each unit uses list' +
            '.'
          Caption = 'Do not indent uses starting with ,'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 7
        end
        object grp_ExtraIndentBefore: TGroupBox
          Left = 8
          Top = 56
          Width = 193
          Height = 121
          Caption = 'Extra Indent Before'
          TabOrder = 2
          object chk_IndentCaseElse: TCheckBox
            Left = 8
            Top = 96
            Width = 140
            Height = 14
            Caption = 'else in a case block'
            TabOrder = 3
          end
          object chk_IndentTryElse: TCheckBox
            Left = 8
            Top = 72
            Width = 140
            Height = 13
            Caption = 'else in a try block'
            TabOrder = 2
          end
          object chk_IndentTry: TCheckBox
            Left = 8
            Top = 48
            Width = 140
            Height = 14
            Caption = 'try'
            TabOrder = 1
          end
          object chk_IndentBegin: TCheckBox
            Left = 8
            Top = 24
            Width = 140
            Height = 13
            Caption = 'begin'
            TabOrder = 0
          end
        end
        object chk_NoIndentVarDecl: TCheckBox
          Left = 216
          Top = 84
          Width = 238
          Height = 14
          Caption = 'Never indent var declaration'
          TabOrder = 4
        end
      end
      object ts_Spacing: TTabSheet
        Caption = 'Spacing'
        DesignSize = (
          456
          298)
        object grid_Spacing: TStringGrid
          Left = 7
          Top = 7
          Width = 444
          Height = 286
          Anchors = [akLeft, akTop, akRight, akBottom]
          ColCount = 3
          DefaultColWidth = 100
          DefaultRowHeight = 16
          FixedCols = 0
          RowCount = 15
          Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowSizing, goColSizing, goEditing]
          TabOrder = 0
          ColWidths = (
            187
            119
            108)
        end
      end
      object ts_LineBreaks: TTabSheet
        Caption = 'Line Breaks'
        object l_BeginStyle: TLabel
          Left = 240
          Top = 96
          Width = 52
          Height = 13
          Caption = 'Begin style'
        end
        object l_WrapAtPosition: TLabel
          Left = 248
          Top = 216
          Width = 51
          Height = 13
          Caption = 'At position'
        end
        object l_TryStyle: TLabel
          Left = 240
          Top = 144
          Width = 42
          Height = 13
          Caption = 'Try style'
        end
        object cmb_FeedRoundBegin: TComboBox
          Left = 240
          Top = 112
          Width = 169
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 2
          Items.Strings = (
            'Unchanged'
            'Hanging begin'
            'Break before and after begin')
        end
        object chk_WrapLines: TCheckBox
          Left = 240
          Top = 192
          Width = 167
          Height = 18
          Caption = 'Wrap long lines'
          TabOrder = 4
        end
        object ed_WrapPosition: TEdit
          Left = 264
          Top = 232
          Width = 33
          Height = 21
          TabOrder = 5
          Text = '0'
        end
        object ud_WrapPosition: TUpDown
          Left = 297
          Top = 232
          Width = 13
          Height = 21
          Associate = ed_WrapPosition
          Min = 0
          Max = 254
          Position = 0
          TabOrder = 6
          Wrap = False
        end
        object grp_AlwaysBreakLine: TGroupBox
          Left = 8
          Top = 8
          Width = 217
          Height = 266
          Caption = 'Always Break Line'
          TabOrder = 0
          object chk_FeedAfterVar: TCheckBox
            Left = 8
            Top = 24
            Width = 201
            Height = 13
            Caption = 'After "var", "type" etc.'
            TabOrder = 0
          end
          object chk_FeedBeforeEnd: TCheckBox
            Left = 8
            Top = 72
            Width = 201
            Height = 13
            Caption = 'Before "end"'
            TabOrder = 2
          end
          object chk_FeedAfterSemiColon: TCheckBox
            Left = 8
            Top = 96
            Width = 201
            Height = 14
            Caption = 'After semicolon (except directives)'
            TabOrder = 3
          end
          object chk_FeedElseIf: TCheckBox
            Left = 8
            Top = 120
            Width = 201
            Height = 13
            Caption = 'Between else and if'
            TabOrder = 4
          end
          object chk_FeedAfterThen: TCheckBox
            Left = 8
            Top = 144
            Width = 201
            Height = 14
            Caption = 'After "then","else","do",":"'
            TabOrder = 5
            OnClick = chk_FeedAfterThenClick
          end
          object chk_ExceptSingle: TCheckBox
            Left = 16
            Top = 168
            Width = 193
            Height = 13
            Caption = 'Except single lines'
            TabOrder = 6
          end
          object chk_NoFeedBeforeThen: TCheckBox
            Left = 8
            Top = 192
            Width = 201
            Height = 14
            Caption = 'Never before "then", "do"'
            TabOrder = 7
          end
          object chk_FeedEachUnit: TCheckBox
            Left = 8
            Top = 216
            Width = 201
            Height = 13
            Caption = 'Between every unit in "uses"'
            TabOrder = 8
          end
          object chk_RemoveDoubleBlank: TCheckBox
            Left = 8
            Top = 240
            Width = 201
            Height = 14
            Caption = 'Remove double blank lines'
            TabOrder = 9
          end
          object chk_FeedBeforeElse: TCheckBox
            Left = 8
            Top = 48
            Width = 201
            Height = 14
            Caption = 'Before "else"'
            TabOrder = 1
          end
        end
        object grp_ForceBlankLineBetween: TGroupBox
          Left = 232
          Top = 8
          Width = 193
          Height = 74
          Caption = 'Force a Blank Line Between'
          TabOrder = 1
          object chk_BlankProc: TCheckBox
            Left = 8
            Top = 24
            Width = 177
            Height = 13
            Caption = 'Main procedures/functions'
            TabOrder = 0
          end
          object chk_BlankSubProc: TCheckBox
            Left = 8
            Top = 48
            Width = 177
            Height = 14
            Caption = 'Local procedures/functions'
            TabOrder = 1
          end
        end
        object cmb_FeedRoundTry: TComboBox
          Left = 240
          Top = 160
          Width = 169
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 3
          Items.Strings = (
            'Unchanged'
            'Hanging try'
            'Break before and after try')
        end
      end
      object ts_Capitalization: TTabSheet
        Caption = 'Capitalization'
        DesignSize = (
          456
          298)
        object l_Capitalize: TLabel
          Left = 16
          Top = 8
          Width = 46
          Height = 13
          Caption = 'Capitalize'
        end
        object l_ReservedWords: TLabel
          Left = 16
          Top = 48
          Width = 78
          Height = 13
          Caption = 'Reserved words'
        end
        object l_StandardDirectives: TLabel
          Left = 144
          Top = 48
          Width = 93
          Height = 13
          Caption = 'Standard directives'
        end
        object l_Identifiers: TLabel
          Left = 272
          Top = 48
          Width = 49
          Height = 13
          Caption = 'Identifiers'
        end
        object chk_UpperCompDirectives: TCheckBox
          Left = 16
          Top = 24
          Width = 118
          Height = 20
          Caption = 'Compiler directives'
          TabOrder = 0
        end
        object chk_UpperNumbers: TCheckBox
          Left = 136
          Top = 24
          Width = 117
          Height = 20
          Caption = 'Hex numbers'
          TabOrder = 1
        end
        object cmb_ReservedCase: TComboBox
          Left = 16
          Top = 64
          Width = 118
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 2
          Items.Strings = (
            'Lower case'
            'Upper case'
            'Only first up'
            'Unchanged')
        end
        object cmb_StandDirectives: TComboBox
          Left = 144
          Top = 64
          Width = 117
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 3
          Items.Strings = (
            'Lower case'
            'Upper case'
            'Only first up'
            'Unchanged')
        end
        object b_EditCapitalization: TButton
          Left = 377
          Top = 232
          Width = 75
          Height = 25
          Anchors = [akTop, akRight]
          Caption = 'Edit ...'
          TabOrder = 8
          OnClick = b_EditCapitalizationClick
        end
        object rb_CapitalizationInRegistry: TRadioButton
          Left = 16
          Top = 232
          Width = 353
          Height = 13
          Caption = 'Stored in Registry (slow for large lists)'
          Checked = True
          TabOrder = 6
          TabStop = True
          OnClick = rb_CapitalizationInRegistryClick
        end
        object rb_CapitalizationInFile: TRadioButton
          Left = 16
          Top = 248
          Width = 353
          Height = 14
          Caption = 'Stored in File'
          TabOrder = 7
          TabStop = True
          OnClick = rb_CapitalizationInFileClick
        end
        object ed_CapitalizationFile: TEdit
          Left = 32
          Top = 264
          Width = 337
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          Enabled = False
          TabOrder = 9
        end
        object b_CapitalizationSelect: TButton
          Left = 377
          Top = 262
          Width = 75
          Height = 25
          Anchors = [akTop, akRight]
          Caption = 'Select ...'
          Enabled = False
          TabOrder = 10
          OnClick = b_CapitalizationSelectClick
        end
        object rg_Capitalization: TRadioGroup
          Left = 8
          Top = 96
          Width = 385
          Height = 121
          Caption = 'User Defined Capitalization'
          Items.Strings = (
            'Do not use list'
            'Add new words only'
            'Use list'
            'Use list (except standard directives)'
            'Add and use'
            'Add and use (except standard directives)')
          TabOrder = 5
        end
        object cmb_IdentifiersCase: TComboBox
          Left = 272
          Top = 64
          Width = 118
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 4
          Items.Strings = (
            'Lower case'
            'Upper case'
            'Only first up'
            'Unchanged'
            'Use first occurence')
        end
      end
      object ts_Align: TTabSheet
        Caption = 'Align'
        object l_AlignComentsAtPosition: TLabel
          Left = 40
          Top = 32
          Width = 51
          Height = 13
          Caption = 'At position'
        end
        object l_AlignVarAtPosition: TLabel
          Left = 40
          Top = 96
          Width = 51
          Height = 13
          Caption = 'At position'
        end
        object chk_AlignComments: TCheckBox
          Left = 16
          Top = 16
          Width = 275
          Height = 14
          Caption = 'Align simple comments after code'
          TabOrder = 0
        end
        object ed_AlignCommentPos: TEdit
          Left = 40
          Top = 48
          Width = 51
          Height = 21
          TabOrder = 1
          Text = '0'
        end
        object ud_AlignCommentPos: TUpDown
          Left = 91
          Top = 48
          Width = 13
          Height = 21
          Associate = ed_AlignCommentPos
          Min = 0
          Position = 0
          TabOrder = 2
          Wrap = False
        end
        object chk_AlignVar: TCheckBox
          Left = 16
          Top = 80
          Width = 275
          Height = 14
          Caption = 'Align var/const statements'
          TabOrder = 3
        end
        object ed_AlignVarPos: TEdit
          Left = 40
          Top = 112
          Width = 51
          Height = 21
          TabOrder = 4
          Text = '0'
        end
        object ud_AlignVarPos: TUpDown
          Left = 91
          Top = 112
          Width = 13
          Height = 21
          Associate = ed_AlignVarPos
          Min = 0
          Position = 0
          TabOrder = 5
          Wrap = False
        end
      end
      object ts_Misc: TTabSheet
        Caption = 'Misc.'
        object grp_ConfigPrecedence: TGroupBox
          Left = 7
          Top = 72
          Width = 274
          Height = 97
          Caption = 'Configuration Precedence'
          TabOrder = 1
          object lb_Precedence: TListBox
            Left = 7
            Top = 20
            Width = 178
            Height = 69
            ItemHeight = 13
            TabOrder = 0
            OnClick = lb_PrecedenceClick
          end
          object b_PrecedenceUp: TButton
            Left = 193
            Top = 20
            Width = 75
            Height = 25
            Caption = 'Move Up'
            TabOrder = 1
            OnClick = b_PrecedenceUpClick
          end
          object b_PrecedenceDown: TButton
            Left = 193
            Top = 54
            Width = 75
            Height = 25
            Caption = 'Move Down'
            TabOrder = 2
            OnClick = b_PrecedenceDownClick
          end
        end
        object grp_DirectivesPreventFormatting: TGroupBox
          Left = 7
          Top = 7
          Width = 250
          Height = 59
          Caption = 'Source Directives to Prevent Formatting'
          TabOrder = 0
          object l_MiscStart: TLabel
            Left = 7
            Top = 20
            Width = 24
            Height = 13
            Caption = 'Start'
          end
          object l_MiscEnd: TLabel
            Left = 124
            Top = 20
            Width = 18
            Height = 13
            Caption = 'End'
          end
          object ed_StartComment: TEdit
            Left = 7
            Top = 33
            Width = 108
            Height = 21
            MaxLength = 20
            TabOrder = 0
          end
          object ed_EndCommentOut: TEdit
            Left = 124
            Top = 33
            Width = 108
            Height = 21
            MaxLength = 20
            TabOrder = 1
          end
        end
      end
      object ts_Preview: TTabSheet
        Caption = 'Preview'
        ImageIndex = 6
        OnResize = ts_PreviewResize
        OnShow = ts_PreviewShow
        object l_Before: TLabel
          Left = 0
          Top = 0
          Width = 32
          Height = 13
          Caption = 'Before'
        end
        object l_After: TLabel
          Left = 202
          Top = 0
          Width = 22
          Height = 13
          Caption = 'After'
          Color = clBtnFace
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -9
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentColor = False
          ParentFont = False
        end
      end
    end
  end
  object p_Botton: TPanel
    Left = 0
    Top = 340
    Width = 478
    Height = 34
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      478
      34)
    object b_Help: TButton
      Left = 8
      Top = 1
      Width = 75
      Height = 25
      Caption = 'Help'
      TabOrder = 0
      OnClick = b_HelpClick
    end
    object b_Ok: TButton
      Left = 316
      Top = 1
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 2
    end
    object b_Cancel: TButton
      Left = 397
      Top = 1
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 3
    end
    object b_Tools: TButton
      Left = 88
      Top = 1
      Width = 75
      Height = 25
      Hint = 
        'You can add {GXFormatter.config=<name>} as the first line to a u' +
        'nit to force a configuration for that particular unit.'
      Caption = 'Tools >'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = b_ToolsClick
    end
  end
  object OpenDialog: TOpenDialog
    Filter = 'Any file(*.*)|*.*|Text file (*.txt)|*.txt'
    Left = 581
    Top = 7
  end
  object pm_Extra: TPopupMenu
    Left = 328
    Top = 40
    object mi_ResetTo: TMenuItem
      Caption = 'Reset to'
      object mi_ResetToDefault: TMenuItem
        Caption = '<default>'
        OnClick = mi_ResetToDefaultClick
      end
    end
    object mi_Import: TMenuItem
      Caption = 'Import ...'
      OnClick = mi_ImportClick
    end
    object mi_Export: TMenuItem
      Caption = 'Export ...'
      OnClick = mi_ExportClick
    end
  end
  object od_Import: TOpenDialog
    Filter = 'INI-Files (*.ini)|*.ini|all files (*.*)|*.*'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing, ofDontAddToRecent]
    Left = 240
    Top = 280
  end
  object sd_Export: TSaveDialog
    Filter = 'INI-Files (*.ini)|*.ini|all files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofNoReadOnlyReturn, ofEnableSizing, ofDontAddToRecent]
    Left = 184
    Top = 272
  end
  object od_CapitalizationFile: TOpenDialog
    DefaultExt = 'txt'
    Filter = 'text files (*.txt)|*.txt|all files (*.*)|*.*'
    FilterIndex = 0
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 304
    Top = 288
  end
end
