object fmConfiguration: TfmConfiguration
  Left = 411
  Top = 164
  Width = 519
  Height = 520
  BorderIcons = [biSystemMenu]
  Caption = 'GExperts Configuration'
  Color = clBtnFace
  Constraints.MinHeight = 520
  Constraints.MinWidth = 493
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnKeyDown = FormKeyDown
  OnMouseWheelDown = sbxExpertsMouseWheelDown
  OnMouseWheelUp = sbxExpertsMouseWheelUp
  PixelsPerInch = 96
  TextHeight = 13
  object pnlMain: TPanel
    Left = 0
    Top = 0
    Width = 503
    Height = 450
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 7
    TabOrder = 0
    object pcConfig: TPageControl
      Left = 7
      Top = 7
      Width = 497
      Height = 443
      ActivePage = tshExperts
      Align = alClient
      HotTrack = True
      MultiLine = True
      TabIndex = 0
      TabOrder = 0
      OnChange = pcConfigChange
      object tshExperts: TTabSheet
        Caption = 'Experts'
        object sbxExperts: TScrollBox
          Left = 0
          Top = 0
          Width = 481
          Height = 408
          VertScrollBar.Increment = 40
          VertScrollBar.Range = 920
          VertScrollBar.Tracking = True
          Align = alClient
          AutoScroll = False
          TabOrder = 0
        end
      end
      object tshGeneral: TTabSheet
        Caption = 'File Locations'
        DesignSize = (
          489
          415)
        object gbxLocations: TGroupBox
          Left = 8
          Top = 8
          Width = 471
          Height = 169
          Anchors = [akLeft, akTop, akRight]
          Caption = 'File Locations'
          TabOrder = 0
          DesignSize = (
            471
            169)
          object lblVCL: TLabel
            Left = 12
            Top = 24
            Width = 98
            Height = 13
            Caption = '&VCL source directory'
            FocusControl = edVCLPath
          end
          object lblConfig: TLabel
            Left = 12
            Top = 72
            Width = 124
            Height = 13
            Caption = '&GExperts storage directory'
            FocusControl = edConfigPath
          end
          object lblHelp: TLabel
            Left = 12
            Top = 120
            Width = 38
            Height = 13
            Caption = 'Help &file'
            FocusControl = edHelpFile
          end
          object sbVCLDir: TButton
            Left = 438
            Top = 40
            Width = 21
            Height = 21
            Anchors = [akTop, akRight]
            Caption = '...'
            TabOrder = 1
            OnClick = sbVCLDirClick
          end
          object sbConfigDir: TButton
            Left = 438
            Top = 88
            Width = 21
            Height = 21
            Anchors = [akTop, akRight]
            Caption = '...'
            TabOrder = 3
            OnClick = sbConfigDirClick
          end
          object sbHelpFile: TButton
            Left = 438
            Top = 136
            Width = 21
            Height = 21
            Anchors = [akTop, akRight]
            Caption = '...'
            TabOrder = 5
            OnClick = sbHelpFileClick
          end
          object edVCLPath: TEdit
            Left = 12
            Top = 40
            Width = 426
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 0
          end
          object edConfigPath: TEdit
            Left = 12
            Top = 88
            Width = 426
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 2
          end
          object edHelpFile: TEdit
            Left = 12
            Top = 136
            Width = 426
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 4
          end
        end
      end
      object tshEditorExperts: TTabSheet
        Caption = 'Editor Experts'
        DesignSize = (
          489
          415)
        object gbxKeyboard: TGroupBox
          Left = 8
          Top = 8
          Width = 472
          Height = 397
          Anchors = [akLeft, akTop, akRight, akBottom]
          Caption = 'Editor Experts'
          TabOrder = 0
          DesignSize = (
            472
            397)
          object btnConfigure: TButton
            Left = 387
            Top = 80
            Width = 75
            Height = 26
            Anchors = [akTop, akRight]
            Caption = '&Configure'
            Enabled = False
            TabOrder = 2
            OnClick = btnConfigureClick
          end
          object btnShortcut: TButton
            Left = 387
            Top = 116
            Width = 75
            Height = 26
            Anchors = [akTop, akRight]
            Caption = '&Shortcut'
            Enabled = False
            TabOrder = 3
            OnClick = btnShortcutClick
          end
          object meHelp: TMemo
            Left = 10
            Top = 269
            Width = 452
            Height = 118
            TabStop = False
            Anchors = [akLeft, akRight, akBottom]
            Color = clInfoBk
            ReadOnly = True
            ScrollBars = ssVertical
            TabOrder = 4
          end
          object chkDisableEditorExperts: TCheckBox
            Left = 387
            Top = 22
            Width = 82
            Height = 21
            Anchors = [akTop, akRight]
            Caption = '&Disable'
            TabOrder = 1
            OnClick = chkDisableEditorExpertsClick
          end
          object lvEditorExperts: TListView
            Left = 10
            Top = 22
            Width = 369
            Height = 240
            Anchors = [akLeft, akTop, akRight, akBottom]
            Columns = <
              item
                AutoSize = True
                Caption = 'Expert'
              end
              item
                Caption = 'Shortcut'
                Width = 130
              end>
            ColumnClick = False
            HideSelection = False
            ReadOnly = True
            RowSelect = True
            TabOrder = 0
            ViewStyle = vsReport
            OnChange = lvEditorExpertsChange
            OnDblClick = lvEditorExpertsDblClick
          end
        end
      end
      object tshIDE: TTabSheet
        Caption = 'IDE'
        object gbxIDEMenu: TGroupBox
          Left = 8
          Top = 8
          Width = 330
          Height = 70
          Caption = '&Menu'
          TabOrder = 0
          object chkAlphabetizeMenu: TCheckBox
            Left = 8
            Top = 22
            Width = 310
            Height = 17
            Caption = 'Alphabetize the GExperts menu items'
            TabOrder = 0
          end
          object chkPlaceGxMainMenuInToolsMenu: TCheckBox
            Left = 8
            Top = 41
            Width = 310
            Height = 17
            Caption = 'Place GExperts menu under Tools'
            TabOrder = 1
          end
        end
        object gbxTabDockHost: TGroupBox
          Left = 8
          Top = 165
          Width = 329
          Height = 68
          Caption = 'Tab Dock &Hosts'
          TabOrder = 2
          object chkMultiLineTabDockHost: TCheckBox
            Left = 8
            Top = 22
            Width = 254
            Height = 17
            Caption = 'Enable multiline tabs for docked forms'
            TabOrder = 0
            OnClick = chkMultiLineTabDockHostClick
          end
          object chkDefaultMultiLineTabDockHost: TCheckBox
            Left = 24
            Top = 41
            Width = 237
            Height = 17
            Caption = 'Default to multiline tabs'
            TabOrder = 1
          end
        end
        object gbxCompPalette: TGroupBox
          Left = 8
          Top = 240
          Width = 330
          Height = 165
          Caption = 'Component &Palette'
          TabOrder = 3
          object chkCPMultiLine: TCheckBox
            Left = 8
            Top = 22
            Width = 310
            Height = 17
            Caption = 'Multiline tabs'
            TabOrder = 0
            OnClick = chkCPMultiLineClick
          end
          object chkCPAsButtons: TCheckBox
            Left = 8
            Top = 79
            Width = 310
            Height = 17
            Caption = 'Show tabs as buttons'
            TabOrder = 3
            OnClick = chkCPAsButtonsClick
          end
          object chkCPTabsInPopup: TCheckBox
            Left = 8
            Top = 118
            Width = 310
            Height = 17
            Caption = 'Add popup menu/button with tab names'
            TabOrder = 5
            OnClick = chkCPTabsInPopupClick
          end
          object chkCPFlat: TCheckBox
            Left = 24
            Top = 98
            Width = 300
            Height = 17
            Caption = 'Flat buttons'
            TabOrder = 4
          end
          object chkCPTabsInPopupAlphaSort: TCheckBox
            Left = 24
            Top = 137
            Width = 300
            Height = 19
            Caption = 'Show tab names in alphabetical order'
            TabOrder = 6
          end
          object chkCPScrollOpposite: TCheckBox
            Left = 24
            Top = 41
            Width = 300
            Height = 17
            Caption = 'Scroll &opposite'
            TabOrder = 1
          end
          object chkCPRaggedRight: TCheckBox
            Left = 24
            Top = 60
            Width = 300
            Height = 17
            Caption = 'Ragged &right'
            TabOrder = 2
          end
        end
        object gbxIDEForms: TGroupBox
          Left = 8
          Top = 86
          Width = 330
          Height = 70
          Caption = 'IDE'
          TabOrder = 1
          object chkEnhanceDialogs: TCheckBox
            Left = 9
            Top = 19
            Width = 310
            Height = 17
            Hint = 
              'Enhance IDE options dialogs to allow resizing, remember position' +
              's, increase'#13'combobox DropDownCounts, resizable picture open dial' +
              'ogs, collapse Together'#13'options tree node, etc.  (Most enhancemen' +
              'ts require BDS 2006 or later)'
            Caption = 'Enhance IDE dialogs (allow resize, remember position, etc.)'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 0
            OnClick = chkAutoSaveClick
          end
          object chkOIFontNames: TCheckBox
            Left = 8
            Top = 43
            Width = 310
            Height = 17
            Caption = 'Show object inspector font names using the font'
            TabOrder = 1
          end
        end
      end
      object tshEditor: TTabSheet
        Caption = 'Code Editor'
        object gbxEditor: TGroupBox
          Left = 8
          Top = 8
          Width = 201
          Height = 121
          Caption = 'Editor T&abs'
          TabOrder = 0
          object chkMultiLine: TCheckBox
            Left = 8
            Top = 21
            Width = 189
            Height = 17
            Caption = 'Multiline'
            TabOrder = 0
          end
          object chkHotTrack: TCheckBox
            Left = 8
            Top = 59
            Width = 189
            Height = 17
            Caption = 'Hot tracking'
            TabOrder = 2
          end
          object chkButtons: TCheckBox
            Left = 8
            Top = 78
            Width = 189
            Height = 17
            Caption = 'Button style'
            TabOrder = 3
            OnClick = chkButtonsClick
          end
          object chkEditTabButtonsFlat: TCheckBox
            Left = 24
            Top = 97
            Width = 173
            Height = 17
            Caption = 'Flat buttons'
            Enabled = False
            TabOrder = 4
          end
          object chkMiddleButtonClose: TCheckBox
            Left = 8
            Top = 40
            Width = 189
            Height = 17
            Caption = 'Middle mouse button closes tab'
            TabOrder = 1
          end
        end
        object gbxToolBar: TGroupBox
          Left = 8
          Top = 138
          Width = 201
          Height = 169
          Caption = 'Editor &Toolbar'
          TabOrder = 1
          object chkEditorToolBar: TCheckBox
            Left = 8
            Top = 21
            Width = 145
            Height = 17
            Caption = 'Enable editor toolbar'
            TabOrder = 0
            OnClick = chkEditorToolBarClick
          end
          object rgAlign: TRadioGroup
            Left = 24
            Top = 40
            Width = 161
            Height = 89
            Caption = 'Align'
            Items.Strings = (
              'Top'
              'Bottom'
              'Left'
              'Right')
            TabOrder = 1
          end
          object btnConfigureToolBar: TButton
            Left = 24
            Top = 135
            Width = 137
            Height = 25
            Caption = 'Configure Toolbar...'
            TabOrder = 2
            OnClick = btnConfigureToolBarClick
          end
        end
        object chkDisableEDTEnhancements: TCheckBox
          Left = 216
          Top = 11
          Width = 244
          Height = 17
          Caption = '&Disable all editor enhancements'
          TabOrder = 2
          OnClick = chkDisableEDTEnhancementsClick
        end
      end
      object tshDebug: TTabSheet
        Caption = 'Debug'
        ImageIndex = 6
        object chkEditorKeyTracing: TCheckBox
          Left = 16
          Top = 16
          Width = 233
          Height = 17
          Caption = 'Enable editor key tracing messages'
          TabOrder = 0
          OnClick = chkEditorKeyTracingClick
        end
        object btnEnumerateModules: TButton
          Left = 16
          Top = 39
          Width = 137
          Height = 25
          Caption = 'Enumerate Modules'
          TabOrder = 1
          OnClick = btnEnumerateModulesClick
        end
        object btnEumerateActions: TButton
          Left = 16
          Top = 103
          Width = 137
          Height = 25
          Caption = 'Enumerate Actions'
          TabOrder = 3
          OnClick = btnEumerateActionsClick
        end
        object btnGetFonts: TButton
          Left = 16
          Top = 135
          Width = 137
          Height = 25
          Caption = 'IDE Fonts'
          TabOrder = 4
          OnClick = btnGetFontsClick
        end
        object btnAppBuilder: TButton
          Left = 16
          Top = 167
          Width = 137
          Height = 25
          Caption = 'Application Window'
          TabOrder = 5
          OnClick = btnAppBuilderClick
        end
        object gbxFonts: TGroupBox
          Left = 16
          Top = 207
          Width = 321
          Height = 81
          Caption = 'Custom &Fonts'
          TabOrder = 6
          object btnOIFont: TButton
            Left = 24
            Top = 22
            Width = 119
            Height = 26
            Caption = 'Object Inspector'
            TabOrder = 0
            OnClick = btnFontClick
          end
          object btnCPFont: TButton
            Left = 172
            Top = 22
            Width = 124
            Height = 26
            Caption = 'Component Palette'
            TabOrder = 2
            OnClick = btnFontClick
          end
          object chkOIFontEnabled: TCheckBox
            Left = 55
            Top = 51
            Width = 89
            Height = 17
            Caption = 'Enable'
            TabOrder = 1
            OnClick = chkFontEnabledClick
          end
          object chkCPFontEnabled: TCheckBox
            Left = 203
            Top = 51
            Width = 97
            Height = 17
            Caption = 'Enable'
            TabOrder = 3
            OnClick = chkFontEnabledClick
          end
        end
        object gbxFileSaving: TGroupBox
          Left = 18
          Top = 301
          Width = 175
          Height = 70
          Caption = 'File &Saving'
          TabOrder = 7
          Visible = False
          object lblEvery: TLabel
            Left = 31
            Top = 44
            Width = 27
            Height = 13
            Alignment = taRightJustify
            Caption = 'Every'
          end
          object lblMinutes: TLabel
            Left = 120
            Top = 44
            Width = 42
            Height = 13
            Caption = 'minute(s)'
          end
          object chkAutoSave: TCheckBox
            Left = 12
            Top = 20
            Width = 157
            Height = 17
            Caption = 'Enable auto save of files'
            TabOrder = 0
            OnClick = chkAutoSaveClick
          end
          object edtMinutes: TEdit
            Left = 64
            Top = 40
            Width = 38
            Height = 21
            TabOrder = 1
            Text = '1'
          end
          object udMinutes: TUpDown
            Left = 102
            Top = 40
            Width = 16
            Height = 21
            Associate = edtMinutes
            Min = 1
            Max = 1000
            Position = 1
            TabOrder = 2
            Wrap = False
          end
        end
        object btnEditView: TButton
          Left = 16
          Top = 71
          Width = 137
          Height = 25
          Caption = 'Edit View'
          TabOrder = 2
          OnClick = btnEditViewClick
        end
      end
    end
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 457
    Width = 511
    Height = 34
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      511
      34)
    object btnOK: TButton
      Left = 269
      Top = 1
      Width = 75
      Height = 26
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      Default = True
      TabOrder = 0
      OnClick = btnOKClick
    end
    object btnCancel: TButton
      Left = 349
      Top = 1
      Width = 75
      Height = 26
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object btnHelp: TButton
      Left = 429
      Top = 1
      Width = 75
      Height = 26
      Anchors = [akRight, akBottom]
      Caption = '&Help'
      TabOrder = 2
      OnClick = btnHelpClick
    end
  end
  object dlgHelpFile: TOpenDialog
    DefaultExt = '.chm'
    Filter = 'Help Files (*.chm)|*.chm'
    Options = [ofPathMustExist, ofFileMustExist, ofEnableSizing, ofDontAddToRecent]
    Title = 'Select Help File'
    Left = 358
    Top = 332
  end
  object dlgFont: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    MinFontSize = 6
    MaxFontSize = 24
    Options = []
    Left = 420
    Top = 332
  end
end
