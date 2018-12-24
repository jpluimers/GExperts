object fmConfiguration: TfmConfiguration
  Left = 411
  Top = 164
  Width = 640
  Height = 569
  BorderIcons = [biSystemMenu]
  Caption = 'GExperts Configuration'
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Scaled = False
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 14
  object pnlMain: TPanel
    Left = 0
    Top = 0
    Width = 624
    Height = 496
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 7
    FullRepaint = False
    TabOrder = 0
    object pcConfig: TPageControl
      Left = 7
      Top = 7
      Width = 610
      Height = 482
      ActivePage = tshExperts
      Align = alClient
      HotTrack = True
      MultiLine = True
      TabIndex = 3
      TabOrder = 0
      OnChange = pcConfigChange
      object tshExperts: TTabSheet
        Caption = 'Experts'
      end
      object tshEditorExperts: TTabSheet
        Caption = 'Editor Experts'
        ImageIndex = 7
      end
      object tshGeneral: TTabSheet
        Caption = 'General'
        object pnlGeneral: TPanel
          Left = 0
          Top = 0
          Width = 602
          Height = 453
          Align = alClient
          BevelOuter = bvNone
          BorderWidth = 8
          TabOrder = 0
          object gbxLocations: TGroupBox
            Left = 8
            Top = 8
            Width = 586
            Height = 169
            Align = alTop
            Caption = 'File Locations'
            TabOrder = 0
            DesignSize = (
              586
              169)
            object lblVCL: TLabel
              Left = 12
              Top = 24
              Width = 113
              Height = 14
              Caption = '&VCL source directory'
              FocusControl = edVCLPath
            end
            object lblConfig: TLabel
              Left = 12
              Top = 72
              Width = 146
              Height = 14
              Caption = '&GExperts storage directory'
              FocusControl = edConfigPath
            end
            object lblHelp: TLabel
              Left = 12
              Top = 120
              Width = 43
              Height = 14
              Caption = 'Help &file'
              FocusControl = edHelpFile
            end
            object sbVCLDir: TButton
              Left = 556
              Top = 40
              Width = 22
              Height = 22
              Anchors = [akTop, akRight]
              Caption = '...'
              TabOrder = 1
              OnClick = sbVCLDirClick
            end
            object sbConfigDir: TButton
              Left = 556
              Top = 88
              Width = 22
              Height = 22
              Anchors = [akTop, akRight]
              Caption = '...'
              TabOrder = 3
              OnClick = sbConfigDirClick
            end
            object sbHelpFile: TButton
              Left = 556
              Top = 136
              Width = 22
              Height = 22
              Anchors = [akTop, akRight]
              Caption = '...'
              TabOrder = 5
              OnClick = sbHelpFileClick
            end
            object edVCLPath: TEdit
              Left = 12
              Top = 40
              Width = 541
              Height = 22
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 0
            end
            object edConfigPath: TEdit
              Left = 12
              Top = 88
              Width = 541
              Height = 22
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 2
            end
            object edHelpFile: TEdit
              Left = 12
              Top = 136
              Width = 541
              Height = 22
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 4
            end
          end
          object gbxCustomFont: TGroupBox
            Left = 8
            Top = 183
            Width = 586
            Height = 65
            Align = alTop
            Caption = 'User Interface'
            TabOrder = 1
            object chkUseCustomFont: TCheckBox
              Left = 16
              Top = 27
              Width = 153
              Height = 21
              Caption = 'Use custom UI font'
              TabOrder = 0
            end
            object btnCustomFont: TButton
              Left = 174
              Top = 24
              Width = 79
              Height = 25
              Caption = 'Font...'
              TabOrder = 1
              OnClick = btnCustomFontClick
            end
          end
          object pnlGeneralSpacer: TPanel
            Left = 8
            Top = 177
            Width = 586
            Height = 6
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 2
          end
        end
      end
      object tshIDE: TTabSheet
        Caption = 'IDE'
        object gbxIDEMenu: TGroupBox
          Left = 8
          Top = 8
          Width = 257
          Height = 105
          Caption = '&Menu'
          TabOrder = 0
          object chkAlphabetizeMenu: TCheckBox
            Left = 8
            Top = 22
            Width = 241
            Height = 17
            Caption = 'Alphabetize the GExperts menu items'
            TabOrder = 0
          end
          object chkPlaceGxMainMenuInToolsMenu: TCheckBox
            Left = 8
            Top = 41
            Width = 241
            Height = 17
            Caption = 'Place GExperts menu under Tools'
            TabOrder = 1
          end
          object chkHideWindowMenu: TCheckBox
            Left = 8
            Top = 60
            Width = 241
            Height = 17
            Caption = 'Hide Window menu'
            TabOrder = 2
          end
          object chkMoveComponentMenu: TCheckBox
            Left = 8
            Top = 81
            Width = 241
            Height = 17
            Caption = 'Move the Component menu to Tools'
            TabOrder = 3
          end
        end
        object gbxIDEForms: TGroupBox
          Left = 8
          Top = 115
          Width = 529
          Height = 326
          Caption = 'IDE'
          TabOrder = 2
          object chkEnhanceDialogs: TCheckBox
            Left = 8
            Top = 22
            Width = 505
            Height = 17
            Hint = 
              'Enhance IDE options dialogs to allow resizing, remember position' +
              's, increase'#13'combobox DropDownCounts, resizable picture open dial' +
              'ogs, collapse Together'#13'options tree node, etc.  (Some enhancemen' +
              'ts require BDS 2006 or later)'
            Caption = 'Enhance IDE dialogs (and fix form positioning bugs)'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 0
            OnClick = chkEnhanceDialogsClick
          end
          object chkEnhanceSearchPaths: TCheckBox
            Left = 24
            Top = 76
            Width = 489
            Height = 17
            Caption = 'Enable drag && drop and autocomplete for search paths'
            TabOrder = 3
          end
          object chkEnhanceToolProperties: TCheckBox
            Left = 24
            Top = 95
            Width = 489
            Height = 17
            Caption = 'Enhance Tools menu Tool Properties dialog'
            TabOrder = 4
          end
          object chkAllowResize: TCheckBox
            Left = 24
            Top = 41
            Width = 489
            Height = 17
            Caption = 'Allow resize and remember size'
            TabOrder = 1
          end
          object chkRememberPosition: TCheckBox
            Left = 48
            Top = 57
            Width = 465
            Height = 17
            Caption = 'Also remember position'
            TabOrder = 2
          end
          object chkEnhanceInstallPackages: TCheckBox
            Left = 24
            Top = 114
            Width = 489
            Height = 17
            Caption = 'Enhance Install Packages dialog with ... Explorer button'
            TabOrder = 5
          end
          object chkEnhanceGotoDialog: TCheckBox
            Left = 24
            Top = 133
            Width = 489
            Height = 17
            Caption = 'Enhance Goto dialog with list of source locations'
            TabOrder = 6
          end
          object chkEnhanceBuildEventsDialog: TCheckBox
            Left = 24
            Top = 190
            Width = 489
            Height = 17
            Caption = 'Enhance Build Events dialog with Favourites'
            TabOrder = 9
          end
          object chkEnhanceApplicationSettingsDialog: TCheckBox
            Left = 24
            Top = 152
            Width = 489
            Height = 17
            Caption = 'Enhance Application Settings dialog with version button'
            TabOrder = 7
          end
          object chkAutoCloseMessage: TCheckBox
            Left = 24
            Top = 209
            Width = 489
            Height = 17
            Caption = 'Automatically close message window after successful compile'
            TabOrder = 10
          end
          object chkForceStartupDesktop: TCheckBox
            Left = 24
            Top = 228
            Width = 489
            Height = 17
            Caption = 'Force desktop on startup (leave empty for last selected)'
            TabOrder = 11
          end
          object cbxDesktop: TComboBox
            Left = 40
            Top = 245
            Width = 145
            Height = 22
            ItemHeight = 14
            TabOrder = 12
          end
          object chkEnhanceDockForms: TCheckBox
            Left = 24
            Top = 171
            Width = 489
            Height = 17
            Caption = 'Enhance Dock Forms to allow minimize and Win+arrow positioning'
            TabOrder = 8
          end
        end
        object gbxObjectInspector: TGroupBox
          Left = 280
          Top = 8
          Width = 257
          Height = 105
          Caption = 'Object Inspector'
          TabOrder = 1
          object chkOIFontNames: TCheckBox
            Left = 8
            Top = 22
            Width = 233
            Height = 17
            Caption = 'Show font names using the font'
            TabOrder = 0
          end
          object chkOIHideHotCmds: TCheckBox
            Left = 8
            Top = 41
            Width = 233
            Height = 17
            Caption = 'Hide Quick Action Panel'
            TabOrder = 1
          end
          object chkOIHideDescPane: TCheckBox
            Left = 8
            Top = 60
            Width = 233
            Height = 17
            Caption = 'Hide Description Panel'
            TabOrder = 2
          end
        end
      end
      object tshOldIdes: TTabSheet
        Caption = 'Delphi 6/7'
        ImageIndex = 8
        object gbxTabDockHost: TGroupBox
          Left = 288
          Top = 8
          Width = 249
          Height = 105
          Caption = 'Tab Dock &Hosts'
          TabOrder = 1
          object chkMultiLineTabDockHost: TCheckBox
            Left = 8
            Top = 22
            Width = 233
            Height = 17
            Caption = 'Enable multiline tabs for docked forms'
            TabOrder = 0
            OnClick = chkMultiLineTabDockHostClick
          end
          object chkDefaultMultiLineTabDockHost: TCheckBox
            Left = 24
            Top = 41
            Width = 217
            Height = 17
            Caption = 'Default to multiline tabs'
            TabOrder = 1
          end
        end
        object gbxCompPalette: TGroupBox
          Left = 8
          Top = 8
          Width = 273
          Height = 159
          Caption = 'Component &Palette'
          TabOrder = 0
          object chkCPMultiLine: TCheckBox
            Left = 8
            Top = 18
            Width = 257
            Height = 17
            Caption = 'Multiline tabs'
            TabOrder = 0
            OnClick = chkCPMultiLineClick
          end
          object chkCPAsButtons: TCheckBox
            Left = 8
            Top = 75
            Width = 257
            Height = 17
            Caption = 'Show tabs as buttons'
            TabOrder = 3
            OnClick = chkCPAsButtonsClick
          end
          object chkCPTabsInPopup: TCheckBox
            Left = 8
            Top = 114
            Width = 257
            Height = 17
            Caption = 'Add popup menu/button with tab names'
            TabOrder = 5
            OnClick = chkCPTabsInPopupClick
          end
          object chkCPFlat: TCheckBox
            Left = 24
            Top = 94
            Width = 241
            Height = 17
            Caption = 'Flat buttons'
            TabOrder = 4
          end
          object chkCPTabsInPopupAlphaSort: TCheckBox
            Left = 24
            Top = 133
            Width = 241
            Height = 19
            Caption = 'Show tab names in alphabetical order'
            TabOrder = 6
          end
          object chkCPScrollOpposite: TCheckBox
            Left = 24
            Top = 37
            Width = 241
            Height = 17
            Caption = 'Scroll &opposite'
            TabOrder = 1
          end
          object chkCPRaggedRight: TCheckBox
            Left = 24
            Top = 56
            Width = 241
            Height = 17
            Caption = 'Ragged &right'
            TabOrder = 2
          end
        end
      end
      object tshEditor: TTabSheet
        Caption = 'Code Editor'
        object lblHideNavBar: TLabel
          Left = 8
          Top = 320
          Width = 297
          Height = 49
          AutoSize = False
          Caption = 
            'You can hide the navigation bar in the Tools -> Options dialog. ' +
            'See Editor Options -> Display -> Show Navigation Toolbar'
          Visible = False
          WordWrap = True
        end
        object gbxEditorTabs: TGroupBox
          Left = 8
          Top = 192
          Width = 225
          Height = 121
          Caption = 'Editor T&abs (Delphi 6 and 7)'
          TabOrder = 1
          object chkMultiLine: TCheckBox
            Left = 8
            Top = 21
            Width = 210
            Height = 17
            Caption = 'Multiline'
            TabOrder = 0
          end
          object chkHotTrack: TCheckBox
            Left = 8
            Top = 59
            Width = 210
            Height = 17
            Caption = 'Hot tracking'
            TabOrder = 2
          end
          object chkButtons: TCheckBox
            Left = 8
            Top = 78
            Width = 210
            Height = 17
            Caption = 'Button style'
            TabOrder = 3
            OnClick = chkButtonsClick
          end
          object chkEditTabButtonsFlat: TCheckBox
            Left = 24
            Top = 97
            Width = 194
            Height = 17
            Caption = 'Flat buttons'
            Enabled = False
            TabOrder = 4
          end
          object chkMiddleButtonClose: TCheckBox
            Left = 8
            Top = 40
            Width = 210
            Height = 17
            Caption = 'Middle mouse button closes tab'
            TabOrder = 1
          end
        end
        object gbxEditorToolBar: TGroupBox
          Left = 8
          Top = 10
          Width = 225
          Height = 172
          Caption = 'Editor &Toolbar'
          TabOrder = 0
          object chkEditorToolBar: TCheckBox
            Left = 8
            Top = 21
            Width = 193
            Height = 17
            Caption = 'Enable editor toolbar'
            TabOrder = 0
            OnClick = chkEditorToolBarClick
          end
          object rgAlign: TRadioGroup
            Left = 24
            Top = 40
            Width = 177
            Height = 93
            Caption = 'Align'
            Items.Strings = (
              'Top'
              'Bottom'
              'Left'
              'Right')
            TabOrder = 1
          end
          object btnConfigureToolBar: TButton
            Left = 36
            Top = 138
            Width = 145
            Height = 25
            Caption = 'Configure Toolbar...'
            TabOrder = 2
            OnClick = btnConfigureToolBarClick
          end
        end
        object chkDisableEDTEnhancements: TCheckBox
          Left = 244
          Top = 12
          Width = 238
          Height = 17
          Caption = '&Disable all editor enhancements'
          TabOrder = 3
          OnClick = chkDisableEDTEnhancementsClick
        end
        object chkHideNavbar: TCheckBox
          Left = 8
          Top = 320
          Width = 305
          Height = 17
          Caption = 'Hide Navigation Bar (Delphi 10 and 10.1)'
          TabOrder = 2
        end
      end
      object tshFormEnhancements: TTabSheet
        Caption = 'Forms'
      end
      object tshSuppressedMessages: TTabSheet
        Caption = 'Suppressed Messages'
        ImageIndex = 6
        DesignSize = (
          602
          453)
        object gbSuppressedMessages: TGroupBox
          Left = 8
          Top = 8
          Width = 433
          Height = 433
          Anchors = [akLeft, akTop, akBottom]
          Caption = 'Suppressed Messages'
          TabOrder = 0
          DesignSize = (
            433
            433)
          object lbSuppressedMesages: TListBox
            Left = 8
            Top = 24
            Width = 321
            Height = 401
            Anchors = [akLeft, akTop, akBottom]
            ItemHeight = 14
            TabOrder = 0
          end
          object btnDeleteSuppressedMessage: TButton
            Left = 344
            Top = 24
            Width = 75
            Height = 25
            Caption = 'Delete'
            TabOrder = 1
            OnClick = btnDeleteSuppressedMessageClick
          end
          object btnClearSuppressedMessages: TButton
            Left = 344
            Top = 56
            Width = 75
            Height = 25
            Caption = 'Clear'
            TabOrder = 2
            OnClick = btnClearSuppressedMessagesClick
          end
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
          Width = 191
          Height = 70
          Caption = 'File &Saving'
          TabOrder = 7
          Visible = False
          object lblEvery: TLabel
            Left = 28
            Top = 44
            Width = 30
            Height = 14
            Alignment = taRightJustify
            Caption = 'Every'
            FocusControl = edtMinutes
          end
          object lblMinutes: TLabel
            Left = 121
            Top = 44
            Width = 53
            Height = 14
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
            Height = 22
            TabOrder = 1
            Text = '1'
          end
          object udMinutes: TUpDown
            Left = 102
            Top = 40
            Width = 16
            Height = 22
            Associate = edtMinutes
            Min = 1
            Max = 1000
            Position = 1
            TabOrder = 2
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
    Top = 496
    Width = 624
    Height = 34
    Align = alBottom
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 1
    object pnlButtonsRight: TPanel
      Left = 337
      Top = 0
      Width = 287
      Height = 34
      Align = alRight
      BevelOuter = bvNone
      FullRepaint = False
      TabOrder = 0
      DesignSize = (
        287
        34)
      object btnOK: TButton
        Left = 48
        Top = 0
        Width = 75
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = 'OK'
        Default = True
        TabOrder = 0
        OnClick = btnOKClick
      end
      object btnCancel: TButton
        Left = 128
        Top = 0
        Width = 75
        Height = 25
        Anchors = [akRight, akBottom]
        Cancel = True
        Caption = 'Cancel'
        ModalResult = 2
        TabOrder = 1
      end
      object btnHelp: TButton
        Left = 208
        Top = 0
        Width = 75
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = '&Help'
        TabOrder = 2
        OnClick = btnHelpClick
      end
    end
    object btnImport: TButton
      Left = 216
      Top = 1
      Width = 75
      Height = 25
      Caption = 'Import ...'
      TabOrder = 3
      Visible = False
    end
    object btnExport: TButton
      Left = 136
      Top = 1
      Width = 75
      Height = 25
      Caption = 'Export ...'
      TabOrder = 2
      Visible = False
      OnClick = btnExportClick
    end
    object btnUsage: TButton
      Left = 8
      Top = 0
      Width = 121
      Height = 25
      Caption = 'Usage (%d) ...'
      TabOrder = 1
      OnClick = btnUsageClick
    end
  end
  object dlgUIFont: TFontDialog
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    MinFontSize = 6
    MaxFontSize = 24
    Options = []
    Left = 420
    Top = 332
  end
  object dlgFont: TFontDialog
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    MinFontSize = 6
    MaxFontSize = 24
    Options = []
    Left = 420
    Top = 364
  end
end
