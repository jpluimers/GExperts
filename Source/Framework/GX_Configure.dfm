object fmConfiguration: TfmConfiguration
  Left = 411
  Top = 164
  ActiveControl = edtFilter
  AutoScroll = False
  BorderIcons = [biSystemMenu]
  Caption = 'GExperts Configuration'
  ClientHeight = 531
  ClientWidth = 613
  Color = clBtnFace
  Constraints.MinHeight = 530
  Constraints.MinWidth = 613
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Scaled = False
  OnKeyDown = FormKeyDown
  OnMouseWheelDown = sbxExpertsMouseWheelDown
  OnMouseWheelUp = sbxExpertsMouseWheelUp
  PixelsPerInch = 96
  TextHeight = 14
  object pnlMain: TPanel
    Left = 0
    Top = 0
    Width = 613
    Height = 497
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 7
    FullRepaint = False
    TabOrder = 0
    object pcConfig: TPageControl
      Left = 7
      Top = 7
      Width = 599
      Height = 483
      ActivePage = tshExperts
      Align = alClient
      HotTrack = True
      MultiLine = True
      TabIndex = 0
      TabOrder = 0
      OnChange = pcConfigChange
      object tshExperts: TTabSheet
        Caption = 'Experts'
        object pnlExpertsFilter: TPanel
          Left = 0
          Top = 0
          Width = 591
          Height = 41
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          object lblFilter: TLabel
            Left = 17
            Top = 12
            Width = 26
            Height = 14
            Alignment = taRightJustify
            Caption = '&Filter'
            FocusControl = edtFilter
          end
          object edtFilter: TEdit
            Left = 50
            Top = 8
            Width = 390
            Height = 22
            TabOrder = 0
            OnChange = edtFilterChange
          end
        end
        object sbxExperts: TScrollBox
          Left = 0
          Top = 41
          Width = 591
          Height = 413
          VertScrollBar.Increment = 40
          VertScrollBar.Range = 920
          VertScrollBar.Tracking = True
          Align = alClient
          AutoScroll = False
          TabOrder = 1
          object pnlExpertLayout: TPanel
            Left = 0
            Top = 0
            Width = 572
            Height = 40
            TabOrder = 0
            object imgExpert: TImage
              Left = 4
              Top = 4
              Width = 32
              Height = 32
              Center = True
              Proportional = True
              Transparent = True
            end
            object chkExpert: TCheckBox
              Left = 40
              Top = 12
              Width = 222
              Height = 17
              Caption = 'Set Component Properties'
              TabOrder = 0
            end
            object edtExpert: THotKey
              Left = 267
              Top = 10
              Width = 171
              Height = 22
              HotKey = 32833
              InvalidKeys = [hcNone, hcShift]
              Modifiers = [hkAlt]
              TabOrder = 1
            end
            object btnExpert: TButton
              Left = 455
              Top = 8
              Width = 98
              Height = 25
              Caption = 'Configure...'
              TabOrder = 2
            end
          end
        end
      end
      object tshGeneral: TTabSheet
        Caption = 'General'
        object pnlGeneral: TPanel
          Left = 0
          Top = 0
          Width = 591
          Height = 454
          Align = alClient
          BevelOuter = bvNone
          BorderWidth = 8
          TabOrder = 0
          object gbxLocations: TGroupBox
            Left = 8
            Top = 8
            Width = 575
            Height = 169
            Align = alTop
            Caption = 'File Locations'
            TabOrder = 0
            DesignSize = (
              575
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
              Left = 542
              Top = 40
              Width = 21
              Height = 21
              Anchors = [akTop, akRight]
              Caption = '...'
              TabOrder = 1
              OnClick = sbVCLDirClick
            end
            object sbConfigDir: TButton
              Left = 542
              Top = 88
              Width = 21
              Height = 21
              Anchors = [akTop, akRight]
              Caption = '...'
              TabOrder = 3
              OnClick = sbConfigDirClick
            end
            object sbHelpFile: TButton
              Left = 542
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
              Width = 530
              Height = 22
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 0
            end
            object edConfigPath: TEdit
              Left = 12
              Top = 88
              Width = 530
              Height = 22
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 2
            end
            object edHelpFile: TEdit
              Left = 12
              Top = 136
              Width = 530
              Height = 22
              Anchors = [akLeft, akTop, akRight]
              TabOrder = 4
            end
          end
          object gbxCustomFont: TGroupBox
            Left = 8
            Top = 183
            Width = 575
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
            Width = 575
            Height = 6
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 2
          end
        end
      end
      object tshEditorExperts: TTabSheet
        Caption = 'Editor Experts'
        object pnlExperts: TPanel
          Left = 0
          Top = 0
          Width = 591
          Height = 454
          Align = alClient
          BevelOuter = bvNone
          BorderWidth = 8
          TabOrder = 0
          object gbxKeyboard: TGroupBox
            Left = 8
            Top = 8
            Width = 575
            Height = 438
            Align = alClient
            Caption = 'Editor Experts'
            TabOrder = 0
            DesignSize = (
              575
              438)
            object btnConfigure: TButton
              Left = 488
              Top = 80
              Width = 77
              Height = 26
              Anchors = [akTop, akRight]
              Caption = '&Configure'
              Enabled = False
              TabOrder = 2
              OnClick = btnConfigureClick
            end
            object btnShortcut: TButton
              Left = 488
              Top = 116
              Width = 77
              Height = 26
              Anchors = [akTop, akRight]
              Caption = '&Shortcut'
              Enabled = False
              TabOrder = 3
              OnClick = btnShortcutClick
            end
            object meHelp: TMemo
              Left = 10
              Top = 310
              Width = 555
              Height = 118
              TabStop = False
              Anchors = [akLeft, akRight, akBottom]
              Color = clInfoBk
              ReadOnly = True
              ScrollBars = ssVertical
              TabOrder = 4
            end
            object chkDisableEditorExperts: TCheckBox
              Left = 488
              Top = 22
              Width = 84
              Height = 21
              Anchors = [akTop, akRight]
              Caption = '&Disable'
              TabOrder = 1
              OnClick = chkDisableEditorExpertsClick
            end
            object lvEditorExperts: TListView
              Left = 10
              Top = 22
              Width = 470
              Height = 281
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
      end
      object tshIDE: TTabSheet
        Caption = 'IDE'
        object gbxIDEMenu: TGroupBox
          Left = 8
          Top = 8
          Width = 375
          Height = 70
          Caption = '&Menu'
          TabOrder = 0
          object chkAlphabetizeMenu: TCheckBox
            Left = 8
            Top = 22
            Width = 364
            Height = 17
            Caption = 'Alphabetize the GExperts menu items'
            TabOrder = 0
          end
          object chkPlaceGxMainMenuInToolsMenu: TCheckBox
            Left = 8
            Top = 41
            Width = 364
            Height = 17
            Caption = 'Place GExperts menu under Tools'
            TabOrder = 1
          end
        end
        object gbxTabDockHost: TGroupBox
          Left = 8
          Top = 165
          Width = 375
          Height = 68
          Caption = 'Tab Dock &Hosts'
          TabOrder = 2
          object chkMultiLineTabDockHost: TCheckBox
            Left = 8
            Top = 22
            Width = 364
            Height = 17
            Caption = 'Enable multiline tabs for docked forms'
            TabOrder = 0
            OnClick = chkMultiLineTabDockHostClick
          end
          object chkDefaultMultiLineTabDockHost: TCheckBox
            Left = 24
            Top = 41
            Width = 348
            Height = 17
            Caption = 'Default to multiline tabs'
            TabOrder = 1
          end
        end
        object gbxCompPalette: TGroupBox
          Left = 8
          Top = 240
          Width = 375
          Height = 165
          Caption = 'Component &Palette'
          TabOrder = 3
          object chkCPMultiLine: TCheckBox
            Left = 8
            Top = 22
            Width = 364
            Height = 17
            Caption = 'Multiline tabs'
            TabOrder = 0
            OnClick = chkCPMultiLineClick
          end
          object chkCPAsButtons: TCheckBox
            Left = 8
            Top = 79
            Width = 364
            Height = 17
            Caption = 'Show tabs as buttons'
            TabOrder = 3
            OnClick = chkCPAsButtonsClick
          end
          object chkCPTabsInPopup: TCheckBox
            Left = 8
            Top = 118
            Width = 364
            Height = 17
            Caption = 'Add popup menu/button with tab names'
            TabOrder = 5
            OnClick = chkCPTabsInPopupClick
          end
          object chkCPFlat: TCheckBox
            Left = 24
            Top = 98
            Width = 348
            Height = 17
            Caption = 'Flat buttons'
            TabOrder = 4
          end
          object chkCPTabsInPopupAlphaSort: TCheckBox
            Left = 24
            Top = 137
            Width = 348
            Height = 19
            Caption = 'Show tab names in alphabetical order'
            TabOrder = 6
          end
          object chkCPScrollOpposite: TCheckBox
            Left = 24
            Top = 41
            Width = 348
            Height = 17
            Caption = 'Scroll &opposite'
            TabOrder = 1
          end
          object chkCPRaggedRight: TCheckBox
            Left = 24
            Top = 60
            Width = 348
            Height = 17
            Caption = 'Ragged &right'
            TabOrder = 2
          end
        end
        object gbxIDEForms: TGroupBox
          Left = 8
          Top = 86
          Width = 375
          Height = 70
          Caption = 'IDE'
          TabOrder = 1
          object chkEnhanceDialogs: TCheckBox
            Left = 8
            Top = 19
            Width = 364
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
            Width = 364
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
          Width = 225
          Height = 121
          Caption = 'Editor T&abs'
          TabOrder = 0
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
        object gbxToolBar: TGroupBox
          Left = 8
          Top = 138
          Width = 225
          Height = 172
          Caption = 'Editor &Toolbar'
          TabOrder = 1
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
    Top = 497
    Width = 613
    Height = 34
    Align = alBottom
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 1
    object pnlButtonsRight: TPanel
      Left = 326
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
        Left = 30
        Top = 1
        Width = 75
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = 'OK'
        Default = True
        TabOrder = 0
        OnClick = btnOKClick
      end
      object btnCancel: TButton
        Left = 117
        Top = 1
        Width = 75
        Height = 25
        Anchors = [akRight, akBottom]
        Cancel = True
        Caption = 'Cancel'
        ModalResult = 2
        TabOrder = 1
      end
      object btnHelp: TButton
        Left = 204
        Top = 1
        Width = 75
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = '&Help'
        TabOrder = 2
        OnClick = btnHelpClick
      end
    end
  end
  object dlgHelpFile: TOpenDialog
    DefaultExt = '.chm'
    Filter = 'Help Files (*.chm)|*.chm'
    Options = [ofPathMustExist, ofFileMustExist, ofEnableSizing, ofDontAddToRecent]
    Title = 'Select Help File'
    Left = 390
    Top = 332
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
  object tmrFilter: TTimer
    Enabled = False
    Interval = 10
    OnTimer = tmrFilterTimer
    Left = 16
    Top = 176
  end
end
