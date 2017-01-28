object fmOpenFileConfig: TfmOpenFileConfig
  Left = 332
  Top = 206
  AutoScroll = False
  BorderIcons = [biSystemMenu]
  Caption = 'Open File Configuration'
  ClientHeight = 452
  ClientWidth = 605
  Color = clBtnFace
  Constraints.MinHeight = 400
  Constraints.MinWidth = 550
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 14
  object pnlButtons: TPanel
    Left = 0
    Top = 419
    Width = 605
    Height = 33
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object pnlButtonsRight: TPanel
      Left = 304
      Top = 0
      Width = 301
      Height = 33
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object btnHelp: TButton
        Left = 220
        Top = 2
        Width = 75
        Height = 25
        Caption = 'Help'
        ModalResult = 2
        TabOrder = 2
        OnClick = btnHelpClick
      end
      object btnCancel: TButton
        Left = 139
        Top = 2
        Width = 75
        Height = 25
        Cancel = True
        Caption = 'Cancel'
        ModalResult = 2
        TabOrder = 1
      end
      object btnOK: TButton
        Left = 57
        Top = 2
        Width = 75
        Height = 25
        Caption = 'OK'
        Default = True
        TabOrder = 0
        OnClick = btnOKClick
      end
    end
  end
  object pnlConfig: TPanel
    Left = 0
    Top = 0
    Width = 605
    Height = 419
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 6
    TabOrder = 0
    object pcConfigPages: TPageControl
      Left = 6
      Top = 6
      Width = 593
      Height = 407
      ActivePage = tsTypes
      Align = alClient
      TabIndex = 0
      TabOrder = 0
      object tsTypes: TTabSheet
        Caption = 'File Groups'
        DesignSize = (
          585
          378)
        object lblExtension: TLabel
          Left = 177
          Top = 13
          Width = 79
          Height = 14
          Alignment = taRightJustify
          Caption = 'File extensions'
          FocusControl = edtExtension
        end
        object lblMaxRecentFiles: TLabel
          Left = 171
          Top = 45
          Width = 85
          Height = 14
          Alignment = taRightJustify
          Caption = 'Max recent files'
          FocusControl = edtMaxRecentFiles
        end
        object gbxCustomDirectory: TGroupBox
          Left = 173
          Top = 72
          Width = 403
          Height = 298
          Anchors = [akLeft, akTop, akRight, akBottom]
          Caption = 'Custom Search Path'
          TabOrder = 5
          DesignSize = (
            403
            298)
          object btnDirectory: TSpeedButton
            Left = 371
            Top = 238
            Width = 22
            Height = 20
            Anchors = [akRight, akBottom]
            Caption = '...'
            OnClick = btnDirectoryClick
          end
          object lbxDirectoryList: TListBox
            Left = 10
            Top = 64
            Width = 383
            Height = 169
            Anchors = [akLeft, akTop, akRight, akBottom]
            ItemHeight = 14
            TabOrder = 2
            OnClick = lbxDirectoryListClick
          end
          object btnDirectoryAdd: TBitBtn
            Left = 90
            Top = 265
            Width = 75
            Height = 25
            Anchors = [akLeft, akBottom]
            Caption = 'Add'
            TabOrder = 5
            OnClick = btnDirectoryAddClick
          end
          object btnDirectoryDelete: TBitBtn
            Left = 170
            Top = 265
            Width = 75
            Height = 25
            Anchors = [akLeft, akBottom]
            Caption = 'Delete'
            TabOrder = 6
            OnClick = btnDirectoryDeleteClick
          end
          object chkRecursive: TCheckBox
            Left = 12
            Top = 43
            Width = 250
            Height = 17
            Caption = 'Include subdirectories'
            TabOrder = 1
          end
          object btnDirectoryReplace: TBitBtn
            Left = 10
            Top = 265
            Width = 75
            Height = 25
            Anchors = [akLeft, akBottom]
            Caption = 'Replace'
            TabOrder = 4
            OnClick = btnDirectoryReplaceClick
          end
          object edtDirectory: TEdit
            Left = 10
            Top = 238
            Width = 362
            Height = 22
            Anchors = [akLeft, akRight, akBottom]
            TabOrder = 3
          end
          object chkCustomDirectoryList: TCheckBox
            Left = 12
            Top = 21
            Width = 250
            Height = 17
            Caption = 'Add custom directories to search path'
            TabOrder = 0
            OnClick = chkCustomDirectoryListClick
          end
        end
        object lbxTypeList: TListBox
          Left = 8
          Top = 9
          Width = 153
          Height = 329
          Anchors = [akLeft, akTop, akBottom]
          ItemHeight = 14
          Sorted = True
          TabOrder = 0
          OnClick = lbxTypeListClick
        end
        object edtExtension: TEdit
          Left = 264
          Top = 9
          Width = 311
          Height = 22
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 3
        end
        object btnTypeAdd: TBitBtn
          Left = 8
          Top = 344
          Width = 75
          Height = 25
          Anchors = [akLeft, akBottom]
          Caption = 'Add'
          TabOrder = 1
          OnClick = btnTypeAddClick
        end
        object btnTypeDelete: TBitBtn
          Left = 87
          Top = 344
          Width = 75
          Height = 25
          Anchors = [akLeft, akBottom]
          Caption = 'Delete'
          TabOrder = 2
          OnClick = btnTypeDeleteClick
        end
        object edtMaxRecentFiles: TEdit
          Left = 265
          Top = 40
          Width = 56
          Height = 22
          TabOrder = 4
          Text = '20'
        end
      end
      object tsIDEOptions: TTabSheet
        Caption = 'Settings'
        ImageIndex = 1
        object gbxIDEMenuItems: TGroupBox
          Left = 16
          Top = 161
          Width = 323
          Height = 144
          Caption = 'IDE Menu Item Overrides'
          TabOrder = 1
          object lblMenuItem: TLabel
            Left = 20
            Top = 20
            Width = 83
            Height = 14
            Caption = 'IDE Menu Item'
          end
          object lblFileGroup: TLabel
            Left = 137
            Top = 20
            Width = 97
            Height = 14
            Caption = 'Default File Group'
            FocusControl = cbxViewUnitType
          end
          object bvlRow: TBevel
            Left = 16
            Top = 38
            Width = 295
            Height = 5
            Shape = bsTopLine
          end
          object chkOverrideViewUnit: TCheckBox
            Left = 20
            Top = 49
            Width = 110
            Height = 17
            Caption = 'View units'
            TabOrder = 0
          end
          object chkOverrideViewForm: TCheckBox
            Left = 20
            Top = 80
            Width = 110
            Height = 17
            Caption = 'View forms'
            TabOrder = 2
          end
          object cbxViewUnitType: TComboBox
            Left = 136
            Top = 46
            Width = 160
            Height = 22
            Style = csDropDownList
            ItemHeight = 14
            TabOrder = 1
          end
          object cbxViewFormType: TComboBox
            Left = 136
            Top = 78
            Width = 160
            Height = 22
            Style = csDropDownList
            ItemHeight = 14
            TabOrder = 3
          end
          object chkOverrideOpenProject: TCheckBox
            Left = 20
            Top = 112
            Width = 110
            Height = 17
            Caption = 'Open project'
            TabOrder = 4
          end
          object cbxOpenProjectType: TComboBox
            Left = 136
            Top = 110
            Width = 160
            Height = 22
            Style = csDropDownList
            ItemHeight = 14
            TabOrder = 5
          end
        end
        object gbxGeneralSettings: TGroupBox
          Left = 16
          Top = 12
          Width = 323
          Height = 133
          Caption = 'General Settings'
          TabOrder = 0
          object lblDefault: TLabel
            Left = 20
            Top = 56
            Width = 94
            Height = 14
            Caption = 'Default file group'
            FocusControl = cbxDefaultFileTypes
          end
          object cbxDefaultFileTypes: TComboBox
            Left = 136
            Top = 50
            Width = 160
            Height = 22
            Style = csDropDownList
            ItemHeight = 14
            TabOrder = 1
          end
          object chkMatchAnywhere: TCheckBox
            Left = 20
            Top = 24
            Width = 301
            Height = 17
            Caption = 'Match typed string anywhere in the file names'
            TabOrder = 0
          end
          object chkMapTab: TCheckBox
            Left = 20
            Top = 88
            Width = 301
            Height = 17
            Caption = 'Show Map tab with files from project'#39's .map file'
            TabOrder = 2
          end
        end
      end
    end
  end
end
