object fmOpenFileConfig: TfmOpenFileConfig
  Left = 332
  Top = 206
  Width = 621
  Height = 488
  BorderIcons = [biSystemMenu]
  Caption = 'Open Unit Configuration'
  Color = clBtnFace
  Constraints.MinHeight = 400
  Constraints.MinWidth = 550
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    613
    459)
  PixelsPerInch = 96
  TextHeight = 13
  object pcConfigPages: TPageControl
    Left = 7
    Top = 6
    Width = 599
    Height = 412
    ActivePage = tsTypes
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabIndex = 0
    TabOrder = 0
    object tsTypes: TTabSheet
      Caption = 'File Groups'
      DesignSize = (
        591
        384)
      object lblExtension: TLabel
        Left = 187
        Top = 12
        Width = 69
        Height = 13
        Alignment = taRightJustify
        Caption = 'File extensions'
      end
      object lblMaxRecentFiles: TLabel
        Left = 182
        Top = 45
        Width = 74
        Height = 13
        Alignment = taRightJustify
        Caption = 'Max recent files'
      end
      object gbxCustomDirectory: TGroupBox
        Left = 173
        Top = 72
        Width = 409
        Height = 304
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'Custom Search Path'
        TabOrder = 6
        DesignSize = (
          409
          304)
        object btnDirectory: TSpeedButton
          Left = 377
          Top = 244
          Width = 22
          Height = 20
          Anchors = [akRight, akBottom]
          Caption = '...'
          OnClick = btnDirectoryClick
        end
        object lbxDirectoryList: TListBox
          Left = 10
          Top = 64
          Width = 389
          Height = 175
          Anchors = [akLeft, akTop, akRight, akBottom]
          ItemHeight = 13
          TabOrder = 1
          OnClick = lbxDirectoryListClick
        end
        object btnDirectoryAdd: TBitBtn
          Left = 90
          Top = 271
          Width = 75
          Height = 25
          Anchors = [akLeft, akBottom]
          Caption = 'Add'
          TabOrder = 4
          OnClick = btnDirectoryAddClick
        end
        object btnDirectoryDelete: TBitBtn
          Left = 170
          Top = 271
          Width = 75
          Height = 25
          Anchors = [akLeft, akBottom]
          Caption = 'Delete'
          TabOrder = 5
          OnClick = btnDirectoryDeleteClick
        end
        object chkRecursive: TCheckBox
          Left = 12
          Top = 43
          Width = 250
          Height = 17
          Caption = 'Include subdirectories'
          TabOrder = 0
        end
        object btnDirectoryReplace: TBitBtn
          Left = 10
          Top = 271
          Width = 75
          Height = 25
          Anchors = [akLeft, akBottom]
          Caption = 'Replace'
          TabOrder = 3
          OnClick = btnDirectoryReplaceClick
        end
        object edtDirectory: TEdit
          Left = 10
          Top = 244
          Width = 368
          Height = 21
          Anchors = [akLeft, akRight, akBottom]
          TabOrder = 2
        end
      end
      object lbxTypeList: TListBox
        Left = 8
        Top = 9
        Width = 153
        Height = 335
        Anchors = [akLeft, akTop, akBottom]
        ItemHeight = 13
        Sorted = True
        TabOrder = 0
        OnClick = lbxTypeListClick
      end
      object edtExtension: TEdit
        Left = 264
        Top = 8
        Width = 317
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 3
      end
      object btnTypeAdd: TBitBtn
        Left = 8
        Top = 350
        Width = 75
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Add'
        TabOrder = 1
        OnClick = btnTypeAddClick
      end
      object btnTypeDelete: TBitBtn
        Left = 87
        Top = 350
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
        Height = 21
        TabOrder = 4
        Text = '20'
      end
      object chkCustomDirectoryList: TCheckBox
        Left = 185
        Top = 93
        Width = 250
        Height = 17
        Caption = 'Add custom directories to search path'
        TabOrder = 5
        OnClick = chkCustomDirectoryListClick
      end
    end
    object tsIDEOptions: TTabSheet
      Caption = 'Settings'
      ImageIndex = 1
      object gbxIDEMenuItems: TGroupBox
        Left = 16
        Top = 113
        Width = 313
        Height = 141
        Caption = 'IDE Menu Item Overrides'
        TabOrder = 1
        object lblMenuItem: TLabel
          Left = 20
          Top = 20
          Width = 71
          Height = 13
          Caption = 'IDE Menu Item'
        end
        object lblFileGroup: TLabel
          Left = 137
          Top = 20
          Width = 85
          Height = 13
          Caption = 'Default File Group'
        end
        object bvlRow: TBevel
          Left = 16
          Top = 37
          Width = 285
          Height = 5
          Shape = bsTopLine
        end
        object chkOverrideViewUnit: TCheckBox
          Left = 20
          Top = 46
          Width = 110
          Height = 17
          Caption = 'View units'
          TabOrder = 0
        end
        object chkOverrideViewForm: TCheckBox
          Left = 20
          Top = 77
          Width = 110
          Height = 17
          Caption = 'View forms'
          TabOrder = 2
        end
        object cbxViewUnitType: TComboBox
          Left = 136
          Top = 43
          Width = 160
          Height = 21
          Style = csDropDownList
          ItemHeight = 0
          TabOrder = 1
        end
        object cbxViewFormType: TComboBox
          Left = 136
          Top = 75
          Width = 160
          Height = 21
          Style = csDropDownList
          ItemHeight = 0
          TabOrder = 3
        end
        object chkOverrideOpenProject: TCheckBox
          Left = 20
          Top = 109
          Width = 110
          Height = 17
          Caption = 'Open project'
          TabOrder = 4
        end
        object cbxOpenProjectType: TComboBox
          Left = 136
          Top = 107
          Width = 160
          Height = 21
          Style = csDropDownList
          ItemHeight = 0
          TabOrder = 5
        end
      end
      object gbxGeneralSettings: TGroupBox
        Left = 16
        Top = 12
        Width = 313
        Height = 87
        Caption = 'General Settings'
        TabOrder = 0
        object lblDefault: TLabel
          Left = 20
          Top = 56
          Width = 80
          Height = 13
          Caption = 'Default file group'
        end
        object cbxDefaultFileTypes: TComboBox
          Left = 136
          Top = 50
          Width = 160
          Height = 21
          Style = csDropDownList
          ItemHeight = 0
          TabOrder = 1
        end
        object chkMatchAnywhere: TCheckBox
          Left = 20
          Top = 24
          Width = 281
          Height = 17
          Caption = 'Match typed string anywhere in the file names'
          TabOrder = 0
        end
      end
    end
  end
  object btnOK: TButton
    Left = 367
    Top = 426
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 449
    Top = 426
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object btnHelp: TButton
    Left = 531
    Top = 426
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Help'
    ModalResult = 2
    TabOrder = 3
    OnClick = btnHelpClick
  end
end
