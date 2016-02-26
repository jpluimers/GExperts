object fmGrepSearch: TfmGrepSearch
  Left = 311
  Top = 189
  BorderStyle = bsDialog
  Caption = 'Grep Search'
  ClientHeight = 473
  ClientWidth = 489
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  DesignSize = (
    489
    473)
  PixelsPerInch = 96
  TextHeight = 14
  object lblFind: TLabel
    Left = 14
    Top = 12
    Width = 66
    Height = 14
    Alignment = taRightJustify
    Caption = '&Text to find'
    FocusControl = cbText
  end
  object cbText: TComboBox
    Left = 85
    Top = 8
    Width = 397
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    DropDownCount = 15
    ItemHeight = 14
    TabOrder = 0
    OnKeyDown = ComboKeyDown
  end
  object gbxOptions: TGroupBox
    Left = 8
    Top = 40
    Width = 227
    Height = 145
    Caption = 'Options'
    TabOrder = 1
    object cbCaseSensitive: TCheckBox
      Left = 10
      Top = 18
      Width = 200
      Height = 17
      Caption = '&Case sensitive'
      TabOrder = 0
    end
    object cbForms: TCheckBox
      Left = 10
      Top = 58
      Width = 200
      Height = 17
      Caption = 'Search for&m files'
      TabOrder = 2
    end
    object cbWholeWord: TCheckBox
      Left = 10
      Top = 38
      Width = 200
      Height = 17
      Caption = '&Whole word'
      TabOrder = 1
    end
    object cbRegEx: TCheckBox
      Left = 10
      Top = 98
      Width = 200
      Height = 17
      Caption = 'Regular e&xpression'
      TabOrder = 4
    end
    object cbSQLFiles: TCheckBox
      Left = 10
      Top = 78
      Width = 200
      Height = 17
      Caption = 'Search S&QL files'
      TabOrder = 3
    end
  end
  object gbxWhere: TGroupBox
    Left = 252
    Top = 40
    Width = 230
    Height = 145
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Where'
    TabOrder = 2
    object rbAllProjFiles: TRadioButton
      Left = 10
      Top = 58
      Width = 200
      Height = 17
      Caption = 'All files in &project'
      Checked = True
      TabOrder = 2
      TabStop = True
      OnClick = rbDirectoriesClick
    end
    object rbOpenFiles: TRadioButton
      Left = 10
      Top = 78
      Width = 200
      Height = 17
      Caption = '&Open project files'
      TabOrder = 3
      OnClick = rbDirectoriesClick
    end
    object rbDirectories: TRadioButton
      Left = 10
      Top = 98
      Width = 200
      Height = 17
      Caption = '&Directories'
      TabOrder = 4
      OnClick = rbDirectoriesClick
    end
    object rbCurrentOnly: TRadioButton
      Left = 10
      Top = 18
      Width = 200
      Height = 17
      Caption = 'Current &file'
      TabOrder = 0
      OnClick = rbDirectoriesClick
    end
    object rbAllProjGroupFiles: TRadioButton
      Left = 10
      Top = 38
      Width = 200
      Height = 17
      Caption = 'All files in project &group'
      TabOrder = 1
      OnClick = rbDirectoriesClick
    end
    object rbResults: TRadioButton
      Left = 10
      Top = 119
      Width = 200
      Height = 17
      Caption = 'Pre&vious search result files'
      TabOrder = 5
      OnClick = rbDirectoriesClick
    end
  end
  object gbxContentTypes: TGroupBox
    Left = 8
    Top = 192
    Width = 227
    Height = 102
    Caption = 'Content types'
    TabOrder = 3
    object cbGrepCode: TCheckBox
      Left = 10
      Top = 18
      Width = 200
      Height = 17
      Caption = 'Grep code'
      TabOrder = 0
    end
    object cbGrepStrings: TCheckBox
      Left = 10
      Top = 38
      Width = 200
      Height = 17
      Caption = 'Grep strings'
      TabOrder = 1
    end
    object cbGrepComments: TCheckBox
      Left = 10
      Top = 58
      Width = 200
      Height = 17
      Caption = 'Grep comments'
      TabOrder = 2
    end
  end
  object gbxUnitSections: TGroupBox
    Left = 252
    Top = 192
    Width = 229
    Height = 102
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Unit sections'
    TabOrder = 4
    object cbSectionInterface: TCheckBox
      Left = 10
      Top = 18
      Width = 200
      Height = 17
      Caption = 'Interface'
      TabOrder = 0
    end
    object cbSectionImplementation: TCheckBox
      Left = 10
      Top = 38
      Width = 200
      Height = 17
      Caption = 'Implementation'
      TabOrder = 1
    end
    object cbSectionInitialization: TCheckBox
      Left = 10
      Top = 58
      Width = 200
      Height = 17
      Caption = 'Initialization'
      TabOrder = 2
    end
    object cbSectionFinalization: TCheckBox
      Left = 10
      Top = 78
      Width = 200
      Height = 17
      Caption = 'Finalization'
      TabOrder = 3
    end
  end
  object gbxDirectories: TGroupBox
    Left = 8
    Top = 300
    Width = 474
    Height = 133
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Directory Search'
    TabOrder = 5
    DesignSize = (
      474
      133)
    object lblMasks: TLabel
      Left = 25
      Top = 84
      Width = 53
      Height = 14
      Alignment = taRightJustify
      Caption = 'File mas&ks'
      FocusControl = cbMasks
    end
    object lblDirectory: TLabel
      Left = 21
      Top = 26
      Width = 57
      Height = 14
      Alignment = taRightJustify
      Caption = 'Di&rectories'
      FocusControl = cbDirectory
    end
    object lblExcludeDirs: TLabel
      Left = 13
      Top = 55
      Width = 65
      Height = 14
      Alignment = taRightJustify
      Caption = 'Exclude Dirs'
      FocusControl = cbExcludedDirs
    end
    object cbMasks: TComboBox
      Left = 84
      Top = 80
      Width = 355
      Height = 22
      Anchors = [akLeft, akTop, akRight]
      DropDownCount = 15
      ItemHeight = 14
      TabOrder = 3
      OnKeyDown = ComboKeyDown
    end
    object cbInclude: TCheckBox
      Left = 84
      Top = 106
      Width = 379
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Search su&bdirectories'
      TabOrder = 4
    end
    object cbDirectory: TComboBox
      Left = 84
      Top = 22
      Width = 355
      Height = 22
      Anchors = [akLeft, akTop, akRight]
      DropDownCount = 15
      ItemHeight = 14
      TabOrder = 0
      OnDropDown = cbDirectoryDropDown
      OnKeyDown = ComboKeyDown
    end
    object btnBrowse: TButton
      Left = 440
      Top = 22
      Width = 20
      Height = 20
      Hint = 'Select Directory'
      Anchors = [akTop, akRight]
      Caption = '...'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      TabStop = False
      OnClick = btnBrowseClick
    end
    object cbExcludedDirs: TComboBox
      Left = 84
      Top = 51
      Width = 355
      Height = 22
      Anchors = [akLeft, akTop, akRight]
      DropDownCount = 15
      ItemHeight = 14
      TabOrder = 2
      OnDropDown = cbExcludedDirsDropDown
      OnKeyDown = ComboKeyDown
    end
  end
  object btnOK: TButton
    Left = 248
    Top = 440
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 6
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 328
    Top = 440
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 7
  end
  object btnHelp: TButton
    Left = 408
    Top = 440
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Help'
    TabOrder = 8
    OnClick = btnHelpClick
  end
  object btnOptions: TButton
    Left = 8
    Top = 440
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Options'
    TabOrder = 9
    OnClick = btnOptionsClick
  end
end
