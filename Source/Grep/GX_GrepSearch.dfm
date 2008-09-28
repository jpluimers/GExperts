object fmGrepSearch: TfmGrepSearch
  Left = 318
  Top = 209
  BorderStyle = bsDialog
  Caption = 'Grep Search'
  ClientHeight = 346
  ClientWidth = 489
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  DesignSize = (
    489
    346)
  PixelsPerInch = 96
  TextHeight = 13
  object lblFind: TLabel
    Left = 21
    Top = 12
    Width = 53
    Height = 13
    Alignment = taRightJustify
    Caption = '&Text to find'
    FocusControl = cbText
  end
  object cbText: TComboBox
    Left = 80
    Top = 8
    Width = 403
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    DropDownCount = 15
    ItemHeight = 13
    TabOrder = 0
  end
  object gbxOptions: TGroupBox
    Left = 8
    Top = 35
    Width = 473
    Height = 44
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Options'
    TabOrder = 1
    object cbNoCase: TCheckBox
      Left = 10
      Top = 16
      Width = 100
      Height = 17
      Caption = '&Case sensitive'
      TabOrder = 0
    end
    object cbWholeWord: TCheckBox
      Left = 159
      Top = 16
      Width = 120
      Height = 17
      Caption = '&Whole word'
      TabOrder = 1
    end
    object cbRegEx: TCheckBox
      Left = 311
      Top = 16
      Width = 136
      Height = 17
      Caption = 'Regular e&xpression'
      TabOrder = 2
    end
  end
  object gbxWhere: TGroupBox
    Left = 309
    Top = 85
    Width = 172
    Height = 121
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'File Selection'
    TabOrder = 4
    object rbAllProjFiles: TRadioButton
      Left = 10
      Top = 57
      Width = 155
      Height = 17
      Caption = 'All files in &project'
      Checked = True
      TabOrder = 2
      TabStop = True
      OnClick = rbDirectoriesClick
    end
    object rbOpenFiles: TRadioButton
      Left = 10
      Top = 77
      Width = 155
      Height = 17
      Caption = '&Open project files'
      TabOrder = 3
      OnClick = rbDirectoriesClick
    end
    object rbDirectories: TRadioButton
      Left = 10
      Top = 98
      Width = 155
      Height = 17
      Caption = 'Search in &directories'
      TabOrder = 4
      OnClick = rbDirectoriesClick
    end
    object rbCurrentOnly: TRadioButton
      Left = 10
      Top = 16
      Width = 155
      Height = 17
      Caption = 'Current &file only'
      TabOrder = 0
      OnClick = rbDirectoriesClick
    end
    object rbAllProjGroupFiles: TRadioButton
      Left = 10
      Top = 36
      Width = 155
      Height = 17
      Caption = 'All files in project &group'
      TabOrder = 1
      OnClick = rbDirectoriesClick
    end
  end
  object gbxDirectories: TGroupBox
    Left = 8
    Top = 211
    Width = 474
    Height = 97
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Directory Search'
    TabOrder = 5
    DesignSize = (
      474
      97)
    object lblMasks: TLabel
      Left = 15
      Top = 52
      Width = 49
      Height = 13
      Alignment = taRightJustify
      Caption = 'File mas&ks'
      FocusControl = cbMasks
    end
    object lblDirectory: TLabel
      Left = 14
      Top = 26
      Width = 50
      Height = 13
      Alignment = taRightJustify
      Caption = 'Di&rectories'
      FocusControl = cbDirectory
    end
    object cbMasks: TComboBox
      Left = 70
      Top = 48
      Width = 369
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      DropDownCount = 15
      ItemHeight = 13
      TabOrder = 2
    end
    object cbInclude: TCheckBox
      Left = 70
      Top = 72
      Width = 366
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Include su&bdirectories'
      Checked = True
      State = cbChecked
      TabOrder = 3
    end
    object cbDirectory: TComboBox
      Left = 70
      Top = 22
      Width = 369
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      DropDownCount = 15
      ItemHeight = 13
      TabOrder = 0
      OnDropDown = cbDirectoryDropDown
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
  end
  object btnOK: TButton
    Left = 247
    Top = 314
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 6
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 327
    Top = 314
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 7
  end
  object btnHelp: TButton
    Left = 407
    Top = 314
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Help'
    TabOrder = 8
    OnClick = btnHelpClick
  end
  object gbxSections: TGroupBox
    Left = 157
    Top = 85
    Width = 142
    Height = 121
    Anchors = [akLeft, akTop, akBottom]
    Caption = 'Search Delphi Sections'
    TabOrder = 3
    object cbInterface: TCheckBox
      Left = 10
      Top = 17
      Width = 125
      Height = 18
      Caption = 'Interf&ace'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object cbInitialization: TCheckBox
      Left = 10
      Top = 57
      Width = 125
      Height = 17
      Caption = '&Initialization'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
    object cbImplementation: TCheckBox
      Left = 10
      Top = 37
      Width = 125
      Height = 17
      Caption = 'Imp&lementation'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object cbFinalization: TCheckBox
      Left = 10
      Top = 77
      Width = 125
      Height = 17
      Caption = 'Finali&zation'
      Checked = True
      State = cbChecked
      TabOrder = 3
    end
  end
  object gbxTextTypes: TGroupBox
    Left = 8
    Top = 85
    Width = 139
    Height = 121
    Anchors = [akLeft, akTop, akBottom]
    Caption = 'Search (Delphi, C++)'
    TabOrder = 2
    object cbCode: TCheckBox
      Left = 10
      Top = 17
      Width = 123
      Height = 17
      Caption = 'Cod&e'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object cbStrings: TCheckBox
      Left = 10
      Top = 37
      Width = 123
      Height = 17
      Caption = '&Strings'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object cbComments: TCheckBox
      Left = 10
      Top = 57
      Width = 123
      Height = 17
      Caption = 'Comme&nts'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
    object cbForms: TCheckBox
      Left = 10
      Top = 77
      Width = 123
      Height = 17
      Caption = 'Associated for&ms'
      TabOrder = 3
    end
  end
end
