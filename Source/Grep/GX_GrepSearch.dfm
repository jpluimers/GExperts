object fmGrepSearch: TfmGrepSearch
  Left = 290
  Top = 209
  BorderStyle = bsDialog
  Caption = 'Grep Search'
  ClientHeight = 303
  ClientWidth = 485
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
    485
    303)
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
    Width = 399
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    DropDownCount = 15
    ItemHeight = 13
    TabOrder = 0
  end
  object gbxOptions: TGroupBox
    Left = 8
    Top = 40
    Width = 227
    Height = 121
    Caption = 'Options'
    TabOrder = 1
    object cbCaseSensitive: TCheckBox
      Left = 10
      Top = 16
      Width = 165
      Height = 17
      Caption = '&Case sensitive'
      TabOrder = 0
    end
    object cbNoComments: TCheckBox
      Left = 10
      Top = 98
      Width = 165
      Height = 17
      Caption = '&Ignore comments'
      TabOrder = 4
      Visible = False
    end
    object cbForms: TCheckBox
      Left = 10
      Top = 57
      Width = 165
      Height = 17
      Caption = 'Search for&m files'
      TabOrder = 2
    end
    object cbWholeWord: TCheckBox
      Left = 10
      Top = 36
      Width = 165
      Height = 17
      Caption = '&Whole word'
      TabOrder = 1
    end
    object cbRegEx: TCheckBox
      Left = 10
      Top = 77
      Width = 165
      Height = 17
      Caption = 'Regular e&xpression'
      TabOrder = 3
    end
  end
  object gbxWhere: TGroupBox
    Left = 252
    Top = 40
    Width = 227
    Height = 121
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Where'
    TabOrder = 2
    object rbAllProjFiles: TRadioButton
      Left = 10
      Top = 57
      Width = 177
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
      Width = 177
      Height = 17
      Caption = '&Open project files'
      TabOrder = 3
      OnClick = rbDirectoriesClick
    end
    object rbDirectories: TRadioButton
      Left = 10
      Top = 98
      Width = 177
      Height = 17
      Caption = 'Search in &directories'
      TabOrder = 4
      OnClick = rbDirectoriesClick
    end
    object rbCurrentOnly: TRadioButton
      Left = 10
      Top = 16
      Width = 177
      Height = 17
      Caption = 'Current &file only'
      TabOrder = 0
      OnClick = rbDirectoriesClick
    end
    object rbAllProjGroupFiles: TRadioButton
      Left = 10
      Top = 36
      Width = 177
      Height = 17
      Caption = 'All files in project &group'
      TabOrder = 1
      OnClick = rbDirectoriesClick
    end
  end
  object gbxDirectories: TGroupBox
    Left = 8
    Top = 168
    Width = 471
    Height = 97
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Directory Search'
    TabOrder = 3
    DesignSize = (
      471
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
      Width = 366
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      DropDownCount = 15
      ItemHeight = 13
      TabOrder = 2
    end
    object cbInclude: TCheckBox
      Left = 70
      Top = 72
      Width = 390
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Include su&bdirectories'
      TabOrder = 3
    end
    object cbDirectory: TComboBox
      Left = 70
      Top = 22
      Width = 366
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      DropDownCount = 15
      ItemHeight = 13
      TabOrder = 0
      OnDropDown = cbDirectoryDropDown
    end
    object btnBrowse: TButton
      Left = 437
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
    Left = 244
    Top = 271
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 4
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 324
    Top = 271
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object btnHelp: TButton
    Left = 404
    Top = 271
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Help'
    TabOrder = 6
    OnClick = btnHelpClick
  end
end
