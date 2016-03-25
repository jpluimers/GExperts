object fmToDoOptions: TfmToDoOptions
  Left = 347
  Top = 241
  ActiveControl = lstTokens
  BorderStyle = bsDialog
  Caption = 'To Do Options'
  ClientHeight = 329
  ClientWidth = 542
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    542
    329)
  PixelsPerInch = 96
  TextHeight = 14
  object gbxTokens: TGroupBox
    Left = 8
    Top = 6
    Width = 233
    Height = 315
    Anchors = [akLeft, akTop, akBottom]
    Caption = 'To Do Tokens'
    TabOrder = 0
    DesignSize = (
      233
      315)
    object lblPriority: TLabel
      Left = 131
      Top = 185
      Width = 37
      Height = 14
      Caption = '&Priority'
      FocusControl = cboPriority
    end
    object lblToken: TLabel
      Left = 131
      Top = 139
      Width = 35
      Height = 14
      Caption = 'To&ken'
      FocusControl = edToken
    end
    object lstTokens: TListBox
      Left = 8
      Top = 19
      Width = 118
      Height = 287
      Anchors = [akLeft, akTop, akBottom]
      ItemHeight = 14
      Sorted = True
      TabOrder = 0
      OnClick = lstTokensClick
    end
    object btnInsert: TButton
      Left = 140
      Top = 30
      Width = 77
      Height = 26
      Caption = '&Insert'
      TabOrder = 1
      OnClick = btnInsertClick
    end
    object btnApply: TButton
      Left = 140
      Top = 62
      Width = 77
      Height = 26
      Caption = '&Apply'
      TabOrder = 2
      OnClick = btnApplyClick
    end
    object btnRemove: TButton
      Left = 140
      Top = 94
      Width = 77
      Height = 26
      Caption = '&Remove'
      TabOrder = 3
      OnClick = btnRemoveClick
    end
    object edToken: TEdit
      Left = 131
      Top = 155
      Width = 96
      Height = 22
      TabOrder = 4
      OnChange = edTokenChange
    end
    object cboPriority: TComboBox
      Left = 131
      Top = 203
      Width = 96
      Height = 22
      Style = csDropDownList
      ItemHeight = 14
      TabOrder = 5
      OnChange = cboPriorityChange
    end
  end
  object btnOK: TButton
    Left = 308
    Top = 296
    Width = 80
    Height = 26
    Anchors = [akLeft, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 4
  end
  object btnCancel: TButton
    Left = 400
    Top = 296
    Width = 80
    Height = 26
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object gbxOptions: TGroupBox
    Left = 251
    Top = 6
    Width = 282
    Height = 87
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Options'
    TabOrder = 1
    DesignSize = (
      282
      87)
    object cbShowTokens: TCheckBox
      Left = 8
      Top = 20
      Width = 272
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Show tokens in description'
      TabOrder = 0
    end
    object cbAddMessage: TCheckBox
      Left = 8
      Top = 40
      Width = 272
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Add to do items to &message view'
      TabOrder = 1
    end
    object cbHideOnGoto: TCheckBox
      Left = 8
      Top = 60
      Width = 272
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Hide window after jumping to an item'
      TabOrder = 2
    end
  end
  object gbxSearchFiles: TGroupBox
    Left = 251
    Top = 99
    Width = 282
    Height = 150
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Search for To Do Tokens'
    TabOrder = 2
    DesignSize = (
      282
      150)
    object btnBrowse: TButton
      Left = 255
      Top = 100
      Width = 20
      Height = 20
      Hint = 'Select Directory'
      Anchors = [akTop, akRight]
      Caption = '...'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
      OnClick = btnBrowseClick
    end
    object chkInclude: TCheckBox
      Left = 18
      Top = 125
      Width = 255
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Include su&bdirectories'
      TabOrder = 6
    end
    object cboDirectories: TComboBox
      Left = 12
      Top = 100
      Width = 242
      Height = 22
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 14
      TabOrder = 4
    end
    object radScanProj: TRadioButton
      Left = 8
      Top = 39
      Width = 271
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Scan pro&ject files'
      TabOrder = 1
      OnClick = radScanDirClick
    end
    object radScanOpen: TRadioButton
      Left = 8
      Top = 59
      Width = 271
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Scan &open project files'
      TabOrder = 2
      TabStop = True
      OnClick = radScanDirClick
    end
    object radScanDir: TRadioButton
      Left = 8
      Top = 79
      Width = 271
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Scan &directories'
      TabOrder = 3
      TabStop = True
      OnClick = radScanDirClick
    end
    object radScanProjGroup: TRadioButton
      Left = 8
      Top = 20
      Width = 271
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Scan project &group'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = radScanDirClick
    end
  end
  object btnFont: TButton
    Left = 256
    Top = 256
    Width = 137
    Height = 33
    Caption = 'Font'
    TabOrder = 3
    OnClick = btnFontClick
  end
  object TheFontDialog: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Options = [fdForceFontExist]
    Left = 168
    Top = 256
  end
end
