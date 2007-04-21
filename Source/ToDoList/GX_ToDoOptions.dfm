object fmToDoOptions: TfmToDoOptions
  Left = 347
  Top = 241
  ActiveControl = lstTokens
  BorderStyle = bsDialog
  Caption = 'To Do Options'
  ClientHeight = 270
  ClientWidth = 512
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object gbxTokens: TGroupBox
    Left = 8
    Top = 6
    Width = 233
    Height = 256
    Caption = 'To Do Tokens'
    TabOrder = 0
    object lblPriority: TLabel
      Left = 131
      Top = 185
      Width = 31
      Height = 13
      Caption = '&Priority'
      FocusControl = cboPriority
    end
    object lblToken: TLabel
      Left = 131
      Top = 139
      Width = 31
      Height = 13
      Caption = 'To&ken'
      FocusControl = edToken
    end
    object lstTokens: TListBox
      Left = 8
      Top = 19
      Width = 118
      Height = 228
      ItemHeight = 13
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
      Height = 21
      TabOrder = 4
      OnChange = edTokenChange
    end
    object cboPriority: TComboBox
      Left = 131
      Top = 203
      Width = 96
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 5
      OnChange = cboPriorityChange
      Items.Strings = (
        'High'
        'Normal'
        'Low')
    end
  end
  object btnOK: TButton
    Left = 296
    Top = 237
    Width = 80
    Height = 26
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object btnCancel: TButton
    Left = 388
    Top = 237
    Width = 80
    Height = 26
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object gbxOptions: TGroupBox
    Left = 251
    Top = 6
    Width = 253
    Height = 81
    Caption = 'Options'
    TabOrder = 1
    object cbShowTokens: TCheckBox
      Left = 8
      Top = 19
      Width = 243
      Height = 17
      Caption = '&Show tokens in description'
      TabOrder = 0
    end
    object cbAddMessage: TCheckBox
      Left = 8
      Top = 37
      Width = 243
      Height = 17
      Caption = 'Add to do items to &message view'
      TabOrder = 1
    end
    object cbHideOnGoto: TCheckBox
      Left = 8
      Top = 55
      Width = 243
      Height = 17
      Caption = '&Hide window after jumping to an item'
      TabOrder = 2
    end
  end
  object gbxSearchFiles: TGroupBox
    Left = 251
    Top = 93
    Width = 253
    Height = 137
    Caption = 'Search for To Do Tokens'
    TabOrder = 2
    object btnBrowse: TButton
      Left = 225
      Top = 88
      Width = 20
      Height = 20
      Hint = 'Select Directory'
      Caption = '...'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
      OnClick = btnBrowseClick
    end
    object chkInclude: TCheckBox
      Left = 14
      Top = 113
      Width = 235
      Height = 17
      Caption = 'Include su&bdirectories'
      TabOrder = 6
    end
    object cboDirectories: TComboBox
      Left = 8
      Top = 88
      Width = 216
      Height = 21
      ItemHeight = 13
      TabOrder = 4
    end
    object radScanProj: TRadioButton
      Left = 8
      Top = 34
      Width = 242
      Height = 17
      Caption = 'Scan pro&ject files'
      TabOrder = 1
      OnClick = radScanDirClick
    end
    object radScanOpen: TRadioButton
      Left = 8
      Top = 52
      Width = 242
      Height = 17
      Caption = 'Scan &open project files'
      TabOrder = 2
      TabStop = True
      OnClick = radScanDirClick
    end
    object radScanDir: TRadioButton
      Left = 8
      Top = 70
      Width = 242
      Height = 17
      Caption = 'Scan &directories'
      TabOrder = 3
      TabStop = True
      OnClick = radScanDirClick
    end
    object radScanProjGroup: TRadioButton
      Left = 8
      Top = 17
      Width = 242
      Height = 17
      Caption = 'Scan project &group'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = radScanDirClick
    end
  end
end
