object fmReplaceComp: TfmReplaceComp
  Left = 291
  Top = 311
  BorderStyle = bsDialog
  Caption = 'Replace Components'
  ClientHeight = 226
  ClientWidth = 429
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
    429
    226)
  PixelsPerInch = 96
  TextHeight = 13
  object lblSeach: TLabel
    Left = 81
    Top = 16
    Width = 20
    Height = 13
    Alignment = taRightJustify
    Caption = '&Find'
    FocusControl = cbSearch
  end
  object lblReplace: TLabel
    Left = 41
    Top = 40
    Width = 62
    Height = 13
    Alignment = taRightJustify
    Caption = '&Replace with'
    FocusControl = cbReplace
  end
  object btnOK: TButton
    Left = 94
    Top = 190
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    Enabled = False
    TabOrder = 4
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 177
    Top = 190
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object btnHelp: TButton
    Left = 342
    Top = 190
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Help'
    TabOrder = 7
    OnClick = btnHelpClick
  end
  object cbSearch: TComboBox
    Left = 111
    Top = 12
    Width = 258
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    DropDownCount = 18
    ItemHeight = 13
    Sorted = True
    TabOrder = 0
    OnChange = cbSearchChange
    OnKeyDown = cbSearchKeyDown
  end
  object cbReplace: TComboBox
    Left = 111
    Top = 36
    Width = 258
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    DropDownCount = 18
    ItemHeight = 13
    Sorted = True
    TabOrder = 1
    OnChange = cbSearchChange
    OnKeyDown = cbSearchKeyDown
  end
  object gbxScope: TGroupBox
    Left = 176
    Top = 70
    Width = 241
    Height = 108
    Anchors = [akTop]
    Caption = 'Scope'
    TabOrder = 3
    DesignSize = (
      241
      108)
    object rbAllOnCurrentForm: TRadioButton
      Left = 8
      Top = 51
      Width = 227
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'All components on the &current form'
      TabOrder = 1
    end
    object rbAllInProject: TRadioButton
      Left = 8
      Top = 82
      Width = 227
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'All components on all forms in the &project'
      TabOrder = 2
    end
    object rbSelectedOnCurrentForm: TRadioButton
      Left = 8
      Top = 20
      Width = 227
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = '&Selected components on the current form'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
  end
  object gbxOptions: TGroupBox
    Left = 12
    Top = 70
    Width = 153
    Height = 108
    Caption = '&Options'
    TabOrder = 2
    object chkIgnoreErrors: TCheckBox
      Left = 11
      Top = 17
      Width = 138
      Height = 17
      Caption = 'Ignore errors'
      TabOrder = 0
    end
    object chkLogChanges: TCheckBox
      Left = 11
      Top = 34
      Width = 138
      Height = 17
      Caption = 'Log changes'
      TabOrder = 1
    end
    object chkLogValues: TCheckBox
      Left = 27
      Top = 51
      Width = 122
      Height = 17
      Caption = 'Log all values'
      TabOrder = 2
    end
    object chkOverwriteLog: TCheckBox
      Left = 27
      Top = 68
      Width = 122
      Height = 17
      Caption = 'Overwrite old log'
      TabOrder = 3
    end
    object chkShowLogWin: TCheckBox
      Left = 27
      Top = 85
      Width = 122
      Height = 17
      Caption = 'Show log window'
      TabOrder = 4
    end
  end
  object btnSettings: TButton
    Left = 260
    Top = 190
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'S&ettings...'
    TabOrder = 6
    OnClick = btnSettingsClick
  end
end
