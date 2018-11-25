inherited fmeSortConfig: TfmeSortConfig
  ActiveControl = chkIgnoreFunction
  BorderStyle = bsDialog
  Caption = 'Sort Selected Lines Expert'
  ClientHeight = 233
  ClientWidth = 241
  PixelsPerInch = 96
  TextHeight = 13
  object grpSort: TGroupBox
    Left = 8
    Top = 0
    Width = 225
    Height = 145
    Caption = 'Sort ...'
    TabOrder = 0
    object btnAscending: TButton
      Left = 8
      Top = 16
      Width = 209
      Height = 25
      Caption = '&Ascending'
      Default = True
      ModalResult = 6
      TabOrder = 0
    end
    object btnDescending: TButton
      Left = 8
      Top = 48
      Width = 209
      Height = 25
      Caption = '&Descending'
      ModalResult = 7
      TabOrder = 1
    end
    object chkIgnoreFunction: TCheckBox
      Left = 8
      Top = 80
      Width = 209
      Height = 17
      Caption = '&Ignore procedure, function etc.'
      TabOrder = 2
    end
    object btnCustom: TButton
      Left = 8
      Top = 112
      Width = 177
      Height = 25
      Caption = '&Custom'
      ModalResult = 8
      TabOrder = 3
    end
    object btnConfigCustom: TButton
      Left = 192
      Top = 112
      Width = 25
      Height = 25
      Caption = '...'
      TabOrder = 4
      OnClick = btnConfigCustomClick
    end
  end
  object btnReverse: TButton
    Left = 16
    Top = 160
    Width = 209
    Height = 25
    Caption = '&Reverse Order'
    ModalResult = 4
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 16
    Top = 200
    Width = 209
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
