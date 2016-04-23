inherited fmeSortConfig: TfmeSortConfig
  BorderStyle = bsDialog
  Caption = 'Sort Selected Lines Expert'
  ClientHeight = 168
  ClientWidth = 225
  PixelsPerInch = 96
  TextHeight = 13
  object btnAscending: TButton
    Left = 8
    Top = 8
    Width = 209
    Height = 25
    Caption = 'Sort &Ascending'
    Default = True
    ModalResult = 6
    TabOrder = 0
  end
  object btnDescending: TButton
    Left = 8
    Top = 40
    Width = 209
    Height = 25
    Caption = 'Sort &Descending'
    ModalResult = 7
    TabOrder = 1
  end
  object btnReverse: TButton
    Left = 8
    Top = 72
    Width = 209
    Height = 25
    Caption = '&Reverse Order'
    ModalResult = 4
    TabOrder = 2
  end
  object btnCancel: TButton
    Left = 8
    Top = 104
    Width = 209
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object chkIgnoreFunction: TCheckBox
    Left = 8
    Top = 144
    Width = 209
    Height = 17
    Caption = '&Ignore procedure / function'
    TabOrder = 4
  end
end
