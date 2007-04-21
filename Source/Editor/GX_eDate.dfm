object fmDateFormat: TfmDateFormat
  Left = 229
  Top = 131
  BorderStyle = bsDialog
  Caption = 'Date/Time Format'
  ClientHeight = 70
  ClientWidth = 337
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object lblFormat: TLabel
    Left = 33
    Top = 12
    Width = 79
    Height = 13
    Alignment = taRightJustify
    Caption = 'Date/time &format'
    FocusControl = cbFormat
  end
  object cbFormat: TComboBox
    Left = 120
    Top = 8
    Width = 209
    Height = 21
    ItemHeight = 13
    TabOrder = 0
    Text = 'cbFormat'
    Items.Strings = (
      'dddd, mmmm d, yyyy'
      'dddd, mmmm d'
      'mmmm d'
      'mmmm d, yyyy'
      'dd/mm/yy'
      'mm/dd/yy'
      'dd-mm-yy'
      'mm-dd-yy'
      'dddd, mmmm d, yyyy  h:mm am/pm'
      'dddd, mmmm d h:mm am/pm'
      'mmmm d h:mm am/pm'
      'mmmm d, yyyy h:mm am/pm'
      'dd/mm/yy h:mm am/pm'
      'mm/dd/yy h:mm am/pm'
      'dd-mm-yy h:mm am/pm'
      'mm-dd-yy h:mm am/pm'
      'dddd, mmmm d, yyyy h:mm'
      'dddd, mmmm d h:mm'
      'mmmm d h:mm'
      'mmmm d, yyyy h:mm'
      'dd/mm/yy h:mm'
      'mm/dd/yy h:mm'
      'dd-mm-yy h:mm'
      'mm-dd-yy h:mm')
  end
  object btnOK: TButton
    Left = 168
    Top = 40
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 252
    Top = 40
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
