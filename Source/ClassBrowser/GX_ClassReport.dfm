object fmClassReport: TfmClassReport
  Left = 416
  Top = 215
  BorderStyle = bsDialog
  Caption = 'Class Hierarchy Report'
  ClientHeight = 177
  ClientWidth = 298
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
  PixelsPerInch = 96
  TextHeight = 13
  object gbxPageSettings: TGroupBox
    Left = 9
    Top = 8
    Width = 281
    Height = 129
    Caption = 'Report Settings'
    TabOrder = 0
    object lblFont: TLabel
      Left = 63
      Top = 28
      Width = 21
      Height = 13
      Alignment = taRightJustify
      Caption = 'Font'
    end
    object lblFontSize: TLabel
      Left = 42
      Top = 52
      Width = 42
      Height = 13
      Alignment = taRightJustify
      Caption = 'Font size'
    end
    object lblBoxSize: TLabel
      Left = 45
      Top = 76
      Width = 39
      Height = 13
      Alignment = taRightJustify
      Caption = 'Box size'
    end
    object lblInCharacters: TLabel
      Left = 152
      Top = 76
      Width = 50
      Height = 13
      Caption = 'characters'
    end
    object lblBoxSpacing: TLabel
      Left = 26
      Top = 100
      Width = 58
      Height = 13
      Alignment = taRightJustify
      Caption = 'Box spacing'
    end
    object lblInPixels: TLabel
      Left = 152
      Top = 100
      Width = 26
      Height = 13
      Caption = 'pixels'
    end
    object cbxFont: TComboBox
      Left = 90
      Top = 24
      Width = 181
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
    end
    object spnFontSize: TEdit
      Left = 90
      Top = 48
      Width = 45
      Height = 21
      TabOrder = 1
      Text = '1'
    end
    object spnBoxSize: TEdit
      Left = 90
      Top = 72
      Width = 45
      Height = 21
      TabOrder = 3
      Text = '1'
    end
    object spnBoxSpacing: TEdit
      Left = 90
      Top = 96
      Width = 45
      Height = 21
      TabOrder = 5
      Text = '1'
    end
    object udBoxSize: TUpDown
      Left = 135
      Top = 72
      Width = 12
      Height = 21
      Associate = spnBoxSize
      Min = 1
      Position = 1
      TabOrder = 4
      Wrap = False
    end
    object udBoxSpacing: TUpDown
      Left = 135
      Top = 96
      Width = 12
      Height = 21
      Associate = spnBoxSpacing
      Min = 1
      Position = 1
      TabOrder = 6
      Wrap = False
    end
    object udFontSize: TUpDown
      Left = 135
      Top = 48
      Width = 12
      Height = 21
      Associate = spnFontSize
      Min = 1
      Position = 1
      TabOrder = 2
      Wrap = False
    end
  end
  object btnOK: TButton
    Left = 132
    Top = 144
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 214
    Top = 144
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
