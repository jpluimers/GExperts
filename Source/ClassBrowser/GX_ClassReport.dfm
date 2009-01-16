object fmClassReport: TfmClassReport
  Left = 416
  Top = 215
  BorderStyle = bsDialog
  Caption = 'Class Hierarchy Report'
  ClientHeight = 184
  ClientWidth = 307
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
  DesignSize = (
    307
    184)
  PixelsPerInch = 96
  TextHeight = 14
  object gbxPageSettings: TGroupBox
    Left = 9
    Top = 8
    Width = 290
    Height = 139
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Report Settings'
    TabOrder = 0
    DesignSize = (
      290
      139)
    object lblFont: TLabel
      Left = 65
      Top = 26
      Width = 25
      Height = 14
      Alignment = taRightJustify
      Caption = 'Font'
    end
    object lblFontSize: TLabel
      Left = 42
      Top = 53
      Width = 48
      Height = 14
      Alignment = taRightJustify
      Caption = 'Font size'
    end
    object lblBoxSize: TLabel
      Left = 47
      Top = 80
      Width = 43
      Height = 14
      Alignment = taRightJustify
      Caption = 'Box size'
    end
    object lblInCharacters: TLabel
      Left = 158
      Top = 80
      Width = 56
      Height = 14
      Caption = 'characters'
    end
    object lblBoxSpacing: TLabel
      Left = 26
      Top = 108
      Width = 64
      Height = 14
      Alignment = taRightJustify
      Caption = 'Box spacing'
    end
    object lblInPixels: TLabel
      Left = 158
      Top = 108
      Width = 29
      Height = 14
      Caption = 'pixels'
    end
    object cbxFont: TComboBox
      Left = 96
      Top = 22
      Width = 183
      Height = 22
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 14
      TabOrder = 0
    end
    object spnFontSize: TEdit
      Left = 96
      Top = 49
      Width = 45
      Height = 22
      TabOrder = 1
      Text = '1'
    end
    object spnBoxSize: TEdit
      Left = 96
      Top = 76
      Width = 45
      Height = 22
      TabOrder = 3
      Text = '1'
    end
    object spnBoxSpacing: TEdit
      Left = 96
      Top = 104
      Width = 45
      Height = 22
      TabOrder = 5
      Text = '1'
    end
    object udBoxSize: TUpDown
      Left = 141
      Top = 76
      Width = 12
      Height = 22
      Associate = spnBoxSize
      Min = 1
      Position = 1
      TabOrder = 4
      Wrap = False
    end
    object udBoxSpacing: TUpDown
      Left = 141
      Top = 104
      Width = 12
      Height = 22
      Associate = spnBoxSpacing
      Min = 1
      Position = 1
      TabOrder = 6
      Wrap = False
    end
    object udFontSize: TUpDown
      Left = 141
      Top = 49
      Width = 12
      Height = 22
      Associate = spnFontSize
      Min = 1
      Position = 1
      TabOrder = 2
      Wrap = False
    end
  end
  object btnOK: TButton
    Left = 141
    Top = 153
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 223
    Top = 153
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
