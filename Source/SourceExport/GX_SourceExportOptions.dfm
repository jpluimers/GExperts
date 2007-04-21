object fmSourceExportOptions: TfmSourceExportOptions
  Left = 241
  Top = 144
  BorderStyle = bsDialog
  Caption = 'Source Export Configuration'
  ClientHeight = 372
  ClientWidth = 420
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
  DesignSize = (
    420
    372)
  PixelsPerInch = 96
  TextHeight = 13
  object lblElement: TLabel
    Left = 210
    Top = 12
    Width = 38
    Height = 13
    Alignment = taRightJustify
    Caption = '&Element'
    FocusControl = cbxAttributes
  end
  object btnCancel: TButton
    Left = 335
    Top = 338
    Width = 75
    Height = 26
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 7
  end
  object btnOK: TButton
    Left = 253
    Top = 338
    Width = 75
    Height = 26
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 6
  end
  object rbxCopySettings: TRadioGroup
    Left = 8
    Top = 8
    Width = 181
    Height = 81
    Caption = ' Default Clipboard Format '
    Items.Strings = (
      '&Formatted text/RTF/HTML'
      '&HTML fragment text'
      '&RTF fragment text')
    TabOrder = 0
  end
  object cbxAttributes: TComboBox
    Left = 255
    Top = 8
    Width = 158
    Height = 21
    Style = csDropDownList
    DropDownCount = 14
    ItemHeight = 13
    TabOrder = 3
    OnChange = cbxAttributesChange
  end
  object gbxAttributes: TGroupBox
    Left = 197
    Top = 32
    Width = 215
    Height = 123
    Caption = ' Element Attributes '
    TabOrder = 4
    object chkBold: TCheckBox
      Left = 123
      Top = 17
      Width = 89
      Height = 17
      Caption = '&Bold'
      TabOrder = 0
      OnClick = AttributeChange
    end
    object chkItalic: TCheckBox
      Left = 123
      Top = 35
      Width = 89
      Height = 17
      Caption = '&Italic'
      TabOrder = 1
      OnClick = AttributeChange
    end
    object chkUnderline: TCheckBox
      Left = 123
      Top = 54
      Width = 89
      Height = 17
      Caption = '&Underline'
      TabOrder = 2
      OnClick = AttributeChange
    end
    object chkStrikeOut: TCheckBox
      Left = 123
      Top = 73
      Width = 89
      Height = 17
      Caption = '&Strikeout'
      TabOrder = 3
      OnClick = AttributeChange
    end
  end
  object btnLoadIde: TButton
    Left = 9
    Top = 129
    Width = 160
    Height = 25
    Caption = '&Load IDE Settings'
    TabOrder = 2
    OnClick = btnLoadIdeClick
  end
  object pnlCodeSize: TPanel
    Left = 8
    Top = 162
    Width = 403
    Height = 168
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 5
    Visible = False
  end
  object btnBackgroundColor: TButton
    Left = 9
    Top = 97
    Width = 160
    Height = 25
    Caption = 'HTML Bac&kground'
    TabOrder = 1
    OnClick = btnBackgroundColorClick
  end
  object dlgBackground: TColorDialog
    Left = 8
    Top = 336
  end
end
