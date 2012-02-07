object fmSourceExportOptions: TfmSourceExportOptions
  Left = 299
  Top = 189
  BorderStyle = bsDialog
  Caption = 'Source Export Configuration'
  ClientHeight = 456
  ClientWidth = 491
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
  PixelsPerInch = 96
  TextHeight = 14
  object pnlSettings: TPanel
    Left = 0
    Top = 0
    Width = 491
    Height = 168
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object lblElement: TLabel
      Left = 218
      Top = 11
      Width = 45
      Height = 14
      Alignment = taRightJustify
      Caption = '&Element'
      FocusControl = cbxAttributes
    end
    object gbxAttributes: TGroupBox
      Left = 224
      Top = 32
      Width = 221
      Height = 123
      Caption = ' Element Attributes '
      TabOrder = 4
      object chkBold: TCheckBox
        Left = 123
        Top = 17
        Width = 92
        Height = 17
        Caption = '&Bold'
        TabOrder = 0
        OnClick = AttributeChange
      end
      object chkItalic: TCheckBox
        Left = 123
        Top = 36
        Width = 92
        Height = 17
        Caption = '&Italic'
        TabOrder = 1
        OnClick = AttributeChange
      end
      object chkUnderline: TCheckBox
        Left = 123
        Top = 56
        Width = 92
        Height = 17
        Caption = '&Underline'
        TabOrder = 2
        OnClick = AttributeChange
      end
      object chkStrikeOut: TCheckBox
        Left = 123
        Top = 76
        Width = 92
        Height = 17
        Caption = '&Strikeout'
        TabOrder = 3
        OnClick = AttributeChange
      end
    end
    object cbxAttributes: TComboBox
      Left = 282
      Top = 8
      Width = 164
      Height = 22
      Style = csDropDownList
      DropDownCount = 14
      ItemHeight = 14
      TabOrder = 3
      OnChange = cbxAttributesChange
    end
    object rbxCopySettings: TRadioGroup
      Left = 8
      Top = 8
      Width = 197
      Height = 81
      Caption = ' Default Clipboard Format '
      Items.Strings = (
        '&Formatted text/RTF/HTML'
        '&HTML fragment text'
        '&RTF fragment text')
      TabOrder = 0
    end
    object btnBackgroundColor: TButton
      Left = 23
      Top = 97
      Width = 160
      Height = 25
      Caption = 'HTML Bac&kground'
      TabOrder = 1
      OnClick = btnBackgroundColorClick
    end
    object btnLoadIde: TButton
      Left = 23
      Top = 129
      Width = 160
      Height = 25
      Caption = '&Load IDE Settings'
      TabOrder = 2
      OnClick = btnLoadIdeClick
    end
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 417
    Width = 491
    Height = 39
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object pnlButtonsRight: TPanel
      Left = 306
      Top = 0
      Width = 185
      Height = 39
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object btnOK: TButton
        Left = 13
        Top = 5
        Width = 75
        Height = 26
        Caption = 'OK'
        Default = True
        ModalResult = 1
        TabOrder = 0
      end
      object btnCancel: TButton
        Left = 101
        Top = 5
        Width = 75
        Height = 26
        Cancel = True
        Caption = 'Cancel'
        ModalResult = 2
        TabOrder = 1
      end
    end
  end
  object pnlCode: TPanel
    Left = 0
    Top = 168
    Width = 491
    Height = 249
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 6
    TabOrder = 1
  end
  object dlgBackground: TColorDialog
    Ctl3D = True
    Left = 232
    Top = 56
  end
end
