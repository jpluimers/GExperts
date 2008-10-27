object fmSourceExportOptions: TfmSourceExportOptions
  Left = 241
  Top = 144
  BorderStyle = bsDialog
  Caption = 'Source Export Configuration'
  ClientHeight = 442
  ClientWidth = 451
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
  object pnlSettings: TPanel
    Left = 0
    Top = 0
    Width = 451
    Height = 168
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    Visible = False
    object lblElement: TLabel
      Left = 202
      Top = 11
      Width = 38
      Height = 13
      Alignment = taRightJustify
      Caption = '&Element'
    end
    object gbxAttributes: TGroupBox
      Left = 196
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
    object cbxAttributes: TComboBox
      Left = 245
      Top = 8
      Width = 158
      Height = 21
      Style = csDropDownList
      DropDownCount = 14
      ItemHeight = 13
      TabOrder = 3
      OnChange = cbxAttributesChange
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
    object btnBackgroundColor: TButton
      Left = 9
      Top = 97
      Width = 160
      Height = 25
      Caption = 'HTML Bac&kground'
      TabOrder = 1
      OnClick = btnBackgroundColorClick
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
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 403
    Width = 451
    Height = 39
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object pnlButtonsRight: TPanel
      Left = 266
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
    Width = 451
    Height = 235
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 6
    TabOrder = 1
  end
  object dlgBackground: TColorDialog
    Ctl3D = True
    Left = 8
    Top = 336
  end
end
