object fmCompsToCode: TfmCompsToCode
  Left = 298
  Top = 212
  Width = 694
  Height = 611
  Caption = 'Components to Code'
  Color = clBtnFace
  Constraints.MinHeight = 350
  Constraints.MinWidth = 450
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 14
  object pnlButtons: TPanel
    Left = 0
    Top = 535
    Width = 678
    Height = 38
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      678
      38)
    object pnlCenterButtons: TPanel
      Left = 162
      Top = 2
      Width = 353
      Height = 33
      Anchors = [akTop]
      BevelOuter = bvNone
      TabOrder = 0
      object btnHelp: TButton
        Left = 228
        Top = 5
        Width = 75
        Height = 25
        Caption = 'Help'
        TabOrder = 0
        OnClick = btnHelpClick
      end
      object btnCancel: TButton
        Left = 141
        Top = 5
        Width = 75
        Height = 25
        Cancel = True
        Caption = 'Cancel'
        ModalResult = 2
        TabOrder = 1
      end
      object btnOK: TButton
        Left = 55
        Top = 5
        Width = 75
        Height = 25
        Caption = 'OK'
        Default = True
        ModalResult = 1
        TabOrder = 2
      end
    end
  end
  object pnlMain: TPanel
    Left = 0
    Top = 0
    Width = 678
    Height = 535
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object pnlSettings: TPanel
      Left = 0
      Top = 0
      Width = 298
      Height = 535
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      object gbxGenerated: TGroupBox
        Left = 8
        Top = 95
        Width = 282
        Height = 84
        Caption = ' Generated Source '
        TabOrder = 1
        object chkPrepend: TCheckBox
          Left = 11
          Top = 18
          Width = 262
          Height = 17
          Caption = '&Prepend original component source'
          Checked = True
          State = cbChecked
          TabOrder = 0
          OnClick = SettingsChanged
        end
        object chkUseDelphiWith: TCheckBox
          Left = 11
          Top = 38
          Width = 262
          Height = 17
          Caption = 'Use Delphi &with statement'
          Checked = True
          State = cbChecked
          TabOrder = 1
          OnClick = SettingsChanged
        end
        object chkCreateFreeCode: TCheckBox
          Left = 11
          Top = 58
          Width = 262
          Height = 17
          Caption = 'Generate code to &Free components'
          TabOrder = 2
          OnClick = SettingsChanged
        end
      end
      object rgpBinProps: TRadioGroup
        Left = 8
        Top = 8
        Width = 282
        Height = 81
        Caption = ' Binary Properties '
        ItemIndex = 1
        Items.Strings = (
          '&Skip'
          'Generate c&ommented code'
          'Generate &uncommented code')
        TabOrder = 0
        OnClick = SettingsChanged
      end
      object rgpLanguage: TRadioGroup
        Left = 8
        Top = 184
        Width = 282
        Height = 54
        Caption = ' Language '
        Columns = 2
        ItemIndex = 0
        Items.Strings = (
          '&Delphi'
          '&C++')
        TabOrder = 2
        OnClick = SettingsChanged
      end
    end
    object pnlView: TPanel
      Left = 298
      Top = 0
      Width = 380
      Height = 535
      Align = alClient
      BevelOuter = bvNone
      Caption = 'This editor is created at runtime'
      FullRepaint = False
      TabOrder = 1
    end
  end
  object ParentPanel: TPanel
    Left = 325
    Top = 24
    Width = 276
    Height = 89
    Caption = 'Sample components - invisible at runtime'
    TabOrder = 1
    Visible = False
    object SpeedButton1: TSpeedButton
      Left = 183
      Top = 8
      Width = 23
      Height = 22
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00F66666666666
        6666666666666666666666666666666666666666666666666666666666666666
        6666666666666666666666666666666666666666666666666666666666666666
        6666666666666666666666666666666666666666666666666666666666666666
        6666666666666666666666666666666666666666666666666666}
    end
    object Edit1: TEdit
      Left = 56
      Top = 8
      Width = 121
      Height = 22
      TabOrder = 0
      Text = 'Edit1'
    end
  end
end
