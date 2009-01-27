object fmProcedureListOptions: TfmProcedureListOptions
  Left = 410
  Top = 219
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Procedure List Configuration'
  ClientHeight = 196
  ClientWidth = 526
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnShow = FormShow
  DesignSize = (
    526
    196)
  PixelsPerInch = 96
  TextHeight = 14
  object btnOK: TButton
    Left = 350
    Top = 159
    Width = 75
    Height = 26
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 437
    Top = 159
    Width = 75
    Height = 26
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object gbCodeView: TGroupBox
    Left = 288
    Top = 9
    Width = 225
    Height = 140
    Anchors = [akTop, akRight]
    Caption = 'Code View'
    TabOrder = 1
    object lblDock: TLabel
      Left = 16
      Top = 50
      Width = 27
      Height = 14
      Caption = 'Dock'
    end
    object cbCVDock: TComboBox
      Left = 64
      Top = 46
      Width = 145
      Height = 22
      Style = csDropDownList
      ItemHeight = 14
      TabOrder = 1
      OnChange = cbCVDockChange
      Items.Strings = (
        'Top'
        'Left'
        'Right'
        'Bottom')
    end
    object pnlCVFont: TPanel
      Left = 16
      Top = 78
      Width = 105
      Height = 41
      Caption = 'AaBbYyZz'
      TabOrder = 2
    end
    object btnChangeCodeViewFont: TButton
      Left = 130
      Top = 86
      Width = 85
      Height = 25
      Caption = 'Change Font'
      TabOrder = 3
      OnClick = btnChangeCodeViewFontClick
    end
    object chkShowCodeView: TCheckBox
      Left = 16
      Top = 22
      Width = 113
      Height = 17
      Caption = 'Show code view'
      TabOrder = 0
    end
  end
  object gbDialog: TGroupBox
    Left = 12
    Top = 9
    Width = 265
    Height = 140
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Procedure List'
    TabOrder = 0
    object pnlDialogFont: TPanel
      Left = 16
      Top = 88
      Width = 105
      Height = 41
      Caption = 'AaBbYyZz'
      TabOrder = 3
    end
    object btnChgDialogFont: TButton
      Left = 134
      Top = 96
      Width = 85
      Height = 25
      Caption = 'Change Font'
      TabOrder = 4
      OnClick = btnChgDialogFontClick
    end
    object chkShowObjectName: TCheckBox
      Left = 16
      Top = 22
      Width = 246
      Height = 17
      Caption = 'Show object names'
      TabOrder = 0
    end
    object chkMatchAnywhere: TCheckBox
      Left = 16
      Top = 65
      Width = 246
      Height = 17
      Caption = 'Match anywhere in the name'
      TabOrder = 2
    end
    object chkMatchClass: TCheckBox
      Left = 16
      Top = 43
      Width = 246
      Height = 17
      Caption = 'Match in both class and method names'
      TabOrder = 1
    end
  end
end
