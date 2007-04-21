object fmDebugOptions: TfmDebugOptions
  Left = 357
  Top = 224
  BorderStyle = bsDialog
  Caption = 'Debug Window Options'
  ClientHeight = 136
  ClientWidth = 320
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object gbxView: TGroupBox
    Left = 8
    Top = 8
    Width = 225
    Height = 65
    Caption = 'View Options'
    TabOrder = 0
    object chkShowOnStartup: TCheckBox
      Left = 8
      Top = 21
      Width = 177
      Height = 17
      Caption = 'Show on startup'
      TabOrder = 0
    end
    object chkShowOnMessage: TCheckBox
      Left = 8
      Top = 40
      Width = 209
      Height = 17
      Caption = 'Show on message received'
      TabOrder = 1
    end
  end
  object gbxMessages: TGroupBox
    Left = 8
    Top = 80
    Width = 225
    Height = 49
    Caption = 'Message Options'
    TabOrder = 1
    object chkNewAtBottom: TCheckBox
      Left = 8
      Top = 23
      Width = 209
      Height = 17
      Caption = 'New messages added to the bottom'
      TabOrder = 0
    end
  end
  object btnOK: TButton
    Left = 240
    Top = 12
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 240
    Top = 44
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
end
