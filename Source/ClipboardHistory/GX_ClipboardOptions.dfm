object fmClipboardOptions: TfmClipboardOptions
  Left = 221
  Top = 178
  BorderStyle = bsDialog
  Caption = 'Clipboard History Options'
  ClientHeight = 157
  ClientWidth = 305
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
  object gbxClipboardOptions: TGroupBox
    Left = 8
    Top = 8
    Width = 289
    Height = 109
    Caption = 'Clipboard History Options'
    TabOrder = 0
    object lblMaxEntries: TLabel
      Left = 15
      Top = 30
      Width = 78
      Height = 13
      Caption = '&Maximum entries'
      FocusControl = edtMaxClip
    end
    object edtMaxClip: TEdit
      Left = 112
      Top = 26
      Width = 79
      Height = 21
      TabOrder = 0
    end
    object chkAutoStart: TCheckBox
      Left = 14
      Top = 56
      Width = 271
      Height = 25
      Caption = 'St&art clipboard capture on startup'
      TabOrder = 1
    end
    object chkAutoClose: TCheckBox
      Left = 14
      Top = 80
      Width = 271
      Height = 25
      Caption = 'Auto-cl&ose window after copy'
      TabOrder = 2
    end
  end
  object btnOK: TButton
    Left = 142
    Top = 125
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 222
    Top = 125
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
