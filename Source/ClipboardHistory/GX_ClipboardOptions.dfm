object fmClipboardOptions: TfmClipboardOptions
  Left = 294
  Top = 198
  BorderStyle = bsDialog
  Caption = 'Clipboard History Options'
  ClientHeight = 161
  ClientWidth = 305
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 14
  object gbxClipboardOptions: TGroupBox
    Left = 8
    Top = 8
    Width = 289
    Height = 114
    Caption = 'Clipboard History Options'
    TabOrder = 0
    object lblMaxEntries: TLabel
      Left = 15
      Top = 30
      Width = 91
      Height = 14
      Alignment = taRightJustify
      Caption = '&Maximum entries'
      FocusControl = edtMaxClip
    end
    object edtMaxClip: TEdit
      Left = 117
      Top = 26
      Width = 59
      Height = 22
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
      Top = 82
      Width = 271
      Height = 25
      Caption = 'Auto-cl&ose window after copy'
      TabOrder = 2
    end
  end
  object btnOK: TButton
    Left = 142
    Top = 129
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 222
    Top = 129
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
