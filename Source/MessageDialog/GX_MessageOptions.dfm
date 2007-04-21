object fmMessageOptions: TfmMessageOptions
  Left = 274
  Top = 207
  BorderStyle = bsDialog
  Caption = 'Message Dialog Options'
  ClientHeight = 176
  ClientWidth = 329
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
  object gbxOptions: TGroupBox
    Left = 8
    Top = 8
    Width = 313
    Height = 131
    Caption = 'Message Dialog Options'
    TabOrder = 0
    object lblConcat: TLabel
      Left = 8
      Top = 26
      Width = 247
      Height = 13
      Caption = 'Concatenation string for line breaks in &Pascal source'
      FocusControl = edtMsgString
    end
    object lblCppConcat: TLabel
      Left = 8
      Top = 74
      Width = 234
      Height = 13
      Caption = 'Concatenation string for line breaks in &C++ source'
      FocusControl = edtCppMsgString
    end
    object edtMsgString: TEdit
      Left = 8
      Top = 48
      Width = 297
      Height = 21
      TabOrder = 0
    end
    object edtCppMsgString: TEdit
      Left = 8
      Top = 92
      Width = 297
      Height = 21
      TabOrder = 1
    end
  end
  object btnOK: TButton
    Left = 162
    Top = 146
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 246
    Top = 146
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
