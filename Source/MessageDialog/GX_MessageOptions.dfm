object fmMessageOptions: TfmMessageOptions
  Left = 274
  Top = 207
  BorderStyle = bsDialog
  Caption = 'Message Dialog Options'
  ClientHeight = 289
  ClientWidth = 369
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
  object btnOK: TButton
    Left = 200
    Top = 256
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object btnCancel: TButton
    Left = 280
    Top = 256
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object gbxCrLfString: TGroupBox
    Left = 8
    Top = 8
    Width = 353
    Height = 121
    Caption = 'Concatenation string for line breaks'
    TabOrder = 0
    DesignSize = (
      353
      121)
    object lblCrLfPascal: TLabel
      Left = 8
      Top = 24
      Width = 72
      Height = 14
      Caption = '&Pascal source'
      FocusControl = edtCrLfPascal
    end
    object lblCrLfCPP: TLabel
      Left = 8
      Top = 72
      Width = 63
      Height = 14
      Caption = '&C++ source'
      FocusControl = edtCrLfCPP
    end
    object edtCrLfPascal: TEdit
      Left = 8
      Top = 40
      Width = 337
      Height = 22
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
    object edtCrLfCPP: TEdit
      Left = 8
      Top = 88
      Width = 339
      Height = 22
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
    end
  end
  object grpGnuGettext: TGroupBox
    Left = 8
    Top = 144
    Width = 353
    Height = 105
    Caption = '&GNU Gettext support'
    TabOrder = 1
    object chkGetTextIndividual: TCheckBox
      Left = 8
      Top = 80
      Width = 297
      Height = 17
      Caption = 'For individual lines'
      TabOrder = 2
    end
    object rbGgtFuncUnderscore: TRadioButton
      Left = 8
      Top = 24
      Width = 337
      Height = 17
      Caption = 'Surround text with _(...)'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object rbGgtFuncGetText: TRadioButton
      Left = 8
      Top = 48
      Width = 337
      Height = 17
      Caption = 'Surround text with GetText(...)'
      TabOrder = 1
    end
  end
end
