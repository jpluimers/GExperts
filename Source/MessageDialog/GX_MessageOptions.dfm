object fmMessageOptions: TfmMessageOptions
  Left = 274
  Top = 207
  BorderStyle = bsDialog
  Caption = 'Message Dialog Options'
  ClientHeight = 176
  ClientWidth = 371
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  DesignSize = (
    371
    176)
  PixelsPerInch = 96
  TextHeight = 14
  object gbxOptions: TGroupBox
    Left = 8
    Top = 8
    Width = 355
    Height = 131
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Message Dialog Options'
    TabOrder = 0
    DesignSize = (
      355
      131)
    object lblConcat: TLabel
      Left = 8
      Top = 26
      Width = 282
      Height = 14
      Caption = 'Concatenation string for line breaks in &Pascal source'
      FocusControl = edtMsgString
    end
    object lblCppConcat: TLabel
      Left = 8
      Top = 74
      Width = 273
      Height = 14
      Caption = 'Concatenation string for line breaks in &C++ source'
      FocusControl = edtCppMsgString
    end
    object edtMsgString: TEdit
      Left = 8
      Top = 48
      Width = 339
      Height = 22
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
    object edtCppMsgString: TEdit
      Left = 8
      Top = 92
      Width = 339
      Height = 22
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
    end
  end
  object btnOK: TButton
    Left = 204
    Top = 146
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 288
    Top = 146
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
