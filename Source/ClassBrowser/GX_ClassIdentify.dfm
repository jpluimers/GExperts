object fmClassIdentify: TfmClassIdentify
  Left = 255
  Top = 176
  BorderStyle = bsDialog
  Caption = 'Enter Class Identifier'
  ClientHeight = 183
  ClientWidth = 302
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
  object gbxIdentifier: TGroupBox
    Left = 8
    Top = 8
    Width = 289
    Height = 137
    Caption = 'Class Set Identifier'
    TabOrder = 0
    object lblNotes: TLabel
      Left = 8
      Top = 24
      Width = 273
      Height = 73
      AutoSize = False
      Caption = 
        'Each set of classes requires that a unique identifier be entered' +
        '.  For example, VCL classes might use the identifier "VCL".  Onc' +
        'e you have entered an identifier you will be prompted to select ' +
        'the directory where the classes can be read.'
      WordWrap = True
    end
    object lblIdentifier: TLabel
      Left = 16
      Top = 112
      Width = 40
      Height = 13
      Alignment = taRightJustify
      Caption = '&Identifier'
      FocusControl = edtID
    end
    object edtID: TEdit
      Left = 64
      Top = 108
      Width = 217
      Height = 21
      TabOrder = 0
    end
  end
  object btnOK: TButton
    Left = 140
    Top = 152
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 222
    Top = 152
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
