object fmMacroLibraryNamePrompt: TfmMacroLibraryNamePrompt
  Left = 320
  Top = 249
  Caption = 'Macro Library'
  ClientHeight = 417
  ClientWidth = 425
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object lblMacroName: TLabel
    Left = 8
    Top = 8
    Width = 59
    Height = 13
    Caption = 'Macro Name'
    FocusControl = edtMacroName
  end
  object lblMacroDesc: TLabel
    Left = 8
    Top = 56
    Width = 85
    Height = 13
    Caption = 'Macro Description'
    FocusControl = mmoMacroDescription
  end
  object lblMacroKeystrokes: TLabel
    Left = 8
    Top = 144
    Width = 85
    Height = 13
    Caption = 'Macro Keystrokes'
  end
  object edtMacroName: TEdit
    Left = 8
    Top = 24
    Width = 409
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object chkDoNotShowAgain: TCheckBox
    Left = 8
    Top = 384
    Width = 249
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Do not show this dialog again'
    TabOrder = 2
  end
  object btnOK: TButton
    Left = 264
    Top = 384
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 8
  end
  object btnCancel: TButton
    Left = 344
    Top = 384
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 9
  end
  object mmoMacroDescription: TMemo
    Left = 8
    Top = 72
    Width = 409
    Height = 57
    Anchors = [akLeft, akTop, akRight]
    BevelEdges = [beLeft, beTop, beBottom]
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object pnlMacro: TPanel
    Left = 8
    Top = 160
    Width = 329
    Height = 217
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelEdges = [beLeft, beTop, beBottom]
    Caption = 'Macro content goes here'
    Color = clWindow
    TabOrder = 3
    TabStop = True
  end
  object btnDelete: TButton
    Left = 344
    Top = 224
    Width = 75
    Height = 25
    Caption = 'Delete'
    TabOrder = 6
    OnClick = btnDeleteClick
  end
  object btnEdit: TButton
    Left = 344
    Top = 160
    Width = 75
    Height = 25
    Caption = 'Edit'
    TabOrder = 4
    OnClick = btnEditClick
  end
  object btnInsert: TButton
    Left = 344
    Top = 192
    Width = 75
    Height = 25
    Caption = 'Insert'
    TabOrder = 5
    OnClick = btnInsertClick
  end
  object btnAppend: TButton
    Left = 344
    Top = 256
    Width = 75
    Height = 25
    Caption = 'Append'
    TabOrder = 7
    OnClick = btnAppendClick
  end
end
