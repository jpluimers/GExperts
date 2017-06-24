object fmCompRename: TfmCompRename
  Left = 413
  Top = 304
  ActiveControl = edtNewName
  BorderStyle = bsDialog
  Caption = 'GExperts Rename Component'
  ClientHeight = 97
  ClientWidth = 392
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  DesignSize = (
    392
    97)
  PixelsPerInch = 96
  TextHeight = 14
  object lblOldName: TLabel
    Left = 10
    Top = 12
    Width = 52
    Height = 14
    Caption = '&Old name'
    FocusControl = edtOldName
  end
  object lblNewName: TLabel
    Left = 10
    Top = 36
    Width = 59
    Height = 14
    Caption = '&New name'
    FocusControl = edtNewName
  end
  object lblReason: TLabel
    Left = 10
    Top = 70
    Width = 85
    Height = 14
    Anchors = [akLeft, akBottom]
    Caption = 'Invalid identifier'
    Visible = False
  end
  object edtOldName: TEdit
    Left = 91
    Top = 8
    Width = 293
    Height = 22
    TabStop = False
    Anchors = [akLeft, akTop, akRight]
    ReadOnly = True
    TabOrder = 0
  end
  object edtNewName: TEdit
    Left = 91
    Top = 32
    Width = 293
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    AutoSelect = False
    TabOrder = 1
    OnChange = edtNewNameChange
  end
  object btnCancel: TButton
    Left = 229
    Top = 65
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object btnOK: TButton
    Left = 149
    Top = 65
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object btnSettings: TButton
    Left = 309
    Top = 65
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Settings...'
    TabOrder = 4
    OnClick = btnSettingsClick
  end
end
