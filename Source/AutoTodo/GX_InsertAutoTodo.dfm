object fmInsertAutoTodoForm: TfmInsertAutoTodoForm
  Left = 381
  Top = 212
  BorderIcons = [biSystemMenu]
  Caption = 'Insert Auto TODOs'
  ClientHeight = 161
  ClientWidth = 665
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  DesignSize = (
    665
    161)
  PixelsPerInch = 96
  TextHeight = 14
  object l_Username: TLabel
    Left = 8
    Top = 8
    Width = 228
    Height = 14
    Caption = 'Username ('#39'*'#39' = use Windows username)'
  end
  object l_TextToInsert: TLabel
    Left = 8
    Top = 56
    Width = 293
    Height = 14
    Caption = 'Text to insert (include comment markers if required!)'
  end
  object btnOK: TButton
    Left = 504
    Top = 128
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object btnCancel: TButton
    Left = 584
    Top = 128
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object ed_Username: TEdit
    Left = 8
    Top = 24
    Width = 249
    Height = 22
    TabOrder = 2
  end
  object b_ResetTextToInsert: TButton
    Left = 496
    Top = 48
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Reset'
    TabOrder = 3
    OnClick = b_ResetTextToInsertClick
  end
  object b_Placeholder: TButton
    Left = 576
    Top = 48
    Width = 81
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Placeholders'
    TabOrder = 4
  end
  object m_TextToInsert: TMemo
    Left = 8
    Top = 72
    Width = 649
    Height = 41
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 5
  end
  object pm_Placeholders: TPopupMenu
    Left = 448
    Top = 8
  end
end
