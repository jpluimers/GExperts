object fmProofreaderAutoCorrectEntry: TfmProofreaderAutoCorrectEntry
  Left = 290
  Top = 228
  BorderStyle = bsDialog
  Caption = 'Edit AutoCorrect Entry'
  ClientHeight = 126
  ClientWidth = 418
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  DesignSize = (
    418
    126)
  PixelsPerInch = 96
  TextHeight = 14
  object lblReplaceWhat: TLabel
    Left = 21
    Top = 13
    Width = 72
    Height = 14
    Alignment = taRightJustify
    Caption = 'Replace &Text'
    FocusControl = edtReplaceWhat
  end
  object lblLocation: TLabel
    Left = 47
    Top = 39
    Width = 46
    Height = 14
    Alignment = taRightJustify
    Caption = '&Location'
    FocusControl = cbxLocation
  end
  object lblReplaceWith: TLabel
    Left = 21
    Top = 65
    Width = 72
    Height = 14
    Alignment = taRightJustify
    Caption = 'Replace &With'
    FocusControl = edtReplaceWith
  end
  object edtReplaceWhat: TEdit
    Left = 100
    Top = 9
    Width = 181
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnChange = EditBoxChange
  end
  object btnOK: TButton
    Left = 248
    Top = 93
    Width = 74
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object btnCancel: TButton
    Left = 333
    Top = 93
    Width = 74
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object edtReplaceWith: TEdit
    Left = 100
    Top = 61
    Width = 307
    Height = 22
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    OnChange = EditBoxChange
  end
  object cbxLocation: TComboBox
    Left = 100
    Top = 35
    Width = 181
    Height = 22
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 14
    TabOrder = 1
  end
end
