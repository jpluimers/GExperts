object fmProofreaderAutoCorrectEntry: TfmProofreaderAutoCorrectEntry
  Left = 290
  Top = 228
  BorderStyle = bsDialog
  Caption = 'Edit AutoCorrect Entry'
  ClientHeight = 117
  ClientWidth = 412
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  DesignSize = (
    412
    117)
  PixelsPerInch = 96
  TextHeight = 13
  object lblReplaceWhat: TLabel
    Left = 17
    Top = 13
    Width = 64
    Height = 13
    Alignment = taRightJustify
    Caption = 'Replace &Text'
    FocusControl = edtReplaceWhat
  end
  object lblLocation: TLabel
    Left = 40
    Top = 37
    Width = 41
    Height = 13
    Alignment = taRightJustify
    Caption = '&Location'
    FocusControl = cbxLocation
  end
  object lblReplaceWith: TLabel
    Left = 16
    Top = 61
    Width = 65
    Height = 13
    Alignment = taRightJustify
    Caption = 'Replace &With'
    FocusControl = edtReplaceWith
  end
  object edtReplaceWhat: TEdit
    Left = 88
    Top = 9
    Width = 175
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnChange = EditBoxChange
  end
  object btnOK: TButton
    Left = 245
    Top = 86
    Width = 74
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object btnCancel: TButton
    Left = 330
    Top = 86
    Width = 74
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object edtReplaceWith: TEdit
    Left = 88
    Top = 57
    Width = 316
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    OnChange = EditBoxChange
  end
  object cbxLocation: TComboBox
    Left = 88
    Top = 33
    Width = 175
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 1
  end
end
