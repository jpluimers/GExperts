object fmPasteAsConfig: TfmPasteAsConfig
  Left = 0
  Top = 0
  Width = 312
  Height = 211
  ActiveControl = cbPasteAsType
  Caption = 'PasteAs Options'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    304
    181)
  PixelsPerInch = 96
  TextHeight = 13
  object gbxPasteAsOptions: TGroupBox
    Left = 8
    Top = 8
    Width = 289
    Height = 138
    Caption = 'PasteAs Options'
    TabOrder = 0
    object lblMaxEntries: TLabel
      Left = 25
      Top = 30
      Width = 70
      Height = 13
      Alignment = taRightJustify
      Caption = 'Paste as type:'
    end
    object chkCreateQuotedStrings: TCheckBox
      Left = 23
      Top = 56
      Width = 250
      Height = 25
      Caption = 'Create quoted strings'
      TabOrder = 1
    end
    object cbPasteAsType: TComboBox
      Left = 103
      Top = 27
      Width = 170
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
    end
    object chkAddExtraSpaceAtTheEnd: TCheckBox
      Left = 23
      Top = 80
      Width = 250
      Height = 17
      Caption = 'Add extra space char at the end'
      TabOrder = 2
    end
    object chkShowOptions: TCheckBox
      Left = 23
      Top = 112
      Width = 250
      Height = 17
      Caption = 'Show options dialog when paste as'
      TabOrder = 3
    end
  end
  object btnOK: TButton
    Left = 142
    Top = 152
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
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
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
