object fmCommentConfig: TfmCommentConfig
  Left = 311
  Top = 211
  ActiveControl = lvStyles
  BorderStyle = bsDialog
  Caption = 'Comment Expert'
  ClientHeight = 371
  ClientWidth = 355
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    355
    371)
  PixelsPerInch = 96
  TextHeight = 14
  object btnOK: TButton
    Left = 192
    Top = 341
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object btnCancel: TButton
    Left = 273
    Top = 341
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object gbStyles: TGroupBox
    Left = 4
    Top = 0
    Width = 346
    Height = 142
    Caption = 'Comment Styles'
    TabOrder = 0
    object lvStyles: TListView
      Left = 8
      Top = 23
      Width = 329
      Height = 112
      Columns = <
        item
          Caption = 'Extensions'
          Width = 190
        end
        item
          Caption = 'Style'
          Width = 60
        end
        item
          Caption = 'Space'
        end>
      OwnerData = True
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnData = lvStylesData
      OnSelectItem = lvStylesSelectItem
    end
  end
  object gbStyle: TGroupBox
    Left = 4
    Top = 145
    Width = 346
    Height = 192
    Caption = 'Style settings'
    TabOrder = 1
    object lblExtensions: TLabel
      Left = 12
      Top = 22
      Width = 58
      Height = 14
      Caption = 'Extensions'
    end
    object btnAdd: TButton
      Left = 245
      Top = 38
      Width = 75
      Height = 25
      Caption = 'Add'
      TabOrder = 3
      OnClick = btnAddClick
    end
    object btnDelete: TButton
      Left = 245
      Top = 69
      Width = 75
      Height = 25
      Caption = 'Delete'
      TabOrder = 4
      OnClick = btnDeleteClick
    end
    object chkInsertSpace: TCheckBox
      Left = 12
      Top = 163
      Width = 201
      Height = 17
      Caption = 'Insert and remove &space'
      TabOrder = 2
      OnClick = chkInsertSpaceClick
    end
    object eExtensions: TEdit
      Left = 12
      Top = 39
      Width = 201
      Height = 22
      TabOrder = 0
      OnChange = eExtensionsChange
    end
    object rgStyle: TRadioGroup
      Left = 12
      Top = 67
      Width = 201
      Height = 90
      Caption = 'Comment Style'
      Columns = 2
      Items.Strings = (
        '//'
        '{ }'
        '-- (SQL)'
        '/* */'
        '(* *)')
      TabOrder = 1
      OnClick = rgStyleClick
    end
  end
end
