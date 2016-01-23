object fmCodeFormatterEditCapitalization: TfmCodeFormatterEditCapitalization
  Left = 451
  Top = 195
  BorderStyle = bsSingle
  Caption = 'Code Formatter Capitalization'
  ClientHeight = 349
  ClientWidth = 452
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object l_FileName: TLabel
    Left = 9
    Top = 324
    Width = 3
    Height = 13
  end
  object p_Buttons: TPanel
    Left = 0
    Top = 308
    Width = 452
    Height = 41
    Align = alBottom
    TabOrder = 0
    object b_Help: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Caption = '&Help'
      TabOrder = 2
      OnClick = b_HelpClick
    end
    object b_Ok: TButton
      Left = 288
      Top = 8
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object b_Cancel: TButton
      Left = 369
      Top = 8
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object b_Import: TButton
      Left = 104
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Import ...'
      TabOrder = 3
      OnClick = b_ImportClick
    end
    object b_Export: TButton
      Left = 185
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Export ...'
      TabOrder = 4
      OnClick = b_ExportClick
    end
  end
  object p_Upper: TPanel
    Left = 0
    Top = 0
    Width = 452
    Height = 308
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object l_Select: TLabel
      Left = 8
      Top = 8
      Width = 86
      Height = 13
      Caption = 'Select identifier(s):'
    end
    object l_ChangeInto: TLabel
      Left = 240
      Top = 48
      Width = 60
      Height = 13
      Caption = 'Change into:'
    end
    object lb_Items: TListBox
      Left = 8
      Top = 48
      Width = 217
      Height = 254
      Anchors = [akLeft, akTop, akBottom]
      ItemHeight = 13
      MultiSelect = True
      TabOrder = 0
      OnClick = lb_ItemsClick
    end
    object ed_Search: TEdit
      Left = 8
      Top = 24
      Width = 217
      Height = 21
      TabOrder = 1
      OnChange = ed_SearchChange
      OnKeyDown = ed_SearchKeyDown
    end
    object ed_Change: TEdit
      Left = 240
      Top = 64
      Width = 204
      Height = 21
      TabOrder = 2
      OnChange = ed_ChangeChange
    end
    object b_UpperCase: TButton
      Left = 252
      Top = 88
      Width = 89
      Height = 25
      Caption = 'UpperCase'
      TabOrder = 3
      OnClick = b_UpperCaseClick
    end
    object b_LowerCase: TButton
      Left = 347
      Top = 88
      Width = 89
      Height = 25
      Caption = 'LowerCase'
      TabOrder = 4
      OnClick = b_LowerCaseClick
    end
    object b_FirstCharUp: TButton
      Left = 252
      Top = 120
      Width = 89
      Height = 25
      Caption = 'First Char Up'
      TabOrder = 5
      OnClick = b_FirstCharUpClick
    end
    object b_FirstCharLow: TButton
      Left = 347
      Top = 119
      Width = 89
      Height = 25
      Caption = 'First Char Low'
      TabOrder = 6
      OnClick = b_FirstCharLowClick
    end
    object b_AddIdentifier: TButton
      Left = 252
      Top = 168
      Width = 89
      Height = 26
      Caption = 'Add Identifier'
      TabOrder = 7
      OnClick = b_AddIdentifierClick
    end
    object b_Delete: TButton
      Left = 252
      Top = 200
      Width = 89
      Height = 25
      Caption = 'Delete'
      TabOrder = 8
      OnClick = b_DeleteClick
    end
    object b_ToggleComment: TButton
      Left = 252
      Top = 232
      Width = 89
      Height = 25
      Caption = 'Toggle comment'
      TabOrder = 9
      OnClick = b_ToggleCommentClick
    end
  end
  object od_Import: TOpenDialog
    DefaultExt = 'txt'
    Filter = 'text files (*.txt)|*.txt|all files (*.*)|*.*'
    FilterIndex = 0
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 248
    Top = 256
  end
  object sd_Export: TSaveDialog
    DefaultExt = 'txt'
    Filter = 'text files (*.txt)|*.txt|all files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofNoReadOnlyReturn, ofEnableSizing]
    Left = 328
    Top = 256
  end
end
