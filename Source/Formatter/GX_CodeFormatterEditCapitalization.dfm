object fmCodeFormatterEditCapitalization: TfmCodeFormatterEditCapitalization
  Left = 0
  Top = 0
  Width = 377
  Height = 368
  ActiveControl = ed_Search
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Capitalization'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCloseQuery = FormCloseQuery
  DesignSize = (
    361
    329)
  PixelsPerInch = 96
  TextHeight = 13
  object b_Clear: TSpeedButton
    Left = 172
    Top = 8
    Width = 21
    Height = 21
    Action = act_ClearSearch
    Anchors = [akTop, akRight]
    ParentShowHint = False
    ShowHint = True
  end
  object b_UpperCase: TSpeedButton
    Left = 200
    Top = 8
    Width = 153
    Height = 25
    Action = act_AllUpperCase
    Anchors = [akTop, akRight]
    ParentShowHint = False
    ShowHint = True
  end
  object b_LowerCase: TSpeedButton
    Left = 200
    Top = 72
    Width = 153
    Height = 25
    Action = act_AllLowerCase
    Anchors = [akTop, akRight]
    ParentShowHint = False
    ShowHint = True
  end
  object b_FirstCharUp: TSpeedButton
    Left = 200
    Top = 40
    Width = 153
    Height = 25
    Action = act_FirstCharUp
    Anchors = [akTop, akRight]
    ParentShowHint = False
    ShowHint = True
  end
  object b_ToggleComment: TSpeedButton
    Left = 200
    Top = 112
    Width = 153
    Height = 25
    Action = act_ToggleComment
    Anchors = [akTop, akRight]
    ParentShowHint = False
    ShowHint = True
  end
  object ed_Search: TEdit
    Left = 8
    Top = 8
    Width = 161
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnChange = ed_SearchChange
  end
  object p_Items: TPanel
    Left = 8
    Top = 32
    Width = 185
    Height = 257
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'editor goes here, created at runtime'
    TabOrder = 1
  end
  object p_Buttons: TPanel
    Left = 0
    Top = 288
    Width = 361
    Height = 41
    Align = alBottom
    TabOrder = 2
    DesignSize = (
      361
      41)
    object b_OK: TButton
      Left = 200
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object b_Cancel: TButton
      Left = 280
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object b_Import: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Action = act_Import
      TabOrder = 2
    end
    object b_Export: TButton
      Left = 89
      Top = 8
      Width = 75
      Height = 25
      Action = act_Export
      TabOrder = 3
    end
  end
  object TheActionList: TActionList
    Left = 232
    Top = 152
    object act_AllUpperCase: TAction
      Caption = 'All Upper Case (Alt+PgUp)'
      Hint = 'Alt+PgUp'
      ShortCut = 32801
      OnExecute = act_AllUpperCaseExecute
    end
    object act_AllLowerCase: TAction
      Caption = 'All Lower Case (Alt+PgDn)'
      Hint = 'Alt+PgDn'
      ShortCut = 32802
      OnExecute = act_AllLowerCaseExecute
    end
    object act_FirstCharUp: TAction
      Caption = 'First Char Up (Alt+Up)'
      Hint = 'Alt+Up'
      ShortCut = 32806
      OnExecute = act_FirstCharUpExecute
    end
    object act_ToggleComment: TAction
      Caption = 'Toggle Comment (Alt+Del)'
      Hint = 'Alt+Del'
      ShortCut = 32814
      OnExecute = act_ToggleCommentExecute
    end
    object act_ClearSearch: TAction
      Caption = 'X'
      Hint = 'Ctrl+Del'
      ShortCut = 16430
      OnExecute = act_ClearSearchExecute
    end
    object act_Import: TAction
      Caption = 'Import ...'
      OnExecute = act_ImportExecute
    end
    object act_Export: TAction
      Caption = 'Export ...'
      OnExecute = act_ExportExecute
    end
  end
end
