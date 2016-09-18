inherited fmConfigureIfDef: TfmConfigureIfDef
  Anchors = [akRight, akBottom]
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Insert IFDEF / IFNDEF'
  ClientHeight = 393
  ClientWidth = 699
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object pc_IfClasses: TPageControl
    Left = 0
    Top = 0
    Width = 699
    Height = 352
    Align = alClient
    TabOrder = 0
    OnChange = pc_IfClassesChange
    OnMouseMove = pc_IfClassesMouseMove
  end
  object p_Bottom: TPanel
    Left = 0
    Top = 352
    Width = 699
    Height = 41
    Align = alBottom
    TabOrder = 1
    DesignSize = (
      699
      41)
    object b_OK: TButton
      Left = 536
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object b_Cancel: TButton
      Left = 616
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object chk_AppendComment: TCheckBox
      Left = 8
      Top = 12
      Width = 193
      Height = 17
      Caption = '&Append // Comment'
      TabOrder = 2
      OnClick = chk_AppendCommentClick
    end
    object b_Open: TButton
      Left = 457
      Top = 8
      Width = 75
      Height = 25
      Action = act_Open
      Anchors = [akTop, akRight]
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
    end
  end
  object b_Add: TButton
    Left = 673
    Top = 0
    Width = 25
    Height = 25
    Action = act_Add
    Anchors = [akTop, akRight]
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
  end
  object TheActionList: TActionList
    Left = 64
    Top = 88
    object act_Open: TAction
      Caption = 'Open'
      Hint = 'Open include file'
      ShortCut = 16463
      OnExecute = act_OpenExecute
    end
    object act_Add: TAction
      Caption = '+'
      Hint = 'Add include file'
      ShortCut = 16449
      OnExecute = act_AddExecute
    end
  end
  object pm_IncludeFiles: TPopupMenu
    Left = 208
    Top = 200
  end
end
