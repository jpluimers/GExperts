inherited fmConfigureIfDef: TfmConfigureIfDef
  Anchors = [akRight, akBottom]
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Insert IFDEF / IFNDEF'
  ClientHeight = 393
  ClientWidth = 442
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object pc_IfClasses: TPageControl
    Left = 0
    Top = 0
    Width = 442
    Height = 352
    Align = alClient
    TabOrder = 0
    OnChange = pc_IfClassesChange
  end
  object p_Bottom: TPanel
    Left = 0
    Top = 352
    Width = 442
    Height = 41
    Align = alBottom
    TabOrder = 1
    DesignSize = (
      442
      41)
    object b_OK: TButton
      Left = 279
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
      Left = 359
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
      Width = 185
      Height = 17
      Caption = '&Append // Comment'
      TabOrder = 2
      OnClick = chk_AppendCommentClick
    end
    object b_Open: TButton
      Left = 200
      Top = 8
      Width = 75
      Height = 25
      Caption = 'O&pen'
      TabOrder = 3
      OnClick = b_OpenClick
    end
  end
end
