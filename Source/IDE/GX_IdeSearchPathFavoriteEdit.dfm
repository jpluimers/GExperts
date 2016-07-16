inherited f_IdeSearchPathFavoriteEdit: Tf_IdeSearchPathFavoriteEdit
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Favorite Search Path'
  ClientHeight = 209
  ClientWidth = 417
  ParentFont = False
  OldCreateOrder = True
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object l_Name: TLabel
    Left = 8
    Top = 8
    Width = 28
    Height = 13
    Caption = 'Name'
  end
  object l_Path: TLabel
    Left = 8
    Top = 56
    Width = 22
    Height = 13
    Caption = 'Path'
  end
  object ed_Name: TEdit
    Left = 8
    Top = 24
    Width = 401
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object m_Path: TMemo
    Left = 8
    Top = 72
    Width = 401
    Height = 97
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
  end
  object b_OK: TButton
    Left = 256
    Top = 176
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object b_Cancel: TButton
    Left = 336
    Top = 176
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object b_Select: TButton
    Left = 8
    Top = 176
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Select'
    TabOrder = 2
    OnClick = b_SelectClick
  end
end
