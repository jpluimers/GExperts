inherited f_IdeBuildEventFavoriteEdit: Tf_IdeBuildEventFavoriteEdit
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Favorite Build Event'
  ClientHeight = 137
  ClientWidth = 417
  ParentFont = False
  OldCreateOrder = True
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object l_Name: TLabel
    Left = 8
    Top = 8
    Width = 27
    Height = 13
    Caption = 'Name'
  end
  object l_Command: TLabel
    Left = 8
    Top = 56
    Width = 47
    Height = 13
    Caption = 'Command'
  end
  object ed_Name: TEdit
    Left = 8
    Top = 24
    Width = 401
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object ed_Command: TEdit
    Left = 8
    Top = 72
    Width = 401
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
  end
  object b_OK: TButton
    Left = 256
    Top = 104
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object b_Cancel: TButton
    Left = 336
    Top = 104
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
end
