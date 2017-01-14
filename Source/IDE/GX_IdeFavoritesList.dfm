inherited f_GxIdeFavoritesList: Tf_GxIdeFavoritesList
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Favorite Search Paths'
  ClientHeight = 305
  ClientWidth = 417
  ParentFont = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object b_Close: TButton
    Left = 336
    Top = 272
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    ModalResult = 1
    TabOrder = 4
  end
  object lv_Favorites: TListView
    Left = 8
    Top = 8
    Width = 313
    Height = 289
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'Name'
        Width = 100
      end
      item
        Caption = 'Value'
        Width = 200
      end>
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    OnDblClick = lv_FavoritesDblClick
  end
  object b_Add: TButton
    Left = 336
    Top = 24
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Add ...'
    TabOrder = 1
    OnClick = b_AddClick
  end
  object b_Edit: TButton
    Left = 336
    Top = 56
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Edit ...'
    Default = True
    TabOrder = 2
    OnClick = b_EditClick
  end
  object b_Delete: TButton
    Left = 336
    Top = 88
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Delete'
    TabOrder = 3
    OnClick = b_DeleteClick
  end
end
