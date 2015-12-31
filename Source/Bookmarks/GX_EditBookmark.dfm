inherited fmEditBookmarks: TfmEditBookmarks
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'Edit Bookmark'
  ClientHeight = 137
  ClientWidth = 233
  ParentFont = False
  Position = poOwnerFormCenter
  ExplicitWidth = 241
  ExplicitHeight = 167
  PixelsPerInch = 96
  TextHeight = 13
  object l_BmIndex: TLabel
    Left = 136
    Top = 56
    Width = 46
    Height = 13
    Caption = 'Bookmark'
  end
  object l_Module: TLabel
    Left = 8
    Top = 8
    Width = 34
    Height = 13
    Caption = 'Module'
  end
  object l_Line: TLabel
    Left = 8
    Top = 57
    Width = 19
    Height = 13
    Caption = 'Line'
  end
  object ed_Line: TEdit
    Left = 8
    Top = 72
    Width = 121
    Height = 21
    TabOrder = 1
    OnChange = ed_LineChange
  end
  object cmb_BmIndex: TComboBox
    Left = 136
    Top = 72
    Width = 89
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 2
    OnChange = cmb_BmIndexChange
  end
  object cmb_Module: TComboBox
    Left = 8
    Top = 24
    Width = 217
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 0
    OnChange = cmb_ModuleChange
  end
  object b_Ok: TButton
    Left = 72
    Top = 104
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'OK'
    Default = True
    Enabled = False
    ModalResult = 1
    TabOrder = 3
  end
  object b_Cancel: TButton
    Left = 152
    Top = 104
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
end
