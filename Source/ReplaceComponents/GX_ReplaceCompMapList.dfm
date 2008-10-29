object fmReplaceCompMapList: TfmReplaceCompMapList
  Left = 283
  Top = 224
  AutoScroll = False
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Replace Components - Mapping Definition List'
  ClientHeight = 369
  ClientWidth = 479
  Color = clBtnFace
  Constraints.MinHeight = 171
  Constraints.MinWidth = 285
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlHeader: TPanel
    Left = 0
    Top = 0
    Width = 479
    Height = 34
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object lblGroup: TLabel
      Left = 16
      Top = 12
      Width = 32
      Height = 13
      Caption = 'Group:'
    end
    object comGroupName: TComboBox
      Left = 56
      Top = 8
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      OnChange = comGroupNameChange
    end
    object tbrGroups: TToolBar
      Left = 204
      Top = 8
      Width = 23
      Height = 22
      Align = alNone
      AutoSize = True
      DisabledImages = dmSharedImages.DisabledImages
      EdgeInner = esNone
      EdgeOuter = esNone
      Flat = True
      Images = dmSharedImages.Images
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      object tbnGroups: TToolButton
        Left = 0
        Top = 0
        Action = actOpenGroupList
      end
    end
  end
  object pnlMain: TPanel
    Left = 0
    Top = 34
    Width = 479
    Height = 335
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 1
    object pnlMappings: TPanel
      Left = 3
      Top = 25
      Width = 473
      Height = 307
      Align = alClient
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 0
      object lvMapItems: TListView
        Left = 3
        Top = 3
        Width = 467
        Height = 301
        Align = alClient
        Columns = <
          item
            Caption = 'Group'
            Width = 125
          end
          item
            Caption = 'Source'
            Width = 160
          end
          item
            Caption = 'Destination'
            Width = 160
          end>
        MultiSelect = True
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        OnClick = lvMapItemsClick
        OnColumnClick = lvMapItemsColumnClick
        OnDblClick = lvMapItemsDblClick
      end
    end
    object tbrReplacement: TToolBar
      Left = 3
      Top = 3
      Width = 473
      Height = 22
      AutoSize = True
      DisabledImages = dmSharedImages.DisabledImages
      EdgeBorders = []
      Flat = True
      Images = dmSharedImages.Images
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      object pnlToolSep: TPanel
        Left = 0
        Top = 0
        Width = 5
        Height = 22
        BevelOuter = bvNone
        TabOrder = 0
      end
      object tbnAdd: TToolButton
        Left = 5
        Top = 0
        Action = actAdd
      end
      object tbnEdit: TToolButton
        Left = 28
        Top = 0
        Action = actEdit
      end
      object tbnDelete: TToolButton
        Left = 51
        Top = 0
        Action = actDelete
      end
    end
  end
  object Actions: TActionList
    Images = dmSharedImages.Images
    Left = 112
    Top = 114
    object actAdd: TAction
      Caption = 'Add'
      Hint = 'Add'
      ImageIndex = 41
      OnExecute = actAddExecute
    end
    object actEdit: TAction
      Caption = 'Edit'
      Hint = 'Edit'
      ImageIndex = 38
      OnExecute = actEditExecute
    end
    object actDelete: TAction
      Caption = 'Delete'
      Hint = 'Delete'
      ImageIndex = 42
      OnExecute = actDeleteExecute
    end
    object actOpenGroupList: TAction
      Hint = 'Open group list'
      ImageIndex = 67
      OnExecute = btnOpenGroupListClick
    end
  end
end
