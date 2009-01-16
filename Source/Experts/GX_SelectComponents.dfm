object SelectComponentsForm: TSelectComponentsForm
  Left = 376
  Top = 207
  Width = 404
  Height = 388
  ActiveControl = StayOnTopCheckBox
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeToolWin
  Caption = 'Select Components'
  Color = clBtnFace
  Constraints.MinHeight = 175
  Constraints.MinWidth = 190
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Scaled = False
  OnActivate = FormActivate
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 14
  object FindPanel: TPanel
    Left = 0
    Top = 0
    Width = 388
    Height = 32
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      388
      32)
    object FilterLabel: TLabel
      Left = 12
      Top = 9
      Width = 26
      Height = 14
      Caption = 'Filter'
    end
    object SearchEdit: TEdit
      Left = 43
      Top = 5
      Width = 242
      Height = 22
      Hint = 'Find a component by name and/or type: [<name>][:][<type>]'
      Anchors = [akLeft, akTop, akRight]
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnChange = SearchEditChange
      OnKeyPress = SearchEditKeyPress
      OnKeyUp = SearchEditKeyUp
    end
    object SelectAllButton: TBitBtn
      Left = 287
      Top = 4
      Width = 61
      Height = 24
      Action = SelectAllAction
      Anchors = [akTop, akRight]
      Caption = 'Select'
      ModalResult = 8
      TabOrder = 1
    end
    object ResizeButton: TBitBtn
      Left = 350
      Top = 4
      Width = 28
      Height = 24
      Action = ChangeModeAction
      Anchors = [akTop, akRight]
      TabOrder = 2
      Glyph.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000000000000000000000000000000000000FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00CEA58C00AD734200AD734200AD734200AD734200AD73
        4200734A2900FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00CEA58C00AD734200AD734200AD734200734A
        2900FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00CEA58C00AD734200734A2900FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00AD734200FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000808
        0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF0008080000FF00FF00FF00FF00FF00FF000808
        0000080800000808000008080000080800000808000008080000080800000808
        000008080000080800000808000008080000FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000808
        0000080800000808000008080000080800000808000008080000080800000808
        000008080000080800000808000008080000FF00FF00FF00FF00FF00FF000808
        0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF0008080000FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00AD734200FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00CEA58C00B57B4A00734A2900FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00CEA58C00AD734200AD734200AD734200734A
        2900FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00CEA58C00AD734200AD734200AD734200AD734200AD73
        4200734A2900FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
        FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
    end
  end
  object TreePanel: TPanel
    Left = 0
    Top = 32
    Width = 388
    Height = 320
    Align = alClient
    BevelOuter = bvNone
    Caption = 'TreePanel'
    TabOrder = 1
    DesignSize = (
      388
      320)
    object TreeView: TTreeView
      Left = 6
      Top = 2
      Width = 373
      Height = 295
      Anchors = [akLeft, akTop, akRight, akBottom]
      DragMode = dmAutomatic
      HideSelection = False
      Indent = 19
      ReadOnly = True
      TabOrder = 0
      OnClick = TreeViewClick
      OnKeyUp = TreeViewKeyUp
    end
    object BottomPanel: TPanel
      Left = 0
      Top = 302
      Width = 388
      Height = 18
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      object StayOnTopCheckBox: TCheckBox
        Left = 8
        Top = 0
        Width = 100
        Height = 13
        Caption = 'Stay on &top'
        TabOrder = 0
        OnClick = StayOnTopCheckBoxClick
      end
      object ExactNameCheckBox: TCheckBox
        Left = 112
        Top = 0
        Width = 98
        Height = 13
        Caption = 'Exact &name'
        TabOrder = 1
        OnClick = ExactCheckBoxClick
      end
      object ExactTypeCheckBox: TCheckBox
        Left = 217
        Top = 0
        Width = 104
        Height = 13
        Caption = 'Exact &type'
        TabOrder = 2
        OnClick = ExactCheckBoxClick
      end
    end
  end
  object ActionList: TActionList
    Images = dmSharedImages.Images
    OnUpdate = ActionListUpdate
    Left = 96
    Top = 72
    object SelectAllAction: TAction
      Caption = 'Select'
      ShortCut = 16449
      OnExecute = SelectAllActionExecute
    end
    object ChangeModeAction: TAction
      ImageIndex = 13
      OnExecute = ChangeModeActionExecute
      OnHint = ChangeModeActionHint
    end
  end
end
