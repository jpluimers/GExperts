inherited fmProjOptionSets: TfmProjOptionSets
  Left = 399
  Top = 185
  Width = 198
  Height = 357
  Caption = 'Project Option Sets'
  Position = poScreenCenter
  OnHide = FormHide
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pcSettings: TPageControl
    Left = 0
    Top = 42
    Width = 190
    Height = 286
    ActivePage = tabSets
    Align = alClient
    TabIndex = 0
    TabOrder = 0
    object tabSets: TTabSheet
      Caption = '&Sets'
      ImageIndex = 2
      DesignSize = (
        182
        258)
      object lstSets: TListBox
        Left = 2
        Top = 5
        Width = 177
        Height = 252
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 13
        PopupMenu = pmuSets
        TabOrder = 0
        OnClick = lstSetsClick
      end
    end
    object tabProject: TTabSheet
      Caption = '&Project'
      object lblProjectSettings: TLabel
        Left = 56
        Top = 96
        Width = 3
        Height = 13
        Visible = False
      end
      object pnlFilterComboHost: TPanel
        Left = 0
        Top = 230
        Width = 182
        Height = 28
        Align = alBottom
        BevelOuter = bvNone
        FullRepaint = False
        TabOrder = 0
        DesignSize = (
          182
          28)
        object cbFilter: TComboBox
          Left = 2
          Top = 3
          Width = 178
          Height = 21
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          DropDownCount = 12
          ItemHeight = 13
          TabOrder = 0
          OnChange = cbFilterChange
        end
      end
      object pnlCheckListHost: TPanel
        Left = 0
        Top = 0
        Width = 182
        Height = 230
        Align = alClient
        BevelOuter = bvNone
        Caption = 'TCheckListBox Created At Runtime'
        FullRepaint = False
        ParentShowHint = False
        ShowHint = False
        TabOrder = 1
      end
    end
    object tabEnvironment: TTabSheet
      Caption = '&Environment'
      ImageIndex = 1
      ParentShowHint = False
      ShowHint = False
      object lblCheckListNot: TLabel
        Left = 5
        Top = 24
        Width = 167
        Height = 13
        Caption = 'TCheckListBox Created At Runtime'
        Visible = False
      end
    end
  end
  object pnlCurrentSet: TPanel
    Left = 0
    Top = 22
    Width = 190
    Height = 20
    Align = alTop
    Alignment = taLeftJustify
    BevelOuter = bvNone
    BorderWidth = 2
    Caption = 'Current Set: Default'
    TabOrder = 1
  end
  object ToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 190
    Height = 22
    AutoSize = True
    DisabledImages = dmSharedImages.DisabledImages
    EdgeBorders = []
    Flat = True
    Images = dmSharedImages.Images
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    Wrapable = False
    object tbnNewSet: TToolButton
      Left = 0
      Top = 0
      Action = actNewSet
    end
    object tbnDeleteSet: TToolButton
      Left = 23
      Top = 0
      Action = actDeleteSet
    end
    object tbnSaveSets: TToolButton
      Left = 46
      Top = 0
      Action = actSaveSets
    end
    object tbnApplyToProject: TToolButton
      Left = 69
      Top = 0
      Action = actApplySet
    end
    object tbnSep1: TToolButton
      Left = 92
      Top = 0
      Width = 8
      ImageIndex = 4
      Style = tbsSeparator
    end
    object tbnHelp: TToolButton
      Left = 100
      Top = 0
      Action = actHelp
    end
  end
  object pmuPrjOptions: TPopupMenu
    Images = dmSharedImages.Images
    OnPopup = pmuPrjOptionsPopup
    Left = 80
    Top = 80
    object mniModifyPrjOptionValues: TMenuItem
      Caption = '&Modify Option Values...'
      OnClick = mniModifyPrjOptionValuesClick
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object mniPrjClearAll: TMenuItem
      Caption = 'Clear All'
      OnClick = mniPrjClearAllClick
    end
    object mniPrjCheckAll: TMenuItem
      Caption = 'Check All'
      OnClick = mniPrjCheckAllClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object mniPrjSortByName: TMenuItem
      Caption = 'Sort by Name'
      Checked = True
      GroupIndex = 1
      RadioItem = True
      OnClick = mniPrjSortByCheckmarkClick
    end
    object mniPrjSortByCheckmark: TMenuItem
      Caption = 'Sort by Checkmark'
      GroupIndex = 1
      RadioItem = True
      OnClick = mniPrjSortByCheckmarkClick
    end
    object N2: TMenuItem
      Caption = '-'
      GroupIndex = 1
    end
    object mniPrjAscending: TMenuItem
      Caption = 'Ascending'
      Checked = True
      GroupIndex = 2
      RadioItem = True
      OnClick = mniPrjDescendingClick
    end
    object mniPrjDescending: TMenuItem
      Caption = 'Descending'
      GroupIndex = 2
      RadioItem = True
      OnClick = mniPrjDescendingClick
    end
  end
  object pmuEnvOptions: TPopupMenu
    Images = dmSharedImages.Images
    Left = 80
    Top = 128
    object mniModifyEnvOptionValues: TMenuItem
      Caption = '&Modify Option Values...'
      OnClick = mniModifyEnvOptionValuesClick
    end
  end
  object Actions: TActionList
    Images = dmSharedImages.Images
    OnUpdate = ActionsUpdate
    Left = 16
    Top = 80
    object actNewSet: TAction
      Category = 'Sets'
      Caption = 'New Set...'
      Hint = 'New set...'
      ImageIndex = 10
      ShortCut = 16462
      OnExecute = actNewSetExecute
    end
    object actDeleteSet: TAction
      Category = 'Sets'
      Caption = 'Delete Set...'
      Hint = 'Delete set...'
      ImageIndex = 11
      ShortCut = 16430
      OnExecute = actDeleteSetExecute
    end
    object actSaveSets: TAction
      Category = 'Sets'
      Caption = 'Save Sets'
      Hint = 'Save sets'
      ImageIndex = 31
      ShortCut = 16467
      OnExecute = actSaveSetsExecute
    end
    object actApplySet: TAction
      Category = 'Sets'
      Caption = 'Apply to Project'
      Hint = 'Apply to project'
      ImageIndex = 36
      ShortCut = 16473
      OnExecute = actApplySetExecute
    end
    object actHelp: TAction
      Category = 'Sets'
      Caption = 'Help'
      Hint = 'Help'
      ImageIndex = 0
      ShortCut = 112
      OnExecute = actHelpExecute
    end
    object actRenameSet: TAction
      Category = 'Sets'
      Caption = 'Rename Set...'
      Hint = 'Rename set...'
      ImageIndex = 38
      ShortCut = 16466
      OnExecute = actRenameSetExecute
    end
  end
  object pmuSets: TPopupMenu
    Images = dmSharedImages.Images
    Left = 80
    Top = 176
    object mitPopAdd: TMenuItem
      Action = actNewSet
    end
    object mitPopRename: TMenuItem
      Action = actRenameSet
    end
    object mitPopDelete: TMenuItem
      Action = actDeleteSet
    end
    object mitPopSave: TMenuItem
      Action = actSaveSets
    end
    object mitPopApply: TMenuItem
      Action = actApplySet
    end
    object mitPopSep: TMenuItem
      Caption = '-'
    end
    object mitPopHelp: TMenuItem
      Action = actHelp
    end
  end
end
