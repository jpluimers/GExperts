inherited fmProjOptionSets: TfmProjOptionSets
  Left = 399
  Top = 185
  Caption = 'Project Option Sets'
  ClientHeight = 285
  ClientWidth = 190
  Position = poScreenCenter
  OnHide = FormHide
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object pcSettings: TPageControl
    Left = 0
    Top = 43
    Width = 190
    Height = 242
    ActivePage = tabSets
    Align = alClient
    TabIndex = 0
    TabOrder = 0
    object tabSets: TTabSheet
      Caption = '&Sets'
      ImageIndex = 2
      object pnlSets: TPanel
        Left = 0
        Top = 0
        Width = 182
        Height = 213
        Align = alClient
        BevelOuter = bvNone
        BorderWidth = 4
        FullRepaint = False
        TabOrder = 0
        object lstSets: TListBox
          Left = 4
          Top = 4
          Width = 174
          Height = 205
          Align = alClient
          ItemHeight = 14
          PopupMenu = pmuSets
          TabOrder = 0
          OnClick = lstSetsClick
        end
      end
    end
    object tabProject: TTabSheet
      Caption = '&Project'
      object lblProjectSettings: TLabel
        Left = 56
        Top = 96
        Width = 4
        Height = 14
        Visible = False
      end
      object pnlCheckListHost: TPanel
        Left = 0
        Top = 0
        Width = 252
        Height = 393
        Align = alClient
        BevelOuter = bvNone
        BorderWidth = 4
        Caption = 'TCheckListBox Created At Runtime'
        FullRepaint = False
        ParentShowHint = False
        ShowHint = False
        TabOrder = 0
        object pnlFilterComboHost: TPanel
          Left = 4
          Top = 362
          Width = 244
          Height = 27
          Align = alBottom
          BevelOuter = bvNone
          FullRepaint = False
          TabOrder = 0
          OnResize = pnlFilterComboHostResize
          object cbFilter: TComboBox
            Left = 0
            Top = 3
            Width = 245
            Height = 22
            Style = csDropDownList
            DropDownCount = 16
            ItemHeight = 14
            TabOrder = 0
            OnChange = cbFilterChange
          end
        end
      end
    end
    object tabEnvironment: TTabSheet
      Caption = '&Environment'
      ImageIndex = 1
      ParentShowHint = False
      ShowHint = False
      object pnlEnvironment: TPanel
        Left = 0
        Top = 0
        Width = 252
        Height = 393
        Align = alClient
        BevelOuter = bvNone
        BorderWidth = 4
        Caption = 'TCheckListBox Created At Runtime'
        FullRepaint = False
        ParentShowHint = False
        ShowHint = False
        TabOrder = 0
      end
    end
  end
  object pnlCurrentSet: TPanel
    Left = 0
    Top = 22
    Width = 190
    Height = 21
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
