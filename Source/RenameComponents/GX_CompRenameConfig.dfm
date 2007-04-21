object fmCompRenameConfig: TfmCompRenameConfig
  Left = 336
  Top = 315
  Width = 507
  Height = 425
  BorderIcons = [biSystemMenu]
  Caption = 'Rename Components Configuration'
  Color = clBtnFace
  Constraints.MinHeight = 270
  Constraints.MinWidth = 425
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object pnlFooter: TPanel
    Left = 0
    Top = 320
    Width = 499
    Height = 76
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      499
      76)
    object btnOK: TButton
      Left = 248
      Top = 45
      Width = 75
      Height = 25
      Action = acOK
      Anchors = [akRight, akBottom]
      Default = True
      TabOrder = 2
    end
    object btnClose: TButton
      Left = 332
      Top = 45
      Width = 75
      Height = 25
      Action = acCancel
      Anchors = [akRight, akBottom]
      Cancel = True
      TabOrder = 3
    end
    object btnHelp: TButton
      Left = 417
      Top = 45
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = '&Help'
      TabOrder = 4
      OnClick = btnHelpClick
    end
    object chkAutoAdd: TCheckBox
      Left = 8
      Top = 20
      Width = 236
      Height = 17
      Anchors = [akLeft, akBottom]
      Caption = 'Auto-add rules for new components'
      TabOrder = 1
    end
    object chkShowDialog: TCheckBox
      Left = 8
      Top = 2
      Width = 236
      Height = 17
      Anchors = [akLeft, akBottom]
      Caption = 'Show rename dialog for new components'
      TabOrder = 0
    end
  end
  object pnlRules: TPanel
    Left = 0
    Top = 0
    Width = 499
    Height = 320
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      499
      320)
    object pnlNames: TGroupBox
      Left = 8
      Top = 8
      Width = 483
      Height = 303
      Anchors = [akLeft, akTop, akRight, akBottom]
      Caption = 'Rename &Rules'
      TabOrder = 0
      DesignSize = (
        483
        303)
      object btnAdd: TButton
        Left = 373
        Top = 16
        Width = 100
        Height = 28
        Action = acAdd
        Anchors = [akTop, akRight]
        TabOrder = 0
      end
      object btnDelete: TButton
        Left = 373
        Top = 48
        Width = 100
        Height = 28
        Action = acDelete
        Anchors = [akTop, akRight]
        TabOrder = 1
      end
      object btnDefaults: TButton
        Left = 373
        Top = 269
        Width = 100
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = '&Defaults'
        TabOrder = 3
        OnClick = btnDefaultsClick
      end
      object btnOtherProperties: TButton
        Left = 373
        Top = 80
        Width = 100
        Height = 28
        Action = acOtherProperties
        Anchors = [akTop, akRight]
        TabOrder = 2
      end
    end
  end
  object ActionList: TActionList
    Images = dmSharedImages.Images
    OnUpdate = ActionListUpdate
    Left = 152
    Top = 80
    object acAdd: TAction
      Caption = '&Add'
      ImageIndex = 12
      ShortCut = 45
      OnExecute = acAddExecute
    end
    object acDelete: TAction
      Caption = '&Delete'
      ImageIndex = 13
      ShortCut = 16430
      OnExecute = acDeleteExecute
    end
    object acOtherProperties: TAction
      Caption = 'Other &Properties...'
      OnExecute = acOtherPropertiesExecute
    end
    object acFind: TAction
      Caption = '&Find...'
      ImageIndex = 14
      ShortCut = 16454
      OnExecute = acFindExecute
    end
    object acCancel: TAction
      Caption = 'Cancel'
      OnExecute = acCancelExecute
    end
    object acOK: TAction
      Caption = 'OK'
      OnExecute = acOKExecute
    end
    object acSortByClass: TAction
      Caption = 'By Class'
      OnExecute = acSortByClassExecute
    end
    object acSortByRule: TAction
      Caption = 'By Rename Rule'
      OnExecute = acSortByRuleExecute
    end
  end
  object pmGrid: TPopupMenu
    Images = dmSharedImages.Images
    Left = 108
    Top = 80
    object mnuAdd: TMenuItem
      Action = acAdd
    end
    object mnuDelete: TMenuItem
      Action = acDelete
    end
    object mnuOtherProperties: TMenuItem
      Action = acOtherProperties
    end
    object mnuSep1: TMenuItem
      Caption = '-'
    end
    object mnuFind: TMenuItem
      Action = acFind
    end
    object mnuSep2: TMenuItem
      Caption = '-'
    end
    object mnuSort: TMenuItem
      Caption = 'Sort'
      object mnuSortByClass: TMenuItem
        Action = acSortByClass
      end
      object mnuSortByRule: TMenuItem
        Action = acSortByRule
      end
    end
  end
  object FindDialog: TFindDialog
    Options = [frDown, frDisableMatchCase, frDisableUpDown, frDisableWholeWord]
    OnFind = FindDialogFind
    Left = 64
    Top = 80
  end
end
