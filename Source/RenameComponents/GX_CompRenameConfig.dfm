object fmCompRenameConfig: TfmCompRenameConfig
  Left = 336
  Top = 315
  AutoScroll = False
  BorderIcons = [biSystemMenu]
  Caption = 'Rename Components Configuration'
  ClientHeight = 428
  ClientWidth = 497
  Color = clBtnFace
  Constraints.MinHeight = 285
  Constraints.MinWidth = 425
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 14
  object pnlFooter: TPanel
    Left = 0
    Top = 352
    Width = 497
    Height = 76
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      497
      76)
    object pnlButtonsRight: TPanel
      Left = 224
      Top = 0
      Width = 273
      Height = 76
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 2
      object btnOK: TButton
        Left = 18
        Top = 43
        Width = 75
        Height = 25
        Action = acOK
        Default = True
        TabOrder = 0
      end
      object btnClose: TButton
        Left = 105
        Top = 43
        Width = 75
        Height = 25
        Action = acCancel
        Cancel = True
        TabOrder = 1
      end
      object btnHelp: TButton
        Left = 190
        Top = 43
        Width = 75
        Height = 25
        Cancel = True
        Caption = '&Help'
        TabOrder = 2
        OnClick = btnHelpClick
      end
    end
    object chkAutoAdd: TCheckBox
      Left = 8
      Top = 21
      Width = 350
      Height = 17
      Anchors = [akLeft, akBottom]
      Caption = 'Auto-add rules for new components'
      TabOrder = 1
    end
    object chkShowDialog: TCheckBox
      Left = 8
      Top = 2
      Width = 350
      Height = 17
      Anchors = [akLeft, akBottom]
      Caption = 'Show rename dialog for new components'
      TabOrder = 0
    end
  end
  object pnlRules: TPanel
    Left = 0
    Top = 0
    Width = 497
    Height = 352
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 6
    TabOrder = 0
    object pnlNames: TGroupBox
      Left = 6
      Top = 6
      Width = 485
      Height = 340
      Align = alClient
      Caption = 'Rename &Rules'
      TabOrder = 0
      DesignSize = (
        485
        340)
      object btnAdd: TButton
        Left = 358
        Top = 16
        Width = 117
        Height = 26
        Action = acAdd
        Anchors = [akTop, akRight]
        TabOrder = 0
      end
      object btnDelete: TButton
        Left = 358
        Top = 50
        Width = 117
        Height = 26
        Action = acDelete
        Anchors = [akTop, akRight]
        TabOrder = 1
      end
      object btnDefaults: TButton
        Left = 358
        Top = 301
        Width = 117
        Height = 26
        Anchors = [akRight, akBottom]
        Caption = '&Defaults'
        TabOrder = 3
        OnClick = btnDefaultsClick
      end
      object btnOtherProperties: TButton
        Left = 358
        Top = 84
        Width = 117
        Height = 26
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
