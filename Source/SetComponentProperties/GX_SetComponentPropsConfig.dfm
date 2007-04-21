object fmSetComponentPropsConfig: TfmSetComponentPropsConfig
  Left = 349
  Top = 289
  Width = 427
  Height = 521
  BorderIcons = [biSystemMenu]
  Caption = 'Set Component Properties Configuration'
  Color = clBtnFace
  Constraints.MinHeight = 320
  Constraints.MinWidth = 400
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    419
    492)
  PixelsPerInch = 96
  TextHeight = 13
  object lblGrid: TLabel
    Left = 8
    Top = 8
    Width = 202
    Height = 13
    Caption = 'Processed Properties Before Each Compile'
  end
  object lblPropertyTpes: TLabel
    Left = 295
    Top = 8
    Width = 107
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Shown Property Types'
  end
  object lbxPropertyTypes: TListBox
    Left = 294
    Top = 24
    Width = 117
    Height = 232
    Anchors = [akTop, akRight, akBottom]
    ItemHeight = 13
    MultiSelect = True
    PopupMenu = pnuPropertyTypes
    Sorted = True
    TabOrder = 1
  end
  object btnOK: TButton
    Left = 162
    Top = 458
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 4
  end
  object btnCancel: TButton
    Left = 249
    Top = 458
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object gbxPropertyToSet: TGroupBox
    Left = 8
    Top = 262
    Width = 403
    Height = 100
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Property to Set'
    TabOrder = 2
    DesignSize = (
      403
      100)
    object lblComponent: TLabel
      Left = 8
      Top = 24
      Width = 54
      Height = 13
      Caption = 'Component'
    end
    object lblProperty: TLabel
      Left = 8
      Top = 48
      Width = 39
      Height = 13
      Caption = 'Property'
    end
    object lblValue: TLabel
      Left = 8
      Top = 72
      Width = 27
      Height = 13
      Caption = 'Value'
    end
    object cbxComponents: TComboBox
      Left = 80
      Top = 20
      Width = 203
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      DropDownCount = 18
      ItemHeight = 13
      Sorted = True
      TabOrder = 0
    end
    object cbxProperty: TComboBox
      Left = 80
      Top = 44
      Width = 203
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      DropDownCount = 18
      ItemHeight = 13
      Sorted = True
      TabOrder = 1
      OnEnter = cbxPropertyEnter
    end
    object edtValue: TEdit
      Left = 80
      Top = 68
      Width = 203
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
    end
    object btnAddRule: TButton
      Left = 290
      Top = 20
      Width = 105
      Height = 25
      Action = actnAddRule
      Anchors = [akTop, akRight]
      TabOrder = 3
    end
    object btnDeleteRule: TButton
      Left = 290
      Top = 64
      Width = 105
      Height = 25
      Action = actnDeleteRule
      Anchors = [akTop, akRight]
      TabOrder = 4
    end
  end
  object btnHelp: TButton
    Left = 336
    Top = 458
    Width = 75
    Height = 25
    Action = actnHelp
    Anchors = [akRight, akBottom]
    Cancel = True
    TabOrder = 6
  end
  object gbxOptions: TGroupBox
    Left = 8
    Top = 369
    Width = 403
    Height = 81
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Options'
    TabOrder = 3
    object chkSimulate: TCheckBox
      Left = 12
      Top = 38
      Width = 361
      Height = 17
      Caption = 'Only simulate and log property changes'
      TabOrder = 1
      OnClick = chkSimulateClick
    end
    object chkVerbose: TCheckBox
      Left = 12
      Top = 57
      Width = 361
      Height = 17
      Caption = 'Verbose status display and logging'
      TabOrder = 2
      OnClick = chkVerboseClick
    end
    object chkOnlyOpenFiles: TCheckBox
      Left = 12
      Top = 18
      Width = 361
      Height = 17
      Caption = 'Only process currently open files'
      TabOrder = 0
      OnClick = chkOnlyOpenFilesClick
    end
  end
  object lvProperties: TListView
    Left = 8
    Top = 24
    Width = 283
    Height = 232
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'Component'
        Width = 91
      end
      item
        Caption = 'Property'
        Width = 75
      end
      item
        Caption = 'Value'
        Width = 87
      end>
    GridLines = True
    ReadOnly = True
    RowSelect = True
    PopupMenu = pmuProperties
    TabOrder = 0
    ViewStyle = vsReport
    OnChange = lvPropertiesChange
  end
  object pnuPropertyTypes: TPopupMenu
    Left = 204
    Top = 72
    object itmAdd: TMenuItem
      Action = actnAddPropertyType
    end
    object itmModify: TMenuItem
      Action = actnModifyPropertyType
    end
    object itmDelete: TMenuItem
      Action = actnDeletePropertyType
    end
    object itmN1: TMenuItem
      Caption = '-'
    end
    object itmDefaultTypes: TMenuItem
      Action = actnDefaultPropertyTypes
    end
  end
  object Actions: TActionList
    OnUpdate = ActionsUpdate
    Left = 136
    Top = 72
    object actnAddRule: TAction
      Category = 'Rules'
      Caption = '&Add / Modify'
      OnExecute = actnAddRuleExecute
    end
    object actnDeleteRule: TAction
      Category = 'Rules'
      Caption = '&Delete'
      OnExecute = actnDeleteRuleExecute
    end
    object actnAddPropertyType: TAction
      Category = 'Property Types'
      Caption = '&Add...'
      OnExecute = actnAddPropertyTypeExecute
    end
    object actnDeletePropertyType: TAction
      Category = 'Property Types'
      Caption = '&Delete'
      OnExecute = actnDeletePropertyTypeExecute
    end
    object actnModifyPropertyType: TAction
      Category = 'Property Types'
      Caption = '&Modify...'
      OnExecute = actnModifyPropertyTypeExecute
    end
    object actnDefaultPropertyTypes: TAction
      Category = 'Property Types'
      Caption = 'Default &Types'
      OnExecute = actnDefaultPropertyTypesExecute
    end
    object actnDefaultRules: TAction
      Category = 'Grid'
      Caption = 'Default &Rules'
      OnExecute = actnDefaultRulesExecute
    end
    object actnClearRules: TAction
      Category = 'Grid'
      Caption = '&Clear Rules'
      OnExecute = actnClearRulesExecute
    end
    object actnHelp: TAction
      Caption = '&Help'
      OnExecute = actnHelpExecute
    end
  end
  object pmuProperties: TPopupMenu
    Left = 60
    Top = 72
    object itmDefaultRules: TMenuItem
      Action = actnDefaultRules
    end
    object itmClearRules: TMenuItem
      Action = actnClearRules
    end
  end
end
