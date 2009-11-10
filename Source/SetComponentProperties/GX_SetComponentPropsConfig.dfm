object fmSetComponentPropsConfig: TfmSetComponentPropsConfig
  Left = 374
  Top = 244
  AutoScroll = False
  BorderIcons = [biSystemMenu]
  Caption = 'Set Component Properties Configuration'
  ClientHeight = 521
  ClientWidth = 441
  Color = clBtnFace
  Constraints.MinHeight = 320
  Constraints.MinWidth = 400
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 14
  object pnlContent: TPanel
    Left = 0
    Top = 0
    Width = 441
    Height = 521
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object pnlButtons: TPanel
      Left = 0
      Top = 484
      Width = 441
      Height = 37
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 3
      object pnlButtonsRight: TPanel
        Left = 179
        Top = 0
        Width = 262
        Height = 37
        Align = alRight
        BevelOuter = bvNone
        TabOrder = 0
        object btnOK: TButton
          Left = 6
          Top = 4
          Width = 75
          Height = 25
          Caption = 'OK'
          Default = True
          ModalResult = 1
          TabOrder = 0
        end
        object btnCancel: TButton
          Left = 93
          Top = 4
          Width = 75
          Height = 25
          Cancel = True
          Caption = 'Cancel'
          ModalResult = 2
          TabOrder = 1
        end
        object btnHelp: TButton
          Left = 180
          Top = 4
          Width = 75
          Height = 25
          Action = actnHelp
          Cancel = True
          TabOrder = 2
        end
      end
    end
    object pnlOptions: TPanel
      Left = 0
      Top = 389
      Width = 441
      Height = 95
      Align = alBottom
      BevelOuter = bvNone
      BorderWidth = 6
      TabOrder = 2
      object gbxOptions: TGroupBox
        Left = 6
        Top = 6
        Width = 429
        Height = 83
        Align = alClient
        Caption = 'Options'
        TabOrder = 0
        object chkSimulate: TCheckBox
          Left = 12
          Top = 38
          Width = 340
          Height = 17
          Caption = 'Only simulate and log property changes'
          TabOrder = 1
          OnClick = chkSimulateClick
        end
        object chkVerbose: TCheckBox
          Left = 12
          Top = 57
          Width = 340
          Height = 17
          Caption = 'Verbose status display and logging'
          TabOrder = 2
          OnClick = chkVerboseClick
        end
        object chkOnlyOpenFiles: TCheckBox
          Left = 12
          Top = 18
          Width = 340
          Height = 17
          Caption = 'Only process currently open files'
          TabOrder = 0
          OnClick = chkOnlyOpenFilesClick
        end
      end
    end
    object pnlPropDetails: TPanel
      Left = 0
      Top = 277
      Width = 441
      Height = 112
      Align = alBottom
      BevelOuter = bvNone
      BorderWidth = 6
      TabOrder = 1
      object gbxPropertyToSet: TGroupBox
        Left = 6
        Top = 6
        Width = 429
        Height = 100
        Align = alClient
        Caption = 'Property to Set'
        TabOrder = 0
        DesignSize = (
          429
          100)
        object lblComponent: TLabel
          Left = 8
          Top = 24
          Width = 64
          Height = 14
          Caption = 'Component'
          FocusControl = cbxComponents
        end
        object lblProperty: TLabel
          Left = 8
          Top = 48
          Width = 47
          Height = 14
          Caption = 'Property'
          FocusControl = cbxProperty
        end
        object lblValue: TLabel
          Left = 8
          Top = 72
          Width = 30
          Height = 14
          Caption = 'Value'
          FocusControl = edtValue
        end
        object cbxComponents: TComboBox
          Left = 80
          Top = 20
          Width = 229
          Height = 22
          Anchors = [akLeft, akTop, akRight]
          DropDownCount = 18
          ItemHeight = 14
          Sorted = True
          TabOrder = 0
        end
        object cbxProperty: TComboBox
          Left = 80
          Top = 44
          Width = 229
          Height = 22
          Anchors = [akLeft, akTop, akRight]
          DropDownCount = 18
          ItemHeight = 14
          Sorted = True
          TabOrder = 1
          OnEnter = cbxPropertyEnter
        end
        object edtValue: TEdit
          Left = 80
          Top = 68
          Width = 229
          Height = 22
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 2
        end
        object btnAddRule: TButton
          Left = 316
          Top = 20
          Width = 105
          Height = 25
          Action = actnAddRule
          Anchors = [akTop, akRight]
          TabOrder = 3
        end
        object btnDeleteRule: TButton
          Left = 316
          Top = 64
          Width = 105
          Height = 25
          Action = actnDeleteRule
          Anchors = [akTop, akRight]
          TabOrder = 4
        end
      end
    end
    object pnlList: TPanel
      Left = 0
      Top = 0
      Width = 441
      Height = 277
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      object pnlPropTypes: TPanel
        Left = 303
        Top = 0
        Width = 138
        Height = 277
        Align = alRight
        BevelOuter = bvNone
        BorderWidth = 6
        TabOrder = 1
        object pnlPropTypesHeader: TPanel
          Left = 6
          Top = 6
          Width = 126
          Height = 20
          Align = alTop
          BevelOuter = bvNone
          Caption = 'Show Property Types'
          TabOrder = 0
        end
        object lbxPropertyTypes: TListBox
          Left = 6
          Top = 26
          Width = 126
          Height = 245
          Align = alClient
          ItemHeight = 14
          MultiSelect = True
          PopupMenu = pnuPropertyTypes
          Sorted = True
          TabOrder = 1
        end
      end
      object pnlPropList: TPanel
        Left = 0
        Top = 0
        Width = 303
        Height = 277
        Align = alClient
        BevelOuter = bvNone
        BorderWidth = 6
        TabOrder = 0
        object pnlPropListHeader: TPanel
          Left = 6
          Top = 6
          Width = 291
          Height = 20
          Align = alTop
          BevelOuter = bvNone
          Caption = 'Properties Processed Before Each Compile'
          TabOrder = 0
        end
        object lvProperties: TListView
          Left = 6
          Top = 26
          Width = 291
          Height = 245
          Align = alClient
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
          TabOrder = 1
          ViewStyle = vsReport
          OnChange = lvPropertiesChange
        end
      end
    end
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
