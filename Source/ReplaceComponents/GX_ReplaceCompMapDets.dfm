object fmReplaceCompMapDets: TfmReplaceCompMapDets
  Left = 260
  Top = 222
  Width = 363
  Height = 353
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Replace Components - Mapping Details'
  Color = clBtnFace
  Constraints.MaxHeight = 353
  Constraints.MinHeight = 353
  Constraints.MinWidth = 363
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
  DesignSize = (
    355
    324)
  PixelsPerInch = 96
  TextHeight = 13
  object bvlPos: TBevel
    Left = 178
    Top = 11
    Width = 107
    Height = 25
  end
  object bvlSep: TBevel
    Left = 14
    Top = 276
    Width = 330
    Height = 12
    Anchors = [akLeft, akRight, akBottom]
    Shape = bsTopLine
  end
  object lblPos: TLabel
    Left = 185
    Top = 17
    Width = 40
    Height = 13
    Caption = 'Position:'
  end
  object lblPosition: TLabel
    Left = 227
    Top = 17
    Width = 41
    Height = 13
    Caption = '999/999'
  end
  object btnClose: TButton
    Left = 270
    Top = 289
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '&Close'
    TabOrder = 3
    OnClick = btnCloseClick
  end
  object btnApply: TButton
    Left = 182
    Top = 289
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Apply'
    Default = True
    TabOrder = 2
    OnClick = btnApplyClick
  end
  object pnlEdits: TPanel
    Left = 7
    Top = 48
    Width = 338
    Height = 217
    Anchors = [akLeft, akTop, akRight]
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      338
      217)
    object lblGroup: TLabel
      Left = 7
      Top = 0
      Width = 29
      Height = 13
      Caption = 'Group'
    end
    object lblSourceComp: TLabel
      Left = 7
      Top = 48
      Width = 90
      Height = 13
      Caption = 'Source component'
    end
    object lblDestComp: TLabel
      Left = 7
      Top = 96
      Width = 109
      Height = 13
      Caption = 'Destination component'
    end
    object lblSourceProp: TLabel
      Left = 167
      Top = 48
      Width = 75
      Height = 13
      Caption = 'Source property'
    end
    object lblDestProp: TLabel
      Left = 167
      Top = 96
      Width = 94
      Height = 13
      Caption = 'Destination property'
    end
    object cbxGroupName: TComboBox
      Left = 7
      Top = 16
      Width = 145
      Height = 21
      DropDownCount = 16
      ItemHeight = 13
      Sorted = True
      TabOrder = 0
    end
    object cbxSourceClassName: TComboBox
      Left = 7
      Top = 64
      Width = 145
      Height = 21
      DropDownCount = 16
      ItemHeight = 13
      Sorted = True
      TabOrder = 1
      OnChange = cbxSourceClassNameChange
    end
    object cbxDestClassName: TComboBox
      Left = 7
      Top = 112
      Width = 145
      Height = 21
      DropDownCount = 16
      ItemHeight = 13
      Sorted = True
      TabOrder = 3
      OnChange = cbxDestClassNameChange
    end
    object cbxSourcePropName: TComboBox
      Left = 167
      Top = 64
      Width = 168
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      DropDownCount = 16
      ItemHeight = 13
      Sorted = True
      TabOrder = 2
    end
    object cbxDestPropName: TComboBox
      Left = 167
      Top = 112
      Width = 168
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      DropDownCount = 16
      ItemHeight = 13
      Sorted = True
      TabOrder = 4
    end
    object chkBiDirEnabled: TCheckBox
      Left = 167
      Top = 144
      Width = 97
      Height = 17
      Caption = 'Bi-directional'
      Checked = True
      State = cbChecked
      TabOrder = 6
    end
    object chkDisabled: TCheckBox
      Left = 7
      Top = 144
      Width = 97
      Height = 17
      Caption = 'Disabled'
      Checked = True
      State = cbChecked
      TabOrder = 5
    end
    object chkUseConstValue: TCheckBox
      Left = 7
      Top = 168
      Width = 145
      Height = 17
      Caption = 'Assign constant value'
      TabOrder = 7
      OnClick = chkUseConstValueClick
    end
    object edtConstValue: TEdit
      Left = 7
      Top = 192
      Width = 145
      Height = 21
      TabOrder = 8
    end
    object chkLogValues: TCheckBox
      Left = 167
      Top = 168
      Width = 97
      Height = 17
      Caption = 'Log values'
      TabOrder = 9
      OnClick = chkLogValuesClick
    end
    object chkLogNonDef: TCheckBox
      Left = 183
      Top = 194
      Width = 129
      Height = 17
      Caption = 'Only if not default'
      TabOrder = 10
    end
  end
  object pnlToolbar: TPanel
    Left = 15
    Top = 13
    Width = 147
    Height = 22
    AutoSize = True
    BevelOuter = bvNone
    TabOrder = 0
    object ToolBar: TToolBar
      Left = 0
      Top = 0
      Width = 147
      Height = 22
      AutoSize = True
      DisabledImages = dmSharedImages.DisabledImages
      EdgeInner = esNone
      EdgeOuter = esNone
      Flat = True
      Images = dmSharedImages.Images
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      object tbnFirst: TToolButton
        Left = 0
        Top = 0
        Action = actFirst
      end
      object tbnPrior: TToolButton
        Left = 23
        Top = 0
        Action = actPrior
      end
      object tbnNext: TToolButton
        Left = 46
        Top = 0
        Action = actNext
      end
      object tbnLast: TToolButton
        Left = 69
        Top = 0
        Action = actLast
      end
      object tbnSep: TToolButton
        Left = 92
        Top = 0
        Width = 8
        ImageIndex = 3
        Style = tbsSeparator
      end
      object tbnAdd: TToolButton
        Left = 100
        Top = 0
        Action = actAdd
      end
      object tbnDelete: TToolButton
        Left = 123
        Top = 0
        Action = actDelete
      end
    end
  end
  object Actions: TActionList
    Images = dmSharedImages.Images
    Left = 264
    Top = 72
    object actAdd: TAction
      Hint = 'Add'
      ImageIndex = 41
      OnExecute = btnAddClick
    end
    object actDelete: TAction
      Hint = 'Delete'
      ImageIndex = 42
      OnExecute = btnDeleteClick
    end
    object actFirst: TAction
      Hint = 'First'
      ImageIndex = 63
      OnExecute = btnFirstClick
    end
    object actPrior: TAction
      Hint = 'Prior'
      ImageIndex = 64
      OnExecute = btnPriorClick
    end
    object actNext: TAction
      Hint = 'Next'
      ImageIndex = 65
      OnExecute = btnNextClick
    end
    object actLast: TAction
      Hint = 'Last'
      ImageIndex = 66
      OnExecute = btnLastClick
    end
  end
end
