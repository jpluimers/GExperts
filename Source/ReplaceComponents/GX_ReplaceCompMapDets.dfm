object fmReplaceCompMapDets: TfmReplaceCompMapDets
  Left = 260
  Top = 222
  AutoScroll = False
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Replace Components - Mapping Details'
  ClientHeight = 338
  ClientWidth = 394
  Color = clBtnFace
  Constraints.MinHeight = 360
  Constraints.MinWidth = 363
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object pnlDetails: TPanel
    Left = 0
    Top = 42
    Width = 394
    Height = 262
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 6
    TabOrder = 1
    object gbxMappingDetails: TGroupBox
      Left = 6
      Top = 6
      Width = 382
      Height = 250
      Align = alClient
      Caption = 'Property Mapping Details'
      TabOrder = 0
      DesignSize = (
        382
        250)
      object pnlEdits: TPanel
        Left = 7
        Top = 18
        Width = 356
        Height = 217
        Anchors = [akLeft, akTop, akRight]
        BevelOuter = bvNone
        TabOrder = 0
        DesignSize = (
          356
          217)
        object lblGroup: TLabel
          Left = 7
          Top = 0
          Width = 33
          Height = 14
          Caption = 'Group'
        end
        object lblSourceComp: TLabel
          Left = 7
          Top = 48
          Width = 105
          Height = 14
          Caption = 'Source component'
        end
        object lblDestComp: TLabel
          Left = 7
          Top = 96
          Width = 128
          Height = 14
          Caption = 'Destination component'
        end
        object lblSourceProp: TLabel
          Left = 167
          Top = 48
          Width = 89
          Height = 14
          Caption = 'Source property'
        end
        object lblDestProp: TLabel
          Left = 167
          Top = 96
          Width = 112
          Height = 14
          Caption = 'Destination property'
        end
        object cbxGroupName: TComboBox
          Left = 7
          Top = 16
          Width = 145
          Height = 22
          DropDownCount = 16
          ItemHeight = 14
          Sorted = True
          TabOrder = 0
        end
        object cbxSourceClassName: TComboBox
          Left = 7
          Top = 64
          Width = 145
          Height = 22
          DropDownCount = 16
          ItemHeight = 14
          Sorted = True
          TabOrder = 1
          OnChange = cbxSourceClassNameChange
        end
        object cbxDestClassName: TComboBox
          Left = 7
          Top = 112
          Width = 145
          Height = 22
          DropDownCount = 16
          ItemHeight = 14
          Sorted = True
          TabOrder = 3
          OnChange = cbxDestClassNameChange
        end
        object cbxSourcePropName: TComboBox
          Left = 167
          Top = 64
          Width = 186
          Height = 22
          Anchors = [akLeft, akTop, akRight]
          DropDownCount = 16
          ItemHeight = 14
          Sorted = True
          TabOrder = 2
        end
        object cbxDestPropName: TComboBox
          Left = 167
          Top = 112
          Width = 186
          Height = 22
          Anchors = [akLeft, akTop, akRight]
          DropDownCount = 16
          ItemHeight = 14
          Sorted = True
          TabOrder = 4
        end
        object chkBiDirEnabled: TCheckBox
          Left = 167
          Top = 144
          Width = 150
          Height = 17
          Caption = 'Bi-directional'
          Checked = True
          State = cbChecked
          TabOrder = 6
        end
        object chkDisabled: TCheckBox
          Left = 7
          Top = 144
          Width = 150
          Height = 17
          Caption = 'Disabled'
          Checked = True
          State = cbChecked
          TabOrder = 5
        end
        object chkUseConstValue: TCheckBox
          Left = 7
          Top = 168
          Width = 150
          Height = 17
          Caption = 'Assign constant value'
          TabOrder = 7
          OnClick = chkUseConstValueClick
        end
        object edtConstValue: TEdit
          Left = 7
          Top = 192
          Width = 145
          Height = 22
          TabOrder = 9
        end
        object chkLogValues: TCheckBox
          Left = 167
          Top = 168
          Width = 150
          Height = 17
          Caption = 'Log values'
          TabOrder = 8
          OnClick = chkLogValuesClick
        end
        object chkLogNonDef: TCheckBox
          Left = 183
          Top = 194
          Width = 150
          Height = 17
          Caption = 'Only if not default'
          TabOrder = 10
        end
      end
    end
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 304
    Width = 394
    Height = 34
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object pnlButtonsRight: TPanel
      Left = 209
      Top = 0
      Width = 185
      Height = 34
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      DesignSize = (
        185
        34)
      object btnApply: TButton
        Left = 15
        Top = 2
        Width = 75
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = '&Apply'
        Default = True
        TabOrder = 0
        OnClick = btnApplyClick
      end
      object btnClose: TButton
        Left = 103
        Top = 2
        Width = 75
        Height = 25
        Anchors = [akRight, akBottom]
        Cancel = True
        Caption = '&Close'
        TabOrder = 1
        OnClick = btnCloseClick
      end
    end
  end
  object pnlHeader: TPanel
    Left = 0
    Top = 0
    Width = 394
    Height = 42
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object bvlPos: TBevel
      Left = 178
      Top = 10
      Width = 112
      Height = 25
    end
    object lblPos: TLabel
      Left = 181
      Top = 16
      Width = 46
      Height = 14
      Alignment = taRightJustify
      Caption = 'Position:'
    end
    object lblPosition: TLabel
      Left = 229
      Top = 16
      Width = 47
      Height = 14
      Caption = '999/999'
    end
    object pnlToolbar: TPanel
      Left = 15
      Top = 12
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
