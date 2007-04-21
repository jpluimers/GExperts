object fmCleanDirectories: TfmCleanDirectories
  Left = 303
  Top = 168
  Width = 490
  Height = 430
  ActiveControl = btnClean
  BorderIcons = [biSystemMenu]
  Caption = 'Clean Directories'
  Color = clBtnFace
  Constraints.MinHeight = 400
  Constraints.MinWidth = 400
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object pnlBottom: TPanel
    Left = 0
    Top = 160
    Width = 482
    Height = 241
    Align = alBottom
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 1
    DesignSize = (
      482
      241)
    object laStatus: TLabel
      Left = 59
      Top = 1
      Width = 417
      Height = 13
      Anchors = [akLeft, akRight, akBottom]
      AutoSize = False
      Caption = 'Status'
    end
    object lCleaning: TLabel
      Left = 8
      Top = 1
      Width = 44
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'Cleaning:'
      Visible = False
    end
    object chkReportErrors: TCheckBox
      Left = 8
      Top = 215
      Width = 161
      Height = 17
      Anchors = [akLeft, akBottom]
      Caption = 'Re&port errors'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object gbxExtensions: TGroupBox
      Left = 7
      Top = 18
      Width = 468
      Height = 185
      Anchors = [akLeft, akRight, akBottom]
      Caption = 'Remove Files with E&xtensions'
      TabOrder = 0
      DesignSize = (
        468
        185)
      object clbExtensions: TCheckListBox
        Left = 8
        Top = 16
        Width = 367
        Height = 161
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = 4
        ItemHeight = 13
        PopupMenu = pmuExts
        Sorted = True
        TabOrder = 0
        OnClick = clbExtensionsClick
        OnKeyPress = clbExtensionsKeyPress
      end
      object btnAddExt: TButton
        Left = 383
        Top = 16
        Width = 75
        Height = 26
        Anchors = [akTop, akRight]
        Caption = 'A&dd'
        TabOrder = 1
        OnClick = btnAddExtClick
      end
      object btnRemoveExt: TButton
        Left = 383
        Top = 50
        Width = 75
        Height = 26
        Anchors = [akTop, akRight]
        Caption = 'Re&move'
        Enabled = False
        TabOrder = 2
        OnClick = btnRemoveExtClick
      end
    end
    object btnHelp: TButton
      Left = 399
      Top = 209
      Width = 75
      Height = 26
      Anchors = [akRight, akBottom]
      Caption = '&Help'
      TabOrder = 4
      OnClick = btnHelpClick
    end
    object btnCancel: TButton
      Left = 318
      Top = 209
      Width = 75
      Height = 26
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 3
    end
    object btnClean: TButton
      Left = 237
      Top = 209
      Width = 75
      Height = 26
      Anchors = [akRight, akBottom]
      Caption = 'Clean'
      Default = True
      TabOrder = 2
      OnClick = btnCleanClick
    end
  end
  object pnlDirectories: TPanel
    Left = 0
    Top = 0
    Width = 482
    Height = 160
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      482
      160)
    object gbxDirectories: TGroupBox
      Left = 7
      Top = 5
      Width = 468
      Height = 151
      Anchors = [akLeft, akTop, akRight, akBottom]
      Caption = 'Clean Dir&ectories'
      TabOrder = 0
      DesignSize = (
        468
        151)
      object laRecursingNote: TLabel
        Left = 8
        Top = 131
        Width = 345
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = 
          'All listed directories are cleaned.  Check a directory to clean ' +
          'it recursively.'
      end
      object btnAdd: TButton
        Left = 383
        Top = 16
        Width = 75
        Height = 26
        Anchors = [akTop, akRight]
        Caption = '&Add'
        TabOrder = 1
        OnClick = btnAddClick
      end
      object btnRemove: TButton
        Left = 383
        Top = 49
        Width = 75
        Height = 26
        Anchors = [akTop, akRight]
        Caption = '&Remove'
        Enabled = False
        TabOrder = 2
        OnClick = btnRemoveClick
      end
      object clbDirs: TCheckListBox
        Left = 8
        Top = 16
        Width = 368
        Height = 111
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 13
        PopupMenu = pmuDirs
        TabOrder = 0
        OnClick = clbDirsClick
        OnKeyPress = clbDirsKeyPress
      end
    end
  end
  object ActionList: TActionList
    Left = 24
    Top = 32
    object actDirsCheckAll: TAction
      Caption = '&Check All'
      OnExecute = CheckActionExecute
    end
    object actDirsUncheckAll: TAction
      Caption = '&Uncheck All'
      OnExecute = CheckActionExecute
    end
    object actDirsInvert: TAction
      Caption = '&Invert Checked'
      OnExecute = CheckActionExecute
    end
    object actExtsCheckAll: TAction
      Caption = '&Check All'
      OnExecute = CheckActionExecute
    end
    object actExtsUncheckAll: TAction
      Caption = '&Uncheck All'
      OnExecute = CheckActionExecute
    end
    object actExtsInvert: TAction
      Caption = '&Invert Checked'
      OnExecute = CheckActionExecute
    end
  end
  object pmuDirs: TPopupMenu
    Left = 24
    Top = 80
    object mitDirsCheckAll: TMenuItem
      Action = actDirsCheckAll
    end
    object mitDirsUncheckAll: TMenuItem
      Action = actDirsUncheckAll
    end
    object mitDirsInvertChecked: TMenuItem
      Action = actDirsInvert
    end
  end
  object pmuExts: TPopupMenu
    Left = 24
    Top = 208
    object mitExtsCheckAll: TMenuItem
      Action = actExtsCheckAll
    end
    object mitExtsUncheckAll: TMenuItem
      Action = actExtsUncheckAll
    end
    object mitExtsInvertChecked: TMenuItem
      Action = actExtsInvert
    end
  end
end
