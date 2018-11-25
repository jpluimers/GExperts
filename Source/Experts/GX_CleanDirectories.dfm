object fmCleanDirectories: TfmCleanDirectories
  Left = 297
  Top = 185
  AutoScroll = False
  BorderIcons = [biSystemMenu]
  Caption = 'Clean Directories'
  ClientHeight = 512
  ClientWidth = 553
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 14
  object pnlButtons: TPanel
    Left = 0
    Top = 475
    Width = 553
    Height = 37
    Align = alBottom
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 3
    DesignSize = (
      553
      37)
    object chkReportErrors: TCheckBox
      Left = 8
      Top = 9
      Width = 161
      Height = 17
      Anchors = [akLeft, akBottom]
      Caption = 'Re&port errors'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object pnlButtonsRight: TPanel
      Left = 280
      Top = 0
      Width = 273
      Height = 37
      Align = alRight
      BevelOuter = bvNone
      FullRepaint = False
      TabOrder = 1
      object btnHelp: TButton
        Left = 190
        Top = 3
        Width = 75
        Height = 26
        Caption = '&Help'
        TabOrder = 2
        OnClick = btnHelpClick
      end
      object btnClean: TButton
        Left = 22
        Top = 3
        Width = 75
        Height = 26
        Caption = 'Clean'
        Default = True
        TabOrder = 0
        OnClick = btnCleanClick
      end
      object btnCancel: TButton
        Left = 106
        Top = 3
        Width = 75
        Height = 26
        Cancel = True
        Caption = 'Cancel'
        ModalResult = 2
        TabOrder = 1
      end
    end
  end
  object pnlDirs: TPanel
    Left = 0
    Top = 0
    Width = 553
    Height = 241
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 6
    FullRepaint = False
    TabOrder = 0
    object pnlDirectories: TPanel
      Left = 6
      Top = 6
      Width = 541
      Height = 229
      Align = alClient
      BevelOuter = bvNone
      FullRepaint = False
      TabOrder = 0
      object gbxDirectories: TGroupBox
        Left = 0
        Top = 0
        Width = 541
        Height = 229
        Align = alClient
        Caption = 'Clean Dir&ectories'
        TabOrder = 0
        object pnlDirList: TPanel
          Left = 2
          Top = 16
          Width = 453
          Height = 190
          Align = alClient
          BevelOuter = bvNone
          BorderWidth = 6
          FullRepaint = False
          TabOrder = 0
          object clbDirs: TCheckListBox
            Left = 6
            Top = 6
            Width = 441
            Height = 178
            Align = alClient
            ItemHeight = 14
            PopupMenu = pmuDirs
            TabOrder = 0
            OnClick = clbDirsClick
            OnKeyPress = clbDirsKeyPress
          end
        end
        object pnlDirButtons: TPanel
          Left = 455
          Top = 16
          Width = 84
          Height = 190
          Align = alRight
          BevelOuter = bvNone
          FullRepaint = False
          TabOrder = 1
          DesignSize = (
            84
            190)
          object btnAdd: TButton
            Left = 1
            Top = 6
            Width = 75
            Height = 26
            Anchors = [akTop, akRight]
            Caption = '&Add'
            TabOrder = 0
            OnClick = btnAddClick
          end
          object btnRemove: TButton
            Left = 1
            Top = 39
            Width = 75
            Height = 26
            Anchors = [akTop, akRight]
            Caption = '&Remove'
            Enabled = False
            TabOrder = 1
            OnClick = btnRemoveClick
          end
        end
        object pnlDirMessage: TPanel
          Left = 2
          Top = 206
          Width = 537
          Height = 21
          Align = alBottom
          BevelOuter = bvNone
          BorderWidth = 1
          FullRepaint = False
          TabOrder = 2
          object lblDirMessage: TLabel
            Left = 1
            Top = 1
            Width = 535
            Height = 19
            Align = alClient
            Caption = 
              '   All listed directories are cleaned.  Check a directory to cle' +
              'an it recursively.'
          end
        end
      end
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 257
    Width = 553
    Height = 218
    Align = alBottom
    BevelOuter = bvNone
    BorderWidth = 6
    FullRepaint = False
    TabOrder = 2
    object gbxExtensions: TGroupBox
      Left = 6
      Top = 6
      Width = 541
      Height = 206
      Align = alClient
      Caption = 'Remove Files with E&xtensions'
      TabOrder = 0
      object pnlExtButtons: TPanel
        Left = 455
        Top = 16
        Width = 84
        Height = 188
        Align = alRight
        BevelOuter = bvNone
        FullRepaint = False
        TabOrder = 1
        DesignSize = (
          84
          188)
        object btnAddExt: TButton
          Left = 1
          Top = 5
          Width = 75
          Height = 26
          Anchors = [akTop, akRight]
          Caption = 'A&dd'
          TabOrder = 0
          OnClick = btnAddExtClick
        end
        object btnRemoveExt: TButton
          Left = 1
          Top = 39
          Width = 75
          Height = 26
          Anchors = [akTop, akRight]
          Caption = 'Re&move'
          Enabled = False
          TabOrder = 1
          OnClick = btnRemoveExtClick
        end
      end
      object pnlExtensions: TPanel
        Left = 2
        Top = 16
        Width = 453
        Height = 188
        Align = alClient
        BevelOuter = bvNone
        BorderWidth = 6
        FullRepaint = False
        TabOrder = 0
        object clbExtensions: TCheckListBox
          Left = 6
          Top = 6
          Width = 441
          Height = 176
          Align = alClient
          Columns = 4
          ItemHeight = 14
          PopupMenu = pmuExts
          Sorted = True
          TabOrder = 0
          OnClick = clbExtensionsClick
          OnKeyPress = clbExtensionsKeyPress
        end
      end
    end
  end
  object pnlMessage: TPanel
    Left = 0
    Top = 241
    Width = 553
    Height = 16
    Align = alBottom
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 1
    DesignSize = (
      553
      16)
    object lCleaning: TLabel
      Left = 8
      Top = 1
      Width = 49
      Height = 14
      Caption = 'Cleaning:'
      Visible = False
    end
    object laStatus: TLabel
      Left = 60
      Top = 1
      Width = 484
      Height = 13
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 'Status'
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
