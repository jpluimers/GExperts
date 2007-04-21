object fmIdeShortCuts: TfmIdeShortCuts
  Left = 296
  Top = 249
  Width = 574
  Height = 237
  BorderIcons = [biSystemMenu]
  Caption = 'IDE Menu Shortcuts'
  Color = clBtnFace
  Constraints.MinHeight = 200
  Constraints.MinWidth = 500
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlControls: TPanel
    Left = 0
    Top = 62
    Width = 566
    Height = 146
    Align = alBottom
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 0
    DesignSize = (
      566
      146)
    object lblMenuStruc: TLabel
      Left = 21
      Top = 35
      Width = 71
      Height = 13
      Alignment = taRightJustify
      Caption = 'Menu &structure'
      FocusControl = edtMenuStructure
    end
    object lblMenuItemName: TLabel
      Left = 14
      Top = 6
      Width = 78
      Height = 13
      Alignment = taRightJustify
      Caption = 'Menu item &name'
      FocusControl = edtMenuItemName
    end
    object lblShortcut: TLabel
      Left = 52
      Top = 87
      Width = 40
      Height = 13
      Alignment = taRightJustify
      Caption = 'Sh&ortcut'
    end
    object edtMenuStructure: TEdit
      Left = 100
      Top = 31
      Width = 456
      Height = 21
      TabStop = False
      Anchors = [akLeft, akTop, akRight]
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 1
    end
    object edtMenuItemName: TEdit
      Left = 100
      Top = 3
      Width = 456
      Height = 21
      TabStop = False
      Anchors = [akLeft, akTop, akRight]
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 0
      Text = ' '
    end
    object chkUseShortcut: TCheckBox
      Left = 100
      Top = 62
      Width = 285
      Height = 13
      Caption = '&Apply a custom shortcut to this menu item'
      Enabled = False
      TabOrder = 2
      OnClick = chkUseShortcutClick
    end
    object btOK: TButton
      Left = 289
      Top = 110
      Width = 82
      Height = 27
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 3
      OnClick = btOKClick
    end
    object btCancel: TButton
      Left = 382
      Top = 110
      Width = 82
      Height = 27
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 4
    end
    object btnHelp: TButton
      Left = 474
      Top = 110
      Width = 82
      Height = 27
      Anchors = [akRight, akBottom]
      Caption = '&Help'
      TabOrder = 5
      OnClick = btnHelpClick
    end
  end
  object MainMenu: TMainMenu
    Left = 8
    Top = 8
  end
end
