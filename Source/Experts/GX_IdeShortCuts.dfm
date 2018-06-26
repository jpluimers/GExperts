object fmIdeShortCuts: TfmIdeShortCuts
  Left = 296
  Top = 249
  AutoScroll = False
  BorderIcons = [biSystemMenu]
  Caption = 'IDE Menu Shortcuts'
  ClientHeight = 218
  ClientWidth = 650
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object pnlControls: TPanel
    Left = 0
    Top = 67
    Width = 650
    Height = 113
    Align = alBottom
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 0
    DesignSize = (
      650
      113)
    object lblMenuStruc: TLabel
      Left = 24
      Top = 35
      Width = 84
      Height = 14
      Alignment = taRightJustify
      Caption = 'Menu &structure'
      FocusControl = edtMenuStructure
    end
    object lblMenuItemName: TLabel
      Left = 16
      Top = 6
      Width = 92
      Height = 14
      Alignment = taRightJustify
      Caption = 'Menu item &name'
      FocusControl = edtMenuItemName
    end
    object lblShortcut: TLabel
      Left = 60
      Top = 87
      Width = 48
      Height = 14
      Alignment = taRightJustify
      Caption = 'Sh&ortcut'
      FocusControl = hkShortCut
    end
    object edtMenuStructure: TEdit
      Left = 116
      Top = 31
      Width = 524
      Height = 22
      TabStop = False
      Anchors = [akLeft, akTop, akRight]
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 1
    end
    object edtMenuItemName: TEdit
      Left = 116
      Top = 3
      Width = 524
      Height = 22
      TabStop = False
      Anchors = [akLeft, akTop, akRight]
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 0
      Text = ' '
    end
    object chkUseShortcut: TCheckBox
      Left = 116
      Top = 62
      Width = 357
      Height = 13
      Caption = '&Apply a custom shortcut to this menu item'
      Enabled = False
      TabOrder = 2
      OnClick = chkUseShortcutClick
    end
    object hkShortCut: THotKey
      Left = 116
      Top = 84
      Width = 269
      Height = 22
      Enabled = False
      HotKey = 0
      InvalidKeys = []
      Modifiers = []
      TabOrder = 3
      OnChange = ShortCutChange
    end
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 180
    Width = 650
    Height = 38
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object pnlButtonsRight: TPanel
      Left = 364
      Top = 0
      Width = 286
      Height = 38
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object btOK: TButton
        Left = 9
        Top = 3
        Width = 82
        Height = 27
        Caption = 'OK'
        Default = True
        ModalResult = 1
        TabOrder = 0
        OnClick = btOKClick
      end
      object btCancel: TButton
        Left = 102
        Top = 3
        Width = 82
        Height = 27
        Cancel = True
        Caption = 'Cancel'
        ModalResult = 2
        TabOrder = 1
      end
      object btnHelp: TButton
        Left = 194
        Top = 3
        Width = 82
        Height = 27
        Caption = '&Help'
        TabOrder = 2
        OnClick = btnHelpClick
      end
    end
  end
  object MainMenu: TMainMenu
    Left = 8
    Top = 8
  end
end
