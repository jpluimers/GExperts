object fmMacroLibraryNamePrompt: TfmMacroLibraryNamePrompt
  Left = 320
  Top = 249
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Keyboard Macro Library'
  ClientHeight = 386
  ClientWidth = 425
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object lblMacroName: TLabel
    Left = 8
    Top = 8
    Width = 59
    Height = 13
    Caption = 'Macro &Name'
    FocusControl = edtMacroName
  end
  object lblMacroDesc: TLabel
    Left = 8
    Top = 56
    Width = 85
    Height = 13
    Caption = 'Macro &Description'
    FocusControl = mmoMacroDescription
  end
  object lblMacroKeystrokes: TLabel
    Left = 8
    Top = 144
    Width = 85
    Height = 13
    Caption = 'Macro Keystrokes'
  end
  object edtMacroName: TEdit
    Left = 8
    Top = 24
    Width = 409
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object chkDoNotShowAgain: TCheckBox
    Left = 8
    Top = 355
    Width = 249
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Do not show this dialog again'
    TabOrder = 7
  end
  object btnOK: TButton
    Left = 264
    Top = 352
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 8
  end
  object btnCancel: TButton
    Left = 344
    Top = 352
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 9
  end
  object mmoMacroDescription: TMemo
    Left = 8
    Top = 72
    Width = 409
    Height = 57
    Anchors = [akLeft, akTop, akRight]
    BevelEdges = [beLeft, beTop, beBottom]
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object pnlMacro: TPanel
    Left = 8
    Top = 160
    Width = 329
    Height = 185
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Macro content goes here'
    Color = clWindow
    TabOrder = 2
  end
  object btnDelete: TButton
    Left = 344
    Top = 224
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Delete'
    TabOrder = 5
    OnClick = actDeleteExecute
  end
  object btnEdit: TButton
    Left = 344
    Top = 160
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Edit'
    TabOrder = 3
    OnClick = actEditExecute
  end
  object btnInsert: TButton
    Left = 344
    Top = 192
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Insert'
    TabOrder = 4
    OnClick = actInsertExecute
  end
  object btnAppend: TButton
    Left = 344
    Top = 256
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Append'
    TabOrder = 6
    OnClick = actAppendExecute
  end
  object TheActionList: TActionList
    State = asSuspended
    Left = 200
    Top = 192
    object actEdit: TAction
      Caption = 'Edit'
      ShortCut = 113
      OnExecute = actEditExecute
    end
    object actInsert: TAction
      Caption = 'Insert'
      ShortCut = 16457
      OnExecute = actInsertExecute
    end
    object actDelete: TAction
      Caption = 'Delete'
      ShortCut = 16452
      OnExecute = actDeleteExecute
    end
    object actAppend: TAction
      Caption = 'Append'
      ShortCut = 16458
      OnExecute = actAppendExecute
    end
  end
  object pmKeystrokes: TPopupMenu
    Left = 88
    Top = 200
    object miEdit: TMenuItem
      Action = actEdit
    end
    object miInsert: TMenuItem
      Action = actInsert
    end
    object miDelete: TMenuItem
      Action = actDelete
    end
    object miAppend: TMenuItem
      Action = actAppend
      ShortCut = 24649
    end
  end
end
