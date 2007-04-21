object fmToolbarConfig: TfmToolbarConfig
  Left = 253
  Top = 198
  Width = 580
  Height = 349
  BorderIcons = [biSystemMenu]
  Caption = 'Editor Toolbar Configuration'
  Color = clBtnFace
  Constraints.MinHeight = 340
  Constraints.MinWidth = 450
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object pnlButtons: TPanel
    Left = 0
    Top = 284
    Width = 572
    Height = 36
    Align = alBottom
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 1
    DesignSize = (
      572
      36)
    object btnHelp: TButton
      Left = 490
      Top = 4
      Width = 75
      Height = 26
      Anchors = [akRight, akBottom]
      Caption = '&Help'
      TabOrder = 2
      OnClick = btnHelpClick
    end
    object btnOK: TButton
      Left = 314
      Top = 4
      Width = 75
      Height = 26
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
      OnClick = btnOKClick
    end
    object btnCancel: TButton
      Left = 402
      Top = 4
      Width = 75
      Height = 26
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object pnlContent: TPanel
    Left = 0
    Top = 0
    Width = 572
    Height = 284
    Align = alClient
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 0
    DesignSize = (
      572
      284)
    object lblAvailable: TLabel
      Left = 138
      Top = 8
      Width = 82
      Height = 13
      Caption = 'Available &Buttons'
      FocusControl = lbAvailable
    end
    object lblCategories: TLabel
      Left = 9
      Top = 8
      Width = 50
      Height = 13
      Caption = '&Categories'
      FocusControl = lbCategories
    end
    object lblToolbar: TLabel
      Left = 375
      Top = 8
      Width = 105
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Editor &Toolbar Buttons'
      FocusControl = lbToolbar
    end
    object lbAvailable: TListBox
      Left = 135
      Top = 24
      Width = 206
      Height = 255
      Style = lbOwnerDrawFixed
      Anchors = [akLeft, akTop, akRight, akBottom]
      DragMode = dmAutomatic
      ItemHeight = 21
      TabOrder = 1
      OnDblClick = actAddButtonExecute
      OnDragDrop = lbAvailableDragDrop
      OnDragOver = lbAvailableDragOver
      OnDrawItem = ListboxDrawItem
    end
    object lbCategories: TListBox
      Left = 8
      Top = 24
      Width = 122
      Height = 255
      Anchors = [akLeft, akTop, akBottom]
      ItemHeight = 13
      TabOrder = 0
      OnClick = lbCategoriesClick
    end
    object lbToolbar: TListBox
      Left = 374
      Top = 24
      Width = 191
      Height = 228
      Style = lbOwnerDrawFixed
      Anchors = [akTop, akRight, akBottom]
      DragMode = dmAutomatic
      ItemHeight = 21
      TabOrder = 6
      OnDblClick = actRemoveButtonExecute
      OnDragDrop = lbToolbarDragDrop
      OnDragOver = lbToolbarDragOver
      OnDrawItem = ListboxDrawItem
      OnKeyDown = lbToolbarKeyDown
      OnStartDrag = lbToolbarStartDrag
    end
    object btnAdd: TBitBtn
      Left = 346
      Top = 47
      Width = 25
      Height = 25
      Action = actAddButton
      Anchors = [akTop, akRight]
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      Glyph.Data = {
        DE000000424DDE0000000000000076000000280000000D0000000D0000000100
        0400000000006800000000000000000000001000000010000000000000000000
        BF0000BF000000BFBF00BF000000BF00BF00BFBF0000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
        7000777777077777700077777700777770007777770607777000770000066077
        7000770666666607700077066666666070007706666666077000770000066077
        7000777777060777700077777700777770007777770777777000777777777777
        7000}
    end
    object btnAddSeparator: TBitBtn
      Left = 374
      Top = 253
      Width = 191
      Height = 26
      Action = actAddSeparator
      Anchors = [akRight, akBottom]
      Caption = 'Add &Separator'
      TabOrder = 7
    end
    object btnDown: TBitBtn
      Left = 346
      Top = 184
      Width = 25
      Height = 25
      Action = actMoveDown
      Anchors = [akTop, akRight]
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
      Glyph.Data = {
        DE000000424DDE0000000000000076000000280000000D0000000D0000000100
        04000000000068000000C40E0000C40E00001000000000000000000000000000
        BF0000BF000000BFBF00BF000000BF00BF00BFBF0000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
        7000777777077777700077777060777770007777066607777000777066666077
        7000770666666607700070000666000070007777066607777000777706660777
        7000777706660777700077770000077770007777777777777000777777777777
        7000}
    end
    object btnRemove: TBitBtn
      Left = 346
      Top = 80
      Width = 25
      Height = 25
      Action = actRemoveButton
      Anchors = [akTop, akRight]
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      Glyph.Data = {
        DE000000424DDE0000000000000076000000280000000D0000000D0000000100
        0400000000006800000000000000000000001000000010000000000000000000
        BF0000BF000000BFBF00BF000000BF00BF00BFBF0000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
        7000777777077777700077777007777770007777060777777000777066000007
        7000770666666607700070666666660770007706666666077000777066000007
        7000777706077777700077777007777770007777770777777000777777777777
        7000}
    end
    object btnUp: TBitBtn
      Left = 346
      Top = 151
      Width = 25
      Height = 25
      Action = actMoveUp
      Anchors = [akTop, akRight]
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      Glyph.Data = {
        DE000000424DDE0000000000000076000000280000000D0000000D0000000100
        04000000000068000000C40E0000C40E00001000000000000000000000000000
        BF0000BF000000BFBF00BF000000BF00BF00BFBF0000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
        7000777777777777700077770000077770007777066607777000777706660777
        7000777706660777700070000666000070007706666666077000777066666077
        7000777706660777700077777060777770007777770777777000777777777777
        7000}
    end
  end
  object Actions: TActionList
    OnUpdate = ActionsUpdate
    Left = 40
    Top = 32
    object actAddButton: TAction
      Hint = 'Add button to toolbar'
      OnExecute = actAddButtonExecute
    end
    object actRemoveButton: TAction
      Hint = 'Remove button from toolbar'
      OnExecute = actRemoveButtonExecute
    end
    object actAddSeparator: TAction
      Caption = 'Add Separator'
      Hint = 'Add button separator'
      OnExecute = actAddSeparatorExecute
    end
    object actMoveDown: TAction
      Hint = 'Move button down'
      OnExecute = actMoveDownExecute
    end
    object actMoveUp: TAction
      Hint = 'Move button up'
      OnExecute = actMoveUpExecute
    end
  end
end
