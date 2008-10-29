object fmToolbarConfig: TfmToolbarConfig
  Left = 253
  Top = 198
  AutoScroll = False
  BorderIcons = [biSystemMenu]
  Caption = 'Editor Toolbar Configuration'
  ClientHeight = 363
  ClientWidth = 587
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
    Top = 327
    Width = 587
    Height = 36
    Align = alBottom
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 1
    object pnlButtonsRight: TPanel
      Left = 303
      Top = 0
      Width = 284
      Height = 36
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object btnOK: TButton
        Left = 26
        Top = 2
        Width = 75
        Height = 26
        Caption = 'OK'
        Default = True
        ModalResult = 1
        TabOrder = 0
        OnClick = btnOKClick
      end
      object btnCancel: TButton
        Left = 114
        Top = 2
        Width = 75
        Height = 26
        Cancel = True
        Caption = 'Cancel'
        ModalResult = 2
        TabOrder = 1
      end
      object btnHelp: TButton
        Left = 202
        Top = 2
        Width = 75
        Height = 26
        Caption = '&Help'
        TabOrder = 2
        OnClick = btnHelpClick
      end
    end
  end
  object pnlContent: TPanel
    Left = 0
    Top = 0
    Width = 587
    Height = 327
    Align = alClient
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 0
    object pnlCategories: TPanel
      Left = 0
      Top = 0
      Width = 137
      Height = 327
      Align = alLeft
      BevelOuter = bvNone
      BorderWidth = 6
      TabOrder = 0
      object pnlCatHeader: TPanel
        Left = 6
        Top = 6
        Width = 125
        Height = 18
        Align = alTop
        BevelOuter = bvNone
        Caption = 'Categories'
        TabOrder = 0
      end
      object lbCategories: TListBox
        Left = 6
        Top = 24
        Width = 125
        Height = 297
        Align = alClient
        ItemHeight = 13
        TabOrder = 1
        OnClick = lbCategoriesClick
      end
    end
    object pnlAvailButtons: TPanel
      Left = 137
      Top = 0
      Width = 246
      Height = 327
      Align = alClient
      BevelOuter = bvNone
      BorderWidth = 6
      TabOrder = 1
      object pnlAvailButtonHeader: TPanel
        Left = 6
        Top = 6
        Width = 234
        Height = 18
        Align = alTop
        BevelOuter = bvNone
        Caption = 'Available Buttons'
        TabOrder = 0
      end
      object lbAvailable: TListBox
        Left = 6
        Top = 24
        Width = 234
        Height = 297
        Style = lbOwnerDrawFixed
        Align = alClient
        DragMode = dmAutomatic
        ItemHeight = 21
        TabOrder = 1
        OnDblClick = actAddButtonExecute
        OnDragDrop = lbAvailableDragDrop
        OnDragOver = lbAvailableDragOver
        OnDrawItem = ListboxDrawItem
      end
    end
    object pnlToolbarButtons: TPanel
      Left = 410
      Top = 0
      Width = 177
      Height = 327
      Align = alRight
      BevelOuter = bvNone
      BorderWidth = 6
      TabOrder = 3
      object pnlToolbarHeader: TPanel
        Left = 6
        Top = 6
        Width = 165
        Height = 18
        Align = alTop
        BevelOuter = bvNone
        Caption = 'Editor Toolbar Buttons'
        TabOrder = 0
      end
      object lbToolbar: TListBox
        Left = 6
        Top = 24
        Width = 165
        Height = 271
        Style = lbOwnerDrawFixed
        Align = alClient
        DragMode = dmAutomatic
        ItemHeight = 21
        TabOrder = 1
        OnDblClick = actRemoveButtonExecute
        OnDragDrop = lbToolbarDragDrop
        OnDragOver = lbToolbarDragOver
        OnDrawItem = ListboxDrawItem
        OnKeyDown = lbToolbarKeyDown
        OnStartDrag = lbToolbarStartDrag
      end
      object pnlSep: TPanel
        Left = 6
        Top = 295
        Width = 165
        Height = 26
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 2
        DesignSize = (
          165
          26)
        object btnAddSeparator: TBitBtn
          Left = 1
          Top = 0
          Width = 163
          Height = 26
          Action = actAddSeparator
          Anchors = [akRight, akBottom]
          Caption = 'Add Separator'
          TabOrder = 0
        end
      end
    end
    object pnlToolButtons: TPanel
      Left = 383
      Top = 0
      Width = 27
      Height = 327
      Align = alRight
      BevelOuter = bvNone
      BorderWidth = 6
      TabOrder = 2
      object btnDown: TBitBtn
        Left = 1
        Top = 196
        Width = 25
        Height = 25
        Action = actMoveDown
        ParentShowHint = False
        ShowHint = True
        TabOrder = 3
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
      object btnUp: TBitBtn
        Left = 1
        Top = 163
        Width = 25
        Height = 25
        Action = actMoveUp
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
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
      object btnRemove: TBitBtn
        Left = 1
        Top = 92
        Width = 25
        Height = 25
        Action = actRemoveButton
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
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
      object btnAdd: TBitBtn
        Left = 1
        Top = 59
        Width = 25
        Height = 25
        Action = actAddButton
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
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
