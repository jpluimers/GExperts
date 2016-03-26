inherited fmEditorPopupMenuExpertConfig: TfmEditorPopupMenuExpertConfig
  Left = 0
  Top = 0
  Caption = 'Editor Popup Menu Expert Configuration'
  ClientHeight = 313
  ClientWidth = 577
  ParentFont = False
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object l_DuplicateShortcuts: TLabel
    Left = 304
    Top = 256
    Width = 132
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Warning: Duplicate hotkeys'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Visible = False
  end
  object pc_Main: TPageControl
    Left = 8
    Top = 8
    Width = 201
    Height = 249
    ActivePage = ts_EditorExperts
    Anchors = [akLeft, akTop, akBottom]
    TabOrder = 0
    object ts_Experts: TTabSheet
      Caption = 'Experts'
      object lb_Experts: TListBox
        Left = 0
        Top = 0
        Width = 193
        Height = 221
        Align = alClient
        ItemHeight = 13
        TabOrder = 0
        OnDblClick = lb_ExpertsDblClick
      end
    end
    object ts_EditorExperts: TTabSheet
      Caption = 'Editor Experts'
      ImageIndex = 1
      object lb_EditorExperts: TListBox
        Left = 0
        Top = 0
        Width = 193
        Height = 221
        Align = alClient
        ItemHeight = 13
        Sorted = True
        TabOrder = 0
        OnDblClick = lb_EditorExpertsDblClick
      end
    end
  end
  object lv_Selected: TListView
    Left = 296
    Top = 32
    Width = 273
    Height = 217
    Anchors = [akLeft, akTop, akBottom]
    Columns = <
      item
        Caption = 'Hotkey'
        Width = 80
      end
      item
        Caption = 'Expert'
        Width = 175
      end>
    RowSelect = True
    SortType = stText
    TabOrder = 4
    ViewStyle = vsReport
    OnChange = lv_SelectedChange
    OnDblClick = lv_SelectedDblClick
    OnEditing = lv_SelectedEditing
    OnKeyDown = lv_SelectedKeyDown
  end
  object b_OK: TButton
    Left = 416
    Top = 280
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 6
  end
  object b_Cancel: TButton
    Left = 496
    Top = 280
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 7
  end
  object b_Add: TButton
    Left = 216
    Top = 40
    Width = 75
    Height = 25
    Caption = '>> Add >>'
    TabOrder = 1
    OnClick = b_AddClick
  end
  object b_Remove: TButton
    Left = 216
    Top = 80
    Width = 75
    Height = 25
    Caption = 'Remove <<'
    TabOrder = 2
    OnClick = b_RemoveClick
  end
  object b_Default: TButton
    Left = 216
    Top = 208
    Width = 75
    Height = 25
    Caption = 'Default >>'
    TabOrder = 3
    OnClick = b_DefaultClick
  end
  object b_ClearShortcut: TButton
    Left = 296
    Top = 280
    Width = 115
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Clear Shortcut'
    TabOrder = 5
    OnClick = b_ClearShortcutClick
  end
end
