inherited fmGxBookmarksForm: TfmGxBookmarksForm
  Caption = 'Bookmarks'
  PixelsPerInch = 96
  TextHeight = 14
  object lv_Bookmarks: TListView
    Left = 0
    Top = 0
    Width = 304
    Height = 204
    Align = alClient
    Columns = <
      item
        Caption = 'No.'
        Width = 40
      end
      item
        Caption = 'Unit'
        Width = 200
      end
      item
        Caption = 'Line'
      end>
    ReadOnly = True
    RowSelect = True
    PopupMenu = pm_Bookmarks
    TabOrder = 0
    ViewStyle = vsReport
    OnDblClick = lv_BookmarksDblClick
  end
  object tim_Update: TTimer
    OnTimer = tim_UpdateTimer
    Left = 144
    Top = 104
  end
  object pm_Bookmarks: TPopupMenu
    Left = 232
    Top = 104
    object mi_Edit: TMenuItem
      Caption = 'Edit ...'
      OnClick = mi_EditClick
    end
    object mi_Add: TMenuItem
      Caption = 'Add ...'
      OnClick = mi_AddClick
    end
    object mi_Delete: TMenuItem
      Caption = 'Delete'
      OnClick = mi_DeleteClick
    end
  end
end
