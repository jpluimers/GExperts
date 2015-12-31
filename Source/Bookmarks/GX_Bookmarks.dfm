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
    TabOrder = 0
    ViewStyle = vsReport
    OnDblClick = lv_BookmarksDblClick
  end
  object tim_Update: TTimer
    OnTimer = tim_UpdateTimer
    Left = 144
    Top = 104
  end
end
