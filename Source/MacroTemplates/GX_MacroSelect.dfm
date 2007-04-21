object fmMacroSelect: TfmMacroSelect
  Left = 366
  Top = 321
  Width = 410
  Height = 210
  BorderStyle = bsSizeToolWin
  Caption = 'Select Macro Template'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object pnlMain: TPanel
    Left = 0
    Top = 0
    Width = 402
    Height = 181
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 5
    TabOrder = 0
    object pnlFooter: TPanel
      Left = 5
      Top = 147
      Width = 392
      Height = 29
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      object tbEnter: TMemo
        Left = 0
        Top = 7
        Width = 285
        Height = 20
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        WantReturns = False
        WordWrap = False
        OnChange = tbEnterChange
        OnKeyDown = tbEnterKeyDown
      end
      object btnConfiguration: TButton
        Left = 295
        Top = 4
        Width = 97
        Height = 25
        Anchors = [akTop, akRight]
        Caption = '&Configuration...'
        TabOrder = 1
        OnClick = btnConfigurationClick
      end
    end
    object lvMacros: TListView
      Left = 5
      Top = 5
      Width = 392
      Height = 142
      Align = alClient
      Columns = <
        item
          Caption = 'Name'
          MinWidth = 50
          Width = 100
        end
        item
          Caption = 'Description'
          MinWidth = 100
          Width = 250
        end>
      ColumnClick = False
      HideSelection = False
      ReadOnly = True
      RowSelect = True
      SortType = stText
      TabOrder = 0
      ViewStyle = vsReport
      OnDblClick = lstMacrosDblClick
    end
  end
end
