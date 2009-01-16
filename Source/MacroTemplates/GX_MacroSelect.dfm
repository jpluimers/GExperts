object fmMacroSelect: TfmMacroSelect
  Left = 366
  Top = 321
  Width = 439
  Height = 281
  BorderStyle = bsSizeToolWin
  Caption = 'Select Macro Template'
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
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
  TextHeight = 14
  object pnlMain: TPanel
    Left = 0
    Top = 0
    Width = 423
    Height = 213
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 6
    TabOrder = 0
    object lvMacros: TListView
      Left = 6
      Top = 6
      Width = 411
      Height = 201
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
  object pnlFooter: TPanel
    Left = 0
    Top = 213
    Width = 423
    Height = 32
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      423
      32)
    object tbEnter: TMemo
      Left = 6
      Top = 4
      Width = 282
      Height = 20
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      WantReturns = False
      WordWrap = False
      OnChange = tbEnterChange
      OnKeyDown = tbEnterKeyDown
    end
    object pnlButtonsRight: TPanel
      Left = 298
      Top = 0
      Width = 125
      Height = 32
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
      object btnConfiguration: TButton
        Left = 15
        Top = 1
        Width = 103
        Height = 25
        Caption = '&Configuration...'
        TabOrder = 0
        OnClick = btnConfigurationClick
      end
    end
  end
end
