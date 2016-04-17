object frConfigureEditorExperts: TfrConfigureEditorExperts
  Left = 0
  Top = 0
  Width = 599
  Height = 223
  TabOrder = 0
  OnMouseWheelDown = FrameMouseWheelDown
  OnMouseWheelUp = FrameMouseWheelUp
  object pnlExpertsFilter: TPanel
    Left = 0
    Top = 0
    Width = 599
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object lblFilter: TLabel
      Left = 11
      Top = 12
      Width = 24
      Height = 13
      Alignment = taRightJustify
      Caption = '&Filter'
      FocusControl = edtFilter
    end
    object edtFilter: TEdit
      Left = 42
      Top = 8
      Width = 199
      Height = 21
      TabOrder = 0
      OnChange = edtFilterChange
    end
    object btnEnableAll: TButton
      Left = 432
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Enable All'
      TabOrder = 2
      OnClick = btnEnableAllClick
    end
    object btnDisableAll: TButton
      Left = 512
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Disable All'
      TabOrder = 3
      OnClick = btnDisableAllClick
    end
    object btnClear: TButton
      Left = 236
      Top = 8
      Width = 21
      Height = 21
      Caption = 'X'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBtnText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnClick = btnClearClick
    end
    object btnClearAll: TButton
      Left = 264
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Clear All'
      TabOrder = 4
      OnClick = btnClearAllClick
    end
    object btnSetAllDefault: TButton
      Left = 344
      Top = 6
      Width = 81
      Height = 25
      Caption = 'Set to default'
      TabOrder = 5
      OnClick = btnSetAllDefaultClick
    end
  end
  object sbxExperts: TScrollBox
    Left = 0
    Top = 41
    Width = 599
    Height = 182
    VertScrollBar.Increment = 40
    VertScrollBar.Range = 920
    VertScrollBar.Tracking = True
    Align = alClient
    AutoScroll = False
    TabOrder = 1
    object pnlExpertLayout: TPanel
      Left = 0
      Top = 0
      Width = 572
      Height = 40
      TabOrder = 0
      object imgExpert: TImage
        Left = 4
        Top = 4
        Width = 32
        Height = 32
        Center = True
        Proportional = True
        Transparent = True
      end
      object chkExpert: TCheckBox
        Left = 40
        Top = 12
        Width = 222
        Height = 17
        Caption = 'Set Component Properties'
        TabOrder = 0
      end
      object edtExpert: THotKey
        Left = 267
        Top = 10
        Width = 171
        Height = 22
        HotKey = 32833
        TabOrder = 1
      end
      object btnExpert: TButton
        Left = 455
        Top = 8
        Width = 98
        Height = 25
        Caption = 'Configure...'
        TabOrder = 2
      end
    end
  end
end
