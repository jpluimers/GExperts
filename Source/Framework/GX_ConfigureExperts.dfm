object frConfigureExperts: TfrConfigureExperts
  Left = 0
  Top = 0
  Width = 599
  Height = 223
  TabOrder = 0
  OnMouseWheelDown = FrameMouseWheelDown
  OnMouseWheelUp = FrameMouseWheelUp
  OnResize = FrameResize
  object pnlExpertsFilter: TPanel
    Left = 0
    Top = 0
    Width = 599
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object lblFilter: TLabel
      Left = 13
      Top = 14
      Width = 22
      Height = 13
      Alignment = taRightJustify
      Caption = '&Filter'
      FocusControl = edtFilter
    end
    object edtFilter: TEdit
      Left = 42
      Top = 10
      Width = 143
      Height = 21
      TabOrder = 0
      OnChange = edtFilterChange
      OnKeyDown = edtFilterKeyDown
    end
    object btnEnableAll: TButton
      Left = 406
      Top = 7
      Width = 86
      Height = 25
      Caption = 'Enable All'
      TabOrder = 4
      OnClick = btnEnableAllClick
    end
    object btnDisableAll: TButton
      Left = 499
      Top = 7
      Width = 86
      Height = 25
      Caption = 'Disable All'
      TabOrder = 5
      OnClick = btnDisableAllClick
    end
    object btnClear: TButton
      Left = 185
      Top = 9
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
      Left = 222
      Top = 7
      Width = 86
      Height = 25
      Caption = 'Clear All'
      TabOrder = 2
      OnClick = btnClearAllClick
    end
    object btnSetAllDefault: TButton
      Left = 314
      Top = 7
      Width = 86
      Height = 25
      Caption = 'Load Defaults'
      TabOrder = 3
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
    DesignSize = (
      578
      178)
    object pnlExpertLayout: TPanel
      Left = 0
      Top = 0
      Width = 572
      Height = 40
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      DesignSize = (
        572
        40)
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
        Width = 142
        Height = 22
        Anchors = [akLeft, akTop, akRight]
        HotKey = 32833
        InvalidKeys = [hcNone, hcShift]
        Modifiers = [hkAlt]
        TabOrder = 1
      end
      object btnExpert: TButton
        Left = 478
        Top = 8
        Width = 84
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Configure...'
        TabOrder = 3
      end
      object btnDefault: TButton
        Left = 408
        Top = 8
        Width = 55
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Default'
        TabOrder = 2
      end
    end
  end
end
