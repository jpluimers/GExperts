inherited fmUsageStatistics: TfmUsageStatistics
  Caption = 'GExperts Usage Statistics'
  ClientHeight = 411
  ClientWidth = 593
  Position = poOwnerFormCenter
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object p_Bottom: TPanel
    Left = 0
    Top = 370
    Width = 593
    Height = 41
    Align = alBottom
    TabOrder = 0
    DesignSize = (
      593
      41)
    object l_Blurb: TLabel
      Left = 112
      Top = 4
      Width = 393
      Height = 37
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 
        'Assuming each usage of an Expert saved you just one second, you ' +
        'have saved %s in this session and %s in total.'
      Visible = False
      WordWrap = True
    end
    object b_Close: TButton
      Left = 512
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Close'
      TabOrder = 0
      OnClick = b_CloseClick
    end
    object b_Clear: TButton
      Left = 8
      Top = 8
      Width = 97
      Height = 25
      Caption = 'Clear Statistics'
      TabOrder = 1
      OnClick = b_ClearClick
    end
  end
  object lv_Experts: TListView
    Left = 0
    Top = 0
    Width = 289
    Height = 370
    Align = alLeft
    Columns = <
      item
        Caption = 'Expert'
        Width = 150
      end
      item
        Caption = 'Session'
      end
      item
        Caption = 'Total'
      end>
    ReadOnly = True
    RowSelect = True
    TabOrder = 1
    ViewStyle = vsReport
  end
  object lv_EditorExperts: TListView
    Left = 289
    Top = 0
    Width = 304
    Height = 370
    Align = alClient
    Columns = <
      item
        Caption = 'Expert'
        Width = 150
      end
      item
        Caption = 'Session'
      end
      item
        Caption = 'Total'
      end>
    ReadOnly = True
    RowSelect = True
    TabOrder = 2
    ViewStyle = vsReport
  end
end
