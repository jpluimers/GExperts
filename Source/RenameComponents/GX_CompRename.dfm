object fmCompRename: TfmCompRename
  Left = 413
  Top = 304
  ActiveControl = edtNewName
  BorderStyle = bsDialog
  Caption = 'GExperts Rename Component'
  ClientHeight = 330
  ClientWidth = 321
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 14
  object lblOldName: TLabel
    Left = 8
    Top = 8
    Width = 53
    Height = 14
    Caption = 'O&ld Name'
    FocusControl = edtOldName
  end
  object lblNewName: TLabel
    Left = 8
    Top = 56
    Width = 59
    Height = 14
    Caption = '&New name'
    FocusControl = edtNewName
  end
  object lblReason: TLabel
    Left = 8
    Top = 96
    Width = 85
    Height = 14
    Caption = 'Invalid identifier'
    Visible = False
  end
  object edtOldName: TEdit
    Left = 8
    Top = 24
    Width = 305
    Height = 22
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 0
  end
  object edtNewName: TEdit
    Left = 8
    Top = 72
    Width = 305
    Height = 22
    AutoSelect = False
    TabOrder = 1
    OnChange = edtNewNameChange
  end
  object btnCancel: TButton
    Left = 160
    Top = 120
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object btnOK: TButton
    Left = 80
    Top = 120
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object btnSettings: TButton
    Left = 240
    Top = 120
    Width = 75
    Height = 25
    Caption = '&Settings...'
    TabOrder = 4
    OnClick = btnSettingsClick
  end
  object pc_Additional: TPageControl
    Left = 0
    Top = 153
    Width = 321
    Height = 177
    ActivePage = ts_Align
    Align = alBottom
    TabOrder = 5
    OnChange = pc_AdditionalChange
    object ts_Align: TTabSheet
      Caption = '&Align'
      DesignSize = (
        313
        148)
      object grp_Margins: TGroupBox
        Left = 176
        Top = 0
        Width = 137
        Height = 145
        TabOrder = 8
        object l_MarginTop: TLabel
          Left = 24
          Top = 16
          Width = 22
          Height = 14
          Caption = 'Top'
        end
        object l_MarginLeft: TLabel
          Left = 8
          Top = 56
          Width = 22
          Height = 14
          Caption = 'Left'
        end
        object l_MarginRight: TLabel
          Left = 48
          Top = 56
          Width = 28
          Height = 14
          Caption = 'Right'
        end
        object l_MarginBottom: TLabel
          Left = 24
          Top = 96
          Width = 41
          Height = 14
          Caption = 'Bottom'
        end
        object ed_MarginTop: TEdit
          Left = 24
          Top = 32
          Width = 33
          Height = 22
          TabOrder = 0
        end
        object ed_MarginLeft: TEdit
          Left = 8
          Top = 72
          Width = 33
          Height = 22
          TabOrder = 1
        end
        object ed_MarginRight: TEdit
          Left = 48
          Top = 72
          Width = 33
          Height = 22
          TabOrder = 2
        end
        object ed_MarginBottom: TEdit
          Left = 24
          Top = 112
          Width = 33
          Height = 22
          TabOrder = 3
        end
        object b_Margins0: TButton
          Left = 96
          Top = 16
          Width = 33
          Height = 25
          Caption = '0'
          TabOrder = 4
          OnClick = b_Margins0Click
        end
        object b_Margins3: TButton
          Left = 96
          Top = 48
          Width = 33
          Height = 25
          Caption = '3'
          TabOrder = 5
          OnClick = b_Margins3Click
        end
        object b_Margins6: TButton
          Left = 96
          Top = 80
          Width = 33
          Height = 25
          Caption = '6'
          TabOrder = 6
          OnClick = b_Margins6Click
        end
        object b_Margins8: TButton
          Left = 96
          Top = 112
          Width = 33
          Height = 25
          Caption = '8'
          TabOrder = 7
          OnClick = b_Margins8Click
        end
      end
      object chk_WithMargins: TCheckBox
        Left = 184
        Top = 0
        Width = 73
        Height = 17
        Caption = 'Margins'
        TabOrder = 7
      end
      object b_AlignTop: TBitBtn
        Left = 0
        Top = 0
        Width = 169
        Height = 25
        Anchors = []
        Caption = 'Top'
        TabOrder = 0
        OnClick = b_AlignTopClick
      end
      object b_AlignLeft: TBitBtn
        Left = 0
        Top = 24
        Width = 41
        Height = 65
        Anchors = []
        Caption = 'Left'
        TabOrder = 1
        OnClick = b_AlignLeftClick
      end
      object b_AlignClient: TBitBtn
        Left = 40
        Top = 24
        Width = 89
        Height = 65
        Anchors = []
        Caption = 'Client'
        TabOrder = 2
        OnClick = b_AlignClientClick
      end
      object b_AlignNone: TBitBtn
        Left = 0
        Top = 120
        Width = 65
        Height = 25
        Anchors = []
        Caption = 'None'
        TabOrder = 5
        OnClick = b_AlignNoneClick
      end
      object b_AlignCustom: TBitBtn
        Left = 104
        Top = 120
        Width = 65
        Height = 25
        Anchors = []
        Caption = 'Custom'
        TabOrder = 6
        OnClick = b_AlignCustomClick
      end
      object b_AlignRight: TBitBtn
        Left = 128
        Top = 24
        Width = 41
        Height = 65
        Anchors = []
        Caption = 'Right'
        TabOrder = 3
        OnClick = b_AlignRightClick
      end
      object b_AlignBottom: TBitBtn
        Left = 0
        Top = 88
        Width = 169
        Height = 25
        Anchors = []
        Caption = 'Bottom'
        TabOrder = 4
        OnClick = b_AlignBottomClick
      end
    end
    object ts_Anchors: TTabSheet
      Caption = 'An&chors'
      ImageIndex = 1
      DesignSize = (
        313
        148)
      object b_AnchorLeft: TBitBtn
        Left = 80
        Top = 46
        Width = 49
        Height = 49
        Anchors = []
        Caption = 'Left'
        TabOrder = 1
      end
      object b_AnchorRight: TBitBtn
        Left = 184
        Top = 46
        Width = 49
        Height = 49
        Anchors = []
        Caption = 'Right'
        TabOrder = 2
      end
      object b_AnchorTop: TBitBtn
        Left = 132
        Top = 16
        Width = 49
        Height = 49
        Anchors = []
        Caption = 'Top'
        TabOrder = 0
      end
      object b_AnchorBottom: TBitBtn
        Left = 132
        Top = 71
        Width = 49
        Height = 49
        Anchors = []
        Caption = 'Bottom'
        TabOrder = 3
      end
    end
  end
end
