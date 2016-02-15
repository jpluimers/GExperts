object fmCodeOptions: TfmCodeOptions
  Left = 442
  Top = 167
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Code Librarian Options'
  ClientHeight = 241
  ClientWidth = 321
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 14
  object btnOK: TButton
    Left = 160
    Top = 208
    Width = 73
    Height = 26
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 240
    Top = 208
    Width = 73
    Height = 26
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object pgeCodeOpt: TPageControl
    Left = 8
    Top = 8
    Width = 305
    Height = 185
    ActivePage = tabPaths
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object tabPaths: TTabSheet
      Caption = 'Paths'
      object lblStoragePath: TLabel
        Left = 8
        Top = 16
        Width = 173
        Height = 14
        Caption = 'Code Librarian Storage Location'
      end
      object sbBrowse: TButton
        Left = 268
        Top = 32
        Width = 21
        Height = 21
        Anchors = [akTop, akRight]
        Caption = '...'
        TabOrder = 1
        OnClick = sbBrowseClick
      end
      object edPath: TEdit
        Left = 8
        Top = 32
        Width = 259
        Height = 22
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
      end
    end
    object tabLayout: TTabSheet
      Caption = 'Layout'
      object rbSide: TRadioButton
        Left = 22
        Top = 16
        Width = 137
        Height = 17
        Caption = 'Side by side'
        TabOrder = 0
      end
      object pnlSideSide: TPanel
        Left = 22
        Top = 40
        Width = 105
        Height = 89
        BevelOuter = bvLowered
        Color = clWindow
        TabOrder = 2
        object shpLeft: TShape
          Left = 8
          Top = 8
          Width = 41
          Height = 73
        end
        object shpRight: TShape
          Left = 56
          Top = 8
          Width = 41
          Height = 73
        end
      end
      object rbTop: TRadioButton
        Left = 160
        Top = 16
        Width = 113
        Height = 17
        Caption = 'Top to bottom'
        TabOrder = 1
      end
      object pnlTopBottom: TPanel
        Left = 160
        Top = 40
        Width = 105
        Height = 89
        BevelOuter = bvLowered
        Color = clWindow
        TabOrder = 3
        object shpTop: TShape
          Left = 8
          Top = 8
          Width = 89
          Height = 33
        end
        object shpBottom: TShape
          Left = 8
          Top = 48
          Width = 89
          Height = 33
        end
      end
    end
    object tabFonts: TTabSheet
      Caption = 'Fonts'
      object lblTreeView: TLabel
        Left = 8
        Top = 16
        Width = 51
        Height = 14
        Caption = 'Treeview'
      end
      object lblEditor: TLabel
        Left = 8
        Top = 64
        Width = 32
        Height = 14
        Caption = 'Editor'
      end
      object lblSize: TLabel
        Left = 232
        Top = 13
        Width = 41
        Height = 17
        Alignment = taCenter
        AutoSize = False
        Caption = 'Size'
      end
      object fcTreeview: TComboBox
        Left = 8
        Top = 32
        Width = 205
        Height = 22
        Style = csDropDownList
        DropDownCount = 18
        ItemHeight = 0
        TabOrder = 0
      end
      object fcEditor: TComboBox
        Left = 8
        Top = 80
        Width = 205
        Height = 22
        Style = csDropDownList
        DropDownCount = 18
        ItemHeight = 0
        TabOrder = 3
      end
      object udTreeview: TUpDown
        Left = 273
        Top = 32
        Width = 15
        Height = 21
        Associate = eTreeview
        Min = 4
        Max = 24
        Position = 8
        TabOrder = 2
        Wrap = False
      end
      object udEditor: TUpDown
        Left = 273
        Top = 80
        Width = 15
        Height = 21
        Associate = eEditor
        Min = 4
        Max = 24
        Position = 8
        TabOrder = 5
        Wrap = False
      end
      object eTreeview: TEdit
        Left = 232
        Top = 32
        Width = 41
        Height = 22
        TabOrder = 1
        Text = '8'
        OnKeyPress = eNumericKeyPress
      end
      object eEditor: TEdit
        Left = 232
        Top = 80
        Width = 41
        Height = 22
        TabOrder = 4
        Text = '8'
      end
    end
  end
end
