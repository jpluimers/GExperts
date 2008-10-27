object fmClassOptions: TfmClassOptions
  Left = 274
  Top = 170
  Width = 439
  Height = 421
  BorderIcons = [biSystemMenu]
  Caption = 'Class Browser Options'
  Color = clBtnFace
  Constraints.MinHeight = 365
  Constraints.MinWidth = 360
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pnlContent: TPanel
    Left = 0
    Top = 0
    Width = 423
    Height = 351
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 6
    TabOrder = 0
    object pcClassOptions: TPageControl
      Left = 6
      Top = 6
      Width = 411
      Height = 339
      ActivePage = tshGeneric
      Align = alClient
      TabIndex = 0
      TabOrder = 0
      object tshGeneric: TTabSheet
        Caption = 'Display'
        DesignSize = (
          403
          311)
        object gbxFonts: TGroupBox
          Left = 8
          Top = 8
          Width = 387
          Height = 105
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Fonts'
          TabOrder = 0
          DesignSize = (
            387
            105)
          object lblTreeViewFont: TLabel
            Left = 27
            Top = 24
            Width = 43
            Height = 13
            Alignment = taRightJustify
            Caption = '&Tree font'
            FocusControl = cbTreeView
          end
          object lblListViewFont: TLabel
            Left = 33
            Top = 49
            Width = 37
            Height = 13
            Alignment = taRightJustify
            Caption = '&List font'
            FocusControl = cbListView
          end
          object lblEditorFont: TLabel
            Left = 22
            Top = 73
            Width = 48
            Height = 13
            Alignment = taRightJustify
            Caption = '&Editor font'
            FocusControl = cbEditor
          end
          object cbTreeView: TComboBox
            Left = 77
            Top = 21
            Width = 238
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            ItemHeight = 13
            TabOrder = 0
          end
          object cbListView: TComboBox
            Left = 77
            Top = 45
            Width = 238
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            ItemHeight = 13
            TabOrder = 3
          end
          object cbEditor: TComboBox
            Left = 77
            Top = 69
            Width = 238
            Height = 21
            Style = csDropDownList
            Anchors = [akLeft, akTop, akRight]
            ItemHeight = 13
            TabOrder = 6
          end
          object sTreeView: TEdit
            Left = 319
            Top = 21
            Width = 35
            Height = 21
            Anchors = [akTop, akRight]
            TabOrder = 1
            Text = '1'
          end
          object sListView: TEdit
            Left = 319
            Top = 45
            Width = 35
            Height = 21
            Anchors = [akTop, akRight]
            TabOrder = 4
            Text = '1'
          end
          object sEditor: TEdit
            Left = 319
            Top = 69
            Width = 35
            Height = 21
            Anchors = [akTop, akRight]
            TabOrder = 7
            Text = '1'
          end
          object udTree: TUpDown
            Left = 354
            Top = 21
            Width = 16
            Height = 21
            Anchors = [akTop, akRight]
            Associate = sTreeView
            Min = 1
            Position = 1
            TabOrder = 2
            Wrap = False
          end
          object udList: TUpDown
            Left = 354
            Top = 45
            Width = 16
            Height = 21
            Anchors = [akTop, akRight]
            Associate = sListView
            Min = 1
            Position = 1
            TabOrder = 5
            Wrap = False
          end
          object udEditor: TUpDown
            Left = 354
            Top = 69
            Width = 16
            Height = 21
            Anchors = [akTop, akRight]
            Associate = sEditor
            Min = 1
            Position = 1
            TabOrder = 8
            Wrap = False
          end
        end
        object cbAutoHide: TCheckBox
          Left = 10
          Top = 128
          Width = 343
          Height = 25
          Caption = '&Auto-hide window when jumping to class/member'
          TabOrder = 1
        end
      end
      object tshFilters: TTabSheet
        Caption = 'Configuration'
        DesignSize = (
          403
          311)
        object gbxFilters: TGroupBox
          Left = 8
          Top = 8
          Width = 387
          Height = 113
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Default Filters'
          TabOrder = 0
          object cbConstants: TCheckBox
            Left = 8
            Top = 22
            Width = 169
            Height = 17
            Caption = 'Constants'
            TabOrder = 0
          end
          object cbMethods: TCheckBox
            Tag = 1
            Left = 8
            Top = 38
            Width = 169
            Height = 17
            Caption = 'Methods'
            TabOrder = 1
          end
          object cbTypes: TCheckBox
            Tag = 2
            Left = 8
            Top = 54
            Width = 169
            Height = 17
            Caption = 'Types'
            TabOrder = 2
          end
          object cbVariables: TCheckBox
            Tag = 3
            Left = 8
            Top = 70
            Width = 169
            Height = 17
            Caption = 'Variables'
            TabOrder = 3
          end
          object cbProperties: TCheckBox
            Tag = 4
            Left = 8
            Top = 86
            Width = 169
            Height = 17
            Caption = 'Properties'
            TabOrder = 4
          end
          object cbPrivate: TCheckBox
            Tag = 5
            Left = 179
            Top = 22
            Width = 135
            Height = 17
            Caption = 'Private'
            TabOrder = 5
          end
          object cbProtected: TCheckBox
            Tag = 6
            Left = 179
            Top = 38
            Width = 135
            Height = 17
            Caption = 'Protected'
            TabOrder = 6
          end
          object cbPublic: TCheckBox
            Tag = 7
            Left = 179
            Top = 54
            Width = 135
            Height = 17
            Caption = 'Public'
            TabOrder = 7
          end
          object cbPublished: TCheckBox
            Tag = 8
            Left = 179
            Top = 70
            Width = 135
            Height = 17
            Caption = 'Published'
            TabOrder = 8
          end
        end
        object gbxDiagram: TGroupBox
          Left = 8
          Top = 128
          Width = 387
          Height = 65
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Diagram Options'
          TabOrder = 1
          object cbTop: TCheckBox
            Left = 8
            Top = 22
            Width = 305
            Height = 17
            Caption = 'Show most primitive class at top of diagram'
            TabOrder = 0
          end
          object cbStayInPackage: TCheckBox
            Left = 8
            Top = 38
            Width = 305
            Height = 17
            Caption = 'Stay in source folder'
            TabOrder = 1
          end
        end
        object gbxSearch: TGroupBox
          Left = 8
          Top = 200
          Width = 387
          Height = 49
          Anchors = [akLeft, akTop, akRight]
          Caption = 'Parse Options'
          TabOrder = 2
          object cbParseRecursing: TCheckBox
            Left = 8
            Top = 22
            Width = 305
            Height = 17
            Caption = 'Parsing looks for files recursively in subfolders'
            TabOrder = 0
          end
        end
      end
    end
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 351
    Width = 423
    Height = 34
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object pnlButtonsRight: TPanel
      Left = 238
      Top = 0
      Width = 185
      Height = 34
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object btnOK: TButton
        Left = 18
        Top = 2
        Width = 75
        Height = 26
        Caption = 'OK'
        Default = True
        ModalResult = 1
        TabOrder = 0
      end
      object btnCancel: TButton
        Left = 103
        Top = 2
        Width = 75
        Height = 26
        Cancel = True
        Caption = 'Cancel'
        ModalResult = 2
        TabOrder = 1
      end
    end
  end
end
