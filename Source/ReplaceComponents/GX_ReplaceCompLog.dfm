object fmReplaceCompLog: TfmReplaceCompLog
  Left = 255
  Top = 214
  Width = 620
  Height = 486
  ActiveControl = edtSourceClassName
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Replace Components - Results'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnClose = FormClose
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter: TSplitter
    Left = 0
    Top = 301
    Width = 612
    Height = 7
    Cursor = crVSplit
    Align = alBottom
    Beveled = True
  end
  object pnlHeader: TPanel
    Left = 0
    Top = 0
    Width = 612
    Height = 33
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object lblDest: TLabel
      Left = 235
      Top = 11
      Width = 53
      Height = 13
      Caption = 'Destination'
    end
    object lblSource: TLabel
      Left = 8
      Top = 12
      Width = 34
      Height = 13
      Caption = 'Source'
    end
    object edtDestClassName: TEdit
      Left = 295
      Top = 8
      Width = 165
      Height = 21
      Color = clBtnFace
      TabOrder = 1
    end
    object edtSourceClassName: TEdit
      Left = 49
      Top = 8
      Width = 165
      Height = 21
      Color = clBtnFace
      TabOrder = 0
    end
  end
  object pnlLog: TPanel
    Left = 0
    Top = 33
    Width = 612
    Height = 268
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 5
    TabOrder = 1
    object lvLogItems: TListView
      Left = 5
      Top = 5
      Width = 602
      Height = 258
      Align = alClient
      Columns = <
        item
          Caption = 'Time'
          MinWidth = 80
          Width = 80
        end
        item
          Caption = '[ ! ]'
          Width = 30
        end
        item
          Caption = 'Object'
          Width = 180
        end
        item
          Caption = 'Message'
          Width = 275
        end>
      MultiSelect = True
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnClick = lvLogItemsClick
      OnSelectItem = lvLogItemsSelectItem
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 308
    Width = 612
    Height = 149
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      612
      149)
    object btnSaveAs: TButton
      Left = 331
      Top = 115
      Width = 85
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Save As...'
      TabOrder = 1
      OnClick = btnSaveAsClick
    end
    object btnCopy: TButton
      Left = 425
      Top = 115
      Width = 85
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'C&opy'
      TabOrder = 2
      OnClick = btnCopyClick
    end
    object btnClose: TButton
      Left = 519
      Top = 115
      Width = 85
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Close'
      TabOrder = 3
      OnClick = btnCloseClick
    end
    object pnlDetails: TPanel
      Left = 0
      Top = 0
      Width = 612
      Height = 110
      Align = alTop
      Anchors = [akLeft, akTop, akRight, akBottom]
      BevelOuter = bvNone
      BorderWidth = 5
      TabOrder = 0
      object lbxPreviewItems: TListBox
        Left = 5
        Top = 5
        Width = 602
        Height = 100
        Align = alClient
        ItemHeight = 13
        TabOrder = 0
      end
    end
  end
  object dlgGetExportFile: TSaveDialog
    DefaultExt = 'xml'
    Filter = 'XML Files (*.xml)|*.xml|CSV Files (*.csv)|*.csv'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Title = 'Select a file write the the exported data to'
    Left = 136
    Top = 144
  end
end
