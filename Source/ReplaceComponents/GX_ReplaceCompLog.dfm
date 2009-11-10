object fmReplaceCompLog: TfmReplaceCompLog
  Left = 255
  Top = 214
  ActiveControl = edtSourceClassName
  AutoScroll = False
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Replace Components - Results'
  ClientHeight = 450
  ClientWidth = 604
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Scaled = False
  OnClose = FormClose
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object Splitter: TSplitter
    Left = 0
    Top = 294
    Width = 604
    Height = 7
    Cursor = crVSplit
    Align = alBottom
    Beveled = True
  end
  object pnlHeader: TPanel
    Left = 0
    Top = 0
    Width = 604
    Height = 33
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object lblDest: TLabel
      Left = 260
      Top = 11
      Width = 61
      Height = 14
      Alignment = taRightJustify
      Caption = 'Destination'
      FocusControl = edtDestClassName
    end
    object lblSource: TLabel
      Left = 17
      Top = 12
      Width = 38
      Height = 14
      Alignment = taRightJustify
      Caption = 'Source'
      FocusControl = edtSourceClassName
    end
    object edtDestClassName: TEdit
      Left = 327
      Top = 8
      Width = 175
      Height = 22
      Color = clBtnFace
      TabOrder = 1
    end
    object edtSourceClassName: TEdit
      Left = 62
      Top = 8
      Width = 175
      Height = 22
      Color = clBtnFace
      TabOrder = 0
    end
  end
  object pnlLog: TPanel
    Left = 0
    Top = 33
    Width = 604
    Height = 261
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 6
    TabOrder = 1
    object lvLogItems: TListView
      Left = 6
      Top = 6
      Width = 592
      Height = 249
      Align = alClient
      Columns = <
        item
          Caption = 'Time'
          MinWidth = 80
          Width = 80
        end
        item
          Caption = '[!]'
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
    Top = 301
    Width = 604
    Height = 149
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object pnlDetails: TPanel
      Left = 0
      Top = 0
      Width = 604
      Height = 113
      Align = alClient
      BevelOuter = bvNone
      BorderWidth = 6
      TabOrder = 0
      object lbxPreviewItems: TListBox
        Left = 6
        Top = 6
        Width = 592
        Height = 101
        Align = alClient
        ItemHeight = 14
        TabOrder = 0
      end
    end
    object pnlButtons: TPanel
      Left = 0
      Top = 113
      Width = 604
      Height = 36
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      object pnlButtonsRight: TPanel
        Left = 299
        Top = 0
        Width = 305
        Height = 36
        Align = alRight
        BevelOuter = bvNone
        TabOrder = 0
        object btnSaveAs: TButton
          Left = 25
          Top = 3
          Width = 85
          Height = 25
          Caption = '&Save As...'
          TabOrder = 0
          OnClick = btnSaveAsClick
        end
        object btnCopy: TButton
          Left = 119
          Top = 3
          Width = 85
          Height = 25
          Caption = 'C&opy'
          TabOrder = 1
          OnClick = btnCopyClick
        end
        object btnClose: TButton
          Left = 213
          Top = 3
          Width = 85
          Height = 25
          Caption = '&Close'
          TabOrder = 2
          OnClick = btnCloseClick
        end
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
