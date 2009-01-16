object fmComponentGrid: TfmComponentGrid
  Left = 426
  Top = 235
  AutoScroll = False
  BorderIcons = [biSystemMenu]
  Caption = 'Component Grid'
  ClientHeight = 443
  ClientWidth = 685
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 14
  object ToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 685
    Height = 22
    AutoSize = True
    DisabledImages = dmSharedImages.DisabledImages
    EdgeBorders = []
    Flat = True
    Images = dmSharedImages.Images
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    Wrapable = False
    object tbnSave: TToolButton
      Left = 0
      Top = 0
      Action = actFileSave
    end
    object tbnSep1: TToolButton
      Left = 23
      Top = 0
      Width = 8
      ImageIndex = 1
      Style = tbsSeparator
    end
    object tbnHelp: TToolButton
      Left = 31
      Top = 0
      Action = actHelpHelp
    end
    object tbnSep2: TToolButton
      Left = 54
      Top = 0
      Width = 8
      ImageIndex = 2
      Style = tbsSeparator
    end
    object tbnPrint: TToolButton
      Left = 62
      Top = 0
      Action = actFilePrint
    end
    object tbnSep3: TToolButton
      Left = 85
      Top = 0
      Width = 8
      ImageIndex = 4
      Style = tbsSeparator
    end
    object lblStart: TLabel
      Left = 93
      Top = 0
      Width = 181
      Height = 22
      Caption = '   Renumber HelpContex&t from:  '
      FocusControl = edtStart
      Layout = tlCenter
    end
    object edtStart: TEdit
      Left = 274
      Top = 0
      Width = 50
      Height = 22
      TabOrder = 0
      Text = '0'
    end
    object udStart: TUpDown
      Left = 324
      Top = 0
      Width = 12
      Height = 22
      Associate = edtStart
      Min = 0
      Max = 32767
      Position = 0
      TabOrder = 1
      Wrap = False
    end
    object lblSkipBy: TLabel
      Left = 336
      Top = 0
      Width = 55
      Height = 22
      Alignment = taRightJustify
      Caption = '  S&kip by  '
      FocusControl = edtSkipBy
      Layout = tlCenter
    end
    object edtSkipBy: TEdit
      Left = 391
      Top = 0
      Width = 50
      Height = 22
      TabOrder = 2
      Text = '1'
    end
    object udSkipBy: TUpDown
      Left = 441
      Top = 0
      Width = 12
      Height = 22
      Associate = edtSkipBy
      Min = 1
      Max = 10000
      Position = 1
      TabOrder = 3
      Wrap = False
    end
    object tbnSep4: TToolButton
      Left = 453
      Top = 0
      Width = 8
      ImageIndex = 6
      Style = tbsSeparator
    end
    object tbnRenumber: TToolButton
      Left = 461
      Top = 0
      Action = actFileRenumber
    end
  end
  object Actions: TActionList
    Images = dmSharedImages.Images
    Left = 88
    Top = 72
    object actFileSave: TAction
      Category = 'File'
      Caption = '&Save'
      Hint = 'Save changes'
      ImageIndex = 31
      OnExecute = actFileSaveExecute
    end
    object actHelpHelp: TAction
      Category = 'Help'
      Caption = '&Help'
      Hint = 'Help'
      ImageIndex = 0
      OnExecute = actHelpHelpExecute
    end
    object actFilePrint: TAction
      Category = 'File'
      Caption = '&Print'
      Hint = 'Print component list'
      ImageIndex = 3
      OnExecute = actFilePrintExecute
    end
    object actFileRenumber: TAction
      Category = 'File'
      Caption = '&Renumber HelpContext'
      Hint = 'Renumber HelpContext values'
      ImageIndex = 40
      OnExecute = actFileRenumberExecute
    end
  end
end
