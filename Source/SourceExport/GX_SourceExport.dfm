object fmSourceExport: TfmSourceExport
  Left = 277
  Top = 216
  Width = 475
  Height = 375
  BorderIcons = [biSystemMenu]
  Caption = 'Source Export'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnActivate = FormActivate
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object pnlFooter: TPanel
    Left = 0
    Top = 272
    Width = 467
    Height = 74
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object lblTitle: TLabel
      Left = 14
      Top = 16
      Width = 20
      Height = 13
      Alignment = taRightJustify
      Caption = 'Title'
    end
    object btnCopy: TButton
      Left = 99
      Top = 43
      Width = 84
      Height = 26
      Action = actCopy
      Default = True
      TabOrder = 2
    end
    object btnPrint: TButton
      Left = 191
      Top = 43
      Width = 84
      Height = 26
      Action = actFilePrint
      TabOrder = 3
    end
    object btnConfig: TButton
      Left = 283
      Top = 43
      Width = 84
      Height = 26
      Action = actFileConfigure
      TabOrder = 4
    end
    object btnSave: TButton
      Left = 7
      Top = 43
      Width = 84
      Height = 26
      Action = actFileSave
      TabOrder = 1
    end
    object edtTitle: TEdit
      Left = 40
      Top = 12
      Width = 369
      Height = 21
      TabOrder = 0
    end
    object btnClose: TButton
      Left = 375
      Top = 43
      Width = 84
      Height = 26
      Action = actFileExit
      Cancel = True
      TabOrder = 5
    end
  end
  object ToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 467
    Height = 24
    AutoSize = True
    ButtonHeight = 24
    ButtonWidth = 25
    DisabledImages = dmSharedImages.DisabledImages
    EdgeBorders = [ebTop, ebBottom]
    EdgeInner = esNone
    EdgeOuter = esNone
    Flat = True
    Images = dmSharedImages.Images
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    Wrapable = False
    object tbnRefresh: TToolButton
      Left = 0
      Top = 0
      Action = actFileRefresh
      ParentShowHint = False
      ShowHint = True
    end
    object tbnSave: TToolButton
      Left = 25
      Top = 0
      Action = actFileSave
      ParentShowHint = False
      ShowHint = True
    end
    object tbnCopy: TToolButton
      Left = 50
      Top = 0
      Action = actCopy
      DropdownMenu = pmuCopy
      ParentShowHint = False
      ShowHint = True
      Style = tbsDropDown
    end
    object ToolButton3: TToolButton
      Left = 88
      Top = 0
      Width = 8
      ImageIndex = 6
      Style = tbsSeparator
    end
    object tbnPrint: TToolButton
      Left = 96
      Top = 0
      Action = actFilePrint
      ParentShowHint = False
      ShowHint = True
    end
    object ToolButton2: TToolButton
      Left = 121
      Top = 0
      Width = 8
      ImageIndex = 6
      Style = tbsSeparator
    end
    object tbnConfigure: TToolButton
      Left = 129
      Top = 0
      Action = actFileConfigure
      ParentShowHint = False
      ShowHint = True
    end
    object ToolButton1: TToolButton
      Left = 154
      Top = 0
      Width = 8
      ImageIndex = 6
      Style = tbsSeparator
    end
    object tbnHelp: TToolButton
      Left = 162
      Top = 0
      Action = actHelpHelp
      ParentShowHint = False
      ShowHint = True
    end
  end
  object dlgSave: TSaveDialog
    DefaultExt = 'html'
    Filter = 
      'Web Page (*.html;*.htm)|*.html;*.htm|Rich Text Format (*.rtf)|*.' +
      'rtf'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist]
    Left = 200
    Top = 48
  end
  object pmuCopy: TPopupMenu
    Images = dmSharedImages.Images
    Left = 248
    Top = 48
    object mitCopy: TMenuItem
      Action = actCopyTextRtfHtml
    end
    object mitCopyHtml: TMenuItem
      Action = actCopyHtmlFragment
    end
    object mitCopyRtf: TMenuItem
      Action = actCopyRtfFragment
    end
  end
  object Actions: TActionList
    Images = dmSharedImages.Images
    Left = 16
    Top = 72
    object actFileRefresh: TAction
      Category = 'File'
      Caption = '&Refresh'
      Hint = 'Reload from IDE'
      ImageIndex = 39
      OnExecute = actRefreshExecute
    end
    object actFileSave: TAction
      Category = 'File'
      Caption = '&Save As...'
      Hint = 'Save highlighted text to a file'
      ImageIndex = 31
      OnExecute = actSaveExecute
    end
    object actCopy: TAction
      Category = 'Copy'
      Caption = '&Copy'
      Hint = 'Copy to clipboard'
      ImageIndex = 6
      OnExecute = actCopyExecute
    end
    object actCopyTextRtfHtml: TAction
      Category = 'Copy'
      Caption = 'Copy as Formatted &Text/RTF/HTML'
      Hint = 'Copy as Formatted text/HTML/RTF'
      OnExecute = actCopyTextRtfHtmlExecute
    end
    object actCopyHtmlFragment: TAction
      Category = 'Copy'
      Caption = 'Copy as &HTML Fragment Text'
      Hint = 'Copy as HTML Text'
      OnExecute = actCopyHtmlExecute
    end
    object actCopyRtfFragment: TAction
      Category = 'Copy'
      Caption = 'Copy as &RTF Fragment Text'
      Hint = 'Copy as RTF Text'
      OnExecute = actCopyRtfExecute
    end
    object actFilePrint: TAction
      Category = 'File'
      Caption = '&Print'
      Hint = 'Print the highlighted code'
      ImageIndex = 3
      OnExecute = actPrintExecute
    end
    object actFileConfigure: TAction
      Category = 'File'
      Caption = 'Con&figure...'
      Hint = 'Configuration'
      ImageIndex = 17
      OnExecute = actConfigureExecute
    end
    object actHelpHelp: TAction
      Category = 'Help'
      Caption = '&Help'
      Hint = 'Help'
      ImageIndex = 0
      ShortCut = 112
      OnExecute = actHelpExecute
    end
    object actFileExit: TAction
      Category = 'File'
      Caption = 'E&xit'
      Hint = 'Close Source Export'
      ImageIndex = 8
      OnExecute = actExitExecute
    end
  end
end
