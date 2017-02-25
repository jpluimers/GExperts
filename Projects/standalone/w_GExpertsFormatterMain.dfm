object f_GExpertsFormatterMain: Tf_GExpertsFormatterMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'GExperts Source Code Formatter'
  ClientHeight = 113
  ClientWidth = 345
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    345
    113)
  PixelsPerInch = 96
  TextHeight = 13
  object l_: TLabel
    Left = 8
    Top = 8
    Width = 64
    Height = 13
    Caption = 'File to format'
  end
  object ed_FileToFormat: TEdit
    Left = 8
    Top = 24
    Width = 297
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnChange = ed_FileToFormatChange
  end
  object b_SelectFile: TButton
    Left = 312
    Top = 22
    Width = 25
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 1
    OnClick = b_SelectFileClick
  end
  object b_Format: TButton
    Left = 184
    Top = 64
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Format'
    Default = True
    Enabled = False
    TabOrder = 2
    OnClick = b_FormatClick
  end
  object b_Exit: TButton
    Left = 264
    Top = 64
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = 'Exit'
    TabOrder = 3
    OnClick = b_ExitClick
  end
  object b_Settings: TButton
    Left = 88
    Top = 64
    Width = 75
    Height = 25
    Caption = 'Settings ...'
    TabOrder = 4
    OnClick = b_SettingsClick
  end
  object b_About: TButton
    Left = 8
    Top = 64
    Width = 75
    Height = 25
    Caption = 'About ...'
    TabOrder = 5
    OnClick = b_AboutClick
  end
  object TheStatusBar: TStatusBar
    Left = 0
    Top = 94
    Width = 345
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object od_File: TOpenDialog
    DefaultExt = '.pas'
    Filter = 'Delphi Sourcecode (*.pas)|*.pas|All Files (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing, ofDontAddToRecent]
    Title = 'Select file'
    Left = 142
    Top = 4
  end
end
