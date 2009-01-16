object fmFavOptions: TfmFavOptions
  Left = 279
  Top = 208
  BorderStyle = bsDialog
  Caption = 'Favorite Files Options'
  ClientHeight = 151
  ClientWidth = 289
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
  PixelsPerInch = 96
  TextHeight = 14
  object gbxFavOptions: TGroupBox
    Left = 8
    Top = 8
    Width = 273
    Height = 105
    Caption = 'Favorite Files Options'
    TabOrder = 0
    object chkConfirmFolderDelete: TCheckBox
      Left = 10
      Top = 21
      Width = 257
      Height = 17
      Caption = '&Confirm on folder delete'
      TabOrder = 0
    end
    object chkExpandAllOnLoad: TCheckBox
      Left = 10
      Top = 39
      Width = 257
      Height = 17
      Caption = '&Expand all on load'
      TabOrder = 1
    end
    object chkHideOnExecute: TCheckBox
      Left = 10
      Top = 58
      Width = 257
      Height = 17
      Caption = '&Hide window when executing a file'
      TabOrder = 2
    end
    object chkShowPreview: TCheckBox
      Left = 10
      Top = 76
      Width = 257
      Height = 17
      Caption = '&Show file preview pane'
      TabOrder = 3
    end
  end
  object btnOK: TButton
    Left = 126
    Top = 120
    Width = 74
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 206
    Top = 120
    Width = 74
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
