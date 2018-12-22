object fmIdxPackageRenameDlg: TfmIdxPackageRenameDlg
  Left = 333
  Top = 237
  ActiveControl = ed_Description
  Width = 417
  Height = 151
  BorderIcons = [biSystemMenu]
  Caption = 'Set Package Description'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  DesignSize = (
    401
    113)
  PixelsPerInch = 96
  TextHeight = 13
  object l_PackageFn: TLabel
    Left = 8
    Top = 8
    Width = 224
    Height = 13
    Caption = 'Package Filename goes here (do not translate)'
  end
  object l_Description: TLabel
    Left = 8
    Top = 32
    Width = 53
    Height = 13
    Caption = 'Description'
  end
  object ed_Description: TEdit
    Left = 8
    Top = 48
    Width = 385
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object b_OK: TButton
    Left = 240
    Top = 80
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object b_Cancel: TButton
    Left = 320
    Top = 80
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
