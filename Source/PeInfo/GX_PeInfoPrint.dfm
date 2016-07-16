inherited f_PeInfoPrint: Tf_PeInfoPrint
  Left = 326
  Top = 228
  BorderStyle = bsDialog
  Caption = 'Print PE Information'
  ClientHeight = 225
  ClientWidth = 233
  ParentFont = False
  OldCreateOrder = True
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object chk_MsDosHeader: TCheckBox
    Left = 8
    Top = 8
    Width = 129
    Height = 17
    Caption = '&MS-DOS Header'
    TabOrder = 0
  end
  object chk_PeHeader: TCheckBox
    Left = 8
    Top = 32
    Width = 129
    Height = 17
    Caption = '&PE Header'
    TabOrder = 1
  end
  object chk_PeOptional: TCheckBox
    Left = 8
    Top = 56
    Width = 129
    Height = 17
    Caption = 'PE &Optional Header'
    TabOrder = 2
  end
  object chk_Exports: TCheckBox
    Left = 8
    Top = 104
    Width = 129
    Height = 17
    Caption = '&Exports'
    TabOrder = 4
  end
  object chk_Imports: TCheckBox
    Left = 8
    Top = 80
    Width = 129
    Height = 17
    Caption = '&Imports'
    TabOrder = 3
  end
  object chk_VersionInfo: TCheckBox
    Left = 8
    Top = 128
    Width = 129
    Height = 17
    Caption = '&Version Info'
    TabOrder = 5
  end
  object chk_PackageInfo: TCheckBox
    Left = 8
    Top = 152
    Width = 129
    Height = 17
    Caption = 'Pa&ckage Info'
    TabOrder = 6
  end
  object b_CheckAll: TButton
    Left = 152
    Top = 120
    Width = 75
    Height = 25
    Caption = 'Check &All'
    TabOrder = 7
    OnClick = b_CheckAllClick
  end
  object b_CheckNone: TButton
    Left = 152
    Top = 152
    Width = 75
    Height = 25
    Caption = 'Check &None'
    TabOrder = 8
    OnClick = b_CheckNoneClick
  end
  object b_Print: TButton
    Left = 72
    Top = 192
    Width = 75
    Height = 25
    Caption = 'Print'
    Default = True
    ModalResult = 1
    TabOrder = 9
  end
  object b_Cancel: TButton
    Left = 152
    Top = 192
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 10
  end
end
