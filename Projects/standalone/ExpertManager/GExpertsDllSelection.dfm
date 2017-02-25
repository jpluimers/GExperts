object frGEXpertsDllSelection: TfrGEXpertsDllSelection
  Left = 328
  Top = 228
  Width = 293
  Height = 303
  BorderIcons = [biSystemMenu]
  Caption = 'Select GExperts DLL'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  DesignSize = (
    277
    265)
  PixelsPerInch = 96
  TextHeight = 13
  object l_SelectDll: TLabel
    Left = 8
    Top = 8
    Width = 136
    Height = 13
    Caption = 'Select GExperts DLL to load:'
  end
  object lb_GExpertsDlls: TListBox
    Left = 8
    Top = 24
    Width = 177
    Height = 233
    Anchors = [akLeft, akTop, akBottom]
    ItemHeight = 13
    TabOrder = 0
    OnClick = lb_GExpertsDllsClick
    OnDblClick = lb_GExpertsDllsDblClick
  end
  object b_Load: TButton
    Left = 192
    Top = 200
    Width = 75
    Height = 25
    Caption = 'Load'
    Default = True
    Enabled = False
    TabOrder = 1
    OnClick = b_LoadClick
  end
  object b_Exit: TButton
    Left = 192
    Top = 232
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Exit'
    TabOrder = 2
    OnClick = b_ExitClick
  end
end
