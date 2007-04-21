object fmProjDependProp: TfmProjDependProp
  Left = 226
  Top = 127
  Width = 305
  Height = 384
  BorderIcons = [biSystemMenu]
  Caption = 'Dependency Properties'
  Color = clBtnFace
  Constraints.MinHeight = 260
  Constraints.MinWidth = 215
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object pgeProperties: TPageControl
    Left = 8
    Top = 8
    Width = 281
    Height = 309
    ActivePage = tabProperties
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object tabProperties: TTabSheet
      Caption = 'Properties'
      object lblFileName: TLabel
        Left = 7
        Top = 20
        Width = 42
        Height = 13
        Caption = 'Filename'
      end
      object lblSource: TLabel
        Left = 7
        Top = 48
        Width = 34
        Height = 13
        Caption = '&Source'
        FocusControl = lbxSource
      end
      object laFileName: TStaticText
        Left = 64
        Top = 18
        Width = 193
        Height = 18
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        BorderStyle = sbsSunken
        TabOrder = 0
      end
      object lbxSource: TListBox
        Left = 64
        Top = 48
        Width = 193
        Height = 221
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 13
        Sorted = True
        TabOrder = 1
      end
    end
  end
  object btnOK: TButton
    Left = 214
    Top = 324
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
end
