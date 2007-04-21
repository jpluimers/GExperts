object fmClassProp: TfmClassProp
  Left = 227
  Top = 125
  ActiveControl = btnOK
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Class Properties'
  ClientHeight = 260
  ClientWidth = 371
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object pgeProperties: TPageControl
    Left = 8
    Top = 8
    Width = 356
    Height = 213
    ActivePage = tabProps
    TabOrder = 0
    TabStop = False
    object tabProps: TTabSheet
      Caption = 'Class Properties'
      object lblClassName: TLabel
        Left = 31
        Top = 16
        Width = 54
        Height = 13
        Alignment = taRightJustify
        Caption = 'Class name'
      end
      object lblDerivedFrom: TLabel
        Left = 25
        Top = 40
        Width = 60
        Height = 13
        Alignment = taRightJustify
        Caption = 'Derived from'
      end
      object lblFileName: TLabel
        Left = 40
        Top = 104
        Width = 45
        Height = 13
        Alignment = taRightJustify
        Caption = 'File name'
      end
      object lblLineNumber: TLabel
        Left = 27
        Top = 160
        Width = 58
        Height = 13
        Alignment = taRightJustify
        Caption = 'Line number'
      end
      object lblUnit: TLabel
        Left = 66
        Top = 84
        Width = 19
        Height = 13
        Alignment = taRightJustify
        Caption = 'Unit'
      end
      object edtClassName: TEdit
        Left = 93
        Top = 12
        Width = 248
        Height = 21
        Color = clBtnFace
        ReadOnly = True
        TabOrder = 0
      end
      object edtDerivedFrom: TEdit
        Left = 93
        Top = 36
        Width = 248
        Height = 21
        Color = clBtnFace
        ReadOnly = True
        TabOrder = 1
      end
      object edtLineNo: TEdit
        Left = 93
        Top = 156
        Width = 248
        Height = 21
        Color = clBtnFace
        ReadOnly = True
        TabOrder = 4
      end
      object mmoFileName: TMemo
        Left = 93
        Top = 104
        Width = 248
        Height = 49
        Color = clBtnFace
        ReadOnly = True
        TabOrder = 3
      end
      object edtUnit: TEdit
        Left = 93
        Top = 80
        Width = 248
        Height = 21
        Color = clBtnFace
        ReadOnly = True
        TabOrder = 2
      end
    end
  end
  object btnOK: TButton
    Left = 289
    Top = 228
    Width = 75
    Height = 26
    Cancel = True
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
end
