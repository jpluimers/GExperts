object fmClassProp: TfmClassProp
  Left = 311
  Top = 225
  Width = 479
  Height = 303
  ActiveControl = btnOK
  BorderIcons = [biSystemMenu]
  Caption = 'Class Properties'
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  DesignSize = (
    463
    265)
  PixelsPerInch = 96
  TextHeight = 14
  object pgeProperties: TPageControl
    Left = 8
    Top = 8
    Width = 449
    Height = 220
    ActivePage = tabProps
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabIndex = 0
    TabOrder = 0
    TabStop = False
    object tabProps: TTabSheet
      Caption = 'Class Properties'
      DesignSize = (
        441
        191)
      object lblClassName: TLabel
        Left = 26
        Top = 16
        Width = 59
        Height = 14
        Alignment = taRightJustify
        Caption = 'Class name'
        FocusControl = edtClassName
      end
      object lblDerivedFrom: TLabel
        Left = 15
        Top = 40
        Width = 70
        Height = 14
        Alignment = taRightJustify
        Caption = 'Derived from'
        FocusControl = edtDerivedFrom
      end
      object lblFileName: TLabel
        Left = 34
        Top = 104
        Width = 51
        Height = 14
        Alignment = taRightJustify
        Caption = 'File name'
        FocusControl = mmoFileName
      end
      object lblLineNumber: TLabel
        Left = 17
        Top = 160
        Width = 68
        Height = 14
        Alignment = taRightJustify
        Caption = 'Line number'
        FocusControl = edtLineNo
      end
      object lblUnit: TLabel
        Left = 63
        Top = 84
        Width = 22
        Height = 14
        Alignment = taRightJustify
        Caption = 'Unit'
        FocusControl = edtUnit
      end
      object edtClassName: TEdit
        Left = 93
        Top = 12
        Width = 337
        Height = 22
        Anchors = [akLeft, akTop, akRight]
        Color = clBtnFace
        ReadOnly = True
        TabOrder = 0
      end
      object edtDerivedFrom: TEdit
        Left = 93
        Top = 36
        Width = 337
        Height = 22
        Anchors = [akLeft, akTop, akRight]
        Color = clBtnFace
        ReadOnly = True
        TabOrder = 1
      end
      object edtLineNo: TEdit
        Left = 93
        Top = 156
        Width = 337
        Height = 22
        Anchors = [akLeft, akTop, akRight]
        Color = clBtnFace
        ReadOnly = True
        TabOrder = 4
      end
      object mmoFileName: TMemo
        Left = 93
        Top = 104
        Width = 337
        Height = 49
        Anchors = [akLeft, akTop, akRight]
        Color = clBtnFace
        ReadOnly = True
        TabOrder = 3
      end
      object edtUnit: TEdit
        Left = 93
        Top = 80
        Width = 337
        Height = 22
        Anchors = [akLeft, akTop, akRight]
        Color = clBtnFace
        ReadOnly = True
        TabOrder = 2
      end
    end
  end
  object btnOK: TButton
    Left = 382
    Top = 235
    Width = 75
    Height = 26
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
end
