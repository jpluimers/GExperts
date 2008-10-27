object fmProjDependProp: TfmProjDependProp
  Left = 338
  Top = 216
  Width = 369
  Height = 456
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
  object pnlButtons: TPanel
    Left = 0
    Top = 388
    Width = 353
    Height = 32
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object pnlButtonsRight: TPanel
      Left = 168
      Top = 0
      Width = 185
      Height = 32
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object btnOK: TButton
        Left = 103
        Top = 2
        Width = 75
        Height = 25
        Cancel = True
        Caption = 'OK'
        Default = True
        ModalResult = 1
        TabOrder = 0
      end
    end
  end
  object pnlContent: TPanel
    Left = 0
    Top = 0
    Width = 353
    Height = 388
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 6
    TabOrder = 0
    object pgeProperties: TPageControl
      Left = 6
      Top = 6
      Width = 341
      Height = 376
      ActivePage = tabProperties
      Align = alClient
      TabIndex = 0
      TabOrder = 0
      object tabProperties: TTabSheet
        Caption = 'Properties'
        DesignSize = (
          333
          348)
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
          Width = 253
          Height = 18
          Anchors = [akLeft, akTop, akRight]
          AutoSize = False
          BorderStyle = sbsSunken
          TabOrder = 0
        end
        object lbxSource: TListBox
          Left = 64
          Top = 48
          Width = 253
          Height = 288
          Anchors = [akLeft, akTop, akRight, akBottom]
          ItemHeight = 13
          Sorted = True
          TabOrder = 1
        end
      end
    end
  end
end
