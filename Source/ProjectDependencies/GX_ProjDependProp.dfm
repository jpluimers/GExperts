object fmProjDependProp: TfmProjDependProp
  Left = 338
  Top = 216
  AutoScroll = False
  BorderIcons = [biSystemMenu]
  Caption = 'Dependency Properties'
  ClientHeight = 267
  ClientWidth = 304
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
  object pnlButtons: TPanel
    Left = 0
    Top = 235
    Width = 304
    Height = 32
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object pnlButtonsRight: TPanel
      Left = 119
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
    Width = 304
    Height = 235
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 6
    TabOrder = 0
    object pgeProperties: TPageControl
      Left = 6
      Top = 6
      Width = 292
      Height = 223
      ActivePage = tabProperties
      Align = alClient
      TabIndex = 0
      TabOrder = 0
      object tabProperties: TTabSheet
        Caption = 'Properties'
        DesignSize = (
          284
          194)
        object lblFileName: TLabel
          Left = 7
          Top = 20
          Width = 47
          Height = 14
          Caption = 'Filename'
          FocusControl = laFileName
        end
        object lblSource: TLabel
          Left = 7
          Top = 48
          Width = 38
          Height = 14
          Caption = '&Source'
          FocusControl = lbxSource
        end
        object laFileName: TStaticText
          Left = 64
          Top = 18
          Width = 204
          Height = 18
          Anchors = [akLeft, akTop, akRight]
          AutoSize = False
          BorderStyle = sbsSunken
          TabOrder = 0
        end
        object lbxSource: TListBox
          Left = 64
          Top = 48
          Width = 204
          Height = 134
          Anchors = [akLeft, akTop, akRight, akBottom]
          ItemHeight = 14
          Sorted = True
          TabOrder = 1
        end
      end
    end
  end
end
