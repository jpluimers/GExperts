object fmCompRenameAdvanced: TfmCompRenameAdvanced
  Left = 394
  Top = 238
  AutoScroll = False
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Additional Properties'
  ClientHeight = 319
  ClientWidth = 313
  Color = clBtnFace
  Constraints.MinHeight = 240
  Constraints.MinWidth = 240
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  Scaled = False
  DesignSize = (
    313
    319)
  PixelsPerInch = 96
  TextHeight = 13
  object lblComponentClass: TLabel
    Left = 8
    Top = 8
    Width = 80
    Height = 13
    Caption = 'ComponentClass'
  end
  object lblProperties: TLabel
    Left = 8
    Top = 24
    Width = 297
    Height = 33
    AutoSize = False
    Caption = 
      'Other Properties to Edit with Optional Default Value Examples: C' +
      'aption='#39'OK'#39' or Align=alNone or Hint'
    WordWrap = True
  end
  object btnOk: TButton
    Left = 146
    Top = 286
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 230
    Top = 286
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object mmoPropertyNames: TMemo
    Left = 8
    Top = 57
    Width = 297
    Height = 221
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
end
