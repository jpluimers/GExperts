object fmTabOrder: TfmTabOrder
  Left = 376
  Top = 207
  Width = 404
  Height = 400
  BorderIcons = [biSystemMenu]
  Caption = 'Set Tab Order'
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 300
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pnlButtons: TPanel
    Left = 294
    Top = 0
    Width = 102
    Height = 371
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    object btnOK: TButton
      Left = 1
      Top = 10
      Width = 95
      Height = 26
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object btnHelp: TButton
      Left = 1
      Top = 86
      Width = 95
      Height = 26
      Caption = '&Help'
      TabOrder = 2
      OnClick = btnHelpClick
    end
    object btnClose: TButton
      Left = 1
      Top = 41
      Width = 95
      Height = 26
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object btnOrderByPosition: TButton
      Left = 1
      Top = 129
      Width = 95
      Height = 25
      Caption = 'Order by &Position'
      TabOrder = 3
      OnClick = btnOrderByPositionClick
    end
    object btnResetOrder: TButton
      Left = 1
      Top = 161
      Width = 95
      Height = 25
      Caption = '&Reset Order'
      TabOrder = 4
      OnClick = btnResetOrderClick
    end
  end
  object pnlComponents: TPanel
    Left = 0
    Top = 0
    Width = 294
    Height = 371
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 5
    TabOrder = 0
    object gbxComponents: TGroupBox
      Left = 5
      Top = 5
      Width = 284
      Height = 361
      Align = alClient
      Caption = 'Arrange Components'
      TabOrder = 0
      DesignSize = (
        284
        361)
      object tvComps: TTreeView
        Left = 8
        Top = 17
        Width = 268
        Height = 336
        Anchors = [akLeft, akTop, akRight, akBottom]
        DragMode = dmAutomatic
        Indent = 19
        ReadOnly = True
        TabOrder = 0
        OnClick = tvCompsClick
        OnDragDrop = tvCompsDragDrop
        OnDragOver = tvCompsDragOver
        OnKeyUp = tvCompsKeyUp
      end
    end
  end
end
