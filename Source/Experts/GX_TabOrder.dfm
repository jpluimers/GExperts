object fmTabOrder: TfmTabOrder
  Left = 376
  Top = 207
  AutoScroll = False
  BorderIcons = [biSystemMenu]
  Caption = 'Set Tab Order'
  ClientHeight = 477
  ClientWidth = 496
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 300
  Font.Charset = ANSI_CHARSET
  Font.Color = clBlack
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 14
  object pnlButtons: TPanel
    Left = 382
    Top = 0
    Width = 114
    Height = 477
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    object btnOK: TButton
      Left = 1
      Top = 10
      Width = 107
      Height = 26
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object btnHelp: TButton
      Left = 1
      Top = 86
      Width = 107
      Height = 26
      Caption = '&Help'
      TabOrder = 2
      OnClick = btnHelpClick
    end
    object btnClose: TButton
      Left = 1
      Top = 41
      Width = 107
      Height = 26
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object btnOrderByPosition: TButton
      Left = 1
      Top = 129
      Width = 107
      Height = 25
      Caption = 'Order by &Position'
      TabOrder = 3
      OnClick = btnOrderByPositionClick
    end
    object btnResetOrder: TButton
      Left = 1
      Top = 161
      Width = 107
      Height = 25
      Caption = '&Reset Order'
      TabOrder = 4
      OnClick = btnResetOrderClick
    end
  end
  object pnlComponents: TPanel
    Left = 0
    Top = 0
    Width = 382
    Height = 477
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 6
    TabOrder = 0
    object gbxComponents: TGroupBox
      Left = 6
      Top = 6
      Width = 370
      Height = 465
      Align = alClient
      Caption = 'Arrange Components (Drag and Drop)'
      TabOrder = 0
      object pnlComponentTree: TPanel
        Left = 2
        Top = 15
        Width = 366
        Height = 448
        Align = alClient
        BevelOuter = bvNone
        BorderWidth = 6
        TabOrder = 0
        object tvComps: TTreeView
          Left = 6
          Top = 6
          Width = 354
          Height = 436
          Align = alClient
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
end
