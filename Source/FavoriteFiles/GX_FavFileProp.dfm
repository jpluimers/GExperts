object fmFavFileProp: TfmFavFileProp
  Left = 288
  Top = 192
  BorderStyle = bsDialog
  Caption = 'File Properties'
  ClientHeight = 249
  ClientWidth = 464
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
  OnActivate = FormActivate
  OnCreate = FormCreate
  DesignSize = (
    464
    249)
  PixelsPerInch = 96
  TextHeight = 14
  object pgeProperties: TPageControl
    Left = 8
    Top = 8
    Width = 447
    Height = 201
    ActivePage = tabProperties
    Anchors = [akLeft, akTop, akRight]
    MultiLine = True
    TabIndex = 0
    TabOrder = 0
    TabStop = False
    object tabProperties: TTabSheet
      Caption = 'Properties'
      DesignSize = (
        439
        172)
      object lblFile: TLabel
        Left = 71
        Top = 12
        Width = 17
        Height = 14
        Alignment = taRightJustify
        Caption = '&File'
        FocusControl = edtFilename
      end
      object lblName: TLabel
        Left = 57
        Top = 76
        Width = 31
        Height = 14
        Alignment = taRightJustify
        Caption = '&Name'
        FocusControl = edtName
      end
      object lblDescription: TLabel
        Left = 28
        Top = 100
        Width = 60
        Height = 14
        Alignment = taRightJustify
        Caption = '&Description'
        FocusControl = edtDescription
      end
      object imgFileIcon: TImage
        Left = 96
        Top = 34
        Width = 32
        Height = 32
        Center = True
        Transparent = True
      end
      object lblIcon: TLabel
        Left = 64
        Top = 44
        Width = 24
        Height = 14
        Alignment = taRightJustify
        Caption = 'Icon'
      end
      object lblExecuteType: TLabel
        Left = 14
        Top = 124
        Width = 74
        Height = 14
        Alignment = taRightJustify
        Caption = 'Execute &type'
        FocusControl = cbxExecuteType
      end
      object lblExecuteUsing: TLabel
        Left = 11
        Top = 148
        Width = 77
        Height = 14
        Alignment = taRightJustify
        Caption = 'Execute &using'
        FocusControl = edtExecuteUsing
      end
      object sbnFile: TButton
        Left = 412
        Top = 8
        Width = 20
        Height = 20
        Anchors = [akTop, akRight]
        Caption = '...'
        TabOrder = 1
        OnClick = sbnFileClick
      end
      object sbnExecute: TButton
        Left = 412
        Top = 144
        Width = 20
        Height = 20
        Anchors = [akTop, akRight]
        Caption = '...'
        TabOrder = 6
        OnClick = sbnExecuteClick
      end
      object edtName: TEdit
        Left = 96
        Top = 72
        Width = 316
        Height = 22
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
      end
      object edtDescription: TEdit
        Left = 96
        Top = 96
        Width = 316
        Height = 22
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 3
      end
      object cbxExecuteType: TComboBox
        Left = 96
        Top = 120
        Width = 145
        Height = 22
        Style = csDropDownList
        ItemHeight = 14
        TabOrder = 4
        OnClick = cbxExecuteTypeClick
        Items.Strings = (
          'Load In IDE'
          'Shell Execute'
          'Custom'
          'Source Project')
      end
      object edtFilename: TEdit
        Left = 96
        Top = 8
        Width = 316
        Height = 22
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnExit = edtFilenameExit
      end
      object edtExecuteUsing: TEdit
        Left = 96
        Top = 144
        Width = 316
        Height = 22
        Anchors = [akLeft, akTop, akRight]
        Enabled = False
        TabOrder = 5
      end
    end
  end
  object btnCancel: TButton
    Left = 380
    Top = 216
    Width = 75
    Height = 26
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object btnOK: TButton
    Left = 299
    Top = 216
    Width = 75
    Height = 26
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
end
