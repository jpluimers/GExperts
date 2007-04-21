object fmFavFileProp: TfmFavFileProp
  Left = 288
  Top = 192
  BorderStyle = bsDialog
  Caption = 'File Properties'
  ClientHeight = 249
  ClientWidth = 453
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnActivate = FormActivate
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pgeProperties: TPageControl
    Left = 8
    Top = 8
    Width = 436
    Height = 201
    ActivePage = tabProperties
    MultiLine = True
    TabOrder = 0
    TabStop = False
    object tabProperties: TTabSheet
      Caption = 'Properties'
      object lblFile: TLabel
        Left = 72
        Top = 12
        Width = 16
        Height = 13
        Alignment = taRightJustify
        Caption = '&File'
        FocusControl = edtFilename
      end
      object lblName: TLabel
        Left = 60
        Top = 76
        Width = 28
        Height = 13
        Alignment = taRightJustify
        Caption = '&Name'
        FocusControl = edtName
      end
      object lblDescription: TLabel
        Left = 35
        Top = 100
        Width = 53
        Height = 13
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
        Left = 67
        Top = 44
        Width = 21
        Height = 13
        Alignment = taRightJustify
        Caption = 'Icon'
      end
      object lblExecuteType: TLabel
        Left = 26
        Top = 124
        Width = 62
        Height = 13
        Alignment = taRightJustify
        Caption = 'Execute &type'
        FocusControl = cbxExecuteType
      end
      object lblExecuteUsing: TLabel
        Left = 21
        Top = 148
        Width = 67
        Height = 13
        Alignment = taRightJustify
        Caption = 'Execute &using'
        FocusControl = edtExecuteUsing
      end
      object sbnFile: TButton
        Left = 401
        Top = 8
        Width = 20
        Height = 20
        Caption = '...'
        TabOrder = 1
        OnClick = sbnFileClick
      end
      object sbnExecute: TButton
        Left = 401
        Top = 144
        Width = 20
        Height = 20
        Caption = '...'
        TabOrder = 6
        OnClick = sbnExecuteClick
      end
      object edtName: TEdit
        Left = 96
        Top = 72
        Width = 305
        Height = 21
        TabOrder = 2
      end
      object edtDescription: TEdit
        Left = 96
        Top = 96
        Width = 305
        Height = 21
        TabOrder = 3
      end
      object cbxExecuteType: TComboBox
        Left = 96
        Top = 120
        Width = 145
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
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
        Width = 305
        Height = 21
        TabOrder = 0
        OnExit = edtFilenameExit
      end
      object edtExecuteUsing: TEdit
        Left = 96
        Top = 144
        Width = 305
        Height = 21
        Enabled = False
        TabOrder = 5
      end
    end
  end
  object btnCancel: TButton
    Left = 369
    Top = 216
    Width = 75
    Height = 26
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object btnOK: TButton
    Left = 288
    Top = 216
    Width = 75
    Height = 26
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
end
