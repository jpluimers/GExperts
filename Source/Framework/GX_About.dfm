object fmAbout: TfmAbout
  Left = 285
  Top = 198
  ActiveControl = btnClose
  BorderStyle = bsDialog
  Caption = 'About GExperts'
  ClientHeight = 283
  ClientWidth = 614
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 14
  object lblGExperts: TLabel
    Left = 346
    Top = 19
    Width = 140
    Height = 20
    Alignment = taCenter
    AutoSize = False
    Caption = 'GExperts'
  end
  object lblVersion: TLabel
    Left = 346
    Top = 38
    Width = 140
    Height = 20
    Alignment = taCenter
    AutoSize = False
    Caption = 'Version ?.??'
  end
  object lblWebPage: TLabel
    Left = 358
    Top = 76
    Width = 147
    Height = 14
    Cursor = crHandPoint
    Alignment = taCenter
    Caption = 'http://www.gexperts.org/'
    OnClick = lblWebPageClick
  end
  object lblProjectLeader: TLabel
    Left = 264
    Top = 96
    Width = 84
    Height = 14
    Alignment = taRightJustify
    Caption = 'Project Leader:'
  end
  object lblContributors: TLabel
    Left = 244
    Top = 118
    Width = 104
    Height = 14
    Alignment = taRightJustify
    Caption = 'Major Contributors:'
  end
  object lblErik: TLabel
    Left = 358
    Top = 96
    Width = 189
    Height = 14
    Cursor = crHandPoint
    Caption = 'Erik Berry <eberry@gexperts.org>'
    OnClick = btnEmailClick
  end
  object lblWebSite: TLabel
    Left = 293
    Top = 76
    Width = 55
    Height = 14
    Alignment = taRightJustify
    Caption = 'Web Site:'
  end
  object lblPreRelease1: TLabel
    Left = 241
    Top = 30
    Width = 101
    Height = 19
    Alignment = taCenter
    AutoSize = False
    Caption = 'Pre-Release'
    Visible = False
  end
  object lblPreRelease2: TLabel
    Left = 489
    Top = 30
    Width = 101
    Height = 19
    Alignment = taCenter
    AutoSize = False
    Caption = 'Pre-Release'
    Visible = False
  end
  object mmoBuildDetails: TMemo
    Left = 224
    Top = 241
    Width = 385
    Height = 39
    Alignment = taCenter
    BorderStyle = bsNone
    Color = clBtnFace
    Ctl3D = False
    Lines.Strings = (
      'Experimental build from http://www.domain.com.  '
      'Please report all bugs to email@domain.com.')
    ParentCtl3D = False
    ReadOnly = True
    TabOrder = 4
    Visible = False
  end
  object pnlContributors: TPanel
    Left = 358
    Top = 118
    Width = 250
    Height = 122
    BevelOuter = bvNone
    Enabled = False
    TabOrder = 3
    object lbxContributors: TListBox
      Left = 0
      Top = 0
      Width = 250
      Height = 122
      TabStop = False
      Align = alClient
      BorderStyle = bsNone
      Columns = 2
      ItemHeight = 14
      Items.Strings = (
        'ArentJan Banck'
        'Jim Campbell'
        'Primoz Gabrijelcic'
        'Ulrich Gerhardt'
        'Benjamin Fournier'
        'John Hansen'
        'Taz Higgins'
        'Stefan Hoffmeister'
        'Rick Hollerich'
        'Per-Eric Larsson'
        'Piotr Likus'
        'Ray Lischner'
        'Gerald Nunn'
        'Alex Petrov'
        'Puthoon'
        'Martin Waldenburg')
      ParentColor = True
      TabOrder = 0
    end
  end
  object btnClose: TButton
    Left = 473
    Top = 243
    Width = 105
    Height = 26
    Cancel = True
    Caption = 'Close'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object pnlLogo: TPanel
    Left = 14
    Top = 15
    Width = 203
    Height = 254
    BevelOuter = bvLowered
    TabOrder = 2
    object imgLogo: TImage
      Left = 1
      Top = 1
      Width = 201
      Height = 252
      Center = True
      Proportional = True
      Stretch = True
    end
  end
  object btnEmail: TButton
    Left = 245
    Top = 243
    Width = 213
    Height = 26
    Caption = '&Send a Bug Report/Suggestion'
    TabOrder = 0
    OnClick = btnEmailClick
  end
end
