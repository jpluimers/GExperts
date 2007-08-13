object fmAbout: TfmAbout
  Left = 235
  Top = 173
  HorzScrollBar.Visible = False
  ActiveControl = btnClose
  BorderStyle = bsDialog
  Caption = 'About GExperts'
  ClientHeight = 272
  ClientWidth = 566
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
  PixelsPerInch = 96
  TextHeight = 13
  object lblGExperts: TLabel
    Left = 317
    Top = 19
    Width = 140
    Height = 20
    Alignment = taCenter
    AutoSize = False
    Caption = 'GExperts'
  end
  object lblVersion: TLabel
    Left = 317
    Top = 38
    Width = 140
    Height = 20
    Alignment = taCenter
    AutoSize = False
    Caption = 'Version ?.??'
  end
  object lblWebPage: TLabel
    Left = 350
    Top = 79
    Width = 121
    Height = 13
    Cursor = crHandPoint
    Alignment = taCenter
    Caption = 'http://www.gexperts.org/'
    OnClick = lblWebPageClick
  end
  object lblProjectLeader: TLabel
    Left = 268
    Top = 99
    Width = 72
    Height = 13
    Alignment = taRightJustify
    Caption = 'Project Leader:'
  end
  object lblContributors: TLabel
    Left = 252
    Top = 121
    Width = 88
    Height = 13
    Alignment = taRightJustify
    Caption = 'Major Contributors:'
  end
  object lblErik: TLabel
    Left = 350
    Top = 99
    Width = 158
    Height = 13
    Cursor = crHandPoint
    Caption = 'Erik Berry <eberry@gexperts.org>'
    OnClick = btnEmailClick
  end
  object lblWebSite: TLabel
    Left = 293
    Top = 79
    Width = 47
    Height = 13
    Alignment = taRightJustify
    Caption = 'Web Site:'
  end
  object lblPreRelease1: TLabel
    Left = 212
    Top = 30
    Width = 101
    Height = 19
    Alignment = taCenter
    AutoSize = False
    Caption = 'Pre-Release'
    Visible = False
  end
  object lblPreRelease2: TLabel
    Left = 460
    Top = 30
    Width = 101
    Height = 19
    Alignment = taCenter
    AutoSize = False
    Caption = 'Pre-Release'
    Visible = False
  end
  object mmoBuildDetails: TMemo
    Left = 216
    Top = 230
    Width = 233
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
    Left = 350
    Top = 121
    Width = 213
    Height = 110
    BevelOuter = bvNone
    Enabled = False
    TabOrder = 3
    object lbxContributors: TListBox
      Left = 0
      Top = 0
      Width = 213
      Height = 110
      TabStop = False
      Align = alClient
      BorderStyle = bsNone
      Columns = 2
      ItemHeight = 13
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
    Left = 452
    Top = 238
    Width = 105
    Height = 26
    Cancel = True
    Caption = 'Close'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object pnlLogo: TPanel
    Left = 7
    Top = 10
    Width = 203
    Height = 254
    BevelOuter = bvLowered
    TabOrder = 2
    object imgLogo: TImage
      Left = 1
      Top = 1
      Width = 201
      Height = 252
      Stretch = True
    end
  end
  object btnEmail: TButton
    Left = 224
    Top = 238
    Width = 213
    Height = 26
    Caption = '&Send a Bug Report/Suggestion'
    TabOrder = 0
    OnClick = btnEmailClick
  end
end
