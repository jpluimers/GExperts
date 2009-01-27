object fmSetComponentPropsStatus: TfmSetComponentPropsStatus
  Left = 433
  Top = 371
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Set Component Properties Status'
  ClientHeight = 65
  ClientWidth = 517
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnClose = FormClose
  DesignSize = (
    517
    65)
  PixelsPerInch = 96
  TextHeight = 14
  object grpbxStatus: TGroupBox
    Left = 8
    Top = 8
    Width = 497
    Height = 49
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Processing File'
    TabOrder = 0
    DesignSize = (
      497
      49)
    object pnlProcessedFile: TPanel
      Left = 8
      Top = 16
      Width = 481
      Height = 25
      Anchors = [akLeft, akTop, akRight]
      BevelOuter = bvNone
      TabOrder = 0
    end
  end
end
