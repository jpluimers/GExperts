unit GX_MessageBox;

interface

uses
  Classes, Controls, Forms, Dialogs, StdCtrls, ExtCtrls;

type
  TGxMsgBoxAdaptor = class(TObject)
  protected
    // General optional data passed into ShowGxMessageBox that can be
    // used to customize the error message, etc.
    FData: string;
    // Returns the message text shown in the message text
    function GetMessage: string; virtual; abstract;
    // Returns caption of the message box
    function GetCaption: string; virtual;
    // Returns the set of buttons that the message box
    // should feature.
    function GetButtons: TMsgDlgButtons; virtual;
    // Button to be used as the default
    function GetDefaultButton: TMsgDlgBtn; virtual;
    // Return a unique identifier for this dialog; this
    // is used for storage in the registry.
    // By default, this is the content of Self.ClassName
    function GetUniqueIdentifier: string;
    // Mark this message box as suppressed; by default,
    // state is stored in the registry.
    procedure DoPermanentlySuppress;
    // Returns whether this message box is permanently
    // suppressed.
    function IsPermanentlySuppressed: Boolean;
    // Returns whether the message box should be shown.
    function ShouldShow: Boolean; virtual;
    function AllowSuppress: Boolean; virtual;
    function ConfigurationKey: string;
  end;

  TfmGxMessageBox = class(TForm)
    chkNeverShowAgain: TCheckBox;
    bvlFrame: TBevel;
    mmoMessage: TMemo;
  end;

  TGxMsgBoxAdaptorClass = class of TGxMsgBoxAdaptor;

function ShowGxMessageBox(AdaptorClass: TGxMsgBoxAdaptorClass; const Data: string = ''): TModalResult;

implementation

{$R *.dfm}

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  SysUtils, Consts,
  GX_ConfigurationInfo, GX_GenericUtils;

const
  MsgDlgResults: array[TMsgDlgBtn] of Integer = (
    mrYes, mrNo, mrOk, mrCancel, mrAbort, mrRetry, mrIgnore, mrAll, mrNoToAll,
    mrYesToAll, 0);

  MsgDlgButtonCaptions: array[TMsgDlgBtn] of string = (
   SMsgDlgYes, SNoButton, SMsgDlgOK, SMsgDlgCancel, SMsgDlgAbort, SMsgDlgRetry,
   SMsgDlgIgnore, SMsgDlgAll, SMsgDlgNoToAll, SMsgDlgYesToAll, SMsgDlgHelp);

function ShowGxMessageBox(AdaptorClass: TGxMsgBoxAdaptorClass; const Data: string): TModalResult;
var
  Adaptor: TGxMsgBoxAdaptor;
  Dlg: TfmGxMessageBox;

  procedure CreateButtons;
  const
    SingleButtonWidth = 75;
    SingleButtonHeight = 25;
    ButtonHorizSpacing = 8;
    ButtonYPos = 208;
  var
    BtnType, DefaultBtn, CancelBtn: TMsgDlgBtn;
    DialogButtons: TMsgDlgButtons;
    ButtonRowWidth, NextButonXPos: Integer;
    CurrentButton: TButton;
  begin
    // Calculate the width of all buttons together
    ButtonRowWidth := 0;
    DialogButtons := Adaptor.GetButtons;
    for BtnType := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
      if BtnType in DialogButtons then
        Inc(ButtonRowWidth, SingleButtonWidth + ButtonHorizSpacing);
    Dec(ButtonRowWidth, ButtonHorizSpacing);
    if ButtonRowWidth > Dlg.ClientWidth then
      Dlg.ClientWidth := ButtonRowWidth;

    DefaultBtn := Adaptor.GetDefaultButton;

    if mbCancel in DialogButtons then
      CancelBtn := mbCancel
    else if mbNo in DialogButtons then
      CancelBtn := mbNo
    else
      CancelBtn := mbOK;

    NextButonXPos := (Dlg.ClientWidth - ButtonRowWidth) div 2;

    for BtnType := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
    begin
      if BtnType in DialogButtons then
      begin
        CurrentButton := TButton.Create(Dlg);
        with CurrentButton do
        begin
          Caption := MsgDlgButtonCaptions[BtnType];
          ModalResult := MsgDlgResults[BtnType];
          Parent := Dlg;
          TabOrder := 999;
          SetBounds(NextButonXPos, ButtonYPos, SingleButtonWidth, SingleButtonHeight);
          if BtnType = DefaultBtn then
          begin
            Default := True;
            Dlg.ActiveControl := CurrentButton;
          end;
          if BtnType = CancelBtn then
            Cancel := True;
        end;
        Inc(NextButonXPos, SingleButtonWidth + ButtonHorizSpacing);
      end;
    end;
  end;

begin
  Adaptor := AdaptorClass.Create;
  Adaptor.FData := Data;
  try
    Result := MsgDlgResults[Adaptor.GetDefaultButton];
    if Adaptor.IsPermanentlySuppressed then
      Exit;
    if Adaptor.ShouldShow then
    begin
      Dlg := TfmGxMessageBox.Create(nil);
      try
        SetDefaultFont(Dlg);
        Dlg.Caption := Adaptor.GetCaption;
        Dlg.chkNeverShowAgain.Enabled := Adaptor.AllowSuppress;
        Dlg.mmoMessage.Lines.Text := Adaptor.GetMessage;
        CreateButtons;
        Result := Dlg.ShowModal;
        if Adaptor.AllowSuppress and Dlg.chkNeverShowAgain.Checked then
          Adaptor.DoPermanentlySuppress;
      finally
        FreeAndNil(Dlg);
      end;
    end;
  finally
    FreeAndNil(Adaptor);
  end;
end;

{ TGxMsgBoxAdaptor }

function TGxMsgBoxAdaptor.GetCaption: string;
begin
  Result := 'GExperts Message';
end;

function TGxMsgBoxAdaptor.GetUniqueIdentifier: string;
begin
  Result := Self.ClassName;
end;

procedure TGxMsgBoxAdaptor.DoPermanentlySuppress;
begin
  if not AllowSuppress then
    Exit;
  with TGExpertsSettings.Create do
  try
    WriteBool(ConfigurationKey, Self.ClassName, True); // Do not localize
  finally
    Free;
  end;
end;

function TGxMsgBoxAdaptor.IsPermanentlySuppressed: Boolean;
begin
  with TGExpertsSettings.Create do
  try
    Result := ReadBool(ConfigurationKey, Self.ClassName, False); // Do not localize
  finally
    Free;
  end;
end;

function TGxMsgBoxAdaptor.ShouldShow: Boolean;
begin
  Result := True;
end;

function TGxMsgBoxAdaptor.GetButtons: TMsgDlgButtons;
begin
  Result := [mbOK];
end;

function TGxMsgBoxAdaptor.GetDefaultButton: TMsgDlgBtn;
const
  DefaultButton = mbOK;
begin
  Result := DefaultButton;

{$IFOPT D+}
  if not (DefaultButton in GetButtons) then
    SendDebugError('Message box "' + Self.ClassName + '" has a default button that is not available');
{$ENDIF D+}
end;

function TGxMsgBoxAdaptor.AllowSuppress: Boolean;
begin
  Result := True;
end;

function TGxMsgBoxAdaptor.ConfigurationKey: string;
begin
  Result := 'Misc' + PathDelim + 'SuppressedMessages';
end;

end.
