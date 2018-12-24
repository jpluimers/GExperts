unit GX_MessageDialog;

{$I GX_CondDefine.inc}

interface

uses
  Classes, Graphics, Controls, Forms, StdCtrls, ExtCtrls, ComCtrls, GX_BaseForm,
  GX_MemoEscFix;

type
  TMemo = class(TMemoEscFix)
  end;

type
  TSourceType = (stPascal, stCpp);
  TDialogBoxType = (dlgMessageBox, dlgMessageDlg);
  TGnuGetTextFunction = (ggtUnderscore, ggtGetText);

  TAbstractMessageType = class(TObject)
  protected
    FCommand: string;
    FButtons: string;
    FCaption: string;
    FMessage: string;
    FHelpContext: Integer;
    FDialogType: string;
    FParameterFormat: string;
    FButtonSeparator: string;
    FQuoteCaption: Boolean;
    FQuoteCharacter: string;
    FGnuGetTextFunction: TGnuGetTextFunction;
    FGnuGetTextSupport: Boolean;
  public
    procedure ShowModal; virtual; abstract;
    function GetCode: string; virtual; abstract;
    property Command: string read FCommand;
    property Buttons: string read FButtons write FButtons;
    property Caption: string read FCaption write FCaption;
    property Message: string read FMessage write FMessage;
    property HelpContext: Integer read FHelpContext write FHelpContext;
    property DialogType: string read FDialogType write FDialogType;
    property ParameterFormat: string read FParameterFormat write FParameterFormat;
    property ButtonSeparator: string read FButtonSeparator write FButtonSeparator;
    property QuoteCaption: Boolean read FQuoteCaption write FQuoteCaption;
    property QuoteCharacter: string read FQuoteCharacter write FQuoteCharacter;
    property GnuGetTextFunction: TGnuGetTextFunction read FGnuGetTextFunction write FGnuGetTextFunction;
    property GnuGetTextSupport: Boolean read FGnuGetTextSupport write FGnuGetTextSupport;
  end;

  TMessageDialogSettings = class(TObject)
  private
    FConcatenationString: string;
    FCppConcatenationString: string;
    FGnuGetTextFunction: TGnuGetTextFunction;
    FGnuGetTextIndividual: Boolean;
    FSourceType: TSourceType;
  public
    constructor Create;
    property ConcatenationString: string read FConcatenationString write FConcatenationString;
    property CppConcatenationString: string read FCppConcatenationString write FCppConcatenationString;
    property GnuGetTextFunction: TGnuGetTextFunction read FGnuGetTextFunction write FGnuGetTextFunction;
    property GnuGetTextIndividual: Boolean read FGnuGetTextIndividual write FGnuGetTextIndividual;
    property SourceType: TSourceType read FSourceType write FSourceType;
  end;

  TfmMessageDialog = class(TfmBaseForm)
    chkDefaultButton: TCheckBox;
    chkMbAbort: TCheckBox;
    chkMbAll: TCheckBox;
    chkMbCancel: TCheckBox;
    chkMbHelp: TCheckBox;
    chkMbIgnore: TCheckBox;
    chkMbNo: TCheckBox;
    chkMbNoToAll: TCheckBox;
    chkMbOK: TCheckBox;
    chkMbRetry: TCheckBox;
    chkMbYes: TCheckBox;
    chkMbYesToAll: TCheckBox;
    chkMrAbort: TCheckBox;
    chkMrAll: TCheckBox;
    chkMrCancel: TCheckBox;
    chkMrIgnore: TCheckBox;
    chkMrNo: TCheckBox;
    chkMrNone: TCheckBox;
    chkMrOK: TCheckBox;
    chkMrRetry: TCheckBox;
    chkMrYes: TCheckBox;
    rbnAbortRetryIgnore: TRadioButton;
    chkYes: TCheckBox;
    chkNo: TCheckBox;
    chkOk: TCheckBox;
    chkCancel: TCheckBox;
    chkAbort: TCheckBox;
    chkRetry: TCheckBox;
    chkIgnore: TCheckBox;
    rbnOk: TRadioButton;
    chkDefaultDesktopOnly: TCheckBox;
    chkHelp: TCheckBox;
    chkRight: TCheckBox;
    chkRtlReading: TCheckBox;
    chkSetForeground: TCheckBox;
    chkTopMost: TCheckBox;
    rbnOkCancel: TRadioButton;
    chkServiceNotification: TCheckBox;
    chkServiceNotificationNt3x: TCheckBox;
    rbnRetryCancel: TRadioButton;
    rbnYesNo: TRadioButton;
    rbnYesNoCancel: TRadioButton;
    gbxButtons: TGroupBox;
    gbxDlgFunctionResults: TGroupBox;
    gbxMsgBoxDialogButtons: TGroupBox;
    gbxMsgBoxFunctionResults: TGroupBox;
    gbxMsgBoxMisc: TGroupBox;
    gbxMsgBoxModality: TGroupBox;
    gbxMsgBoxTypes: TGroupBox;
    gbxTypes: TGroupBox;
    imgInformationMsgBox: TImage;
    imgErrorMsgBox: TImage;
    imgConfirmationMsgBox: TImage;
    imgConfirmation: TImage;
    imgError: TImage;
    imgInformation: TImage;
    imgMsgBoxWarning: TImage;
    imgWarning: TImage;
    tabMessageBox: TTabSheet;
    pgeMessageDialog: TPageControl;
    tabMessageDlg: TTabSheet;
    rbnIconWarning: TRadioButton;
    rbnIconInformation: TRadioButton;
    rbnIconQuestion: TRadioButton;
    rbnIconStop: TRadioButton;
    rbnIconExclamation: TRadioButton;
    rbnIconAsterisk: TRadioButton;
    rbnIconError: TRadioButton;
    rbnIconHand: TRadioButton;
    rbnConfirmation: TRadioButton;
    rbnCustom: TRadioButton;
    rbnError: TRadioButton;
    rbnInformation: TRadioButton;
    rbnWarning: TRadioButton;
    pgeMessageBoxOptions: TPageControl;
    tabOptions: TTabSheet;
    tabAdvanced: TTabSheet;
    gbxCaption: TGroupBox;
    chkQuoteCaption: TCheckBox;
    edtMsgBoxCaption: TEdit;
    pnlMessageDlg: TPanel;
    cbxModality: TComboBox;
    chkNoFocus: TCheckBox;
    pnlButtons: TPanel;
    pnlMain: TPanel;
    chkMrNoToAll: TCheckBox;
    chkMrYesToAll: TCheckBox;
    edtDefaultButton: TEdit;
    udDefaultButton: TUpDown;
    pnlMessage: TPanel;
    mmoMessage: TMemo;
    pnlMessageTop: TPanel;
    chkQuotes: TCheckBox;
    chkGNUGettext: TCheckBox;
    lblMessage: TLabel;
    pnlButtonsRight: TPanel;
    btnHelp: TButton;
    btnCancel: TButton;
    btnOK: TButton;
    btnCopyToClipboard: TButton;
    btnTest: TButton;
    pnlSettings: TPanel;
    edtHelpContext: TEdit;
    udHelpContext: TUpDown;
    lblHelpContext: TLabel;
    cbxTypeEmbedded: TComboBox;
    lblEmbed: TLabel;
    chkDefaults: TCheckBox;
    procedure btnCopyToClipboardClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
    procedure cbDialogButtonsOrIfStatementClick(Sender: TObject);
    procedure mmoMessageKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure pgeMessageDialogChange(Sender: TObject);
    procedure cbxTypeEmbeddedChange(Sender: TObject);
    procedure chkQuotesClick(Sender: TObject);
  private
    FFunctionResultsGroupBox: TGroupBox;
    FMessageType: TAbstractMessageType;
    FSettings: TMessageDialogSettings;
    FUsesUnit: string;
    FUsesUnitCLX: string;
    function AssembleGroupBoxCheckBoxesText(GroupBox: TGroupBox): string;
    function AssembleGroupBoxRadioButtonsText(GroupBox: TGroupBox): string;
    procedure DistributeGroupBoxCheckBoxesText(const Value: string; GroupBox: TGroupBox);
    procedure DistributeGroupBoxRadioButtonsText(const Value: string; GroupBox: TGroupBox);
    procedure FixControlsEnablement(const CheckResults: Boolean);
    function GetAllDialogButtons: string;
    function GetAllFunctionResults: string;
    function GetCaptionQuotes: Boolean;
    function GetDefaultButton: string;
    function GetDefaultButtonCheckbox: Boolean;
    function GetDialogButtons: string;
    function GetDialogHelpContext: Integer;
    function GetDialogType: string;
    function GetEmbedSelection: Integer;
    function GetFunctionResults: string;
    function GetGeneratedCode: string;
    function GetQuotes: Boolean;
    procedure LoadEmbedList;
    procedure LoadSettings;
    procedure PopulateMessageType;
    procedure SaveSettings;
    procedure SetAllDialogButtons(const SelectedButtons: string);
    procedure SetAllFunctionResults(const SelectedFunctionResults: string);
    procedure SetCaptionQuotes(const Value: Boolean);
    procedure SetDefaultButton(const Value: string);
    procedure SetDefaultButtonCheckbox(const Value: Boolean);
    procedure SetDialogButtons(const SelectedButtons: string);
    procedure SetDialogHelpContext(const Value: Integer);
    procedure SetDialogType(const TypeText: string);
    procedure SetEmbedSelection(const Value: Integer);
    procedure SetFunctionResults(const SelectedFunctionResults: string);
    procedure SetQuotes(const Value: Boolean);
    function ConfigurationKey: string;
    function GetDialogBoxType: string;
    procedure SetDialogBoxType(const TypeText: string);
    function GetModality: string;
    procedure SetModality(const Value: string);
    function ModalityString: string;
    function GetGNUGettextSupport: Boolean;
    procedure SetGNUGettextSupport(const Value: Boolean);

    property AllDialogButtons: string read GetAllDialogButtons write SetAllDialogButtons;
    property AllFunctionResults: string read GetAllFunctionResults write SetAllFunctionResults;
    property CaptionQuotes: Boolean read GetCaptionQuotes write SetCaptionQuotes;
    property DefaultButton: string read GetDefaultButton write SetDefaultButton;
    property DefaultButtonCheckbox: Boolean read GetDefaultButtonCheckbox write SetDefaultButtonCheckbox;
    property DialogButtons: string read GetDialogButtons write SetDialogButtons;
    property Modality: string read GetModality write SetModality;
    property DialogHelpContext: Integer read GetDialogHelpContext write SetDialogHelpContext;
    property DialogType: string read GetDialogType write SetDialogType;
    property EmbedSelection: Integer read GetEmbedSelection write SetEmbedSelection;
    property FunctionResults: string read GetFunctionResults write SetFunctionResults;
    property Quotes: Boolean read GetQuotes write SetQuotes;
    property GNUGettextSupport: Boolean read GetGNUGettextSupport write SetGNUGettextSupport;
    property DialogBoxType: string read GetDialogBoxType write SetDialogBoxType;
  public
    constructor Create(AOwner: TComponent; Settings: TMessageDialogSettings); reintroduce;
    destructor Destroy; override;
    property GeneratedCode: string read GetGeneratedCode;
    property MessageType: TAbstractMessageType read FMessageType write FMessageType;
    property UsesUnit: string read FUsesUnit write FUsesUnit;
    property UsesUnitCLX: string read FUsesUnitCLX write FUsesUnitCLX;
  end;

implementation

{$R *.dfm}

uses
  SysUtils, Windows, TypInfo, Menus, ActnList, ClipBrd, Dialogs,
  GX_GxUtils, GX_GenericUtils, GX_OtaUtils, GX_UsesManager,
  GX_MessageOptions, GX_ConfigurationInfo, GX_Experts;

type
  TMsgExpExpert = class(TGX_Expert)
  private
    FSettings: TMessageDialogSettings;
  protected
    procedure UpdateAction(Action: TCustomAction); override;
    procedure InternalLoadSettings(Settings: TExpertSettings); override;
    procedure InternalSaveSettings(Settings: TExpertSettings); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetActionCaption: string; override;
    function GetDefaultShortCut: TShortCut; override;
    class function GetName: string; override;
    procedure Execute(Sender: TObject); override;
    procedure Configure; override;
  end;

  TMessageBoxType = class(TAbstractMessageType)
  private
    FBoxButtonsList: TStringList;
    FParent: TCustomForm;
  public
    constructor Create(Parent: TCustomForm);
    destructor Destroy; override;
    function ButtonValue: Integer;
    procedure ShowModal; override;
    function GetCode: string; override;
  end;

  TMessageDialogType = class(TAbstractMessageType)
  private
    function GetMsgDlgButtons: TMsgDlgButtons;
    function GetMsgDlgType: TMsgDlgType;
  public
    constructor Create;
    procedure ShowModal; override;
    function GetCode: string; override;
  end;

  TAbstractMessageDialogBuilder = class(TObject)
  private
    FDialogTypText: string;
    FText: TStrings;
    FHelpContext: Integer;
    FConcatString: string;
    FCppConcatString: string;
    FFunctionResults: TStrings;
    FQuotes: Boolean;
    FEmbedIndex: Integer;
    FMessageType: TAbstractMessageType;
    FUsesUnit: string;
    FUsesUnitCLX: string;
    FGNUGettextSupport: Boolean;
    FGnuGettextFunction: TGnuGetTextFunction;
    FGnuGettextIndividual: Boolean;
    procedure SetDialogTypText(const Value: string);
    procedure SetText(const Value: TStrings);
    procedure SetFunctionResults(const Value: TStrings);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function GetCode: string; virtual; abstract;
    function GetEmbeddedList: string; virtual; abstract;
    property FunctionResults: TStrings read FFunctionResults write SetFunctionResults;
    property ConcatString: string read FConcatString write FConcatString;
    property CppConcatString: string read FCppConcatString write FCppConcatString;
    property DialogTypText: string read FDialogTypText write SetDialogTypText;
    property HelpContext: Integer read FHelpContext write FHelpContext;
    property Text: TStrings read FText write SetText;
    property Quotes: Boolean read FQuotes write FQuotes;
    property GNUGettextSupport: Boolean read FGNUGettextSupport write FGNUGettextSupport;
    property GnuGettextFunction: TGnuGetTextFunction read FGnuGettextFunction write FGnuGettextFunction;
    property GnuGettextIndividual: Boolean read FGnuGettextIndividual write FGnuGettextIndividual;
    property EmbedIndex: Integer read FEmbedIndex write FEmbedIndex;
    property MessageType: TAbstractMessageType read FMessageType write FMessageType;
    property UsesUnit: string read FUsesUnit write FUsesUnit;
    property UsesUnitCLX: string read FUsesUnitCLX write FUsesUnitCLX;
  end;

  TMessageDialogBuilderClass = class of TAbstractMessageDialogBuilder;

  TCppMessageDialogBuilder = class(TAbstractMessageDialogBuilder)
  public
    function GetCode: string; override;
    function GetEmbeddedList: string; override;
  end;

  TPascalMessageDialogBuilder = class(TAbstractMessageDialogBuilder)
  public
    function GetCode: string; override;
    function GetEmbeddedList: string; override;
  end;

{ TfmMessageDialog }

constructor TfmMessageDialog.Create(AOwner: TComponent; Settings: TMessageDialogSettings);
begin
  inherited Create(AOwner);
  FMessageType := nil;
  pgeMessageDialogChange(Self);
  FSettings := Settings;
  LoadEmbedList;
  LoadSettings;
  FixControlsEnablement(False);
  imgMsgBoxWarning.Picture := imgWarning.Picture;
  imgErrorMsgBox.Picture := imgError.Picture;
  imgInformationMsgBox.Picture := imgInformation.Picture;
  imgConfirmationMsgBox.Picture := imgConfirmation.Picture;
  pgeMessageBoxOptions.ActivePageIndex := 0;

  if FSettings.GnuGetTextFunction = ggtUnderscore then
    chkGNUGettext.Caption := 'Add _(...) for &GNU Gettext'
  else
    chkGNUGettext.Caption := 'Add GetText(...) for &GNU Gettext'
end;

function TfmMessageDialog.AssembleGroupBoxCheckBoxesText(GroupBox: TGroupBox): string;
var
  i: Integer;
  CheckBox: TCheckBox;
  List: TStringList;
begin
  List := TStringList.Create;
  try
    for i := 0 to GroupBox.ControlCount - 1 do
    begin
      if GroupBox.Controls[i] is TCheckBox then
      begin
        CheckBox := TCheckBox(GroupBox.Controls[i]);
        if CheckBox.Checked then
          List.Add(StripHotKey(CheckBox.Caption));
      end;
    end;
    Result := List.CommaText;
  finally
    FreeAndNil(List);
  end;
end;

function TfmMessageDialog.AssembleGroupBoxRadioButtonsText(GroupBox: TGroupBox): string;
var
  i: Integer;
  RadioButton: TRadioButton;
begin
  Result := '';
  for i := 0 to GroupBox.ControlCount - 1 do
    if GroupBox.Controls[i] is TRadioButton then
    begin
      RadioButton := TRadioButton(GroupBox.Controls[i]);
      if RadioButton.Checked then
      begin
        Result := StripHotKey(RadioButton.Caption);
        Break;
      end;
    end;
end;

procedure TfmMessageDialog.btnCopyToClipboardClick(Sender: TObject);
begin
  Clipboard.AsText := GetGeneratedCode;
end;

procedure TfmMessageDialog.btnHelpClick(Sender: TObject);
begin
  GxContextHelp(Self, 9); // 9 = Help context for this dialog
end;

procedure TfmMessageDialog.btnTestClick(Sender: TObject);
begin
  FMessageType.Message := mmoMessage.Text;
  PopulateMessageType;
  FMessageType.ShowModal;
end;

procedure TfmMessageDialog.cbDialogButtonsOrIfStatementClick(Sender: TObject);
begin
  FixControlsEnablement(True);
end;

procedure TfmMessageDialog.DistributeGroupBoxCheckBoxesText(const Value: string; GroupBox: TGroupBox);
var
  i: Integer;
  CheckBox: TCheckBox;
begin
  with TStringList.Create do
  try
    CommaText := AnsiUpperCase(Value);
    for i := 0 to GroupBox.ControlCount - 1 do
      if GroupBox.Controls[i] is TCheckBox then
      begin
        CheckBox := TCheckBox(GroupBox.Controls[i]);
        if IndexOf(StripHotKey(CheckBox.Caption)) > -1 then
          CheckBox.Checked := True
        else
          CheckBox.Checked := False;
      end;
  finally
    Free;
  end;
end;

procedure TfmMessageDialog.DistributeGroupBoxRadioButtonsText(const Value: string; GroupBox: TGroupBox);
var
  i: Integer;
  RadioButton: TRadioButton;
begin
  with TStringList.Create do
  try
    CommaText := AnsiUpperCase(Value);
    for i := 0 to GroupBox.ControlCount - 1 do
    begin
      if GroupBox.Controls[i] is TRadioButton then
      begin
        RadioButton := TRadioButton(GroupBox.Controls[i]);
        if IndexOf(StripHotKey(RadioButton.Caption)) > -1 then
        begin
          RadioButton.Checked := True;
          Break;
        end;
      end;
    end;
  finally
    Free;
  end;
end;

procedure TfmMessageDialog.FixControlsEnablement(const CheckResults: Boolean);
var
  EnableControlIndex: Integer;
  Index: Integer;
  LocalButtons: TGroupBox;
  Control: TControl;
begin
  if pgeMessageDialog.ActivePage = tabMessageDlg then
    LocalButtons := gbxButtons
  else
    LocalButtons := gbxMsgBoxDialogButtons;
  FFunctionResultsGroupBox.Enabled := (Trim(cbxTypeEmbedded.Text) <> '');

  EnableControlIndex := 0;
  for Index := 0 to LocalButtons.ControlCount - 1 do
  begin
    Control := LocalButtons.Controls[Index];
    if (Control is TCheckBox) and ((Control as TCheckBox).Checked) then
      EnableControlIndex := EnableControlIndex or  LocalButtons.Controls[Index].Tag
    else if (Control is TRadioButton) and ((Control as TRadioButton).Checked) then
      EnableControlIndex := EnableControlIndex or LocalButtons.Controls[Index].Tag;
  end;
  for Index := 0 to FFunctionResultsGroupBox.ControlCount - 1 do
  begin
    Control := FFunctionResultsGroupBox.Controls[Index];
    Control.Enabled := (FFunctionResultsGroupBox.Enabled and (((EnableControlIndex and Control.Tag) > 0) or (Control.Name = 'chkMrNone')));
    if CheckResults then
      (Control as TCheckBox).Checked := Control.Enabled;
  end;
end;

function TfmMessageDialog.GetAllDialogButtons: string;
begin
  Result := AssembleGroupBoxCheckBoxesText(gbxButtons) + ',' +
    AssembleGroupBoxRadioButtonsText(gbxMsgBoxDialogButtons) + ',' +
    AssembleGroupBoxCheckBoxesText(gbxMsgBoxMisc);
end;

function TfmMessageDialog.GetAllFunctionResults: string;
begin
  Result := AssembleGroupBoxCheckBoxesText(gbxDlgFunctionResults);
  Result := Result + ',' + AssembleGroupBoxCheckBoxesText(gbxMsgBoxFunctionResults);
end;

function TfmMessageDialog.GetCaptionQuotes: Boolean;
begin
  Result := chkQuoteCaption.Checked;
end;

function TfmMessageDialog.GetDefaultButton: string;
begin
  Result := IntToStr(udDefaultButton.Position);
end;

function TfmMessageDialog.GetDefaultButtonCheckbox: Boolean;
begin
  Result := chkDefaultButton.Checked;
end;

function TfmMessageDialog.GetDialogButtons: string;
begin
  Result := AssembleGroupBoxCheckBoxesText(gbxButtons)
end;

function TfmMessageDialog.GetDialogHelpContext: Integer;
begin
  Result := udHelpContext.Position;
end;

function TfmMessageDialog.GetDialogType: string;
begin
  Result := AssembleGroupBoxRadioButtonsText(gbxTypes);
end;

function TfmMessageDialog.GetEmbedSelection: Integer;
begin
  Result := cbxTypeEmbedded.ItemIndex;
end;

function TfmMessageDialog.GetFunctionResults: string;
begin
  Result := AssembleGroupBoxCheckBoxesText(FFunctionResultsGroupBox);
end;

function TfmMessageDialog.GetGeneratedCode: string;
var
  BuilderClass: TMessageDialogBuilderClass;
  Builder: TAbstractMessageDialogBuilder;
begin
  case FSettings.SourceType of
    stPascal: BuilderClass := TPascalMessageDialogBuilder;
    stCpp: BuilderClass := TCppMessageDialogBuilder;
  else
    raise Exception.Create('Invalid source code type');
  end;

  PopulateMessageType;

  Builder := BuilderClass.Create;
  try
    Builder.DialogTypText := DialogType;
    Builder.FunctionResults.CommaText := FunctionResults;
    Builder.HelpContext := DialogHelpContext;
    Builder.ConcatString := FSettings.ConcatenationString;
    Builder.CppConcatString := FSettings.CppConcatenationString;
    Builder.Text := mmoMessage.Lines;
    Builder.Quotes := chkQuotes.Checked;
    Builder.GNUGettextSupport := chkGNUGettext.Checked;
    Builder.GnuGettextFunction := FSettings.GnuGetTextFunction;
    Builder.GnuGettextIndividual := FSettings.GnuGetTextIndividual;
    Builder.EmbedIndex := cbxTypeEmbedded.ItemIndex;
    Builder.MessageType := FMessageType;
    Result := Builder.GetCode;
    FUsesUnit := Builder.UsesUnit;
    FUsesUnitCLX := Builder.UsesUnitCLX;
  finally
    FreeAndNil(Builder);
  end;
end;

function TfmMessageDialog.GetGNUGettextSupport: Boolean;
begin
  Result := chkGNUGettext.Checked;
end;

function TfmMessageDialog.GetQuotes: Boolean;
begin
  Result := chkQuotes.Checked;
end;

procedure TfmMessageDialog.LoadEmbedList;
var
  BuilderClass: TMessageDialogBuilderClass;
  Builder: TAbstractMessageDialogBuilder;
  EmbedList: TStringList;
begin
  case FSettings.SourceType of
    stPascal: BuilderClass := TPascalMessageDialogBuilder;
    stCpp   : BuilderClass := TCppMessageDialogBuilder;
  else
    raise Exception.Create('Invalid source code type');
  end;

  EmbedList := nil;
  Builder := BuilderClass.Create;
  try
    EmbedList := TStringList.Create;
    EmbedList.CommaText := Builder.GetEmbeddedList;
    cbxTypeEmbedded.Items.AddStrings(EmbedList);
  finally
    FreeAndNil(EmbedList);
    FreeAndNil(Builder);
  end;
  if cbxTypeEmbedded.Items.Count > 0 then
    cbxTypeEmbedded.ItemIndex := 0;
end;

const
  // Do not localize the strings below:
  MsgDlgConcateIdent = 'Concate';
  MsgDlgCppConcateIdent = 'CppConcate';
  MsgExpMsgTypeIdent = 'MsgType';
  MsgExpMsgTypeDefault = 'mtWarning';
  MsgExpMsgButtonsIdent = 'Buttons';
  MsgExpMsgButtonsDefault = 'mbOK';
  MsgExpMsgModalityIdent = 'Modality';
  MsgExpMsgModalityDefault = '';
  MsgExpMsgFunctionResultsIdent = 'FunctionResults';
  MsgExpMsgFunctionResultsDefault = '';
  MsgExpMsgHelpContextIdent = 'HelpContext';
  MsgExpMsgHelpContextDefault = 0;
  MsgExpMsgEmbed = 'Embed';
  MsgExpMsgQuotesIdent = 'Quotes';
  MsgExpMsgGNUGettextSupportIdent = 'GNUGettextSupport';
  MsgExpMsgGNUGettextFunctionIdent = 'MsgExpMsgGNUGettextFunction';
  MsgExpMsgGNUGettextIndividualIdent = 'MsgExpMsgGNUGettextIndividual';
  MsgExpMsgQuotesDefault = True;
  MsgExpMsgGNUGettextSupportDefault = False;
  MsgExpMsgGNUGettextFunctionDefault = ggtUnderscore;
  MsgExpMsgGNUGettextIndividualDefault = False;
  MsgExpMsgActivePageIdent = 'ActivePage';
  MsgExpMsgActivePageDefault = 0;
  MsgExpMsgCaptionQuotesIdent = 'TitleQuotes';
  MsgExpMsgCaptionQuotesDefault = True;
  MsgExpMsgDefaultButtonIdent = 'DefaultButton';
  MsgExpMsgDefaultButtonDefault = '1';
  MsgExpMsgEnableDefaultButtonIdent = 'EnableDefaultButton';
  MsgExpMsgEnableDefaultButtonDefault = False;
  MsgExpMsgBoxTypeIdent = 'MsgBoxType';
  MsgExpMsgBoxTypeDefault = 'mb_IconWarning';
  MsgExpMsgEmbedDefault = 0;

procedure TfmMessageDialog.SaveSettings;
var
  GExpertsSettings: TGExpertsSettings;
  ExpSettings: TExpertSettings;
begin
  ExpSettings := nil;
  GExpertsSettings := TGExpertsSettings.Create;
  try
    ExpSettings := GExpertsSettings.CreateExpertSettings(ConfigurationKey);
    ExpSettings.WriteInteger(MsgExpMsgActivePageIdent, pgeMessageDialog.ActivePageIndex);
    ExpSettings.WriteString(MsgExpMsgTypeIdent, DialogType);
    ExpSettings.WriteString(MsgExpMsgBoxTypeIdent, DialogBoxType);
    ExpSettings.WriteString(MsgExpMsgButtonsIdent, DialogButtons);
    ExpSettings.WriteString(MsgExpMsgModalityIdent, Modality);
    ExpSettings.WriteString(MsgExpMsgFunctionResultsIdent, FunctionResults);
    ExpSettings.WriteInteger(MsgExpMsgHelpContextIdent, DialogHelpContext);
    ExpSettings.WriteInteger(MsgExpMsgEmbed, EmbedSelection);
    ExpSettings.WriteBool(MsgExpMsgQuotesIdent, Quotes);
    ExpSettings.WriteBool(MsgExpMsgGNUGettextSupportIdent, GNUGettextSupport);
    ExpSettings.WriteBool(MsgExpMsgCaptionQuotesIdent, CaptionQuotes);
    ExpSettings.WriteString(MsgExpMsgDefaultButtonIdent, DefaultButton);
    ExpSettings.WriteBool(MsgExpMsgEnableDefaultButtonIdent, DefaultButtonCheckbox);
  finally
    FreeAndNil(ExpSettings);
    FreeAndNil(GExpertsSettings);
  end;
end;

procedure TfmMessageDialog.LoadSettings;
var
  GExpertsSettings: TGExpertsSettings;
  ExpSettings: TExpertSettings;
  LastPageIndex: Integer;
begin
  ExpSettings := nil;
  GExpertsSettings := TGExpertsSettings.Create;
  try
    ExpSettings := GExpertsSettings.CreateExpertSettings(ConfigurationKey);
    EmbedSelection := ExpSettings.ReadInteger(MsgExpMsgEmbed, MsgExpMsgEmbedDefault);
    LastPageIndex := ExpSettings.ReadInteger(MsgExpMsgActivePageIdent, MsgExpMsgActivePageDefault);
    if (LastPageIndex >= 0) and (LastPageIndex < pgeMessageDialog.PageCount) then
    begin
      pgeMessageDialog.ActivePageIndex := LastPageIndex;
      pgeMessageDialogChange(pgeMessageDialog);
    end;
    DialogType := ExpSettings.ReadString(MsgExpMsgTypeIdent, MsgExpMsgTypeDefault);
    DialogBoxType := ExpSettings.ReadString(MsgExpMsgBoxTypeIdent, MsgExpMsgBoxTypeDefault);
    AllDialogButtons := ExpSettings.ReadString(MsgExpMsgButtonsIdent, MsgExpMsgButtonsDefault);
    Modality := ExpSettings.ReadString(MsgExpMsgModalityIdent, MsgExpMsgModalityDefault);
    AllFunctionResults := ExpSettings.ReadString(MsgExpMsgFunctionResultsIdent, MsgExpMsgFunctionResultsDefault);
    DialogHelpContext := ExpSettings.ReadInteger(MsgExpMsgHelpContextIdent, MsgExpMsgHelpContextDefault);
    Quotes := ExpSettings.ReadBool(MsgExpMsgQuotesIdent, MsgExpMsgQuotesDefault);
    GNUGettextSupport := ExpSettings.ReadBool(MsgExpMsgGNUGettextSupportIdent, MsgExpMsgGNUGettextSupportDefault);
    CaptionQuotes := ExpSettings.ReadBool(MsgExpMsgCaptionQuotesIdent, MsgExpMsgCaptionQuotesDefault);
    DefaultButton := ExpSettings.ReadString(MsgExpMsgDefaultButtonIdent, MsgExpMsgDefaultButtonDefault);
    DefaultButtonCheckbox := ExpSettings.ReadBool(MsgExpMsgEnableDefaultButtonIdent, MsgExpMsgEnableDefaultButtonDefault);
  finally
    FreeAndNil(ExpSettings);
    FreeAndNil(GExpertsSettings);
  end;
end;

procedure TfmMessageDialog.mmoMessageKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_RETURN) and (ssCtrl in Shift) then
  begin
    Key := 0;
    ModalResult := mrOk;
  end;
end;

procedure TfmMessageDialog.pgeMessageDialogChange(Sender: TObject);
begin
  FreeAndNil(FMessageType);
  if pgeMessageDialog.ActivePage = tabMessageDlg then
  begin
    FFunctionResultsGroupBox := gbxDlgFunctionResults;
    FMessageType := TMessageDialogType.Create;
  end
  else
  begin
    FFunctionResultsGroupBox := gbxMsgBoxFunctionResults;
    FMessageType := TMessageBoxType.Create(Self);
  end;
  FixControlsEnablement(False);
end;

procedure TfmMessageDialog.PopulateMessageType;
begin
  if FMessageType is TMessageDialogType then
  begin
    FMessageType.Buttons := DialogButtons;
    FMessageType.DialogType := AssembleGroupBoxRadioButtonsText(gbxTypes);
  end
  else
  begin
    FMessageType.Buttons := AssembleGroupBoxRadioButtonsText(gbxMsgBoxTypes) + ',' +
      AssembleGroupBoxRadioButtonsText(gbxMsgBoxDialogButtons) + ',' +
      AssembleGroupBoxCheckBoxesText(gbxMsgBoxMisc) + ModalityString;
    while StrEndsWith(',', FMessageType.Buttons) do
      FMessageType.Buttons := DeleteRight(FMessageType.Buttons, 1);
    FMessageType.Buttons := StringReplace(FMessageType.Buttons, ',,', ',', [rfReplaceAll]);
    if DefaultButtonCheckbox then
      FMessageType.Buttons := FMessageType.Buttons + ',' + 'MB_DEFBUTTON' + DefaultButton;
  end;
  FMessageType.QuoteCaption := CaptionQuotes;
  FMessageType.HelpContext := udHelpContext.Position;
  FMessageType.Caption := edtMsgBoxCaption.Text;
  FMessageType.GnuGetTextSupport :=chkGNUGettext.Checked;
  FMessageType.GnuGetTextFunction := FSettings.GnuGetTextFunction;
end;

procedure TfmMessageDialog.SetAllDialogButtons(const SelectedButtons: string);
begin
  DistributeGroupBoxCheckBoxesText(SelectedButtons, gbxButtons);
  DistributeGroupBoxRadioButtonsText(SelectedButtons, gbxMsgBoxDialogButtons);
  DistributeGroupBoxCheckBoxesText(SelectedButtons, gbxMsgBoxMisc);
end;

procedure TfmMessageDialog.SetAllFunctionResults(const SelectedFunctionResults: string);
begin
  DistributeGroupBoxCheckBoxesText(SelectedFunctionResults, gbxDlgFunctionResults);
  DistributeGroupBoxCheckBoxesText(SelectedFunctionResults, gbxMsgBoxFunctionResults);
end;

procedure TfmMessageDialog.SetCaptionQuotes(const Value: Boolean);
begin
  chkQuoteCaption.Checked := Value;
end;

procedure TfmMessageDialog.SetDefaultButton(const Value: string);
begin
  udDefaultButton.Position := StrToIntDef(Value, 1);
end;

procedure TfmMessageDialog.SetDefaultButtonCheckbox(const Value: Boolean);
begin
  chkDefaultButton.Checked := Value;
end;

procedure TfmMessageDialog.SetDialogButtons(const SelectedButtons: string);
begin
  DistributeGroupBoxCheckBoxesText(SelectedButtons, gbxButtons);
end;

procedure TfmMessageDialog.SetDialogHelpContext(const Value: Integer);
begin
  udHelpContext.Position := Value;
end;

procedure TfmMessageDialog.SetDialogType(const TypeText: string);
begin
  DistributeGroupBoxRadioButtonsText(TypeText, gbxTypes);
end;

procedure TfmMessageDialog.SetEmbedSelection(const Value: Integer);
begin
  cbxTypeEmbedded.ItemIndex := Value;
end;

procedure TfmMessageDialog.SetFunctionResults(const SelectedFunctionResults: string);
begin
  DistributeGroupBoxCheckBoxesText(SelectedFunctionResults, FFunctionResultsGroupBox);
end;

procedure TfmMessageDialog.SetGNUGettextSupport(const Value: Boolean);
begin
  chkGNUGettext.Checked := Value;
end;

procedure TfmMessageDialog.SetQuotes(const Value: Boolean);
begin
  chkQuotes.Checked := Value;
end;

function TfmMessageDialog.ConfigurationKey: string;
begin
  Result := TMsgExpExpert.ConfigurationKey;
end;

function TfmMessageDialog.GetDialogBoxType: string;
begin
  Result := AssembleGroupBoxRadioButtonsText(gbxMsgBoxTypes);
end;

procedure TfmMessageDialog.SetDialogBoxType(const TypeText: string);
begin
  DistributeGroupBoxRadioButtonsText(TypeText, gbxMsgBoxTypes);
end;

function TfmMessageDialog.GetModality: string;
begin
  Result := cbxModality.Text;
end;

procedure TfmMessageDialog.SetModality(const Value: string);
begin
  cbxModality.ItemIndex := cbxModality.Items.IndexOf(Value);
  if cbxModality.ItemIndex < 0 then
    cbxModality.ItemIndex := 0;
end;

function TfmMessageDialog.ModalityString: string;
begin
  Result := Modality;
  if Result <> '' then
    Result := ',' + Result;
end;

procedure TfmMessageDialog.cbxTypeEmbeddedChange(Sender: TObject);
begin
  FixControlsEnablement(False);
end;

procedure TfmMessageDialog.chkQuotesClick(Sender: TObject);
begin
  chkGNUGettext.Visible := chkQuotes.Checked;
end;

destructor TfmMessageDialog.Destroy;
begin
  FreeAndNil(FMessageType);
  inherited;
end;

{ TMsgExpExpert }

constructor TMsgExpExpert.Create;
begin
  inherited;
  FSettings := TMessageDialogSettings.Create;
end;

destructor TMsgExpExpert.Destroy;
begin
  FreeAndNil(FSettings);
  inherited;
end;

function TMsgExpExpert.GetActionCaption: string;
resourcestring
  SMenuCaption = '&Message Dialog...';
begin
  Result := SMenuCaption;
end;

function TMsgExpExpert.GetDefaultShortCut: TShortCut;
begin
  Result := 0;
  // this shortcut conflicts with a Declare Field standard shortcut in the IDE
  //  Result := Menus.ShortCut(Ord('D'), [ssCtrl, ssShift]);
end;

class function TMsgExpExpert.GetName: string;
begin
  Result := 'MessageDialog';
end;

procedure TMsgExpExpert.Execute(Sender: TObject);
resourcestring
  SWrongFileType = 'This expert is for use in pas, dpr, inc, and cpp files only.';
var
  Dlg: TfmMessageDialog;
  FileName: string;
  SourceType: TSourceType;
  DefaultMsg: string;
begin
  FileName := GxOtaGetCurrentSourceFile;
  if IsCpp(FileName) then
    SourceType := stCpp
  else if (IsPascalSourceFile(FileName)) then
    SourceType := stPascal
  else
    raise Exception.Create(SWrongFileType);

  FSettings.SourceType := SourceType;

  Dlg := TfmMessageDialog.Create(nil, FSettings);
  try
    SetFormIcon(Dlg);
    DefaultMsg := Trim(GxOtaGetCurrentSelection(False));
    if DefaultMsg <> '' then
      Dlg.mmoMessage.Text := DefaultMsg;
    if Dlg.ShowModal = mrOk then
    begin
      if StrContains(#10, Dlg.GeneratedCode) then
        GxOtaInsertTextIntoEditor(Dlg.GeneratedCode)
      else
        GxOtaInsertLineIntoEditor(Dlg.GeneratedCode);

      if FSettings.FSourceType = stPascal then
      begin
        if not IsDpr(GxOtaGetCurrentSourceFile) then
        begin
          if GxOtaActiveDesignerIsVCL or GxOtaActiveDesignerIsNFM then
            UseUnitInImplementation(Dlg.UsesUnit)
          else if GxOtaActiveDesignerIsCLX then
            UseUnitInImplementation(Dlg.UsesUnitCLX);
        end;
      end;
      if Dlg.chkDefaults.Checked then
        Dlg.SaveSettings;
      GxOtaFocusCurrentIDEEditControl;

      IncCallCount;
    end;
  finally
    FreeAndNil(Dlg);
  end;
end;

procedure TMsgExpExpert.InternalLoadSettings(Settings: TExpertSettings);
begin
  inherited;
  FSettings.ConcatenationString :=
    Settings.ReadString(MsgDlgConcateIdent, FSettings.ConcatenationString);
  FSettings.CppConcatenationString :=
    Settings.ReadString(MsgDlgCppConcateIdent, FSettings.CppConcatenationString);
  FSettings.GnuGetTextFunction := TGnuGetTextFunction(Settings.ReadEnumerated(
    MsgExpMsgGNUGettextFunctionIdent, TypeInfo(TGnuGetTextFunction), Ord(MsgExpMsgGNUGettextFunctionDefault)));
  FSettings.GnuGetTextIndividual := Settings.ReadBool(MsgExpMsgGNUGettextIndividualIdent, MsgExpMsgGNUGettextIndividualDefault);
end;

procedure TMsgExpExpert.InternalSaveSettings(Settings: TExpertSettings);
begin
  Settings.WriteString(MsgDlgConcateIdent, FSettings.ConcatenationString);
  Settings.WriteString(MsgDlgCppConcateIdent, FSettings.CppConcatenationString);
  Settings.WriteEnumerated(MsgExpMsgGNUGettextFunctionIdent,TypeInfo(TGnuGetTextFunction), Ord(FSettings.GNUGettextFunction));
  Settings.WriteBool(MsgExpMsgGNUGettextIndividualIdent, FSettings.GNUGettextIndividual);
  inherited InternalSaveSettings(Settings);
end;

procedure TMsgExpExpert.Configure;
begin
  if TfmMessageOptions.Execute(nil,
    FSettings.FConcatenationString, FSettings.FCppConcatenationString,
    FSettings.FGnuGetTextFunction, FSettings.FGnuGetTextIndividual) then begin
  end;
end;

procedure TMsgExpExpert.UpdateAction(Action: TCustomAction);
const
  SAllowableFileExtensions = '.pas;.dpr;.inc;.cpp';
begin
  Action.Enabled := FileMatchesExtensions(GxOtaGetCurrentSourceFile, SAllowableFileExtensions);
end;

{ TMessageDialogSettings }

constructor TMessageDialogSettings.Create;
begin
  inherited;
  FConcatenationString := '+#13+#10+';
  FCppConcatenationString := '\n';
end;

{ TAbstractMessageDialogBuilder }

constructor TAbstractMessageDialogBuilder.Create;
begin
  inherited;
  FFunctionResults := TStringList.Create;
  FText := TStringList.Create;
end;

destructor TAbstractMessageDialogBuilder.Destroy;
begin
  FreeAndNil(FFunctionResults);
  FreeAndNil(FText);
  inherited;
end;

procedure TAbstractMessageDialogBuilder.SetDialogTypText(const Value: string);
begin
  FDialogTypText := Value;
end;

procedure TAbstractMessageDialogBuilder.SetFunctionResults(const Value: TStrings);
begin
  FFunctionResults.Assign(Value);
end;

procedure TAbstractMessageDialogBuilder.SetText(const Value: TStrings);
begin
  FText.Assign(Value);
end;

{ TCppMessageDialogBuilder }

function EscapeForCpp(const Value: string): string;
var
  i: Integer;
begin
  Result := Value;

  for i := Length(Value) downto 1 do
  begin
    if CharInSet(Result[i], ['"', '\']) then
      Insert('\', Result, i);
  end;
end;

function TCppMessageDialogBuilder.GetCode: string;

  function SurroundGnuGettext(Value: string): string;
  begin
    if not FGNUGettextSupport then begin
      Result := Value;
    end else begin
      if FGnuGetTextFunction = ggtUnderscore then
        Result := '_(' + Value + ')'
      else
        Result := 'GetText(' + Value + ')';
    end;
  end;

  function BuildString(Value: string): string;
  begin
    if FQuotes then begin
      Result := EscapeForCpp(Value);
      if FGNUGettextSupport and FGnuGetTextIndividual then begin
        Result := SurroundGnuGettext('"' + Result + '"');
      end;
    end else
      Result := Value;
  end;

  function BuildIfStatement(const Condition: string): string;
  var
    i: Integer;
    FunctionText: string;
  begin
    if MessageType is TMessageBoxType then
      FunctionResults.Text := UpperCase(FunctionResults.Text);
    if FunctionResults.Count > 1 then // complex compare to set
    begin
      for i := 0 to FunctionResults.Count - 1 do
        FunctionText := FunctionText + ' << ' + Trim(FunctionResults[i]);
      Result := Format('(Set<int, mrNone, mrYesToAll>()%s).Contains(%s)', [FunctionText, Condition]);
    end
    else // simple: use '... == ...'
      Result := Format('%s == %s', [Condition, Trim(FunctionResults.Text)]);
  end;

  function BuildSwitchStatement(const Condition: string): string;
  var
    i: Integer;
    Column: Integer;
    Indent: string;
    StartOffset, LineNo: Integer;
    SwitchValue: string;
  begin
    GxOtaGetCurrentLineData(StartOffset, Column, LineNo);

    Result := Format('switch (%s) {' + sLineBreak, [Condition]);
    Indent := Indent + StringOfChar(' ', Column);

    for i := 0 to FunctionResults.Count - 1 do
    begin
      SwitchValue := FunctionResults[i];
      if FMessageType is TMessageBoxType then
        SwitchValue := UpperCase(SwitchValue);

      Result := Result + Format('%s   case %s:' + sLineBreak + '%s      break;' +
        sLineBreak, [Indent, SwitchValue, Indent]);
    end;
    Result := Result + Indent + '}';
  end;

var
  i: Integer;
  ConcatStr: string;
begin
  if FQuotes and FGnuGetTextSupport and FGnuGetTextIndividual then
    ConcatStr := '+"' + CppConcatString + '"+'
  else
    ConcatStr := CppConcatString;

  Result := '';
  if FText.Count > 0 then
  begin
    for i := 0 to FText.Count - 2 do
    begin
      Result := Result + BuildString(FText[i]);
      Result := Result + ConcatStr;
    end;
    Result := Result + BuildString(FText[FText.Count - 1]);
  end;

  if FQuotes then begin
    if FGnuGetTextSupport then begin
      if not FGnuGetTextIndividual then begin
        Result := SurroundGnuGettext('"' + Result + '"');
      end;
    end else begin
      Result := '"' + Result + '"';
    end;
  end;
  FMessageType.QuoteCharacter := '"';
  if FMessageType is TMessageDialogType then
  begin
    // MessageDlg('Test', 'inforamton', TMsgDlgButtons() << MBYESNO, 0)
    FMessageType.ButtonSeparator := '<<';
    FMessageType.ParameterFormat := '(%s, %s, TMsgDlgButtons() << %s, %d)';
  end
  else
  begin
    // MessageBox(0, ErrorMessage, "Error", MB_ICONERROR | MB_OK);
    FMessageType.ButtonSeparator := '|';
    FMessageType.ParameterFormat := '(0, %s, %s, %s)';
  end;
  FUsesUnit := 'Windows';
  FUsesUnitCLX := 'Windows';
  FMessageType.Message := Result;
  Result := FMessageType.GetCode;

  // Build code statements around message boxes
  case FEmbedIndex of
    1: Result := Format('if (%s)', [BuildIfStatement(Result)]);
    2: Result := Format('if (!(%s))', [BuildIfStatement(Result)]);
    3: Result := BuildSwitchStatement(Result);
  else Result := Result + ';';
  end;
end;

function TCppMessageDialogBuilder.GetEmbeddedList: string;
begin
  Result := '"","If statement","Negative if statement","Switch statement"';
end;

{ TPascalMessageDialogBuilder }

function TPascalMessageDialogBuilder.GetCode: string;

  function SurroundGnuGettext(Value: string): string;
  begin
    if not FGNUGettextSupport then begin
      Result := Value;
    end else begin
      if FGnuGetTextFunction = ggtUnderscore then
        Result := '_(' + Value + ')'
      else
        Result := 'GetText(' + Value + ')';
    end;
  end;

  function BuildString(Value: string): string;
  begin
    if FQuotes then begin
      Result := QuotedStr(Value);
      if FGNUGettextSupport and FGnuGetTextIndividual then begin
        Result := SurroundGnuGettext(Result);
      end;
    end else
      Result := Value;
  end;

  function BuildIfStatement(const Condition: string): string;
  begin
    if FunctionResults.Count > 1 then // complex: use '... in [...]'
      Result := Format('%s in [%s]', [Condition, SpacedCommaText(FunctionResults)])
    else // simple: use '... = ...'
      Result := Format('%s = %s', [Condition, SpacedCommaText(FunctionResults)]);
  end;

  function BuildCaseStatement(const Condition: string): string;
  var
    i: Integer;
    Column: Integer;
    Indent: string;
    StartOffset, LineNo: Integer;
  begin
    GxOtaGetCurrentLineData(StartOffset, Column, LineNo);

    Result := Format('case %s of' + sLineBreak, [Condition]);
    Indent := Indent + StringOfChar(' ', Column);

    for i := 0 to FunctionResults.Count - 1 do
      Result := Result + Format('%s  %s: ;' + sLineBreak, [Indent, FunctionResults[i]]);
    Result := Result + Indent + 'end;';
  end;

var
  i: Integer;
begin
  // Build Pascal source code for MessageDlg
  Result := '';
  if (FText.Count > 0) then
  begin
    for i := 0 to FText.Count - 2 do
    begin
      Result := Result + BuildString(FText[i]);
      Result := Result + ConcatString;
    end;
    Result := Result + BuildString(FText[FText.Count - 1]);
  end;

  if Result <> '' then begin
    if FGNUGettextSupport and not FGnuGetTextIndividual then
      Result := SurroundGnuGettext(Result);
  end else begin
    if FQuotes then begin
      Result := SurroundGnuGettext(#39#39);
    end;
  end;

  FMessageType.QuoteCharacter := #39;

  // MessageDlg('FooBar', mtWarning, [mbYes, mbNo], 0)
  // MessageBox('FooBar', 'Open Error', MB_OKCANCEL or MB_DEFBUTTON1)
  if FMessageType is TMessageDialogType then
  begin
    FMessageType.ButtonSeparator := ', ';
    FMessageType.ParameterFormat := '(%s, %s, [%s], %d)';
    FUsesUnit := 'Dialogs';
    FUsesUnitCLX := 'QDialogs';
  end
  else
  begin
    FMessageType.ButtonSeparator := ' or ';
    FMessageType.ParameterFormat := '(0, PChar(%s), PChar(%s), %s)';
    FUsesUnit := 'Windows';
    FUsesUnitCLX := 'Windows';
  end;
  FMessageType.Message := Result;
  Result := FMessageType.GetCode;
  // Build code statements around message boxes
  case FEmbedIndex of
    1: Result := Format('if (%s) then', [BuildIfStatement(Result)]);
    2: Result := Format('if not (%s) then', [BuildIfStatement(Result)]);
    3: Result := BuildCaseStatement(Result);
  else Result := Result + ';';
  end;
end;

function TPascalMessageDialogBuilder.GetEmbeddedList: string;
begin
  Result := '"","If statement","Negative if statement","Case statement"';
end;

constructor TMessageBoxType.Create(Parent: TCustomForm);
begin
  inherited Create;
  FParent := Parent;
  FCommand := 'MessageBox';
  FBoxButtonsList := TStringList.Create;
  FBoxButtonsList.Add('MB_ABORTRETRYIGNORE=' + IntToStr(MB_ABORTRETRYIGNORE));
  FBoxButtonsList.Add('MB_OK=' + IntToStr(MB_OK));
  FBoxButtonsList.Add('MB_OKCANCEL=' + IntToStr(MB_OKCANCEL));
  FBoxButtonsList.Add('MB_RETRYCANCEL=' + IntToStr(MB_RETRYCANCEL));
  FBoxButtonsList.Add('MB_YESNO=' + IntToStr(MB_YESNO));
  FBoxButtonsList.Add('MB_YESNOCANCEL=' + IntToStr(MB_YESNOCANCEL));
  FBoxButtonsList.Add('MB_ICONINFORMATION=' + IntToStr(MB_ICONINFORMATION));
  FBoxButtonsList.Add('MB_ICONQUESTION=' + IntToStr(MB_ICONQUESTION));
  FBoxButtonsList.Add('MB_ICONSTOP=' + IntToStr(MB_ICONSTOP));
  FBoxButtonsList.Add('MB_ICONEXCLAMATION=' + IntToStr(MB_ICONEXCLAMATION));
  FBoxButtonsList.Add('MB_ICONASTERISK=' + IntToStr(MB_ICONASTERISK));
  FBoxButtonsList.Add('MB_ICONERROR=' + IntToStr(MB_ICONERROR));
  FBoxButtonsList.Add('MB_ICONHAND=' + IntToStr(MB_ICONHAND));
  FBoxButtonsList.Add('MB_ICONWARNING=' + IntToStr(MB_ICONWARNING));
  FBoxButtonsList.Add('MB_APPLMODAL=' + IntToStr(MB_APPLMODAL));
  FBoxButtonsList.Add('MB_SYSTEMMODAL=' + IntToStr(MB_SYSTEMMODAL));
  FBoxButtonsList.Add('MB_TASKMODAL=' + IntToStr(MB_TASKMODAL));
  FBoxButtonsList.Add('MB_SERVICE_NOTIFICATION_NT3X=' + IntToStr(MB_SERVICE_NOTIFICATION_NT3X));
  FBoxButtonsList.Add('MB_SERVICE_NOTIFICATION=' + IntToStr(MB_SERVICE_NOTIFICATION));
  FBoxButtonsList.Add('MB_TOPMOST=' + IntToStr(MB_TOPMOST));
  FBoxButtonsList.Add('MB_SETFOREGROUND=' + IntToStr(MB_SETFOREGROUND));
  FBoxButtonsList.Add('MB_RTLREADING=' + IntToStr(MB_RTLREADING));
  FBoxButtonsList.Add('MB_RIGHT=' + IntToStr(MB_RIGHT));
  FBoxButtonsList.Add('MB_HELP=' + IntToStr(MB_HELP));
  FBoxButtonsList.Add('MB_NOFOCUS=' + IntToStr(MB_NOFOCUS));
  FBoxButtonsList.Add('MB_DEFAULT_DESKTOP_ONLY=' + IntToStr(MB_DEFAULT_DESKTOP_ONLY));
  FBoxButtonsList.Add('MB_DEFBUTTON1=' + IntToStr(MB_DEFBUTTON1));
  FBoxButtonsList.Add('MB_DEFBUTTON2=' + IntToStr(MB_DEFBUTTON2));
  FBoxButtonsList.Add('MB_DEFBUTTON3=' + IntToStr(MB_DEFBUTTON3));
  FBoxButtonsList.Add('MB_DEFBUTTON4=' + IntToStr(MB_DEFBUTTON4));
end;

destructor TMessageBoxType.Destroy;
begin
  FreeAndNil(FBoxButtonsList);
  FParent := nil;
  inherited;
end;

function TMessageBoxType.ButtonValue: Integer;
var
  i: Integer;
  ButtonList: TStringList;
begin
  ButtonList := TStringList.Create;
  try
    Result := 0;
    ButtonList.CommaText := FButtons;
    for i := 0 to ButtonList.Count - 1 do
      if ButtonList[i] <> '' then
        Result := Result or StrToIntDef(FBoxButtonsList.Values[ButtonList[i]], 0);
  finally
    FreeAndNil(ButtonList);
  end;
end;

procedure TMessageBoxType.ShowModal;
begin
  Assert(Assigned(FParent));
  if MessageBox(FParent.Handle, PChar(FMessage), PChar(FCaption), ButtonValue) = 0 then
    raise Exception.Create('The MessageBox call failed (returned 0)');
end;

function TMessageBoxType.GetCode: string;
var
  Title: string;
begin
  if QuoteCaption then begin
    Title := FQuoteCharacter + FCaption + FQuoteCharacter;
    if GnuGetTextSupport then begin
      if FGnuGetTextFunction = ggtUnderscore then
        Title := '_(' + Title + ')'
      else
        Title := 'GetText(' + Title + ')';
    end;
  end else
    Title := FCaption;
  Result := Format(FCommand + ParameterFormat, [FMessage, Title,
    StringReplace(AnsiUpperCase(FButtons), ',', FButtonSeparator, [rfReplaceAll, rfIgnoreCase])]);
end;

constructor TMessageDialogType.Create;
begin
  inherited;
  FCommand := 'MessageDlg';
end;

function TMessageDialogType.GetMsgDlgButtons: TMsgDlgButtons;
var
  i: Integer;
  SelectedMsgDlgButton: TMsgDlgBtn;
  ButtonList: TStringList;
begin
  ButtonList := TStringList.Create;
  try
    Result := [];
    ButtonList.CommaText := FButtons;
    for i := 0 to ButtonList.Count - 1 do
    begin
      SelectedMsgDlgButton := TMsgDlgBtn(GetEnumValue(TypeInfo(TMsgDlgBtn), ButtonList[i]));
      Assert(SelectedMsgDlgButton in [Low(TMsgDlgBtn)..High(TMsgDlgBtn)]);
      Include(Result, SelectedMsgDlgButton);
    end;
  finally
    FreeAndNil(ButtonList);
  end;
end;

function TMessageDialogType.GetMsgDlgType: TMsgDlgType;
begin
  Result := TMsgDlgType(GetEnumValue(TypeInfo(TMsgDlgType), FDialogType));
end;

procedure TMessageDialogType.ShowModal;
begin
  MessageDlg(FMessage, GetMsgDlgType, GetMsgDlgButtons, FHelpContext);
end;

function TMessageDialogType.GetCode: string;
begin
  Result := Format(Command + ParameterFormat, [FMessage, FDialogType,
    StringReplace(FButtons, ',', FButtonSeparator, [rfReplaceAll, rfIgnoreCase]), FHelpContext])
end;

initialization
  RegisterGX_Expert(TMsgExpExpert);
end.

