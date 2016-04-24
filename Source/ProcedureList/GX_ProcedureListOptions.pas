unit GX_ProcedureListOptions;

interface

uses
  Types, Classes, Graphics, Controls, Forms, ExtCtrls, StdCtrls, GX_BaseForm;

type
  TProcedureListOptions = class(TObject)
  private
    FAlignmentChanged: Boolean;
    FDialogFont: TFont;
    FCodeViewFont: TFont;
    FCodeViewAlignment: TAlign;
    FBoundsRect: TRect;
    FSortOnColumn: Integer;
    FCodeViewVisible: Boolean;
    FSearchAll: Boolean;
    FCodeViewWidth, FCodeViewHeight: Integer;
    FOptions: TProcedureListOptions;
    FObjectNameVisible: Boolean;
    FSearchClassName: Boolean;
    procedure SetBoundsRect(const AValue: TRect);
  public
    property AlignmentChanged: Boolean read FAlignmentChanged write FAlignmentChanged;
    property DialogFont: TFont read FDialogFont write FDialogFont;
    property CodeViewFont: TFont read FCodeViewFont write FCodeViewFont;
    property CodeViewAlignment: TAlign read FCodeViewAlignment write FCodeViewAlignment;
    property CodeViewVisible: Boolean read FCodeViewVisible write FCodeViewVisible;
    property CodeViewHeight: Integer read FCodeViewHeight write FCodeViewHeight;
    property CodeViewWidth: Integer read FCodeViewWidth write FCodeViewWidth;
    property BoundsRect: TRect read FBoundsRect write SetBoundsRect;
    property SortOnColumn: Integer read FSortOnColumn write FSortOnColumn;
    property SearchAll: Boolean read FSearchAll write FSearchAll;
    property SearchClassName: Boolean read FSearchClassName write FSearchClassName;
    property Options: TProcedureListOptions read FOptions write FOptions;
    property ObjectNameVisible: Boolean read FObjectNameVisible write FObjectNameVisible;
    procedure LoadSettings(const ConfigurationKey: string);
    procedure SaveSettings(const ConfigurationKey: string);

    constructor Create;
    destructor Destroy; override;
  end;

  TfmProcedureListOptions = class(TfmBaseForm)
    btnOK: TButton;
    btnCancel: TButton;
    gbCodeView: TGroupBox;
    cbCVDock: TComboBox;
    lblDock: TLabel;
    pnlCVFont: TPanel;
    btnChangeCodeViewFont: TButton;
    gbDialog: TGroupBox;
    pnlDialogFont: TPanel;
    btnChgDialogFont: TButton;
    chkShowCodeView: TCheckBox;
    chkShowObjectName: TCheckBox;
    chkMatchAnywhere: TCheckBox;
    chkMatchClass: TCheckBox;
    procedure btnChgDialogFontClick(Sender: TObject);
    procedure btnChangeCodeViewFontClick(Sender: TObject);
    procedure cbCVDockChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    FOptions: TProcedureListOptions;
    procedure SetCodeViewAlignment(Value: TAlign);
    function GetCodeViewAlignment: TAlign;
  public
    property Options: TProcedureListOptions read FOptions write FOptions;
  end;

implementation

{$R *.dfm}

uses
  SysUtils, Dialogs,
  GX_ConfigurationInfo, GX_GenericUtils;

const
  CodeViewFontKey = 'CodeViewFont';
  DialogFontKey = 'DialogFont';

constructor TProcedureListOptions.Create;
begin
  FDialogFont := TFont.Create;
  FCodeViewFont := TFont.Create;
  FCodeViewFont.Name := 'Courier New';
  FCodeViewFont.Size := 8;
  AlignmentChanged := False;
  ObjectNameVisible := True;
  SortOnColumn := 1;
end;

destructor TProcedureListOptions.Destroy;
begin
  FreeAndNil(FDialogFont);
  FreeAndNil(FCodeViewFont);
  inherited;
end;

procedure TProcedureListOptions.LoadSettings(const ConfigurationKey: string);

  function GetCodeViewAlignment(Value: string): TAlign;
  begin
    if Value = 'Top' then
      Result := alTop
    else if Value = 'Right' then
      Result := alRight
    else if Value = 'Left' then
      Result := alLeft
    else
      Result := alBottom;
  end;

var
  GxSettings: TGExpertsSettings;
  Settings: TExpertSettings;
begin
  // Do not localize any of the following lines
  Settings := nil;
  GxSettings := TGExpertsSettings.Create(ConfigInfo.GExpertsIdeRootRegistryKey);
  try
    Settings := GxSettings.CreateExpertSettings(ConfigurationKey);
    FBoundsRect := Settings.ReadBounds(Bounds(317, 279, 550, 500));
    FCodeViewVisible := Settings.ReadBool('ShowProcedureBody', False);
    FCodeViewWidth := Settings.ReadInteger('ProcedureWidth', 292);
    FCodeViewHeight := Settings.ReadInteger('ProcedureHeight', 100);
    FCodeViewAlignment := GetCodeViewAlignment(Settings.ReadString('ProcedureAlignment', 'Right'));
    FAlignmentChanged := Settings.ReadBool('AlignmentChanged', False);
    FSortOnColumn := Settings.ReadInteger('SortColumn', FSortOnColumn);
    Settings.LoadFont(DialogFontKey, FDialogFont);
    Settings.LoadFont(CodeViewFontKey, FCodeViewFont);
    FSearchAll := Settings.ReadBool('SearchAll', True);
    FSearchClassName := Settings.ReadBool('SearchClassName', True);
    FObjectNameVisible := Settings.ReadBool('ShowObjectName', True);
  finally
    FreeAndNil(Settings);
    FreeAndNil(GxSettings);
  end;
end;

procedure TProcedureListOptions.SaveSettings(const ConfigurationKey: string);

  function GetAlignmentString(Value: TAlign): string;
  begin
    case Value of
      alTop: Result := 'Top';
      alLeft: Result := 'Left';
      alRight: Result := 'Right';
      alBottom: Result := 'Bottom';
    else
      Result := 'Right';
    end;
  end;

var
  GxSettings: TGExpertsSettings;
  Settings: TExpertSettings;
begin
  // Do not localize any of the following lines
  Settings := nil;
  GxSettings := TGExpertsSettings.Create(ConfigInfo.GExpertsIdeRootRegistryKey);
  try
    Settings := GxSettings.CreateExpertSettings(ConfigurationKey);
    Settings.WriteBool('SearchAll', FSearchAll);
    Settings.WriteBool('SearchClassName', FSearchClassName);
    Settings.WriteBounds(BoundsRect);
    Settings.WriteInteger('SortColumn', FSortOnColumn);
    Settings.WriteInteger('ProcedureWidth', FCodeViewWidth);
    Settings.WriteInteger('ProcedureHeight', FCodeViewHeight);
    Settings.WriteString('ProcedureAlignment', GetAlignmentString(FCodeViewAlignment));
    Settings.SaveFont(DialogFontKey, FDialogFont);
    Settings.SaveFont(CodeViewFontKey, FCodeViewFont);
    Settings.WriteBool('ShowProcedureBody', FCodeViewVisible);
    Settings.WriteBool('AlignmentChanged', FAlignmentChanged);
    Settings.WriteBool('ShowObjectName', FObjectNameVisible);
  finally
    FreeAndNil(Settings);
    FreeAndNil(GxSettings);
  end;
end;

procedure TProcedureListOptions.SetBoundsRect(const AValue: TRect);
begin
  FBoundsRect := AValue;
end;

procedure TfmProcedureListOptions.btnChgDialogFontClick(Sender: TObject);
begin
  with TFontDialog.Create(nil) do
  try
    Font.Assign(pnlDialogFont.Font);
    if Execute then
      pnlDialogFont.Font.Assign(Font);
  finally
    Free;
  end;
end;

procedure TfmProcedureListOptions.btnChangeCodeViewFontClick(Sender: TObject);
begin
  with TFontDialog.Create(nil) do
  try
    Options := Options + [fdFixedPitchOnly];
    // Only show fixed font for source code
    Font.Assign(pnlCVFont.Font);
    if Execute then
      pnlCVFont.Font.Assign(Font);
  finally
    Free;
  end;
end;

function TfmProcedureListOptions.GetCodeViewAlignment: TAlign;
var
  sDock: string;
begin
  sDock := cbCVDock.Items[cbCVDock.ItemIndex];
  if sDock = 'Top' then
    Result := alTop
  else if sDock = 'Right' then
    Result := alRight
  else if sDock = 'Left' then
    Result := alLeft
  else
    Result := alBottom;
end;

procedure TfmProcedureListOptions.SetCodeViewAlignment(Value: TAlign);
begin
  case Value of
    alTop: cbCVDock.ItemIndex := cbCVDock.Items.IndexOf('Top');
    alLeft: cbCVDock.ItemIndex := cbCVDock.Items.IndexOf('Left');
    alRight: cbCVDock.ItemIndex := cbCVDock.Items.IndexOf('Right');
    alBottom: cbCVDock.ItemIndex := cbCVDock.Items.IndexOf('Bottom');
  else
    cbCVDock.ItemIndex := cbCVDock.Items.IndexOf('Right');
  end;
end;

procedure TfmProcedureListOptions.cbCVDockChange(Sender: TObject);
begin
  FOptions.AlignmentChanged := True;
end;

procedure TfmProcedureListOptions.FormShow(Sender: TObject);
begin
  FOptions.AlignmentChanged := False;
  chkShowCodeView.Checked := FOptions.CodeViewVisible;
  chkShowObjectName.Checked := FOptions.ObjectNameVisible;
  SetCodeViewAlignment(FOptions.CodeViewAlignment);
  pnlDialogFont.Font.Assign(FOptions.DialogFont);
  pnlCVFont.Font.Assign(FOptions.CodeViewFont);
  chkMatchAnywhere.Checked := FOptions.SearchAll;
  chkMatchClass.Checked := FOptions.SearchClassName;
end;

procedure TfmProcedureListOptions.btnOKClick(Sender: TObject);
begin
  FOptions.CodeViewVisible := chkShowCodeView.Checked;
  FOptions.ObjectNameVisible := chkShowObjectName.Checked;
  FOptions.CodeViewAlignment := GetCodeViewAlignment;
  FOptions.DialogFont.Assign(pnlDialogFont.Font);
  FOptions.CodeViewFont.Assign(pnlCVFont.Font);
  FOptions.SearchAll := chkMatchAnywhere.Checked;
  FOptions.SearchClassName := chkMatchClass.Checked;
end;

end.

