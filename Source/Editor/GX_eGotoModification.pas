unit GX_eGotoModification;

{$I GX_CondDefine.inc}

interface
{.$IFDEF GX_VER290_up} // RAD Studio XE 8 (23; BDS 16)
uses
  Windows,
  SysUtils,
  Classes,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  ComCtrls,
  Menus,
  ToolsAPI,
  DockForm,
  GX_Experts,
  GX_EditorExpert,
  GX_ConfigurationInfo,
  GX_BaseForm;

// There are already IDE actions for navigating to the previous/next modification in the editor
// window. They have keyboard shortcuts but apparently no menu entries:
//
// http://docwiki.embarcadero.com/RADStudio/Tokyo/en/Code_Editor#Finding_the_Next_and_Previous_Changes
//
// The purpose of these editor experts is to make this founctionality more visible by creating
// menu entries for them.

type
  TOnEditViewChanged = procedure(_Sender: TObject; const _EditWindow: INTAEditWindow) of object;

  TEditorNotifier = class(TNotifierObject, INTAEditServicesNotifier)
  private
    FOnEditViewOpened: TOnEditViewChanged;
    FOnEditViewClosed: TOnEditViewChanged;
    procedure doOnEditWindowOpened(const _EditWindow: INTAEditWindow);
    procedure doOnEditWindowClosed(const _EditWindow: INTAEditWindow);
  private
    procedure WindowShow(const EditWindow: INTAEditWindow; Show, LoadedFromDesktop: Boolean);
    procedure WindowNotification(const EditWindow: INTAEditWindow; Operation: TOperation);
    procedure WindowActivated(const EditWindow: INTAEditWindow);
    procedure WindowCommand(const EditWindow: INTAEditWindow; Command, Param: Integer; var Handled: Boolean);
    procedure EditorViewActivated(const EditWindow: INTAEditWindow; const EditView: IOTAEditView);
    procedure EditorViewModified(const EditWindow: INTAEditWindow; const EditView: IOTAEditView);
    procedure DockFormVisibleChanged(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
    procedure DockFormUpdated(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
    procedure DockFormRefresh(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
  public
    constructor Create(_OnEditViewOpened: TOnEditViewChanged; _OnEditViewClosed: TOnEditViewChanged);
  end;

type
  TGxGotoModification = class(TEditorExpert)
  private
    FNotifier: Integer;
    procedure HandleWindowOpened(_Sender: TObject; const _EditWindow: INTAEditWindow);
    procedure CreateMenuItem(_EditForm: TCustomForm);
  protected
    procedure SetActive(New: Boolean); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

type
  TGxGotoPrevModification = class(TGxGotoModification)
  public
    function GetDefaultShortCut: TShortCut; override;
    procedure Execute(Sender: TObject); override;
    function GetDisplayName: string; override;
    function GetHelpString: string; override;
    function HasConfigOptions: Boolean; override;
  end;

type
  TGxGotoNextModification = class(TGxGotoModification)
  public
    function GetDefaultShortCut: TShortCut; override;
    procedure Execute(Sender: TObject); override;
    function GetDisplayName: string; override;
    function GetHelpString: string; override;
    function HasConfigOptions: Boolean; override;
  end;
{.$ENDIF}
implementation
{.$IFDEF GX_VER290_up} // RAD Studio XE 8 (23; BDS 16)

uses
{$IFOPT D+}GX_DbugIntf,
{$ENDIF}
  Registry,
  GX_OtaUtils,
  GX_GenericUtils,
  GX_GExperts,
  GX_dzVclUtils;

{ Called when a new edit view is created(opInsert) or destroyed(opRemove) }

constructor TEditorNotifier.Create(_OnEditViewOpened, _OnEditViewClosed: TOnEditViewChanged);
begin
  inherited Create;
  FOnEditViewOpened := _OnEditViewOpened;
  FOnEditViewClosed := _OnEditViewClosed;
end;

procedure TEditorNotifier.DockFormRefresh(const EditWindow: INTAEditWindow;
  DockForm: TDockableForm);
begin
  // we don't care
end;

procedure TEditorNotifier.DockFormUpdated(const EditWindow: INTAEditWindow;
  DockForm: TDockableForm);
begin
  // we don't care
end;

procedure TEditorNotifier.DockFormVisibleChanged(const EditWindow: INTAEditWindow;
  DockForm: TDockableForm);
begin
  // we don't care
end;

procedure TEditorNotifier.EditorViewActivated(const EditWindow: INTAEditWindow;
  const EditView: IOTAEditView);
begin
  // we don't care
end;

procedure TEditorNotifier.EditorViewModified(const EditWindow: INTAEditWindow;
  const EditView: IOTAEditView);
begin
  // we don't care
end;

procedure TEditorNotifier.WindowActivated(const EditWindow: INTAEditWindow);
begin
  // we don't care
end;

procedure TEditorNotifier.WindowCommand(const EditWindow: INTAEditWindow; Command, Param: Integer;
  var Handled: Boolean);
begin
  // we don't care
end;

procedure TEditorNotifier.WindowNotification(const EditWindow: INTAEditWindow;
  Operation: TOperation);
begin
  case Operation of
    opInsert: doOnEditWindowOpened(EditWindow);
    opRemove: doOnEditWindowClosed(EditWindow);
  end;

end;

procedure TEditorNotifier.doOnEditWindowOpened(const _EditWindow: INTAEditWindow);
begin
  if Assigned(FOnEditViewOpened) then
    FOnEditViewOpened(Self, _EditWindow);
end;

procedure TEditorNotifier.doOnEditWindowClosed(const _EditWindow: INTAEditWindow);
begin
  if Assigned(FOnEditViewClosed) then
    FOnEditViewClosed(Self, _EditWindow);
end;

procedure TEditorNotifier.WindowShow(const EditWindow: INTAEditWindow; Show,
  LoadedFromDesktop: Boolean);
begin
  // we don't care
end;

{ TGxGotoModification }

constructor TGxGotoModification.Create;
begin
  inherited Create;
  FNotifier := GxOtaGetEditorServices.AddNotifier(TEditorNotifier.Create(HandleWindowOpened, nil));
end;

destructor TGxGotoModification.Destroy;
begin
  if FNotifier <> InvalidNotifierIndex then begin
    GxOtaGetEditorServices.RemoveNotifier(FNotifier);
  end;
  inherited;
end;

procedure TGxGotoModification.HandleWindowOpened(_Sender: TObject; const _EditWindow: INTAEditWindow);
begin
  CreateMenuItem(_EditWindow.GetForm);
end;

procedure TGxGotoModification.CreateMenuItem(_EditForm: TCustomForm);
var
  EditView: IOTAEditView;
  cmp: TComponent;
  pm: TPopupMenu;
  mi: TMenuItem;
begin
  if not Active then
    Exit; //==>

  if not Assigned(_EditForm) then begin
    if not GxOtaTryGetTopMostEditView(EditView) then
      Exit; //==>
    _EditForm := EditView.GetEditWindow.Form;
  end;
  cmp := _EditForm.FindComponent('EditorLocalMenu');
  if not Assigned(cmp) or not (cmp is TPopupMenu) then
    Exit; //==>
  pm := TPopupMenu(cmp);

  cmp := pm.FindComponent('GX' + Self.GetName);
  if Assigned(cmp) then
    Exit; //==>

  mi := TPopupMenu_AppendMenuItem(pm, Self.GetDisplayName, Self.Execute);
  mi.Name := 'GX' + Self.GetName;
  mi.ShortCut := Self.ShortCut;
end;

procedure TGxGotoModification.SetActive(New: Boolean);
begin
  if New then begin
    CreateMenuItem(nil);
  end else begin
//    if Assigned(FMenuItem) then begin
//      FreeAndNil(FMenuItem);
//    end;
  end;
  inherited;
end;

{ TGxGotoPrevModification }

procedure TGxGotoPrevModification.Execute(Sender: TObject);
var
  View: IOTAEditView;
begin
  if not GxOtaTryGetTopMostEditView(View) then
    Exit; //==>

  View.NavigateToModification(sdBackward, mtAnyMod);
end;

function TGxGotoPrevModification.GetDefaultShortCut: TShortCut;
begin
  // This is the default shortcut the IDE uses for this functionality, so it should be
  // safe to hijack it.
  // And even if it reverts back to the IDE action, the shortcut would still
  // work. ;-)
  Result := Menus.ShortCut(VK_F7, [ssCtrl, ssShift])
end;

function TGxGotoPrevModification.GetDisplayName: string;
resourcestring
  SDisplayName = 'Goto Previous Modification';
begin
  Result := SDisplayName;
end;

function TGxGotoPrevModification.GetHelpString: string;
resourcestring
  SSampleEditorExpertHelp =
    'Navigates to the previous modification in the editor window.';
begin
  Result := SSampleEditorExpertHelp;
end;

function TGxGotoPrevModification.HasConfigOptions: Boolean;
begin
  Result := False;
end;

{ TGxGotoNextModification }

procedure TGxGotoNextModification.Execute(Sender: TObject);
var
  View: IOTAEditView;
begin
  if not GxOtaTryGetTopMostEditView(View) then
    Exit; //==>
  View.NavigateToModification(sdForward, mtAnyMod);
end;

function TGxGotoNextModification.GetDefaultShortCut: TShortCut;
begin
  // This is the default shortcut the IDE uses for this functionality, so it should be
  // safe to hijack it.
  // And even if it reverts back to the IDE action, the shortcut would still
  // work. ;-)
  Result := Menus.ShortCut(VK_F8, [ssCtrl, ssShift])
end;

function TGxGotoNextModification.GetDisplayName: string;
resourcestring
  SDisplayName = 'Goto Next Modification';
begin
  Result := SDisplayName;
end;

function TGxGotoNextModification.GetHelpString: string;
resourcestring
  SSampleEditorExpertHelp =
    'Navigates to the next modification in the editor window.';
begin
  Result := SSampleEditorExpertHelp;
end;

function TGxGotoNextModification.HasConfigOptions: Boolean;
begin
  Result := False;
end;

initialization
  RegisterEditorExpert(TGxGotoPrevModification);
  RegisterEditorExpert(TGxGotoNextModification);
{.$ENDIF}

end.

