unit GX_IdeDock;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils, Classes, Forms, Controls,
  MenuBar, Menus, Messages,
  // You must link to the DesignIde package to compile this unit
  DockForm;

type
{$IFDEF EnableIdeDockingSupport}
  TDummyPopupMenu = class(TPopupMenu)
  private
    OwnerMenu: TMenu;
  public
    function IsShortCut(var Message: TWMKey): Boolean; override;
  end;
{$ENDIF EnableIdeDockingSupport}

{$UNDEF TrickTheIdeAncestorForm}  // this must always be undefined, so that
{$IFDEF TrickTheIdeAncestorForm}  // <--- this define is always false
  TfmIdeDockForm = class(TDummyIdeDockForm);
{$ELSE}
  TfmIdeDockForm = class(TDockableForm)
{$ENDIF TrickTheIdeAncestorForm}
  protected
    FMenuBar: TMenuBar;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IFDEF EnableIdeDockingSupport}
    {$ENDIF EnableIdeDockingSupport}
  end;

type
  TIdeDockFormClass = class of TfmIdeDockForm;

type
  EIdeDockError = class(Exception);

  IIdeDockManager = interface(IUnknown)
  ['{408FC1B1-BD7A-4401-93C2-B41E1D19580B}']
    // Note: IdeDockFormName must be IDE-unique
    procedure RegisterDockableForm(IdeDockFormClass: TIdeDockFormClass;
       var IdeDockFormVar; const IdeDockFormName: string);
    procedure UnRegisterDockableForm(
       var IdeDockFormVar; const IdeDockFormName: string);

    procedure ShowForm(Form: TForm);
  end;

function IdeDockManager: IIdeDockManager;

implementation

uses
  Windows, DeskForm, DeskUtil, GX_GenericClasses, GX_GxUtils;

{$R *.dfm}

type
  TIdeDockManager = class(TSingletonInterfacedObject, IIdeDockManager)
  public
    // Note: IdeDockFormName must be IDE-unique
    procedure RegisterDockableForm(IdeDockFormClass: TIdeDockFormClass;
       var IdeDockFormVar; const IdeDockFormName: string);
    procedure UnRegisterDockableForm(
       var IdeDockFormVar; const IdeDockFormName: string);

    procedure ShowForm(Form: TForm);
  end;

{ TIdeDockManager }

procedure TIdeDockManager.ShowForm(Form: TForm);
begin
  {$IFDEF EnableIdeDockingSupport}
  with Form as TDockableForm do
  begin
    if not Floating then
    begin
      ForceShow;
      FocusWindow(Form);
    end
    else
      Show;
  end;
  {$ELSE}
    Form.Show;
  {$ENDIF EnableIdeDockingSupport}
end;

procedure TIdeDockManager.RegisterDockableForm(IdeDockFormClass: TIdeDockFormClass;
       var IdeDockFormVar; const IdeDockFormName: string);
begin
  {$IFDEF EnableIdeDockingSupport}
  if @RegisterFieldAddress <> nil then
    RegisterFieldAddress(IdeDockFormName, @IdeDockFormVar);

  RegisterDesktopFormClass(IdeDockFormClass, IdeDockFormName, IdeDockFormName);
  {$ENDIF EnableIdeDockingSupport}
end;

procedure TIdeDockManager.UnRegisterDockableForm(var IdeDockFormVar; const IdeDockFormName: string);
{$IFDEF EnableIdeDockingSupport}
{$ENDIF EnableIdeDockingSupport}
begin
  {$IFDEF EnableIdeDockingSupport}
  if @UnregisterFieldAddress <> nil then
    UnregisterFieldAddress(@IdeDockFormVar);
  {$ENDIF EnableIdeDockingSupport}
end;

var
  PrivateIdeDockManager: TIdeDockManager = nil;

function IdeDockManager: IIdeDockManager;
begin
  Result := PrivateIdeDockManager as IIdeDockManager;
end;

{ TfmIdeDockForm }

constructor TfmIdeDockForm.Create(AOwner: TComponent);
begin
  inherited;
  GxSetDefaultFont(Self);

  {$IFDEF EnableIdeDockingSupport}
  if Menu <> nil then
  begin
    FMenuBar := TMenuBar.Create(Self);
    FMenuBar.Parent := Self;
    FMenuBar.Menu := Menu;
    FMenuBar.Height := GetSystemMetrics(SM_CYMENU) + 2;
    Menu := nil;
  end;
  if (PopupMenu = nil) and (FMenuBar <> nil) then
  begin
    PopupMenu := TDummyPopupMenu.Create(Self);
    TDummyPopupMenu(PopupMenu).OwnerMenu := FMenuBar.Menu;
  end;

  DeskSection := Name;
  AutoSave := True;
  SaveStateNecessary := True;
  {$ENDIF EnableIdeDockingSupport}
end;

destructor TfmIdeDockForm.Destroy;
begin
  {$IFDEF EnableIdeDockingSupport}
  SaveStateNecessary := True;
  {$ENDIF EnableIdeDockingSupport}
  inherited Destroy;
end;

{$IFDEF EnableIdeDockingSupport}

{ TDummyPopupMenu }

function TDummyPopupMenu.IsShortCut(var Message: TWMKey): Boolean;
begin
  // Call the form's IsShortCut so docked forms can use main menu shortcuts
  Result := (OwnerMenu <> nil) and OwnerMenu.IsShortCut(Message);
end;
{$ENDIF EnableIdeDockingSupport}

initialization
  PrivateIdeDockManager := TIdeDockManager.Create;

finalization
  FreeAndNil(PrivateIdeDockManager);

end.
