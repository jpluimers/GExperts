unit GX_IdeDialogEnhancer;

interface

uses
  Classes,
  Controls,
  Forms,
  GX_IdeFormEnhancer;

type
  TComponentClass = class of TComponent;

type
  TIdeDialogEnhancer = class
  private
    FFormCallbackHandle: TFormChangeHandle;

    ///<summary>
    /// frm can be nil </summary>
    procedure HandleFormChanged(_Sender: TObject; _Form: TCustomForm);
  protected
    function TryFindComponent(_Owner: TComponent; const _CmpName: string; out _Cmp: TComponent;
      _CmpClass: TComponentClass = nil): Boolean;
    function IsDesiredForm(_Form: TCustomForm): Boolean; virtual;
    procedure EnhanceForm(_Form: TForm); virtual;
  public

    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  GX_dzClassUtils;

{ TIdeDialogEnhancer }

constructor TIdeDialogEnhancer.Create;
begin
  inherited Create;
  FFormCallbackHandle := TIDEFormEnhancements.RegisterFormChangeCallback(HandleFormChanged)
end;

destructor TIdeDialogEnhancer.Destroy;
begin
  TIDEFormEnhancements.UnregisterFormChangeCallback(FFormCallbackHandle);
  inherited;
end;

procedure TIdeDialogEnhancer.EnhanceForm(_Form: TForm);
begin
  // initialize whatever form enhancement this class implements
end;

procedure TIdeDialogEnhancer.HandleFormChanged(_Sender: TObject; _Form: TCustomForm);
var
  frm: TForm;
begin
  if not Assigned(_Form) then
    Exit;
  if IsDesiredForm(_Form) then begin
    frm := _Form as TForm;
    EnhanceForm(frm);
  end;
end;

function TIdeDialogEnhancer.IsDesiredForm(_Form: TCustomForm): Boolean;
begin
  Result := False;
end;

function TIdeDialogEnhancer.TryFindComponent(_Owner: TComponent; const _CmpName: string; out _Cmp: TComponent;
  _CmpClass: TComponentClass = nil): Boolean;
begin
  if TComponent_FindComponent(_Owner, _CmpName, True, _Cmp) then begin
    if Assigned(_CmpClass) then
      Result := (_Cmp is _CmpClass)
    else
      Result := True;
  end else
    Result := False;
end;

end.
