unit GX_CopyComponentNames;

interface

uses GX_Experts, ActnList;

type
  TCopyComponentNamesExpert = class(TGX_Expert)
  protected
    procedure UpdateAction(Action: TCustomAction); override;
  public
    constructor Create; override;
    function GetActionCaption: string; override;
    class function GetName: string; override;
    procedure Execute(Sender: TObject); override;
    function HasConfigOptions: Boolean; override;
    function HasDesignerMenuItem: Boolean; override;
  end;

implementation

uses
  SysUtils, Classes, TypInfo, Clipbrd, ToolsAPI, Math, GX_OtaUtils;

{ TCopyComponentNamesExpert }

procedure TCopyComponentNamesExpert.UpdateAction(Action: TCustomAction);
begin
  Action.Enabled := GxOtaFormEditorHasSelectedComponent;
end;

function InternalComponentSort(List: TStringList; Index1, Index2: Integer): Integer;
var
  N1, N2 : Integer;
begin
  N1 := Integer(List.Objects[Index1]); // "TabOrder" property value
  N2 := Integer(List.Objects[Index2]);
  Result := CompareValue(N1, N2);

  if Result = 0 then
    Result := CompareText(List.Strings[Index1], List.Strings[Index2]);
end;

procedure TCopyComponentNamesExpert.Execute(Sender: TObject);
var
  FormEditor: IOTAFormEditor;
  SelCount: Integer;
  CurrentComponent: IOTAComponent;
  Names: TStringList;
  ComponentName: WideString;
  LTypeKind : TTypeKind;
  LTabOrder : Integer;
  i: Integer;
begin
  if not GxOtaTryGetCurrentFormEditor(FormEditor) then
    Exit;
  SelCount := FormEditor.GetSelCount;
  Names := TStringList.Create;
  try
    for i := 0 to SelCount - 1 do
    begin
      CurrentComponent := FormEditor.GetSelComponent(i);
      ComponentName := GxOtaGetComponentName(CurrentComponent);
      if ComponentName <> '' then
      begin
        // The order of selected components is not defined, or at least
        // unexpected. Try to sort the component names by the "TabOrder"
        // property value:
        // todo: Since there are many components that do not have a tab order, maybe use
        //       the YX-position instead? But even then: It is possible to select
        //       components with different parents, so the list might still be sorted in
        //       an unexpected way. Maybe sort by YX-Position on the form? That might be best
        //       but then the sorting is much more complicated because it has to compare two
        //       values that can not be easily stored in the Object pointer. OTOH why not
        //       store references to the components there and use GetPropertyValueByName in
        //       the compare function?
        LTabOrder := 0;
        LTypeKind := CurrentComponent.GetPropTypeByName('TabOrder');
        if LTypeKind = tkInteger then
          CurrentComponent.GetPropValueByName('TabOrder', LTabOrder);

        Names.AddObject(ComponentName, Pointer(LTabOrder));
      end;
    end;
    Names.CustomSort(InternalComponentSort);
    Clipboard.AsText := Trim(Names.Text);
  finally
    FreeAndNil(Names);
  end;
  IncCallCount;
end;

constructor TCopyComponentNamesExpert.Create;
begin
  inherited Create;
  //ShortCut := scCtrl + scShift + Ord('N');
end;

function TCopyComponentNamesExpert.GetActionCaption: string;
resourcestring
  SMenuCaption = 'Copy Component &Names';
begin
  Result := SMenuCaption;
end;

class function TCopyComponentNamesExpert.GetName: string;
begin
  Result := 'CopyComponentNames';
end;

function TCopyComponentNamesExpert.HasConfigOptions: Boolean;
begin
  Result := False;
end;

function TCopyComponentNamesExpert.HasDesignerMenuItem: Boolean;
begin
  Result := True;
end;

initialization
  RegisterGX_Expert(TCopyComponentNamesExpert);
  
end.
