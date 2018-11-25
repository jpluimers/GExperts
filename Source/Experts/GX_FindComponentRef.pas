// Find Component Reference
// Original Author: Kristofer G. Skaug <kristofer@xs4all.nl>
// This locates the first source reference to a component if a form's component
// is selected.  If the source editor is active, it tries to locate and select
// a form component with the same name as the current source identifier.
//
// Notes:
// - Going from a form identifier in the source to a selected form designer
//   works, but not vice-versa
// - Comments containing a component's name may cause the tool to jump to the
//   comment instead of live/compiled source
// - We do not distinguish between components of the same name on different frames

unit GX_FindComponentRef;

interface

implementation

uses SysUtils, Classes, ToolsAPI,
  GX_OtaUtils, GX_Experts, GX_GenericUtils, GX_KbdShortCutBroker;

resourcestring
  rsNoComponentFound = 'A component named %s was not found on the form';
  rsNoValidIdentifier = 'No identifier was found at the edit cursor';
  rsNoAssociatedForm = 'There is no associated form for %s';
  rsNoAssociatedUnit = 'No source code unit was found for %s';
  rsNoSelectedComponent = 'No component is selected';
  rsIdentiferNotFound =  '%s was not found in the source';
  rsFindComponentReference = 'Find Component Reference';

type
  TFindCompRefWizard = class(TGX_Expert)
  private
    procedure FindSelectedComponentInSource(Module: IOTAModule; FrmEditor: IOTAFormEditor);
    procedure FindSelectedComponentOnForm(Module: IOTAModule; SrcEditor: IOTASourceEditor);
  public
    procedure Execute(Sender: TObject); override;
    class function GetName: string; override;
    function GetDefaultShortCut: TShortCut; override;
    function GetDisplayName: string; override;
    function GetActionCaption: string; override;
    function HasConfigOptions: Boolean; override;
    function HasDesignerMenuItem: Boolean; override;
    constructor Create; override;
  end;

class function TFindCompRefWizard.GetName: string;
begin
  Result := 'FindComponentReference';
end;

procedure TFindCompRefWizard.Execute(Sender: TObject);
var
  Module: IOTAModule;
  CurEditor: IOTAEditor;
  FrmEditor: IOTAFormEditor;
  SrcEditor: IOTASourceEditor;
begin
  Module := GxOtaGetCurrentModule;
  if not Assigned(Module) then
    Exit;
  CurEditor := GxOtaGetCurrentEditor;
  if Supports(CurEditor, IOTAFormEditor, FrmEditor) then
    FindSelectedComponentInSource(Module, FrmEditor)
  else if Supports(CurEditor, IOTASourceEditor, SrcEditor) then
    FindSelectedComponentOnForm(Module, SrcEditor);
end;

procedure TFindCompRefWizard.FindSelectedComponentOnForm(Module: IOTAModule;
  SrcEditor: IOTASourceEditor);
var
  SrcText, CurWord: string;
  FrmEditor: IOTAFormEditor;
  Comp: IOTAComponent;
begin
  FrmEditor := GxOtaGetFormEditorFromModule(Module);
  if Assigned(FrmEditor) then
  begin
    GxOtaGetActiveEditorTextAsString(SrcText, False);
    CurWord := GxOtaGetCurrentIdent;
    if Trim(CurWord) <> '' then
    begin
      Comp := FrmEditor.FindComponent(CurWord);
      if Assigned(Comp) then
      begin
        FrmEditor.Show; // Clears the selection in D10
        Comp.Select(False);
      end
      else
        raise Exception.CreateFmt(rsNoComponentFound, [CurWord]);
    end
    else
      raise Exception.Create(rsNoValidIdentifier);
  end
  else
    raise Exception.CreateFmt(rsNoAssociatedForm, [SrcEditor.FileName]);
end;

procedure TFindCompRefWizard.FindSelectedComponentInSource(Module: IOTAModule;
  FrmEditor: IOTAFormEditor);
var
  Comp: IOTAComponent;
  SrcText: string;
  CompName: string;
  FoundPos: Integer;
  SrcEditor: IOTASourceEditor;
begin
  Comp := FrmEditor.GetSelComponent(0);
  if Assigned(Comp) then
  begin
    CompName := GxOtaGetComponentName(Comp);
    if CompName <> '' then
    begin
      SrcEditor := GxOtaGetSourceEditorFromModule(Module);
      if Assigned(SrcEditor) then
      begin
        SrcEditor.Show;
        GxOtaGetActiveEditorTextAsString(SrcText, False);
        if FindTextIdent(CompName, SrcText, 0, False, FoundPos) then
        begin
          // Try to locate the second reference to the component for Delphi files
          // since the first one is most likely the declaration
          if IsPascalSourceFile(SrcEditor.FileName) then
            FindTextIdent(CompName, SrcText, FoundPos, False, FoundPos);
        end
        else
          raise Exception.CreateFmt(rsIdentiferNotFound, [CompName]);
        GxOtaSetCurrentSourcePosition(FoundPos);
      end
      else
        raise Exception.CreateFmt(rsNoAssociatedUnit, [FrmEditor.FileName]);
    end
  end
  else
    raise Exception.Create(rsNoSelectedComponent);
end;

function TFindCompRefWizard.GetDisplayName: string;
begin
  Result := rsFindComponentReference;
end;

constructor TFindCompRefWizard.Create;
begin
  inherited;
end;

function TFindCompRefWizard.GetDefaultShortCut: TShortCut;
begin
  Result := scCtrl + scShift + Ord('F');
end;

function TFindCompRefWizard.GetActionCaption: string;
begin
  Result := rsFindComponentReference;
end;

function TFindCompRefWizard.HasConfigOptions: Boolean;
begin
  Result := False;
end;

function TFindCompRefWizard.HasDesignerMenuItem: Boolean;
begin
  Result := True;
end;

initialization
  RegisterGX_Expert(TFindCompRefWizard);

end.

