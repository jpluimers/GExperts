unit GX_SetFocusControl;

interface

implementation

uses
  Classes, ActnList, Menus, ToolsAPI, Controls, StdCtrls,
  GX_Experts, GX_GExperts, GX_OtaUtils, Contnrs, SysUtils, Types, ExtCtrls,
  ComCtrls;

type
  TGxSetFocusControlExpert = class(TGX_Expert)
  protected
    procedure UpdateAction(Action: TCustomAction); override;
  public
    constructor Create; override;
    class function GetName: string; override;
    function GetActionCaption: string; override;
    function HasConfigOptions: Boolean; override;
    function HasDesignerMenuItem: Boolean; override;
    procedure Click(Sender: TObject); override;
  end;

{ TGxSetFocusControlExpert }

constructor TGxSetFocusControlExpert.Create;
begin
  inherited;
  ShortCut := Menus.ShortCut(Word('A'), [ssCtrl, ssShift]);
end;

function TGxSetFocusControlExpert.GetActionCaption: string;
resourcestring
  SMenuCaption = 'Set &FocusControl';
begin
  Result := SMenuCaption;
end;

class function TGxSetFocusControlExpert.GetName: string;
begin
  Result := 'SetFocusControl';
end;

function TGxSetFocusControlExpert.HasConfigOptions: Boolean;
begin
  Result := False;
end;

function TGxSetFocusControlExpert.HasDesignerMenuItem: Boolean;
begin
  Result := True;
end;

procedure TGxSetFocusControlExpert.UpdateAction(Action: TCustomAction);
begin
  Action.Enabled := GxOtaCurrentlyEditingForm;
end;

type
  TLabelCracker = class(TCustomLabel);

procedure TGxSetFocusControlExpert.Click(Sender: TObject);
var
  Form: IOTAFormEditor;
  FirstSelected: Boolean;

  function CheckIntersect(Control: TWinControl; ControlRect: TRect; ALabel: TCustomLabel): Boolean;
  var
    Intersection: TRect;
    LabelIntf: IOTAComponent;
  begin
    Result := IntersectRect(Intersection, ControlRect, ALabel.BoundsRect);
    // Stop once we find the first label, even if it is already assigned
    if Result and (not Assigned(TLabelCracker(ALabel).FocusControl)) then begin
      TLabelCracker(ALabel).FocusControl := Control;
      LabelIntf := Form.FindComponent(ALabel.Name);
      if Assigned(LabelIntf) then
      begin
        if FirstSelected then
          LabelIntf.Select(False)
        else
          LabelIntf.Select(True);
      end;
      FirstSelected := False;
    end;
  end;

var
  SelLabel: TCustomLabel;
  SelCtrl: TWinControl;
  OTALabel: IOTAComponent;
  ComponentList: TComponentList;
  WinControls: TComponentList;
  Labels: TComponentList;
  ParentLabels: TComponentList;
  i, j: Integer;
  WinControl: TWinControl;
  ControlLeftRect: TRect;
  ControlUpRect: TRect;
  FoundControl: Boolean;
  SearchCount: Integer;
label
  SelWinControls;
begin
  FirstSelected := True;
  Form := GxOtaGetCurrentFormEditor;
  if Assigned(Form) then
  begin
    WinControls := nil;
    Labels := nil;
    ParentLabels := nil;
    ComponentList := TComponentList.Create(False);
    try
      GxOtaGetSelectedComponents(Form, ComponentList);
      if (ComponentList.Count < 1) or GxOtaSelectedComponentIsRoot(Form) then
        raise Exception.Create('Please select either a TLabel and a TWinControl or a group of TWinControls.');
      if (ComponentList.Count = 2) then
      begin
        if (ComponentList[0] is TCustomLabel) and (ComponentList[1] is TWinControl) then
        begin
          SelLabel := ComponentList[0] as TCustomLabel;
          SelCtrl := ComponentList[1] as TWinControl;
        end
        else if (ComponentList[1] is TCustomLabel) and (ComponentList[0] is TWinControl) then
        begin
          SelLabel := ComponentList[1] as TCustomLabel;
          SelCtrl := ComponentList[0] as TWinControl;
        end
        else 
          goto SelWinControls;
        if Assigned(SelLabel) and Assigned(SelCtrl) then
        begin
          TLabelCracker(SelLabel).FocusControl := SelCtrl;
          OTALabel := Form.FindComponent(SelLabel.Name);
          if Assigned(OTALabel) then
            OTALabel.Select(False);
        end;
        Exit;
      end;
      // Assume the user has selected a group of TWinControls, and wants us
      // to auto-set the FocusControl of the "nearest" label (left or above)
      SelWinControls:
      WinControls := TComponentList.Create(False);
      Labels := TComponentList.Create(False);
      ParentLabels := TComponentList.Create(False);
      GxOtaGetComponentList(Form, Labels, TCustomLabel);
      GxOtaGetSelectedComponents(Form, WinControls, TWinControl);
      for i := WinControls.Count - 1 downto 0 do
      begin
        // Ignore certain controls you likely don't want to be focused via a label accelerator
        if (WinControls[i] is TGroupBox) or (WinControls[i] is TButtonControl) or
           (WinControls[i] is TPanel) or (WinControls[i] is TProgressBar) or
           (WinControls[i] is TRadioButton) then
          WinControls.Delete(i);
      end;
      if (WinControls.Count > 0) and (Labels.Count > 0) then
      begin
        GxOtaClearSelectionOnCurrentForm;
        for i := Labels.Count - 1 downto 0 do
          if Assigned(TLabelCracker((Labels[i] as TCustomLabel)).FocusControl) then
            Labels.Delete(i);
        for i := 0 to WinControls.Count - 1 do
        begin
          FoundControl := False;
          WinControl := WinControls[i] as TWinControl;
          for j :=  0 to Labels.Count - 1 do
            if (Labels[j] as TCustomLabel).Parent = WinControl.Parent then
              ParentLabels.Add(Labels[j]);
          ControlLeftRect := WinControl.BoundsRect;
          ControlUpRect := WinControl.BoundsRect;
          SearchCount := 0;
          while ((ControlLeftRect.Left >= 0) or (ControlUpRect.Top >= 0)) and (SearchCount <= 200) do
          begin
            Inc(SearchCount);
            if (ControlLeftRect.Left >= 0) then begin
              for j := 0 to ParentLabels.Count - 1 do
              begin
                if CheckIntersect(WinControl, ControlLeftRect, ParentLabels[j] as TCustomLabel) then
                begin
                  FoundControl := True;
                  Break; // for
                end;
              end;
            end;
            if FoundControl then
              Break; // Next control
            if (ControlUpRect.Top >= 0) and (SearchCount <= 40) then begin
              for j := 0 to ParentLabels.Count - 1 do
              begin
                if CheckIntersect(WinControl, ControlUpRect, ParentLabels[j] as TCustomLabel) then
                begin
                  FoundControl := True;
                  Break; // for
                end;
              end;
            end;
            if FoundControl then
              Break; // Next control
            ControlLeftRect.Left := ControlLeftRect.Left - 1; // Check BiDiMode here?
            ControlUpRect.Top := ControlUpRect.Top - 1;
          end;
          ParentLabels.Clear;
        end;
      end;
    finally
      FreeAndNil(ComponentList);
      FreeAndNil(WinControls);
      FreeAndNil(Labels);
      FreeAndNil(ParentLabels);
    end;
  end
  else
    raise Exception.Create('Please select a form designer');
end;

initialization
  RegisterGX_Expert(TGxSetFocusControlExpert);
end.
