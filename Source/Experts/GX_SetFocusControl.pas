unit GX_SetFocusControl;

interface

implementation

uses
  Classes, ActnList, Menus, Windows, Controls, StdCtrls, Contnrs, Types,
  Math, ToolsAPI, GX_Experts, GX_GExperts, GX_OtaUtils;

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
    procedure Execute(Sender: TObject); override;
  end;

  TLabelHack = class(TCustomLabel);

{ TGxSetFocusControlExpert }

constructor TGxSetFocusControlExpert.Create;
begin
  inherited;
  //ShortCut := Menus.ShortCut(Word('F'), [ssShift, ssCtrl]);
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

procedure TGxSetFocusControlExpert.Execute(Sender: TObject);
var
  Form: IOTAFormEditor;
  FirstControlSelected: Boolean;

  procedure SetFocusControl(ALabel: TCustomLabel; AControl: TWinControl; Force: Boolean);
  var
    OTALabel: IOTAComponent;
  begin
    if Force or (TLabelHack(ALabel).FocusControl = nil) then
    begin
      TLabelHack(ALabel).FocusControl := AControl;
      OTALabel := Form.FindComponent(ALabel.Name);
      if Assigned(OTALabel) then
        OTALabel.Select(FirstControlSelected);
      FirstControlSelected := True;
    end;
  end;

  function ProcessFixed: Boolean;
  var
    Component1, Component2: TComponent;
  begin
    if Form.GetSelCount = 2 then
    begin
      Component1 := GxOtaGetNativeComponent(Form.GetSelComponent(0));
      Component2 := GxOtaGetNativeComponent(Form.GetSelComponent(1));
      if (Component1 is TCustomLabel) and (Component2 is TWinControl) then
        SetFocusControl(TCustomLabel(Component1), TWinControl(Component2), True)
      else if (Component2 is TCustomLabel) and (Component1 is TWinControl) then
        SetFocusControl(TCustomLabel(Component2), TWinControl(Component1), True);
    end;
    Result := FirstControlSelected;
  end;

  function LabelDistance(ALabel: TCustomLabel; AControl: TWinControl): Integer;
  begin
    if (ALabel.Left > (AControl.Left + AControl.Width)) or (ALabel.Top > (AControl.Top + AControl.Height)) then
      Result := MaxInt
    else
      Result := Max(Abs(AControl.Left - ALabel.Left), Abs(AControl.Top - ALabel.Top));
  end;

  function LabelInControlRange(ALabel: TCustomLabel; AControl: TWinControl): Boolean;
  var
    Intersection: TRect;
    LeftRect, TopRect: TRect;
  const
    LeftDistLimit = 130;
    TopDistLimit = 30;
  begin
    LeftRect := Rect(AControl.Left - LeftDistLimit, AControl.Top, AControl.Left, AControl.Top + AControl.Height);
    TopRect := Rect(AControl.Left, AControl.Top - TopDistLimit, AControl.Left + AControl.Width, AControl.Top);
    Result :=
      IntersectRect(Intersection, ALabel.BoundsRect, LeftRect) or
      IntersectRect(Intersection, ALabel.BoundsRect, TopRect);
  end;

  function FindNearestLabel(AControl: TWinControl; Labels: TComponentList;
    out ResultLabel: TCustomLabel): Boolean;
  var
    LeastDistance, Distance: Integer;
    i: Integer;
    ALabel: TCustomLabel;
  begin
    ResultLabel := nil;
    Result := False;
    LeastDistance := MaxInt;
    for i := 0 to Labels.Count - 1 do
    begin
      ALabel := TCustomLabel(Labels[i]);
      if ALabel.Parent = AControl.Parent then
      begin
        Distance := LabelDistance(ALabel, AControl);
        if LabelInControlRange(ALabel, AControl) and ((Distance < LeastDistance)
          or ((Distance = LeastDistance) and Assigned(ResultLabel) and (ALabel.Top > ResultLabel.Top))) then
        begin
          LeastDistance := Distance;
          ResultLabel := ALabel;
          Result := True;
        end;
      end;
    end;
  end;

  procedure ProcessAutomatic;
  var
    Labels, Controls: TComponentList;
    Force: Boolean;
    i: Integer;
    ALabel: TCustomLabel;
    AControl: TWinControl;
  begin
    Labels := TComponentList.Create(False);
    Controls := TComponentList.Create(False);
    try
      GxOtaGetComponentList(Form, Labels, TCustomLabel);
      if (Form.GetSelCount = 0) or GxOtaSelectedComponentIsRoot(Form) then
        GxOtaGetComponentList(Form, Controls, TWinControl)
      else
        GxOtaGetSelectedComponents(Form, Controls, TWinControl);
      Force := GetKeyState(VK_SHIFT) < 0;
      for i := 0 to Controls.Count - 1 do
      begin
        AControl := TWinControl(Controls[i]);
        if AControl.TabStop and (not (AControl is TButtonControl)) then
          if FindNearestLabel(AControl, Labels, ALabel) then
            SetFocusControl(ALabel, AControl, Force);
      end;
    finally
      Labels.Free;
      Controls.Free;
    end;
  end;

begin
  Form := GxOtaGetCurrentFormEditor;
  FirstControlSelected := False;
  if not ProcessFixed then
    ProcessAutomatic;

  IncCallCount;
end;

initialization
  RegisterGX_Expert(TGxSetFocusControlExpert);
end.
