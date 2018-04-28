unit GX_UsageStatistics;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  GX_BaseForm,
  ComCtrls,
  StdCtrls,
  ExtCtrls;

type
  TfmUsageStatistics = class(TfmBaseForm)
    p_Bottom: TPanel;
    b_Close: TButton;
    lv_Experts: TListView;
    lv_EditorExperts: TListView;
    b_Clear: TButton;
    l_Blurb: TLabel;
    procedure b_CloseClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure b_ClearClick(Sender: TObject);
  private
    procedure FillStats;
  public
    class procedure Execute(_Owner: TWinControl);
    constructor Create(_Owner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  GX_GExperts,
  GX_BaseExpert,
  GX_dzVclUtils,
  GX_dzDateUtils;

{ TfmUsageStatistics }

class procedure TfmUsageStatistics.Execute(_Owner: TWinControl);
var
  frm: TfmUsageStatistics;
begin
  frm := TfmUsageStatistics.Create(_Owner);
  try
    frm.ShowModal;
  finally
    FreeAndNil(frm);
  end;
end;

constructor TfmUsageStatistics.Create(_Owner: TComponent);
begin
  inherited;

  TControl_SetMinConstraints(Self);

  p_Bottom.BevelOuter := bvNone;
  FillStats;
end;

procedure TfmUsageStatistics.FormResize(Sender: TObject);
begin
  lv_Experts.Width := ClientWidth div 2;
  TListView_Resize(lv_Experts);
  TListView_Resize(lv_EditorExperts);
end;

procedure TfmUsageStatistics.b_ClearClick(Sender: TObject);
var
  GExperts: TGExperts;
  i: Integer;
  Expert: TGX_BaseExpert;
begin
  if MessageDlg('This will clear all current statistics. Are you sure?', mtConfirmation, [mbYes, mbCancel], 0) <> mrYes then
    Exit; //==>

  GExperts := GExpertsInst(True);
  for i := 0 to GExperts.ExpertCount - 1 do begin
    Expert := GExperts.ExpertList[i];
    Expert.ClearCallCounts;
  end;

  for i := 0 to GExperts.EditorExpertManager.EditorExpertCount - 1 do begin
    Expert := GExperts.EditorExpertManager.EditorExpertList[i];
    Expert.ClearCallCounts;
  end;

  FillStats;
end;

procedure TfmUsageStatistics.b_CloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfmUsageStatistics.FillStats;
var
  GExperts: TGExperts;
  ClassUsageCount: Integer;
  ClassTotalCount: Integer;
  AllUsageCount: Integer;
  AllTotalCount: Integer;
  cnt: Integer;
  i: Integer;
  Expert: TGX_BaseExpert;
  li: TListItem;
begin
  lv_Experts.Clear;

  GExperts := GExpertsInst(True);

  ClassUsageCount := 0;
  ClassTotalCount := 0;
  for i := 0 to GExperts.ExpertCount - 1 do begin
    Expert := GExperts.ExpertList[i];
    li := lv_Experts.Items.Add;
    li.Caption := Expert.GetDisplayName;
    cnt := Expert.CallCount;
    li.SubItems.Add(IntToStr(cnt));
    Inc(ClassUsageCount, cnt);
    Inc(cnt, Expert.TotalCallCount);
    li.SubItems.Add(IntToStr(cnt));
    Inc(ClassTotalCount, cnt);
  end;
  li := lv_Experts.Items.Add;
  li.Caption := 'Total';
  li.SubItems.Add(IntToStr(ClassUsageCount));
  li.SubItems.Add(IntToStr(ClassTotalCount));
  TListView_Resize(lv_Experts);

  AllUsageCount := ClassUsageCount;
  AllTotalCount := ClassTotalCount;
  ClassUsageCount := 0;
  ClassTotalCount := 0;
  for i := 0 to GExperts.EditorExpertManager.EditorExpertCount - 1 do begin
    Expert := GExperts.EditorExpertManager.EditorExpertList[i];
    li := lv_EditorExperts.Items.Add;
    li.Caption := Expert.GetDisplayName;
    cnt := Expert.CallCount;
    li.SubItems.Add(IntToStr(cnt));
    Inc(ClassUsageCount, cnt);
    Inc(cnt, Expert.TotalCallCount);
    li.SubItems.Add(IntToStr(cnt));
    Inc(ClassTotalCount, cnt);
  end;
  li := lv_EditorExperts.Items.Add;
  li.Caption := 'Total';
  li.SubItems.Add(IntToStr(ClassUsageCount));
  li.SubItems.Add(IntToStr(ClassTotalCount));
  TListView_Resize(lv_EditorExperts);

  Inc(AllUsageCount, ClassUsageCount);
  Inc(AllTotalCount, ClassTotalCount);

  if AllTotalCount = 0 then
    l_Blurb.Visible := False
  else begin
    l_Blurb.Caption := Format(
      'Assuming each usage of an Expert saved you just one second, you have saved %s in this session and %s in total.',
      [SecondsToHumanReadableString(AllUsageCount), SecondsToHumanReadableString(AllTotalCount)]);
    l_Blurb.Visible := True;
  end;
end;

end.
