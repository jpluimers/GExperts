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

  procedure AppendExpert(_Expert: TGX_BaseExpert; _Items: TListItems; var _CurrentCnt, _TotalCnt: Integer);
  var
    li: TListItem;
    cnt: Integer;
  begin
    if _Expert.HasCallCount then begin
      li := _Items.Add;
      li.Caption := _Expert.GetDisplayName;
      cnt := _Expert.CallCount;
      li.SubItems.Add(IntToStr(cnt));
      Inc(_CurrentCnt, cnt);
      Inc(cnt, _Expert.TotalCallCount);
      li.SubItems.Add(IntToStr(cnt));
      Inc(_TotalCnt, cnt);
    end;
  end;

  procedure AppendTotals(_Items: TListItems; _CurrentCnt, _TotalCnt: Integer);
  var
    li: TListItem;
  begin
    li := _Items.Add;
    li.Caption := 'Total';
    li.SubItems.Add(IntToStr(_CurrentCnt));
    li.SubItems.Add(IntToStr(_TotalCnt));
  end;

var
  GExperts: TGExperts;
  ExpCurCnt: Integer;
  ExpTotalCnt: Integer;
  EdtCurCnt: Integer;
  EdtTotalCnt: Integer;
  i: Integer;
  Items: TListItems;
begin
  GExperts := GExpertsInst(True);

  ExpCurCnt := 0;
  ExpTotalCnt := 0;
  EdtCurCnt := 0;
  EdtTotalCnt := 0;

  Items := lv_Experts.Items;
  Items.BeginUpdate;
  try
    Items.Clear;
    for i := 0 to GExperts.ExpertCount - 1 do begin
      AppendExpert(GExperts.ExpertList[i], Items, ExpCurCnt, ExpTotalCnt);
    end;
    AppendTotals(Items, ExpCurCnt, ExpTotalCnt);
  finally
    Items.EndUpdate;
  end;

  Items := lv_EditorExperts.Items;
  Items.BeginUpdate;
  try
    Items.Clear;
    for i := 0 to GExperts.EditorExpertManager.EditorExpertCount - 1 do begin
      AppendExpert(GExperts.EditorExpertManager.EditorExpertList[i], Items, EdtCurCnt, EdtTotalCnt);
    end;
    AppendTotals(Items, EdtCurCnt, EdtTotalCnt);
  finally
    Items.EndUpdate;
  end;

  Inc(ExpCurCnt, EdtCurCnt);
  Inc(ExpTotalCnt, EdtTotalCnt);

  if ExpTotalCnt = 0 then
    l_Blurb.Visible := False
  else begin
    l_Blurb.Caption := Format(
      'Assuming each usage of an Expert saved you just one second, you have saved %s in this session and %s in total.',
      [SecondsToHumanReadableString(ExpCurCnt), SecondsToHumanReadableString(ExpTotalCnt)]);
    l_Blurb.Visible := True;
  end;
end;

end.
