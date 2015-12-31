unit GX_EditBookmark;

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
  StdCtrls,
  GX_BaseForm;

type
  TfmEditBookmarks = class(TfmBaseForm)
    ed_Line: TEdit;
    l_BmIndex: TLabel;
    cmb_BmIndex: TComboBox;
    l_Module: TLabel;
    cmb_Module: TComboBox;
    l_Line: TLabel;
    b_Ok: TButton;
    b_Cancel: TButton;
    procedure cmb_ModuleChange(Sender: TObject);
    procedure ed_LineChange(Sender: TObject);
    procedure cmb_BmIndexChange(Sender: TObject);
  private
    procedure SetData(const _Module: string; _LineNo: Integer; _BmIndex: Integer);
    procedure GetData(out _Module: string; out _LineNo: Integer; out _BmIndex: Integer);
    procedure CheckInput;
  public
    class function Execute(_Owner: TWinControl;
      var _Module: string; var _LineNo: Integer; var _BmIndex: Integer): Boolean; overload; static;
    class function Execute(_Owner: TWinControl; const _Module: string;
      var _LineNo: Integer): Boolean; overload; static;
    constructor Create(_Owner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

uses
  GX_OtaUtils,
  GX_dzVclUtils;

{ TfmEditBookmarks }

class function TfmEditBookmarks.Execute(_Owner: TWinControl; var _Module: string;
  var _LineNo, _BmIndex: Integer): Boolean;
var
  frm: TfmEditBookmarks;
begin
  frm := TfmEditBookmarks.Create(_Owner);
  try
    frm.SetData(_Module, _LineNo, _BmIndex);
    Result := (frm.ShowModal = mrok);
    if Result then
      frm.GetData(_Module, _LineNo, _BmIndex);
  finally
    FreeAndNil(frm);
  end;
end;

class function TfmEditBookmarks.Execute(_Owner: TWinControl; const _Module: string;
  var _LineNo: Integer): Boolean;
var
  frm: TfmEditBookmarks;
  BmIndex: Integer;
  Module: string;
begin
  frm := TfmEditBookmarks.Create(_Owner);
  try
    frm.SetData(_Module, _LineNo, -1);
    frm.cmb_BmIndex.Visible := False;
    frm.l_BmIndex.Visible := False;
    Result := (frm.ShowModal = mrok);
    if Result then
      frm.GetData(Module, _LineNo, BmIndex);
  finally
    FreeAndNil(frm);
  end;
end;

constructor TfmEditBookmarks.Create(_Owner: TComponent);
resourcestring
  rcNext = 'Next';
var
  i: Integer;
  List: TList;
  ui: TUnitInfo;
begin
  inherited;

  TControl_SetMinConstraints(Self);
  TComboBox_SetDropdownWidth(cmb_Module, Self.Width);

  List := TList.Create;
  try
    GxOtaFillUnitInfoListForCurrentProject(List);
    for i := 0 to List.Count - 1 do begin
      ui := TObject(List[i]) as TUnitInfo;
      cmb_Module.Items.AddObject(ExtractFileName(ui.Filename), ui);
    end;
  finally
    FreeAndNil(List);
  end;

  TComboBox_AddIntObject(cmb_BmIndex, rcNext, -1);
  for i := 0 to 19 do begin
    TComboBox_AddIntObject(cmb_BmIndex, IntToStr(i), i);
  end;
end;

destructor TfmEditBookmarks.Destroy;
begin
  TComboBox_ClearWithObjects(cmb_Module);
  inherited;
end;

procedure TfmEditBookmarks.cmb_BmIndexChange(Sender: TObject);
begin
  CheckInput;
end;

procedure TfmEditBookmarks.cmb_ModuleChange(Sender: TObject);
begin
  CheckInput;
end;

procedure TfmEditBookmarks.ed_LineChange(Sender: TObject);
begin
  CheckInput;
end;

procedure TfmEditBookmarks.CheckInput;
var
  b: Boolean;
  Value: Integer;
  s: string;
begin
  b := TComboBox_GetSelected(cmb_Module, s);
  b := b and TryStrToInt(ed_Line.Text, Value) and (Value > 0);
  // bmIndex may be left empty
  b_Ok.Enabled := b;
end;

procedure TfmEditBookmarks.SetData(const _Module: string; _LineNo, _BmIndex: Integer);
begin
  TComboBox_Select(cmb_Module, ExtractFileName(_Module));
  TComboBox_SelectByObject(cmb_BmIndex, _BmIndex);
  ed_Line.Text := IntToStr(_LineNo);
end;

procedure TfmEditBookmarks.GetData(out _Module: string; out _LineNo, _BmIndex: Integer);
resourcestring
  rcSelectAModule = 'You must select a module.';
var
  ui: TUnitInfo;
begin
  if not TComboBox_GetSelectedObject(cmb_Module, Pointer(ui)) then
    raise Exception.Create(rcSelectAModule);
  _Module := ui.Filename;
  _BmIndex := TComboBox_GetSelectedObjectDef(cmb_BmIndex, -1);
  _LineNo := StrToInt(ed_Line.Text);
end;

end.
