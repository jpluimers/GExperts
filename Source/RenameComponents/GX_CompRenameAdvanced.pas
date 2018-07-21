unit GX_CompRenameAdvanced;

interface

uses
  SysUtils,
  Classes,
  Controls,
  StdCtrls,
  Forms,
  GX_BaseForm;

type
  TfmCompRenameAdvanced = class(TfmBaseForm)
    lblComponentClass: TLabel;
    lblProperties: TLabel;
    btnOk: TButton;
    btnCancel: TButton;
    mmoPropertyNames: TMemo;
  private
    procedure SetData(const _CompClass: string; _Additional: TStrings);
    procedure GetData(var _Additional: TStringList);
  public
    class function Execute(_Owner: TWinControl; const _CompClass: string; var _Additional: TStringList): Boolean;
  end;

implementation

{$R *.dfm}

uses
  GX_dzVclUtils;

{ TfmCompRenameAdvanced }

class function TfmCompRenameAdvanced.Execute(_Owner: TWinControl; const _CompClass: string;
  var _Additional: TStringList): Boolean;
var
  frm: TfmCompRenameAdvanced;
begin
  frm := TfmCompRenameAdvanced.Create(_Owner);
  try
    TForm_CenterOn(frm, _Owner);
    frm.SetData(_CompClass, _Additional);
    Result := (frm.ShowModal = mrOK);
    if Result then
      frm.GetData(_Additional);
  finally
    FreeAndNil(frm);
  end;
end;

procedure TfmCompRenameAdvanced.GetData(var _Additional: TStringList);

  function RemoveSpaces(const Value: string): string;
  begin
    Result := Value;
    Result := StringReplace(Result, ' =', '=', []);
    Result := StringReplace(Result, '= ', '=', []);
  end;

var
  sl: TStringList;
  Lines: TStrings;
  s: string;
  i: Integer;
begin
  sl := TStringList.Create;
  try
    Lines := mmoPropertyNames.Lines;
    for i := 0 to Lines.Count - 1 do begin
      s := Trim(RemoveSpaces(Lines[i]));
      if s <> '' then
        sl.Add(s);
    end;
    if sl.Count > 0 then begin
      if Assigned(_Additional) then
        _Additional.Free;
      _Additional := sl;
      sl := nil;
    end else
      FreeAndNil(_Additional);
  finally
    FreeAndNil(sl);
  end;
end;

procedure TfmCompRenameAdvanced.SetData(const _CompClass: string; _Additional: TStrings);
begin
  lblComponentClass.Caption := _CompClass;
  if Assigned(_Additional) then
    mmoPropertyNames.Lines.Assign(_Additional);
end;

end.
