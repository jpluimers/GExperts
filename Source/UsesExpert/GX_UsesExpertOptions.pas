unit GX_UsesExpertOptions;

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
  StdCtrls;

type
  TfmUsesExpertOptions = class(TForm)
    chkSingleActionMode: TCheckBox;
    chkReplaceFileUnit: TCheckBox;
    btnOK: TButton;
    btnCancel: TButton;
    chkReadMap: TCheckBox;
  private
    procedure SetData(const _CanReplaceFindUseUnit: Boolean;
      const _ReadMapFile, _SingleActionMode, _ReplaceFileUseUnit: Boolean);
    procedure GetData(out _ReadMapFile, _SingleActionMode, _ReplaceFileUseUnit: Boolean);
  public
    class function Execute(_Owner: TComponent; _CanReplaceFindUseUnit: Boolean;
      var _ReadMapFile, _SingleActionMode, _ReplaceFileUseUnit: Boolean): Boolean;
  end;

implementation

{$R *.dfm}

{ TfmUsesExpertOptions }

class function TfmUsesExpertOptions.Execute(_Owner: TComponent; _CanReplaceFindUseUnit: Boolean;
  var _ReadMapFile, _SingleActionMode, _ReplaceFileUseUnit: Boolean): Boolean;
var
  frm: TfmUsesExpertOptions;
begin
  frm := TfmUsesExpertOptions.Create(_Owner);
  try
    frm.SetData(_CanReplaceFindUseUnit, _ReadMapFile, _SingleActionMode, _ReplaceFileUseUnit);
    Result := (frm.ShowModal = mrOk);
    if Result then
      frm.GetData(_ReadMapFile, _SingleActionMode, _ReplaceFileUseUnit);
  finally
    FreeAndNil(frm);
  end;
end;

procedure TfmUsesExpertOptions.GetData(out _ReadMapFile, _SingleActionMode, _ReplaceFileUseUnit: Boolean);
begin
  _ReadMapFile := chkReadMap.Checked;
  _SingleActionMode := chkSingleActionMode.Checked;
  _ReplaceFileUseUnit := chkReplaceFileUnit.Checked;
end;

procedure TfmUsesExpertOptions.SetData(const _CanReplaceFindUseUnit: Boolean;
      const _ReadMapFile, _SingleActionMode, _ReplaceFileUseUnit: Boolean);
begin
  chkReplaceFileUnit.Enabled := _CanReplaceFindUseUnit;
  chkReadMap.Checked := _ReadMapFile;
  chkSingleActionMode.Checked := _SingleActionMode;
  chkReplaceFileUnit.Checked := _ReplaceFileUseUnit;
end;

end.
