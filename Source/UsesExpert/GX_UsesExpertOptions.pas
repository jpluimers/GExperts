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
    chkReplaceFileUnit: TCheckBox;
    btnOK: TButton;
    btnCancel: TButton;
    chkReadMap: TCheckBox;
    chkParseAll: TCheckBox;
  private
    procedure SetData(const _CanReplaceFindUseUnit: Boolean;
      const _ReadMapFile, _ReplaceFileUseUnit, _ParseAll: Boolean);
    procedure GetData(out _ReadMapFile, _ReplaceFileUseUnit, _ParseAll: Boolean);
  public
    class function Execute(_Owner: TComponent; _CanReplaceFindUseUnit: Boolean;
      var _ReadMapFile, _ReplaceFileUseUnit, _ParseAll: Boolean): Boolean;
  end;

implementation

{$R *.dfm}

{ TfmUsesExpertOptions }

class function TfmUsesExpertOptions.Execute(_Owner: TComponent; _CanReplaceFindUseUnit: Boolean;
  var _ReadMapFile, _ReplaceFileUseUnit, _ParseAll: Boolean): Boolean;
var
  frm: TfmUsesExpertOptions;
begin
  frm := TfmUsesExpertOptions.Create(_Owner);
  try
    frm.SetData(_CanReplaceFindUseUnit, _ReadMapFile, _ReplaceFileUseUnit, _ParseAll);
    Result := (frm.ShowModal = mrOk);
    if Result then
      frm.GetData(_ReadMapFile, _ReplaceFileUseUnit, _ParseAll);
  finally
    FreeAndNil(frm);
  end;
end;

procedure TfmUsesExpertOptions.GetData(out _ReadMapFile, _ReplaceFileUseUnit, _ParseAll: Boolean);
begin
  _ReadMapFile := chkReadMap.Checked;
  _ReplaceFileUseUnit := chkReplaceFileUnit.Checked;
  _ParseAll := chkParseAll.Checked;
end;

procedure TfmUsesExpertOptions.SetData(const _CanReplaceFindUseUnit: Boolean;
  const _ReadMapFile, _ReplaceFileUseUnit, _ParseAll: Boolean);
begin
  chkReplaceFileUnit.Enabled := _CanReplaceFindUseUnit;
  chkReadMap.Checked := _ReadMapFile;
  chkReplaceFileUnit.Checked := _ReplaceFileUseUnit;
  chkParseAll.Checked := _ParseAll;
end;

end.
