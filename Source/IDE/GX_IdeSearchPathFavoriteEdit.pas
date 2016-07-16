unit GX_IdeSearchPathFavoriteEdit;

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
  Tf_IdeSearchPathFavoriteEdit = class(TfmBaseForm)
    l_Name: TLabel;
    ed_Name: TEdit;
    l_Path: TLabel;
    m_Path: TMemo;
    b_OK: TButton;
    b_Cancel: TButton;
    b_Select: TButton;
    procedure b_SelectClick(Sender: TObject);
  private
    procedure GetData(out _Name, _Path: string);
    procedure SetData(const _Name, _Path: string);
    procedure HandleFilesDropped(_Sender: TObject; _Files: TStrings);
  public
    class function Execute(_Owner: TComponent; var _Name, _Path: string): Boolean;
    constructor Create(_Owner: TComponent); override;
  end;

implementation

uses
  StrUtils,
  GX_dzVclUtils,
  GX_dzSelectDirectoryFix;

{$R *.dfm}

{ Tf_IdeSearchPathFavoriteEdit }

class function Tf_IdeSearchPathFavoriteEdit.Execute(_Owner: TComponent; var _Name, _Path: string): Boolean;
var
  frm: Tf_IdeSearchPathFavoriteEdit;
begin
  frm := Tf_IdeSearchPathFavoriteEdit.Create(_Owner);
  try
    frm.SetData(_Name, _Path);
    Result := (frm.ShowModal = mrok);
    if Result then
      frm.GetData(_Name, _Path);
  finally
    FreeAndNil(frm);
  end;
end;

procedure Tf_IdeSearchPathFavoriteEdit.b_SelectClick(Sender: TObject);
var
  Dir: string;
begin
  Dir := '';
  if not dzSelectDirectory('Select directory to add', '', Dir, Self) then
    Exit;
  m_Path.Lines.Add(Dir);
end;

constructor Tf_IdeSearchPathFavoriteEdit.Create(_Owner: TComponent);
begin
  inherited;

  TControl_SetMinConstraints(Self);

  m_Path.Lines.Delimiter := ';';
  TWinControl_ActivateDropFiles(m_Path, HandleFilesDropped);
end;

procedure Tf_IdeSearchPathFavoriteEdit.HandleFilesDropped(_Sender: TObject; _Files: TStrings);
begin
  m_Path.Lines.AddStrings(_Files);
end;

procedure Tf_IdeSearchPathFavoriteEdit.GetData(out _Name, _Path: string);
begin
  _Name := ed_Name.Text;
  _Path := AnsiDequotedStr(m_Path.Lines.DelimitedText, '"');
end;

procedure Tf_IdeSearchPathFavoriteEdit.SetData(const _Name, _Path: string);
begin
  ed_Name.Text := _Name;
  m_Path.Lines.DelimitedText := AnsiDequotedStr(_Path, '"');
end;

end.
