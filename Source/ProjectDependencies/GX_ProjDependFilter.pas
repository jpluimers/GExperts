unit GX_ProjDependFilter;

interface

uses
  Classes, Controls, Forms, Dialogs, StdCtrls, Menus, ExtCtrls;

type
  TfmProjDependFilter = class(TForm)
    lblExcluded: TLabel;
    lbFilter: TListBox;
    dlgOpen: TOpenDialog;
    mnuPopup: TPopupMenu;
    mitAddFiles: TMenuItem;
    mitDeleteFiles: TMenuItem;
    btnOK: TButton;
    btnCancel: TButton;
    edtUnitName: TEdit;
    btnAdd: TButton;
    mitClear: TMenuItem;
    mitSep1: TMenuItem;
    mitSave: TMenuItem;
    mitLoad: TMenuItem;
    dlgFilterOpen: TOpenDialog;
    dlgFilterSave: TSaveDialog;
    pnlFooter: TPanel;
    pnlFilters: TPanel;
    procedure mitAddFilesClick(Sender: TObject);
    procedure mitDeleteFilesClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure mitClearClick(Sender: TObject);
    procedure mitSaveClick(Sender: TObject);
    procedure mitLoadClick(Sender: TObject);
    procedure mnuPopupPopup(Sender: TObject);
    procedure edtUnitNameChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    function GetUnitList: TStrings;
    procedure SetUnitList(const Value: TStrings);
    procedure UpdateButtonState;
  public
    property UnitList: TStrings read GetUnitList write SetUnitList;
  end;

implementation

{$R *.dfm}

uses SysUtils, GX_GenericUtils;

procedure TfmProjDependFilter.mitAddFilesClick(Sender: TObject);
var
  i: Integer;
  TempStr: string;
begin
  if dlgOpen.Execute then
  begin
    for i := 0 to dlgOpen.Files.Count - 1 do
    begin
      TempStr := ExtractPureFileName(dlgOpen.Files[i]);
      EnsureStringInList(lbFilter.Items, TempStr);
    end;
  end;
end;

procedure TfmProjDependFilter.mitDeleteFilesClick(Sender: TObject);
var
  i: Integer;
begin
  if lbFilter.SelCount > 0 then
    for i := lbFilter.Items.Count - 1 downto 0 do
      if lbFilter.Selected[i] then
        lbFilter.Items.Delete(i);
end;

function TfmProjDependFilter.GetUnitList: TStrings;
begin
  Result := lbFilter.Items;
end;

procedure TfmProjDependFilter.SetUnitList(const Value: TStrings);
begin
  lbFilter.Items.Assign(Value);
end;

procedure TfmProjDependFilter.btnAddClick(Sender: TObject);
var
  TempStr: string;
begin
  TempStr := edtUnitName.Text;
  if Trim(TempStr) = '' then
    Exit;
  if lbFilter.Items.IndexOf(TempStr) = -1 then
    lbFilter.Items.Add(TempStr);
   edtUnitName.Clear;
   edtUnitName.SetFocus;
end;

procedure TfmProjDependFilter.mitClearClick(Sender: TObject);
resourcestring
  sConfirmClear = 'Clear the filter list?';
begin
  if MessageDlg(sConfirmClear, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    lbFilter.Clear;
end;

procedure TfmProjDependFilter.mitSaveClick(Sender: TObject);
begin
  if dlgFilterSave.Execute then
    lbFilter.Items.SaveToFile(dlgFilterSave.FileName);
end;

procedure TfmProjDependFilter.mitLoadClick(Sender: TObject);
begin
  if dlgFilterOpen.Execute then
    lbFilter.Items.LoadFromFile(dlgFilterOpen.FileName);
end;

procedure TfmProjDependFilter.mnuPopupPopup(Sender: TObject);
begin
  mitDeleteFiles.Enabled := lbFilter.SelCount > 0;
end;

procedure TfmProjDependFilter.edtUnitNameChange(Sender: TObject);
begin
  UpdateButtonState;
end;

procedure TfmProjDependFilter.FormShow(Sender: TObject);
begin
  UpdateButtonState;
end;

procedure TfmProjDependFilter.UpdateButtonState;
begin
  btnAdd.Enabled := Trim(edtUnitName.Text) <> '';
end;

end.
