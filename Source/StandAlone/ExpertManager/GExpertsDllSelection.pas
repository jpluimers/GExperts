unit GExpertsDllSelection;

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
  TfrGEXpertsDllSelection = class(TForm)
    lb_GExpertsDlls: TListBox;
    l_SelectDll: TLabel;
    b_Load: TButton;
    b_Exit: TButton;
    procedure b_ExitClick(Sender: TObject);
    procedure lb_GExpertsDllsClick(Sender: TObject);
    procedure lb_GExpertsDllsDblClick(Sender: TObject);
    procedure b_LoadClick(Sender: TObject);
  private
    FPath: string;
    procedure SetData(const _Path: string; _DLLs: TStrings);
    procedure LoadSelectedDll;
    class procedure LoadDll(const _GExpertsDLLName: string);
  public
    class procedure Execute; overload;
    class procedure Execute(const _Path: string; _DLLs: TStrings); overload;
  end;

implementation

{$R *.dfm}

uses
  GX_VerDepConst;

type
  TShowExpertManager = procedure;

{ TfrGEXpertsDllSelection }

class procedure TfrGEXpertsDllSelection.Execute;
var
  GExpertsDLLName: string;
  sr: TSearchRec;
  Path: string;
  sl: TStringList;
begin
  if ParamCount > 0 then
    GExpertsDLLName := ParamStr(1)
  else begin
    sl := TStringList.Create;
    try
      Path := IncludeTrailingPathDelimiter(ExtractFileDir(Application.ExeName));
      if 0 = FindFirst(Path + 'GExperts*.dll', faArchive, sr) then begin
        try
          repeat
            sl.Add(sr.Name);
          until 0 <> FindNext(sr);
        finally
          FindClose(sr);
        end;
      end;
      if sl.Count = 0 then
        GExpertsDLLName := GExpertsDll
      else if sl.Count = 1 then
        GExpertsDLLName := sl[0]
      else begin
        Execute(Path, sl);
        exit;
      end;
    finally
      FreeAndNil(sl);
    end;
  end;

  LoadDll(GExpertsDLLName);
end;

class procedure TfrGEXpertsDllSelection.LoadDll(const _GExpertsDLLName: string);
var
  Handle: Cardinal;
  ShowExpertManager: TShowExpertManager;
begin
  Handle := SysUtils.SafeLoadLibrary(_GExpertsDLLName);
  if Handle = 0 then
    raise Exception.CreateFmt('Could not load library %s.', [_GExpertsDLLName]);
  @ShowExpertManager := GetProcAddress(Handle, 'ShowExpertManager');
  if not Assigned(ShowExpertManager) then
    raise Exception.CreateFmt('Could not find entry point "ShowExpertManager" in dll %s', [_GExpertsDLLName]);
  ShowExpertManager;
end;

procedure TfrGEXpertsDllSelection.b_ExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrGEXpertsDllSelection.b_LoadClick(Sender: TObject);
begin
  LoadSelectedDll;
end;

class procedure TfrGEXpertsDllSelection.Execute(const _Path: string; _DLLs: TStrings);
var
  frm: TfrGEXpertsDllSelection;
begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrGEXpertsDllSelection, frm);
  frm.SetData(_Path, _DLLs);
  Application.Run;
end;

procedure TfrGEXpertsDllSelection.lb_GExpertsDllsClick(Sender: TObject);
begin
  b_Load.Enabled := (lb_GExpertsDlls.ItemIndex <> -1);
end;

procedure TfrGEXpertsDllSelection.lb_GExpertsDllsDblClick(Sender: TObject);
begin
  LoadSelectedDll;
end;

procedure TfrGEXpertsDllSelection.SetData(const _Path: string; _DLLs: TStrings);
begin
  FPath := _Path;
  lb_GExpertsDlls.Items.Assign(_DLLs);
end;

procedure TfrGEXpertsDllSelection.LoadSelectedDll;
var
  Idx: Integer;
begin
  Idx := lb_GExpertsDlls.ItemIndex;
  if Idx = -1 then
    exit;
  LoadDll(FPath + lb_GExpertsDlls.Items[Idx]);
end;

end.
