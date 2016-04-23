unit GX_eSort;

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
  GX_BaseForm, StdCtrls;

type
  TeSortOrder = (esoAscending, esoDescending, esoReverse);

type
  TfmeSortConfig = class(TfmBaseForm)
    btnAscending: TButton;
    btnDescending: TButton;
    btnReverse: TButton;
    btnCancel: TButton;
    chkIgnoreFunction: TCheckBox;
  private
  public
    class function Execute(out _SortOrder: TeSortOrder; out _Ignore: Boolean): Boolean;
  end;

implementation

{$R *.dfm}

uses
  GX_EditorExpert, GX_eSelectionEditorExpert, StrUtils;

{ TfmeSortConfig }

class function TfmeSortConfig.Execute(out _SortOrder: TeSortOrder; out _Ignore: Boolean): Boolean;
var
  frm: TfmeSortConfig;
begin
  frm := TfmeSortConfig.Create(nil);
  try
    Result := True;
    case frm.ShowModal of
      mrYes:
        _SortOrder := esoAscending;
      mrNo:
        _SortOrder := esoDescending;
      mrRetry:
        _SortOrder := esoReverse;
      else
        Result := False;
      _Ignore := frm.chkIgnoreFunction.Checked;
    end;
  finally
    FreeAndNil(frm);
  end;
end;

type
  TSortExpert = class(TSelectionEditorExpert)
  protected
    function ProcessSelected(Lines: TStrings): Boolean; override;
  public
    class function GetName: string; override;
    function GetDisplayName: string; override;
    function GetHelpString: string; override;
    function HasConfigOptions: boolean; override;
  end;

{ TSortExpert }

function TSortExpert.GetDisplayName: string;
resourcestring
  SSortName = 'Sort Selected Lines';
begin
  Result := SSortName;
end;

function TSortExpert.GetHelpString: string;
resourcestring
  SSortHelp = '  This expert sorts the lines in a selected block of code.  ' +
    'To use it, select several lines in the IDE code editor and ' +
    'activate this expert.';
begin
  Result := SSortHelp;
end;

class function TSortExpert.GetName: string;
begin
  Result := 'SortLines';
end;

function TSortExpert.HasConfigOptions: boolean;
begin
  Result := False;
end;

function TSortExpert.ProcessSelected(Lines: TStrings): Boolean;

  function StripPrefix(const _Prefix: string; var _Line: string): boolean;
  begin
    Result := AnsiStartsText(_Prefix, _Line);
    if Result then
      _Line := Copy(_Line, Length(_Prefix), 255);
  end;

var
  TrimList, SortedList: TStringList;
  i: Integer;
  SortOrder: TESortOrder;
  Direction: integer;
  IgnoreFunction: Boolean;
  s: string;
begin
  Result := False;

  if Lines.Count > 1 then
  begin
    if not  TfmeSortConfig.Execute(SortOrder, IgnoreFunction) then
      exit;

    // The trim mess here is so we can ignore whitespace when sorting
    SortedList := nil;
    TrimList := TStringList.Create;
    try
      SortedList := TStringList.Create;
      for i := 0 to Lines.Count - 1 do begin
        s := TrimLeft(Lines[i]);
        if IgnoreFunction then
          if not StripPrefix('procedure ', s) then
            StripPrefix('function ', s);
        TrimList.AddObject(s, TObject(i));
      end;  
      case SortOrder of
        esoAscending: begin
          i := 0;
          Direction := 1;
          TrimList.Sort;
        end;
        esoDescending: begin
          i := TrimList.Count - 1;
          Direction := -1;
          TrimList.Sort;
        end
      else // esoReverse:
        i := TrimList.Count - 1;
        Direction := -1;
      end;
      while (i >= 0) and (i < TrimList.Count) do begin
        SortedList.Add(Lines[Integer(TrimList.Objects[i])]);
        i := i + Direction;
      end;
      Lines.Clear;
      Lines.AddStrings(SortedList);
    finally
      FreeAndNil(SortedList);
      FreeAndNil(TrimList);
    end;
    Result := True;
  end;
end;

initialization
  RegisterEditorExpert(TSortExpert);
end.
