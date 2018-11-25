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
  StdCtrls,
  GX_BaseForm,
  GX_GenericUtils;

type
  TeSortOrder = (esoAscending, esoDescending, esoReverse, esoCustom);

type
  TfmeSortConfig = class(TfmBaseForm)
    btnAscending: TButton;
    btnDescending: TButton;
    btnReverse: TButton;
    btnCancel: TButton;
    grpSort: TGroupBox;
    chkIgnoreFunction: TCheckBox;
    btnCustom: TButton;
    btnConfigCustom: TButton;
    procedure btnConfigCustomClick(Sender: TObject);
  private
    FCustomPrefixOrder: TGXUnicodeStringList;
  public
    class function Execute(_CustomPrefixOrder: TGXUnicodeStringList; out _SortOrder: TeSortOrder; out _Ignore: Boolean): Boolean;
  end;

implementation

{$R *.dfm}

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  StrUtils,
  {$IFNDEF UNICODE} SynUnicode, {$ENDIF UNICODE}
  GX_EditorExpert, GX_eSelectionEditorExpert, GX_ConfigurationInfo,
  GX_eSortOptions, GX_dzQuicksort, Math;

{ TfmeSortConfig }

procedure TfmeSortConfig.btnConfigCustomClick(Sender: TObject);
begin
  TfrmSortOptions.Execute(Self, FCustomPrefixOrder);
end;

class function TfmeSortConfig.Execute(_CustomPrefixOrder: TGXUnicodeStringList; out _SortOrder: TeSortOrder;
  out _Ignore: Boolean): Boolean;
var
  frm: TfmeSortConfig;
begin
  frm := TfmeSortConfig.Create(nil);
  try
    frm.FCustomPrefixOrder := _CustomPrefixOrder;
    Result := True;
    case frm.ShowModal of
      mrYes:
        _SortOrder := esoAscending;
      mrNo:
        _SortOrder := esoDescending;
      mrRetry:
        _SortOrder := esoReverse;
      mrAll:
        _SortOrder := esoCustom;
    else
      Result := False;
    end;
    _Ignore := frm.chkIgnoreFunction.Checked;
  finally
    FreeAndNil(frm);
  end;
end;

const
  PREFIX_NONE = '<none>';

type
  TSortExpert = class(TSelectionEditorExpert)
  private
    FCustomPrefixOrder: TGXUnicodeStringList;
  protected
    function ProcessSelected(Lines: TStrings): Boolean; override;
  public
    class function GetName: string; override;
    constructor Create; override;
    destructor Destroy; override;
    function GetDisplayName: string; override;
    function GetHelpString: string; override;
    function HasConfigOptions: boolean; override;
    procedure Configure; override;
    procedure InternalLoadSettings(Settings: TExpertSettings); override;
    procedure InternalSaveSettings(Settings: TExpertSettings); override;
  end;

{ TSortExpert }

const
  PrefixArray: array[1..9] of string = (
    // default sort order for custom sort
    // do not translate!
    'class function',
    'class procedure',
    'class operator',
    'class property',
    'constructor',
    'destructor',
    'function',
    'procedure',
    'property');


constructor TSortExpert.Create;
var
  i: integer;
begin
  inherited;

  FCustomPrefixOrder:= TGXUnicodeStringList.Create;

  FCustomPrefixOrder.Add(PREFIX_NONE);
  for i := Low(PrefixArray) to High(PrefixArray) do
    FCustomPrefixOrder.Add(PrefixArray[i]);
end;

destructor TSortExpert.Destroy;
begin
  FreeAndNil(FCustomPrefixOrder);
  inherited;
end;

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
  Result := True;
end;


procedure TSortExpert.Configure;
begin
  TfrmSortOptions.Execute(nil, FCustomPrefixOrder);
end;

procedure TSortExpert.InternalLoadSettings(Settings: TExpertSettings);
var
  sl: TStringList;
  i: Integer;
begin
  inherited;
  sl := TStringList.Create;
  try
    Settings.ReadStrings('CustomSortOrder', sl);
    if sl.Count <> Length(PrefixArray) + 1 then begin
      {$IFOPT D+}SendDebug('Number of CustomSortOrder items is wrong, ignoring them.'); {$ENDIF}
      Exit; //==>
    end;

    FCustomPrefixOrder.Clear;
    for i := 0 to sl.Count - 1 do begin
      FCustomPrefixOrder.Add(sl[i]);
    end;
  finally
    FreeAndNil(sl);
  end;
end;

procedure TSortExpert.InternalSaveSettings(Settings: TExpertSettings);
var
  sl: TStringList;
  i: Integer;
begin
  inherited;
  sl := TStringList.Create;
  try
    for i := 0 to FCustomPrefixOrder.Count - 1 do begin
      sl.Add(String(FCustomPrefixOrder[i]));
    end;
    Settings.WriteStrings('CustomSortOrder', sl);
  finally
    FreeAndNil(sl);
  end;
end;

type
  TStringListSorter = class
  private
    FStringList: TGXUnicodeStringList;
  protected
    function CompareItems(_Idx1, _Idx2: Integer): Integer; virtual; abstract;
  public
    constructor Create(_StringList: TGXUnicodeStringList);
    procedure Sort;
  end;

  TStringListCustomSorter = class(TStringListSorter)
  private
    FCustomPrefixOrder: TGXUnicodeStringList;
  protected
    function CompareItems(_Idx1, _Idx2: Integer): Integer; override;
  public
    constructor Create(_StringList: TGXUnicodeStringList; _CustomPrefixOrder: TGXUnicodeStringList);
  end;

function TSortExpert.ProcessSelected(Lines: TStrings): Boolean;

  function StripPrefix(const _Prefix: string; var _Line: string): Boolean;
  begin
    Result := AnsiStartsText(_Prefix + ' ', _Line);
    if Result then
      _Line := Trim(Copy(_Line, Length(_Prefix) + 1, 255));
  end;

var
  TrimList, SortedList: TGXUnicodeStringList;
  i: Integer;
  SortOrder: TESortOrder;
  Direction: integer;
  IgnoreFunction: Boolean;
  s: string;
  PrefixIdx: Integer;
  Sorter: TStringListCustomSorter;
begin
  Result := False;

  if Lines.Count < 1 then
    Exit; //==>

  if not TfmeSortConfig.Execute(FCustomPrefixOrder, SortOrder, IgnoreFunction) then
    exit;

  if not (SortOrder in [esoAscending, esoDescending]) then
    IgnoreFunction := False;

  // we use a special SortedList in order to ignore leading white space and prefixes 
  SortedList := nil;
  TrimList := TGXUnicodeStringList.Create;
  try
    SortedList := TGXUnicodeStringList.Create;
    for i := 0 to Lines.Count - 1 do begin
      s := TrimLeft(Lines[i]);
      if IgnoreFunction then begin
        for PrefixIdx := Low(PrefixArray) to High(PrefixArray) do begin
          if StripPrefix(PrefixArray[PrefixIdx], s) then
            Break; //==>
        end;
      end;
      TrimList.AddObject(s, TObject(i));
    end;
    case SortOrder of
      esoAscending: begin
        i := 0;
        Direction := 1;
        TrimList.SortLogical;
      end;
      esoDescending: begin
        i := TrimList.Count - 1;
        Direction := -1;
        TrimList.SortLogical;
      end;
      esoCustom: begin
        i := 0;
        Direction := 1;
        Sorter := TStringListCustomSorter.Create(TrimList, FCustomPrefixOrder);
        try
          Sorter.Sort;
        finally
                    FreeAndNil(Sorter);
        end;
      end;
    else // esoReverse:
      i := TrimList.Count - 1;
      Direction := -1;
    end;
    while (i >= 0) and (i < TrimList.Count) do begin
      SortedList.Add(Lines[GXNativeInt(TrimList.Objects[i])]);
      i := i + Direction;
    end;
    Lines.Clear;
    for i := 0 to SortedList.Count - 1 do
      Lines.add(SortedList[i]);
  finally
    FreeAndNil(SortedList);
    FreeAndNil(TrimList);
  end;
  Result := True;
end;

{ TStringListSorter }

constructor TStringListSorter.Create(_StringList: TGXUnicodeStringList);
begin
  inherited Create;
  FStringList := _StringList;
end;

procedure TStringListSorter.Sort;
begin
  QuickSort(0, FStringList.Count-1, CompareItems, FStringList.Exchange);
end;

{ TStringListCustomSorter }

constructor TStringListCustomSorter.Create(_StringList, _CustomPrefixOrder: TGXUnicodeStringList);
begin
  inherited Create(_StringList);
  FCustomPrefixOrder := _CustomPrefixOrder;
end;

function TStringListCustomSorter.CompareItems(_Idx1, _Idx2: Integer): Integer;
var
  s1: TGXUnicodeString;
  s2: TGXUnicodeString;
  Prefix: TGXUnicodeString;
  NoneIdx: Integer;
  PrefixIdx1: Integer;
  PrefixIdx2: Integer;
  i: Integer;
begin
  s1 := FStringList[_Idx1];
  s2 := FStringList[_Idx2];

  // Determine the PrefixIdx for the given strings
  NoneIdx := -1;
  PrefixIdx1 := -1;
  PrefixIdx2 := -1;
  for i := 0 to FCustomPrefixOrder.Count - 1 do begin
    Prefix := FCustomPrefixOrder[i];
    if Prefix = PREFIX_NONE then
      NoneIdx := i
    else begin
      Prefix := Prefix + ' ';
      if (PrefixIdx1 = -1) and StartsText(Prefix, s1) then begin
        PrefixIdx1 := i;
        s1 := Copy(s1, Length(Prefix) + 1, 255);
      end;
      if (PrefixIdx2 = -1) and StartsText(Prefix, s2) then begin
        PrefixIdx2 := i;
        s2 := Copy(s2, Length(Prefix) + 1, 255);
      end;
    end;
  end;

  // Set the PrefixIdx to NoneIdx, if they are still -1
  if PrefixIdx1 = -1 then
    PrefixIdx1 := NoneIdx;
  if PrefixIdx2 = -1 then
    PrefixIdx2 := NoneIdx;

  // Compare by PrefixIdx first and then alphabetically
  Result := CompareValue(PrefixIdx1, PrefixIdx2);
  if Result = 0 then begin
    Result := CompareText(s1, s2);
  end;
end;

initialization
  RegisterEditorExpert(TSortExpert);
end.
