unit GX_RegExp;

interface

uses SysUtils, Classes,
  RegExpr,
  GX_EditReader;

type
  TSearchOption = (soCaseSensitive, soWholeWord, soRegEx);

  TSearchOptions = set of TSearchOption;

  TFileComment = (fcNone, fcPas, fcCPP);

  TFoundEvent = procedure(Sender: TObject; LineNo: Integer; const Line: WideString; SPos, FEditReaderPos: Integer) of object;

  ELineTooLong = class(Exception);

  TSearcher = class(TRegExpr)
  private
    FIncludeForms: Boolean;
    FNoComments: Boolean;
    FBufSize: Integer;
    FPattern: WideString;
    FFileName: WideString;
    FOnFound: TFoundEvent;
    FMode: TModuleMode;
    FOnStartSearch: TNotifyEvent;
    FSearchOptions: TSearchOptions;
    procedure SetANSICompatible(const Value: Boolean);
    procedure SetBufSize(const Value: Integer);
    procedure SetFileName(const Value: WideString);
  public
    constructor Create(const SearchFileName: WideString); overload;
    destructor Destroy; override;
    procedure SetPattern(const Source: WideString);
    property ANSICompatible: Boolean write SetANSICompatible;
    property BufSize: Integer read FBufSize write SetBufSize;
    property NoComments: Boolean read FNoComments write FNoComments;
    property Pattern: WideString read FPattern;
    property SearchOptions: TSearchOptions read FSearchOptions write FSearchOptions;
    property OnFound: TFoundEvent read FOnFound write FOnFound;
    property OnStartSearch: TNotifyEvent read FOnStartSearch write FOnStartSearch;
    procedure Execute;
    property FileName: WideString read FFileName write SetFileName;
    property IncludeForms: Boolean read FIncludeForms write FIncludeForms;
    property Mode: TModuleMode read FMode;
    procedure NotifyMatch;
 end;

implementation

uses
  GX_OtaUtils;

{ TSearcher }

constructor TSearcher.Create(const SearchFileName: WideString);
begin
  inherited Create;
  FileName := SearchFileName;
end;

destructor TSearcher.Destroy;
begin
  inherited;
end;

procedure TSearcher.Execute;
begin
  if Trim(FFileName) = '' then
    raise Exception.Create('File not found');
  if FileExists(FFileName) then
  begin
    if Self.Exec(''{GxOtaFileToWideString(FileName)}) then
    begin
      NotifyMatch;
      while Self.ExecNext do
        NotifyMatch;
    end;
  end;
end;

procedure TSearcher.NotifyMatch;
begin
  if Assigned(FOnFound) then
  begin
    FOnFound(Self, 1, 'Test line of text that is a little longer than an average line of code', 1, 25);
    
  end;
end;

procedure TSearcher.SetANSICompatible(const Value: Boolean);
begin
end;

procedure TSearcher.SetBufSize(const Value: Integer);
begin
  FBufSize := Value;
end;

procedure TSearcher.SetFileName(const Value: WideString);
begin
  FFileName := Value;
end;

procedure TSearcher.SetPattern(const Source: WideString);
begin
  Self.Expression := Source;
end;

end.
