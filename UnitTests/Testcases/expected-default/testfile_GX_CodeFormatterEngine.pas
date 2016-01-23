// the main code formatter engine, combines the parser and formatter do do the work
// Original Author:     Egbert van Nes (http://www.dow.wau.nl/aew/People/Egbert_van_Nes.html)
// Contributors:        Thomas Mueller (http://www.dummzeuch.de)
unit GX_CodeFormatterEngine;

interface

uses
{$IFDEF debug}
  // NOTE: including DbugIntf adds around 300 k to the dll
  DbugIntf,
{$ENDIF}
  SysUtils,
  Classes,
  GX_CollectionLikeLists,
  GX_CodeFormatterTypes,
  GX_CodeFormatterTokens,
  GX_CodeFormatterSettings;

(*
WISHLIST:
- suppress read-only file message
- read capitalization from var const type blocks
- sorting methods
- Is it possible to insert a "user customisable" line or group of lines before each
function/procedure, to allow the developer to comment it. Ex :

{------------Name of the proc------------------------}  (Simple)

{***************************
 * ...Comment ...
 * ...Comment ...
 ***************************/ (A few lines)}

 *)

type
  TCodeFormatterEngine = class(TObject)
  private
    FSettings: TCodeFormatterSettings;
    FTokens: TOCollection;
    FText: TStringList;
    function GetLine(var TokenNo: Integer): string;
    function GetSourcecode: string;
    procedure SetSourcecode(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;

    function Execute: Boolean;
    property Sourcecode: string read GetSourcecode write SetSourcecode;
    property Settings: TCodeFormatterSettings read FSettings write FSettings;
  end;

implementation

uses
  StrUtils,
  GX_CodeFormatterFormatter,
  GX_CodeFormatterParser;

constructor TCodeFormatterEngine.Create;
begin
  inherited;
  FSettings := TCodeFormatterSettings.Create;
  FText := TStringList.Create;
end;

destructor TCodeFormatterEngine.Destroy;
begin
  FreeAndNil(FText);
  FreeAndNil(FSettings);
  FreeAndNil(FTokens);
  inherited;
end;

function TCodeFormatterEngine.Execute: Boolean;
begin
  try
    if Assigned(FTokens) then
      FTokens.Free;
    FTokens := TCodeFormatterParser.Execute(FText, FSettings);
    FText.Clear; // we can free some memory since the text is now stored in the tokens
    TCodeFormatterFormatter.Execute(FTokens, FSettings);
    Result := True;
  except
    on E: Exception do begin
      Result := False;
      {ShowMessage('Error occurred, cannot format');}
    end;
  end;
end;

function TCodeFormatterEngine.GetLine(var TokenNo: Integer): string;
var
  Token: TPascalToken;
  i: Integer;
begin
  Result := '';
  if not Assigned(FTokens) then
    Exit;

  if (TokenNo >= 0) and (TokenNo < FTokens.Count) then begin
    Token := FTokens.Items[TokenNo];
    repeat
      Result := Result + Token.GetString;
      Inc(TokenNo);
      if TokenNo >= FTokens.Count then
        break;
      Token := TPascalToken(FTokens.Items[TokenNo]);
    until Token.ReservedType = rtLineFeed;
  end;
  // remove spaces and tabs at the end
  i := Length(Result);
  while (i > 0) and (Result[i] in [' ', Tab]) do begin
    Dec(i);
    SetLength(Result, i);
  end;
end;

function TCodeFormatterEngine.GetSourcecode: string;
var
  Line: string;
  TokenNo: Integer;
  Size: Cardinal;
  LB: string;
begin
  FText.Clear;

  LB := sLineBreak;
  Size := 0;
  if Assigned(FTokens) then begin
    TokenNo := 0;
    while TokenNo < FTokens.Count do begin
      Line := GetLine(TokenNo);
      FText.Add(Line);
      Size := Size + Cardinal(Length(Line)) + Cardinal(Length(LB));
    end;
  end;

  Result := FText.Text;
  FText.Clear;
end;

procedure TCodeFormatterEngine.SetSourcecode(const Value: string);
begin
  FText.Text := Value;
end;

end.
