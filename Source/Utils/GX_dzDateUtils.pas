unit GX_dzDateUtils;

interface

uses
  SysUtils;

function SecondsToHumanReadableString(_Seconds: Integer): string;

implementation

function SecondsToHumanReadableString(_Seconds: Integer): string;
begin
  if _Seconds < 60 then
    Result := Format('%d seconds', [_Seconds])
  else if _Seconds < 60 * 5 then
    Result := Format('%d minutes and %d seconds', [_Seconds div 60, _Seconds mod 60])
  else if _Seconds < 60 * 60 then
    Result := Format('%d minutes', [_Seconds div 60])
  else if _Seconds < 60 * 60 * 5 then
    Result := Format('%d hours and %d minutes', [_Seconds div 60 div 60, (_Seconds div 60) mod 60])
  else if _Seconds < 60 * 60 * 36 then
    Result := Format('%d days and %d hours', [_Seconds div 60 div 60 div 24, (_Seconds div 60 div 60) mod 24])
  else
    Result := Format('%d days', [_Seconds div 60 div 60 div 24]);
end;

end.
