unit testfile_unicode;

interface

implementation
function TFSend.IsGSMChar(C: WideChar): Boolean;
begin
  Result := (C =  '@') or (C = '£') or (C = '$') or (C = '¥') or (C = 'è') or (C = 'é') or
    (C = 'ù') or (C = 'î') or (C = 'ô') or (C = 'Ç') or (C = #10) or (C = 'Ø') or (C = 'ø') or
    (C = #13) or (C = 'Å') or (C = 'å') or (C = 'Δ') or (C = '_') or (C = 'Φ') or (C = 'Γ') or
    (C = 'Λ') or (C = 'Ω') or (C = 'Π') or (C = 'Ψ') or (C = 'Σ') or (C = 'Θ') or (C = 'Ξ') or
    (C = #27) or (C = #28) or (C = '^') or (C = '{') or (C = '}') or (C = '\') or (C = '[') or
    (C = '~') or (C = ']') or (C = '|') or (C = '€') or (C = 'Æ') or (C = 'æ') or (C = 'ß') or
    (C = 'É') or CharInSet(C, [' '..'#']) or CharInSet(C, ['%'..'?']) or (C = '¡') or
    CharInSet(C, ['A'..'Z']) or (C = 'Ä') or (C = 'Ö') or (C = 'Ñ') or (C = 'Ü') or
    (C = '§') or (C = '¿') or CharInSet(C, ['a'..'z']) or (C = 'ä') or (C = 'ö') or
    (C = 'ñ') or (C = 'ü') or (C = 'à');
end;
end.
