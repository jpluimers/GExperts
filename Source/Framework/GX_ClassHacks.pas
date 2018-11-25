unit GX_ClassHacks;

interface

// Return the class reference that is identified by the class name "ClassName"
// and which is implemented in the binary PE module (e.g. DLL / package)
// ImplementationModule.
//
// GetClassType may return nil if ClassName could not be found in ImplementationModule.
function GetClassReference(const ClassName: string; const ImplementationModule: string): TClass;

implementation

uses
  Windows,
  SysUtils,
  GX_GenericUtils;

function GetClassReference(const ClassName: string; const ImplementationModule: string): TClass;
type
  PPointer = ^Pointer;

  TImageDosHeader = packed record
    e_magic: Word;
    e_cblp: Word;
    e_cp: Word;
    e_crlc: Word;
    e_cparhdr: Word;
    e_minalloc: Word;
    e_maxalloc: Word;
    e_ss: Word;
    e_sp: Word;
    e_csum: Word;
    e_ip: Word;
    e_cs: Word;
    e_lfarlc: Word;
    e_ovno: Word;
    e_res: array[0..3] of Word;
    e_oemid: Word;
    e_oeminfo: Word;
    e_res2: array[0..9] of Word;
    e_lfanew: Longint;
  end;
  PImageDosHeader = ^TImageDosHeader;

var
  NtHeader: PImageNtHeaders;
  SectionHeader: PImageSectionHeader;

  function GetSectionHeader(const ASectionName: string): Boolean;
  var
    i: Integer;
  begin
    SectionHeader := PImageSectionHeader(NtHeader);
    Inc(PImageNtHeaders(SectionHeader));
    Result := False;

    for i := 0 to NtHeader.FileHeader.NumberOfSections - 1 do //FI:W528
    begin
      if StrLIComp(PChar(@SectionHeader^.Name), PChar(ASectionName), IMAGE_SIZEOF_SHORT_NAME) = 0 then
      begin
        Result := True;
        Break;
      end;
      Inc(SectionHeader);
    end;
  end;

  function InRangeOrNil(const APointer, PLowerBound, PUpperBound: Pointer): Boolean;
  begin
    Result := (APointer = nil) or
                ((GXNativeInt(PLowerBound) <= GXNativeInt(APointer)) and
                 (GXNativeInt(APointer) <= GXNativeInt(PUpperBound)));
  end;

var
  DosHeader: PImageDosHeader;
  PCodeBegin: PAnsiChar;
  PCodeEnd: PAnsiChar;
  PCodeScanner: PAnsiChar;
  P: PAnsiChar;

  FoundClassName: PShortString;

  ScannedModule: HMODULE;
begin
  Result := nil;

  // The following scanner is based on code posted by Ludovic Dubois
  //
  //   From: "Ludovic Dubois" <ldubois@prettyobjects.com>
  //   Newsgroups: borland.public.delphi.opentoolsapi
  //   Subject: [Hack] Classes Hack!
  //   Date: Sat, 7 Nov 1998 15:40:48 -0500
  //   Message-ID: <722b75$mer7@forums.borland.com>
  //
  // Note: this code is **not** safe for general purpose use, since there
  // are MANY hidden assumptions which in this use-case incidentally
  // happen to be all valid.
  //
  // All changes with respect to the semantics of the posted
  // code are fully intentional.
  //

  ScannedModule := GetModuleHandle(PChar(ImplementationModule));
  if ScannedModule = 0 then
    Exit;

  // Get PE DOS header
  DosHeader := PImageDosHeader(ScannedModule);

  if not DosHeader.e_magic = IMAGE_DOS_SIGNATURE then
    Exit;

  // Get NT header
  NtHeader := PImageNtHeaders(Longint(DosHeader) + DosHeader.e_lfanew);
  if IsBadReadPtr(NtHeader, SizeOf(IMAGE_NT_HEADERS)) or
     (NtHeader.Signature <> IMAGE_NT_SIGNATURE) or
     (NtHeader.FileHeader.SizeOfOptionalHeader <> SizeOf(NtHeader.OptionalHeader)) then
  begin
    Exit;
  end;

  // Find the code section
  if not GetSectionHeader('CODE') then
    Exit;

  // Compute beginning and end of the code section
  PCodeBegin := PAnsiChar(ScannedModule + SectionHeader.VirtualAddress);
  PCodeEnd := PCodeBegin + (SectionHeader.SizeOfRawData - 3);
  PCodeScanner := PCodeBegin;
  while PCodeScanner < PCodeEnd do
  begin
    P := PPointer(PCodeScanner)^;
    // Search for a(ny) class, employing some heuristics
    if (P = (PCodeScanner - vmtSelfPtr)) and
       InRangeOrNil(PPointer(P + vmtClassName)^,    P,          PCodeEnd) and
       InRangeOrNil(PPointer(P + vmtDynamicTable)^, P,          PCodeEnd) and
       InRangeOrNil(PPointer(P + vmtMethodTable)^,  P,          PCodeEnd) and
       InRangeOrNil(PPointer(P + vmtFieldTable)^,   P,          PCodeEnd) and
       InRangeOrNil(PPointer(P + vmtInitTable)^,    PCodeBegin, PCodeEnd) and
       InRangeOrNil(PPointer(P + vmtAutoTable)^,    PCodeBegin, PCodeEnd) and
       InRangeOrNil(PPointer(P + vmtIntfTable)^,    PCodeBegin, PCodeEnd) then
    begin
      FoundClassName := PShortString(PPointer(P + vmtClassName)^);
      if IsValidIdent(string(FoundClassName^)) and (string(FoundClassName^) = ClassName) then
      begin
        Result := TClass(P);
        Break;
      end;

      Inc(PCodeScanner, 4);
    end
    else
      Inc(PCodeScanner);
  end;
end;

end.
