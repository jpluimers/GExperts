unit GX_PeInfo;

{$I GX_CondDefine.inc}

// We don't support reading:
//   Delay loading info
//   PE+ (64-bit) binaries
//   Section details
//   Symbol table
//   Data directory details
//   Relocations
//   Fixups
//   Resources
//   Thread local storage

interface

uses
  Classes;

type
  DosHeader = record
    e_magic: SmallInt; // Magic number
    e_cblp: SmallInt; // Bytes on last page of file
    e_cp: SmallInt; // Pages in file
    e_crcl: SmallInt; // Relocations
    e_cparhdr: SmallInt; // Size of header in paragraphs
    e_minalloc: SmallInt; // Minimum extra paragraphs needed
    e_maxalloc: SmallInt; // Maximum extra paragraphs needed
    e_ss: SmallInt; // Initial (relative) SS value
    e_sp: SmallInt; // Initial SP value
    e_csum: SmallInt; // Checksum
    e_ip: SmallInt; // Initial IP value
    e_cs: SmallInt; // Initial (relative) CS value
    e_lfarclc: SmallInt; // File address of relocation table
    e_ovno: SmallInt; // Overlay number
    e_res: array[1..4] of SmallInt; // Reserved words
    e_oemid: SmallInt; // OEM identifier (for e_oeminfo)
    e_oeminfo: SmallInt; // OEM information; e_oemid specific
    e_res2: array[1..10] of SmallInt; // Reserved words
    e_lfanew: Integer; // File address of new exe header
  end;

  PEImgHeader = record
    Machine: Word;
    NumberOfSections: SmallInt;
    TimeDateStamp: Integer;
    PointerToSymboltable: Integer;
    NumberofSymbols: Integer;
    SizeOfOptionalHeader: SmallInt;
    Characteristics: SmallInt;
  end;

  Image_Data_Directory = record
    VirtualAddress: Integer;
    Size: Integer;
  end;

  Thunk_Data = record
    AddressOfData: Integer;
  end;

  Import_Function = record
    Ordinal: SmallInt;
    Name: array[0..255] of AnsiChar;
  end;

  PEOptionalHeader = record
    Magic: SmallInt;
    MajorLinkerVersion: Byte;
    MinorLinkerVersion: Byte;
    SizeOfCode: Integer;
    SizeOfInitializedData: Integer;
    SizeOfUninitializedData: Integer;
    AddressOfEntryPoint: Integer;
    BaseOfCode: Integer;
    BaseOfData: Integer;
    ImageBase: Integer;
    SectionAlignment: Integer;
    FileAlignment: Integer;
    MajorOperatingSystemVersion: SmallInt;
    MinorOperatingSystemVersion: SmallInt;
    MajorImageVersion: SmallInt;
    MinorImageVersion: SmallInt;
    MajorSubsystemVersion: SmallInt;
    MinorSubsystemVersion: SmallInt;
    Win32Version: Integer;
    SizeOfImage: Integer;
    SizeOfHeaders: Integer;
    Checksum: Integer;
    Subsystem: SmallInt;
    DLLCharacteristics: SmallInt;
    SizeOfStackReserve: Integer;
    SizeOfStackCommit: Integer;
    SizeOfHeapReserve: Integer;
    SizeOfHeapCommit: Integer;
    LoaderFlags: Integer;
    NumberofRVAandSizes: Integer;
    DataDirectories: array[0..15] of Image_Data_Directory;
  end;

  ImageSectionHeader = record
    Name: array[0..7] of AnsiChar;
    VirtualSize: Integer;
    VirtualAddress: Integer;
    SizeOfRawData: Integer;
    PointerToRawData: Integer;
    PointerToRelocations: Integer;
    PointerToLineNumbers: Integer;
    NumberofRelocations: SmallInt;
    NumberofLineNumbers: SmallInt;
    Characteristics: Integer;
  end;

  PEImportDescriptors = record
    Characteristics: Integer;
    TimeDateStamp: Integer;
    ForwarderChain: Integer;
    Name: Integer;
    FirstThunk: Integer;
  end;

  PEExportImage = record
    Characteristics: Integer;
    TimeDateStamp: Integer;
    MajorVersion: SmallInt;
    MinorVersion: SmallInt;
    Name: Integer;
    Base: Integer;
    NumberOfFunctions: Integer;
    NumberOfNames: Integer;
    AddressOfFunctions: Integer;
    AddressOfNames: Integer;
    AddressOfNameOrdinals: Integer;
  end;

  TImportExportItem = class(TCollectionItem)
  private
    FFunctionName: string;
    FOrdinal: Integer;
  public
    property FunctionName: string read FFunctionName write FFunctionName;
    property Ordinal: Integer read FOrdinal write FOrdinal;
  end;

  TImportExport = class(TCollection)
  private
    FPEName: string;
    function GetItem(Index: Integer): TImportExportItem;
    procedure SetItem(Index: Integer; Value: TImportExportItem);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    function Add: TImportExportItem;
    property PEName: string read FPEName write FPEName;
    property Items[Index: Integer]: TImportExportItem read GetItem write SetItem; default;
  end;

  TNumberType = (ntDecimal, ntHex);

  TPEFileInfo = class(TObject)
  private
    FStream: TFileStream;
    FFileName: string;
    FIsPE: Boolean;
    FIsPE32: Boolean;
    FIsNE: Boolean;
    FIsLE: Boolean;
    FIsMSDos: Boolean;
    FImportList: TStringList;
    FExportList: TStringList;
    FMSDOSHeader: TStringList;
    FPEHeaderList: TStringList;
    FPEOptionalHeaderList: TStringList;
    FSectionStart: Integer;
    FDosHeader: DosHeader;
    FPEHeader: PEImgHeader;
    FPEOptionalHeader: PEOptionalHeader;
    FNumberType: TNumberType;
    procedure ReadPEFileFormat;
    procedure ReadImportList;
    function GetEnclosingSectionHeader(rva: Integer; var SectionHeader: ImageSectionHeader): Boolean;
    procedure GetImportFunctions(const Import: PEImportDescriptors; Delta: Integer; ImpExp: TImportExport);
    procedure ReadExportList;
    function GetCPUType: string;
    procedure FillMSDOSHeader;
    procedure FillPEOptionalHeader;
    procedure FillPEHeader;
    function GetExportList: TStrings;
    function GetImportList: TStrings;
    function GetMSDOSHeader: TStrings;
    function GetPEHeaderList: TStrings;
    function GetPEOptionalHeaderList: TStrings;
  public
    function IntToNum(Num: Integer): string;
    constructor Create(const FName: string; NType: TNumberType);
    destructor Destroy; override;
    property FileName: string read FFileName;
    property CPUType: string read GetCPUType;
    property NumberType: TNumberType read FNumberType write FNumberType;
    property IsPE: Boolean read FIsPE;
    property IsPE32: Boolean read FIsPE32;
    property IsNE: Boolean read FIsNE;
    property IsLE: Boolean read FIsLE;
    property IsMSDos: Boolean read FIsMSDos;
    property ImportList: TStrings read GetImportList;
    property ExportList: TStrings read GetExportList;
    property MSDOSHeader: TStrings read GetMSDOSHeader;
    property PEHeaderList: TStrings read GetPEHeaderList;
    property PEOptionalHeaderList: TStrings read GetPEOptionalHeaderList;
  end;

const
  IMAGE_DIRECTORY_ENTRY_EXPORT = 0;
  IMAGE_DIRECTORY_ENTRY_IMPORT = 1; // Import Directory
  IMAGE_DIRECTORY_ENTRY_RESOURCE = 2; // Resource Directory
  IMAGE_DIRECTORY_ENTRY_EXCEPTION = 3; // Exception Directory
  IMAGE_DIRECTORY_ENTRY_SECURITY = 4; // Security Directory
  IMAGE_DIRECTORY_ENTRY_BASERELOC = 5; // Base Relocation Table
  IMAGE_DIRECTORY_ENTRY_DEBUG = 6; // Debug Directory
  IMAGE_DIRECTORY_ENTRY_COPYRIGHT = 7; // Description String
  IMAGE_DIRECTORY_ENTRY_GLOBALPTR = 8; // Machine Value (MIPS GP)
  IMAGE_DIRECTORY_ENTRY_TLS = 9; // TLS Directory
  IMAGE_DIRECTORY_ENTRY_LOAD_CONFIG = 10; // Load Configuration Directory

implementation

uses
  SysUtils,
{$IFDEF GX_VER250_up}
  AnsiStrings,
{$ENDIF GX_VER250_up}
  DateUtils, Windows;

function TImportExport.GetItem(Index: Integer): TImportExportItem;
begin
  Result := TImportExportItem(inherited GetItem(Index));
end;

procedure TImportExport.SetItem(Index: Integer; Value: TImportExportItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TImportExport.Update(Item: TCollectionItem);
begin //FI:W519
   {Nothing for now}
end;

function TImportExport.Add: TImportExportItem;
begin
  Result := TImportExportItem(inherited Add);
end;

constructor TPEFileInfo.Create(const FName: string; NType: TNumberType);
begin
  inherited Create;
  NumberType := NType;
  FImportList := TStringList.Create;
  FExportList := TStringList.Create;
  FMSDOSHeader := TStringList.Create;
  FPEHeaderList := TStringList.Create;
  FPEOptionalHeaderList := TStringList.Create;

  FFileName := FName;
  ReadPEFileFormat;
end;

destructor TPEFileInfo.Destroy;
var
  i: Integer;
begin
  FreeAndNil(FMSDOSHeader);
  FreeAndNil(FPEHeaderList);
  FreeAndNil(FPEOptionalHeaderList);
  FreeAndNil(FExportList);

  if Assigned(FImportList) then
  begin
    for i := 0 to FImportList.Count-1 do
      TImportExport(FImportList.Objects[i]).Free;

    FreeAndNil(FImportList);
  end;

  inherited Destroy;
end;

function TPEFileInfo.IntToNum(Num: Integer): string;
const // Duplicated here for use outside of GExperts
  GXHexPrefix = {$IFDEF GX_BCB}'0x'{$ELSE}'$'{$ENDIF};
begin
  try
    case FNumberType of
      ntDecimal: Result := IntToStr(Num);
      ntHex: Result := Format(GXHexPrefix+'%x', [Num]);
    end;
  except
    on E: EConvertError do
      Result := '';
  end;
end;

procedure TPEFileInfo.ReadPEFileFormat;
resourcestring
  SInvalidPeFile = 'Invalid PE file format';
  SExecutableFormat = 'Executable format';
var
  PE: array[0..3] of AnsiChar;
  BytesRead: Integer;
begin
  FStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    BytesRead := FStream.Read(FDosHeader, 64);
    if BytesRead <> 64 then
      raise Exception.Create(SInvalidPeFile);

    FIsMSDos := (FDosHeader.e_magic = $5A4D);
    FillMSDOSHeader;

    FStream.Position := FDosHeader.e_lfanew;
    BytesRead := FStream.Read(PE, 4);
    if BytesRead <> 4 then
      raise Exception.Create(SInvalidPeFile + ': No PE signature found');

    if (PE[0] = 'P') and (PE[1] = 'E') and (PE[2] = #0) and (PE[3] = #0) then
    begin
      FIsPE := True;
      FPEHeaderList.Add(SExecutableFormat + #9 + 'PE Format');
    end
    else if (PE[0] = 'N') and (PE[1] = 'E') then
    begin
      FIsNE := True;
      FPEHeaderList.Add(SExecutableFormat + #9 + 'Unsupported 16-bit NE Format');
    end
    else if (PE[0] = 'L') and (PE[1] = 'E') then
    begin
      FIsLE := True;
      FPEHeaderList.Add(SExecutableFormat + #9 + 'Unsupported LE Format');
    end
    else
      FPEHeaderList.Add(SExecutableFormat + #9 + 'Unsupported (Unknown)');

    BytesRead := FStream.Read(FPEHeader, 20);
    if BytesRead <> 20 then
      raise Exception.Create(SInvalidPeFile);

    BytesRead := FStream.Read(FPEOptionalHeader, SizeOf(FPEOptionalHeader));
    if BytesRead <>  SizeOf(FPEOptionalHeader) then
      raise Exception.Create(SInvalidPeFile);

    if IsPE then
    begin
      FSectionStart := FStream.Position;
      FillPEHeader;
      FillPEOptionalHeader;
      if IsPE32 then
      begin
        ReadImportList;
        ReadExportList;
      end;
    end;
  finally
    FreeAndNil(FStream);
  end;
end;

procedure TPEFileInfo.ReadImportList;

  function GetName(L: Integer): string;
  var
    SPos: Integer;
    Buf: array[0..1024] of AnsiChar;
  begin
    SPos := FStream.Position;
    FStream.Position := L;
    FStream.Read(Buf, 1024);
    Result := string({$IFDEF GX_VER250_up}AnsiStrings.{$ENDIF}StrPas(Buf));
    FStream.Position := SPos;
  end;

var
  Import: PEImportDescriptors;
  Delta, i: Integer;
  SectionHeader: ImageSectionHeader;
  Name: string;
  ImpExp: TImportExport;
begin
  FImportList.Sorted := False;
  FImportList.Clear;
  if not GetEnclosingSectionHeader(FPEOptionalHeader.DataDirectories[1].VirtualAddress, SectionHeader) then
    Exit;
  Delta := SectionHeader.VirtualAddress - SectionHeader.PointerToRawData;
  FStream.Position := FPEOptionalHeader.DataDirectories[1].VirtualAddress - Delta;
  FStream.Read(Import, SizeOf(Import));
  while (Import.Name <> 0) and (FStream.Position < FStream.Size) do
  begin
    Name := GetName(Import.Name - Delta);
    if FImportList.Indexof(Name) < 0 then
    begin
      ImpExp := TImportExport.Create(TImportExportItem);
      ImpExp.PEName := Name;
      FImportList.AddObject(Name, ImpExp);
    end;
    i := FImportList.Indexof(Name);
    GetImportFunctions(Import, Delta, TImportExport(FImportList.Objects[i]));
    FStream.Read(Import, SizeOf(Import));
  end;
  FImportList.Sorted := True;
end;

procedure TPEFileInfo.GetImportFunctions(const Import: PEImportDescriptors; Delta: Integer; ImpExp: TImportExport);
var
  Thunk: Integer;
  SPos, i: Integer;
  ThunkData: Thunk_Data;
  ImpF: Import_Function;
begin
  SPos := FStream.Position;
  try
    if Import.Characteristics = 0 then
      Thunk := Import.FirstThunk - Delta
    else
      Thunk := Import.Characteristics - Delta;
    FStream.Position := Thunk;
    FStream.Read(ThunkData, SizeOf(ThunkData));
    while (FStream.Position < FStream.Size) and (ThunkData.AddressOfData <> 0) do
    begin
      i := FStream.Position;
      if ThunkData.AddressOfData < 0 then
      begin {Imported by Ordinal}
        FStream.Position := Thunk;
        with ImpExp.Add do
        begin
          Ordinal := Word(ThunkData.AddressOfData);
          FunctionName := '';
        end;
      end
      else
      begin
        FStream.Position := ThunkData.AddressOfData - Delta;
        FStream.Read(ImpF, SizeOf(ImpF));
        with ImpExp.Add do
        begin
          Ordinal := ImpF.Ordinal;
          FunctionName := string({$IFDEF GX_VER250_up}AnsiStrings.{$ENDIF}StrPas(ImpF.Name));
        end;
      end;
      FStream.Position := i;
      FStream.Read(ThunkData, SizeOf(ThunkData));
    end;
  finally
    FStream.Position := SPos;
  end;
end;

function TPEFileInfo.GetEnclosingSectionHeader(rva: Integer; var SectionHeader: ImageSectionHeader): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to FPEHeader.NumberOfSections - 1 do
  begin
    FStream.Position := FSectionStart + SizeOf(SectionHeader) * i;
    FStream.Read(SectionHeader, SizeOf(SectionHeader));
    if (rva >= SectionHeader.VirtualAddress) and
       (rva < SectionHeader.VirtualAddress + SectionHeader.VirtualSize) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

procedure TPEFileInfo.ReadExportList;
var
  SectionHeader: ImageSectionHeader;
  ExportAddress: Integer;
  NameAddress: Integer;
  Buffer: array[0..255] of AnsiChar;
  j: Integer;
  MaxExports: Longint;
  Ordinal: Word;
  ExportVA: DWORD;
  ExportInfo: PEExportImage;
  Delta: Integer;
begin
  FExportList.Clear;
  with FPEOptionalHeader.DataDirectories[0] do
    ExportVA := VirtualAddress;
  if not GetEnclosingSectionHeader(ExportVA, SectionHeader) then
    Exit;
  Delta := SectionHeader.VirtualAddress - SectionHeader.PointerToRawData;
  FStream.Position := Longint(ExportVA) - Delta;
  FStream.Read(ExportInfo, SizeOf(ExportInfo));
  if ExportInfo.Characteristics <> 0 then
    Exit;
  FStream.Position := Longint(ExportInfo.Name) - Delta;
  with ExportInfo do
  begin
    MaxExports := NumberOfFunctions;
    if NumberOfNames < MaxExports then
      MaxExports := NumberOfNames;
  end;
  FExportList.Sorted := False;
  try
    FExportList.Capacity := MaxExports;
    FStream.Position := Longint(ExportInfo.AddressOfNameOrdinals) - Delta;
    for j := 0 to MaxExports-1 do //FI:W528
    begin
      FStream.ReadBuffer(Ordinal, SizeOf(Ordinal));
      FExportList.Add(#9 + IntToNum(Ordinal + ExportInfo.Base) + #9);
    end;
    // Now read the names themselves
    for j := 0 to MaxExports-1 do
    begin
      FStream.Position := (Longint(ExportInfo.AddressOfNames) - Delta)
                          + (j * SizeOf(NameAddress));
      FStream.ReadBuffer(NameAddress, SizeOf(NameAddress));
      FStream.Position := NameAddress - Delta;
      FStream.Read(Buffer, SizeOf(Buffer));
      FExportList[j] := string({$IFDEF GX_VER250_up}AnsiStrings.{$ENDIF}StrPas(Buffer)) + FExportList[j];
    end;
    // And finally read the function associated with each export
    FStream.Position := Longint(ExportInfo.AddressOfFunctions) - Delta;
    for j := 0 to MaxExports-1 do
    begin
      FStream.ReadBuffer(ExportAddress, SizeOf(ExportAddress));
      FExportList[j] := FExportList[j] + IntToNum(ExportAddress);
    end;
  finally
    FExportList.Sorted := True;
  end;
end;

function TPEFileInfo.GetCPUType: string;
resourcestring
  SAllCpuTypes = 'Any architecture';
  SIntel_80386 = 'Intel 80386 or later';
  SIntel_80486 = 'Intel 80486';
  SIntel_Pentium = 'Intel Pentium';
  SMIPS_R3000_BigEndian = 'MIPS R3000 (big endian)';
  SMIPS_Mark_I = 'MIPS Mark I R2000 and R3000 (little endian)';
  SMIPS_Mark_II = 'MIPS Mark II (R6000)';
  SMIPS_Mark_III = 'MIPS Mark III (R4000, little endian)';
  SMIPS_R10000 = 'MIPS R10000';
  SDEC_Alpha_XP = 'DEC Alpha XP';
  SPower_PC = 'Power PC (little endian)';
  SPower_PC_FP = 'Power PC (floating point)';
  SMotorola_68000 = 'Motorola 68000';
  SPA_RISC = 'HP PA RISC';
  SUnknown_CPU = 'Unknown CPU';
  SHitachiSH3 = 'Hitachi SH3';
  SHitachiSH3E = 'Hitachi SH3E DSP';
  SHitachiSH4 = 'Hitachi SH4';
  SHitachiSH5 = 'Hitachi SH5';
  SARM = 'ARM (little endian)';
  SThumb = 'Thumb';
  SIntelIA64 = 'Intel IA64 Itanium';
  SMips16 = 'MIPS 16';
  SAlphaAXP64 = '64-bit Alpha AXP';
  SMipsFPU = 'MIPS with FPU';
  SMips16FPU = 'MIPS 16 with FPU';
  SMipsWceV2 = 'MIPS WCE v2 (little endian)';
  SArm33 = 'Matsushita ARM 33';
  SAMD64 = 'AMD K8 (64-bit)';
  SInfineonTricore = 'Infineon Tricore';
  SCEF = 'CEF';

begin
  if not IsPE then
  begin
    Result := '';
    Exit;
  end;

  case FPEHeader.Machine of
    $000: Result := SAllCpuTypes;
    $14C: Result := SIntel_80386;
    $14D: Result := SIntel_80486;
    $14E: Result := SIntel_Pentium;
    $160: Result := SMIPS_R3000_BigEndian;
    $162: Result := SMIPS_Mark_I;
    $163: Result := SMIPS_Mark_II;
    $166: Result := SMIPS_Mark_III;
    $168: Result := SMIPS_R10000;
    $169: Result := SMipsWceV2;
    $184: Result := SDEC_Alpha_XP;
    $1A2: Result := SHitachiSH3;
    $1A4: Result := SHitachiSH3E;
    $1A6: Result := SHitachiSH4;
    $1A8: Result := SHitachiSH5;
    $1C0: Result := SARM;
    $1C2: Result := SThumb;
    $1D3: Result := SArm33;
    $1F0: Result := SPower_PC;
    $1F1: Result := SPower_PC_FP;
    $200: Result := SIntelIA64;
    $266: Result := SMips16;
    $268: Result := SMotorola_68000;
    $284: Result := SAlphaAXP64;
    $290: Result := SPA_RISC;
    $366: Result := SMipsFPU;
    $466: Result := SMips16FPU;
    $500: Result := SAMD64;
    $520: Result := SInfineonTricore;
    $CEF: Result := SCEF;
    $EBC: Result := 'EFI Byte Code';
    $8664: Result := 'AMD x64';
    $9041: Result := 'Mitsubishi M32R (little endian)';
  else
    Result := SUnknown_CPU;
  end;
end;

procedure TPEFileInfo.FillMSDOSHeader;
resourcestring
  SMagicNumber = 'Magic number';
  SBytesOnLastPage = 'Bytes on last page of file';
  SPagesInFile = 'Pages in file';
  SRelocations = 'Relocations';
  SSizeOfHeaderInParagraphs = 'Size of header in paragraphs';
  SMinimumExtraParagraphs = 'Minimum extra paragraphs';
  SMaximumExtraParagraphs = 'Maximum extra paragraphs';
  SInitialSS = 'Initial (relative) SS value';
  SInitialSP = 'Initial SP value';
  SChecksum = 'Checksum';
  SInitialIP = 'Initial IP value';
  SInitialCS = 'Initial (relative) CS value';
  SFileAddressRelocation = 'File address of relocation table';
  SOverlayNumber = 'Overlay number';
  // SReservedWords = 'Reserved words';
  SOemIdentifier = 'OEM identifier';
  SOemInformation = 'OEM information';
  SPeHeaderAddress = 'PE header address';
begin
  FMSDOSHeader.Clear;
  with FMSDOSHeader, FDosHeader do
  begin
    Add(SMagicNumber + #9 + IntToNum(e_magic));
    Add(SBytesOnLastPage + #9 + IntToNum(e_cblp));
    Add(SPagesInFile + #9 + IntToNum(e_cp));
    Add(SRelocations + #9 + IntToNum(e_crcl));
    Add(SSizeOfHeaderInParagraphs + #9 + IntToNum(e_cparhdr));
    Add(SMinimumExtraParagraphs + #9 + IntToNum(e_minalloc));
    Add(SMaximumExtraParagraphs + #9 + IntToNum(e_maxalloc));
    Add(SInitialSS + #9 + IntToNum(e_ss));
    Add(SInitialSP + #9 + IntToNum(e_sp));
    Add(SChecksum + #9 + IntToNum(e_csum));
    Add(SInitialIP + #9 + IntToNum(e_ip));
    Add(SInitialCS + #9 + IntToNum(e_cs));
    Add(SFileAddressRelocation + #9 + IntToNum(e_lfarclc));
    Add(SOverlayNumber + #9 + IntToNum(e_ovno));
    // Add(SReserved words #9 + IntToNum(e_res));
    Add(SOemIdentifier + #9 + IntToNum(e_oemid));
    Add(SOemInformation + #9 + IntToNum(e_oeminfo));
    Add(SPeHeaderAddress + #9 + IntToNum(e_lfanew));
  end;
end;

procedure TPEFileInfo.FillPEHeader;
resourcestring
  SMachine = 'Machine';
  SNumberSections = 'Number of sections';
  STimeDate = 'Time/Date stamp';
  SAddressSymbolTable = 'Address of symbol table';
  SNumberSymbols = 'Number of symbols';
  SSizeOptionalHeader = 'Size of optional header';
  SCharacteristics = 'Characteristics';
  SCharacteristic = '  Characteristic';
  SRelocsStripped = 'Does not contain base relocations';
  SExecutableImage  = 'File is valid and can be run';
  SLineNumsStripped = 'COFF line numbers have been removed';
  SLocalSymsStripped = 'COFF symbol table entries have been removed';
  SAgressiveWSTrim = 'Aggressively trim working set';
  SLargeAddressAware = 'Can handle > 2 GB addresses';
  S16BitMachine  = '16-bit machine (reserved)';
  SBytesReversedLow = 'Low bytes of machine word are reversed';
  S32BitMachine = 'Machine based on 32-bit word architecture';
  SDebugStripped = 'Debugging information removed from image';
  SRemovableRunFromSwap = 'If image is on removable media, copy and run from swap';
  SNetRunFromSwap = 'If image is on the network, copy and run from swap';
  SSystemFile = 'Image is a system file, not a user program';
  SDLLFile = 'Image is a DLL';
  SUPOnly = 'Image compatible with single processor machines only';
  SBytesReversedHi  = 'High bytes of machine word are reversed';

begin
  with FPEHeaderList, FPEHeader do
  begin
    Add(SMachine + #9 + CPUType);
    Add(SNumberSections + #9 + IntToNum(NumberOfSections));

    Add(STimeDate + #9 + IntToNum(TimeDateStamp) + ' ('+ DateTimeToStr(UnixToDateTime(TimeDateStamp)) +' UTC)');
    Add(SAddressSymbolTable + #9 + IntToNum(PointerToSymboltable));
    Add(SNumberSymbols + #9 + IntToNum(NumberofSymbols));
    Add(SSizeOptionalHeader + #9 + IntToNum(SizeOfOptionalHeader));
    Add(SCharacteristics + #9 + IntToNum(Characteristics));
    if ((Characteristics and $0001) > 0) then
      Add(SCharacteristic + #9 + SRelocsStripped);
    if ((Characteristics and $0002) > 0) then
      Add(SCharacteristic + #9 + SExecutableImage);
    if ((Characteristics and $0004) > 0) then
      Add(SCharacteristic + #9 + SLineNumsStripped);
    if ((Characteristics and $0008) > 0) then
      Add(SCharacteristic + #9 + SLocalSymsStripped);
    if ((Characteristics and $0010) > 0) then
      Add(SCharacteristic + #9 + SAgressiveWSTrim);
    if ((Characteristics and $0020) > 0) then
      Add(SCharacteristic + #9 + SLargeAddressAware);
    if ((Characteristics and $0040) > 0) then
      Add(SCharacteristic + #9 + S16BitMachine);
    if ((Characteristics and $0080) > 0) then
      Add(SCharacteristic + #9 + SBytesReversedLow);
    if ((Characteristics and $0100) > 0) then
      Add(SCharacteristic + #9 + S32BitMachine);
    if ((Characteristics and $0200) > 0) then
      Add(SCharacteristic + #9 + SDebugStripped);
    if ((Characteristics and $0400) > 0) then
      Add(SCharacteristic + #9 + SRemovableRunFromSwap);
    if ((Characteristics and $0800) > 0) then
      Add(SCharacteristic + #9 + SNetRunFromSwap);
    if ((Characteristics and $1000) > 0) then
      Add(SCharacteristic + #9 + SSystemFile);
    if ((Characteristics and $2000) > 0) then
      Add(SCharacteristic + #9 + SDLLFile);
    if ((Characteristics and $4000) > 0) then
      Add(SCharacteristic + #9 + SUPOnly);
    if ((Characteristics and $8000) > 0) then
      Add(SCharacteristic + #9 + SBytesReversedHi);
  end;
end;

procedure TPEFileInfo.FillPEOptionalHeader;
resourcestring
  SMagic = 'Magic';
  SMajorLinker = 'Major linker version';
  SMinorLinker = 'Minor linker version';
  SSizeCode = 'Size of Code';
  SInitializedData = 'Size of initialized data';
  SUninitializedData = 'Size of uninitialized data';
  SAddressEntryPoint = 'Address of entry point';
  SBaseCode = 'Base of code';
  SBaseData = 'Base of data';
  SImageBase = 'Image base';
  SSectionAlignment = 'Section alignment';
  SFileAlignment = 'File alignment';
  SMajorOS = 'Major OS version';
  SMinorOS = 'Minor OS version';
  SMajorImage = 'Major image version';
  SMinorImage = 'Minor image version';
  SMajorSubsystem = 'Major subsystem version';
  SMinorSubsystem = 'Minor subsystem version';
  SWin32Version = 'Win32 Version';
  SSizeImage = 'Size of image';
  SSizeHeaders = 'Size of headers';
  SCrc = 'CRC Checksum';

  SSub = 'Subsystem';
  SSubNative = 'Native, no subsystem required';
  SSubWinGui = 'Windows GUI subsystem required';
  SSubWinConsole = 'Windows console subsystem required';
  SSubOS2Console = 'OS/2 console subsystem required';
  SSubPosix = 'POSIX console subsystem required';
  SSubCEGui = 'Windows CE GUI';
  SSubEfiApp = 'EFI application';
  SSubEBSDriver = 'EFI boot service driver';
  SSubERDriver = 'EFI runtime driver';
  SSubERom = 'EFI ROM';
  SSubXBox = 'XBox';
  SSubUnknown = 'Unknown';

  SDll = 'DLL Characteristics';
  SDllPerProcessInit = 'Per-process library initialization';
  SDllPerProcessTermination = 'Per-process library termination';
  SDllPerThreadInit = 'Per-thread library initialization';
  SDllPerThreadTermination = 'Per-thread library termination';
  SDllDoNotBindImage = 'Do not bind image';
  SDllWDMDriver = 'WDM Driver';
  SDllTerminalServerAware = 'Terminal Server aware';

  SStackReserve = 'Size of stack reserve';
  SStackCommit = 'Size of stack commit';
  SHeapReserve = 'Size of heap reserve';
  SHeapCommit = 'Size of heap commit';

  SLoaderFlags = 'Loader Flags';
  SLoaderFlagsBP = 'Invoke a breakpoint before starting process';
  SLoaderFlagsDBG = 'Invoke a debugger after process has been loaded';
  SLoaderFlagsNone = 'No flags set';

  SNumDataEntries = 'Number of entries in data directory';

  SMagicPE32 = ' (Standard PE32 format)';
  SMagicPE32Plus = ' (Unsupported PE32+/64bit format)';
  SMagicROM = ' (ROM image)';

begin
  FPEOptionalHeaderList.Clear;
  with FPEOptionalHeaderList, FPEOptionalHeader do
  begin
    case Magic of
      $107: Add(SMagic + #9 + IntToNum(Magic) + SMagicROM);
      $10B:
        begin
          Add(SMagic + #9 + IntToNum(Magic) + SMagicPE32);
          FIsPE32 := True;
        end;
      $20B: Add(SMagic + #9 + IntToNum(Magic) + SMagicPE32Plus);
      else  Add(SMagic + #9 + IntToNum(Magic));
    end;
    Add(SMajorLinker + #9 + IntToNum(MajorLinkerVersion));
    Add(SMinorLinker + #9 + IntToNum(MinorLinkerVersion));
    Add(SSizeCode + #9 + IntToNum(SizeOfCode));
    Add(SInitializedData + #9 + IntToNum(SizeOfInitializedData));
    Add(SUninitializedData + #9 + IntToNum(SizeOfUninitializedData));
    Add(SAddressEntryPoint + #9 + IntToNum(AddressOfEntryPoint));
    Add(SBaseCode + #9 + IntToNum(BaseOfCode));
    Add(SBaseData + #9 + IntToNum(BaseOfData));
    if ISPE32 then
    begin
      Add(SImageBase + #9 + IntToNum(ImageBase));
      Add(SSectionAlignment + #9 + IntToNum(SectionAlignment));
      Add(SFileAlignment + #9 + IntToNum(FileAlignment));
      Add(SMajorOS + #9 + IntToNum(MajorOperatingSystemVersion));
      Add(SMinorOS + #9 + IntToNum(MinorOperatingSystemVersion));
      Add(SMajorImage + #9 + IntToNum(MajorImageVersion));
      Add(SMinorImage + #9 + IntToNum(MinorImageVersion));
      Add(SMajorSubsystem + #9 + IntToNum(MajorSubsystemVersion));
      Add(SMinorSubsystem + #9 + IntToNum(MinorSubsystemVersion));
      Add(SWin32Version + #9 + IntToNum(Win32Version));
      Add(SSizeImage + #9 + IntToNum(SizeOfImage));
      Add(SSizeHeaders + #9 + IntToNum(SizeOfHeaders));
      Add(SCrc + #9 + IntToNum(Checksum));
      case Subsystem of
        1: Add(SSub + #9 + SSubNative);
        2: Add(SSub + #9 + SSubWinGui);
        3: Add(SSub + #9 + SSubWinConsole);
        5: Add(SSub + #9 + SSubOS2Console);
        7: Add(SSub + #9 + SSubPosix);
        8: Add(SSub + #9 + SSubNative);
        9: Add(SSub + #9 + SSubCEGui);
        10: Add(SSub + #9 + SSubEfiApp);
        11: Add(SSub + #9 + SSubEBSDriver);
        12: Add(SSub + #9 + SSubERDriver);
        13: Add(SSub + #9 + SSubERom);
        14: Add(SSub + #9 + SSubXBox);
        else Add(SSub + #9 + SSubUnknown);
      end;

      if ((DLLCharacteristics and $0001) > 0) then
        Add(SDll + #9 + SDllPerProcessInit);
      if ((DLLCharacteristics and $0002) > 0) then
        Add(SDll + #9 + SDllPerProcessTermination);
      if ((DLLCharacteristics and $0004) > 0) then
        Add(SDll + #9 + SDllPerThreadInit);
      if ((DLLCharacteristics and $0008) > 0) then
        Add(SDll + #9 + SDllPerThreadTermination);
      if ((DLLCharacteristics and $0800) > 0) then
        Add(SDll + #9 + SDllDoNotBindImage);
      if ((DLLCharacteristics and $2000) > 0) then
        Add(SDll + #9 + SDllWDMDriver);
      if ((DLLCharacteristics and $8000) > 0) then
        Add(SDll + #9 + SDllTerminalServerAware);
      Add(SStackReserve + #9 + IntToNum(SizeOfStackReserve));
      Add(SStackCommit + #9 + IntToNum(SizeOfStackCommit));
      Add(SHeapReserve + #9 + IntToNum(SizeOfHeapReserve));
      Add(SHeapCommit + #9 + IntToNum(SizeOfHeapCommit));
      case LoaderFlags of
        1: Add(SLoaderFlags + #9 + SLoaderFlagsBP);
        2: Add(SLoaderFlags + #9 + SLoaderFlagsDBG);
      else
        Add(SLoaderFlags + #9 + SLoaderFlagsNone);
      end;
      Add(SNumDataEntries + #9 + IntToNum(NumberofRVAandSizes));
    end;
  end;
end;

function TPEFileInfo.GetExportList: TStrings;
begin
  Result := FExportList;
end;

function TPEFileInfo.GetImportList: TStrings;
begin
  Result := FImportList;
end;

function TPEFileInfo.GetMSDOSHeader: TStrings;
begin
  Result := FMSDOSHeader;
end;

function TPEFileInfo.GetPEHeaderList: TStrings;
begin
  Result := FPEHeaderList;
end;

function TPEFileInfo.GetPEOptionalHeaderList: TStrings;
begin
  Result := FPEOptionalHeaderList;
end;

end.

