unit VMTUtils;

interface

uses
  SysUtils,
  Classes;

type
  EVMTError = class(Exception);

function GetVirtualMethodCount(AClass: TClass): Integer;
function GetVirtualMethod(AClass: TClass; const Index: Cardinal): Pointer;
procedure SetVirtualMethod(AClass: TClass; const Index: Cardinal; const Method: Pointer);

procedure SetClassParent(AClass: TClass; NewClassParent: TClass);
function GetClassParent(AClass: TClass): TClass;

implementation

uses
  Windows;

resourcestring
  SMemoryWriteError = 'Error writing VMT memory (%s).';
  SErrorRetrievingVmtCount = 'Error retrieving VMT entry count.';

type
  PLongint = ^Longint;
  PPointer = ^Pointer;

function GetVirtualMethodCount(AClass: TClass): Integer;
var
  BeginVMT: Longint;
  EndVMT: Longint;
  TablePointer: Pointer;
  I: Integer;
begin
  BeginVMT := Longint(AClass);
  EndVMT := 0;

  // Scan the offset entries in the class table for the various fields,
  // namely vmtIntfTable, vmtAutoTable, ..., vmtDynamicTable
  // The last entry is always the vmtClassName, so stop once we got there
  //
  // This assumes that all these tables come after each other, basically:
  //
  //    First VMT table entry         (pointed to by vmtSelfPtr = -76)
  //        ...
  //    Last VMT table entry
  //    First IntfTable table entry   (pointed to by vmtIntfTable = -72)
  //        ...
  //    Last IntfTable table entry
  //    First AutoTable table entry   (pointed to by vmtAutoTable = - 68)
  //        ...
  //    Last AutoTable table entry
  // ...
  //    ClassName ShortString         (pointed to by vmtClassName = -44)
  //
  // Note: Listed offsets are examples for Delphi 4.0.
  //

  // Set iterator to first item behind VMT table pointer
  I := vmtSelfPtr + SizeOf(Pointer);
  repeat
    // We need to skip vmtTypeInfo since as of Delphi 3+ it points
    // to a pointer. This retrieved pointer may point to a package
    // which may or may not be loaded at a higher address than the
    // module that contains AClass.
    // This issue was brought up by Hallvard Vassbotn.
    if I = vmtTypeInfo then
    begin
      Inc(I, SizeOf(Pointer));
      Continue;
    end;

    TablePointer := PPointer(Longint(AClass) + I)^;
    if TablePointer <> nil then
    begin
      // Check that the end of the VMT is beyond the beginning (Hallvard Vassbotn)
      if Longint(TablePointer) > BeginVMT then
        EndVMT := Longint(TablePointer);

      Break;
    end;
    Inc(I, SizeOf(Pointer));
  until I > vmtClassName;

  if EndVMT = 0 then
    raise EVMTError.Create(SErrorRetrievingVmtCount);

  // Hallvard Vassbotn suggests to check each VMT entry
  // for validity; essentially this involves testing
  // whether the pointers themselves reside in PAGE_EXECUTE*
  // memory [+] and whether they point to memory inside the
  // process that also has the PAGE_EXECUTE* attribute.
  //
  // [+] Note that this is compiler dependent.
  //
  // Left out for the moment.

  Result := (EndVMT - BeginVMT) div SizeOf(Pointer);
end;

function GetVirtualMethod(AClass: TClass; const Index: Cardinal): Pointer;
var
  PatchAddress: Pointer;
  DummyProtection: DWORD;
  OldProtection: DWORD;
begin
  PatchAddress := PPointer(Cardinal(AClass) + Index * SizeOf(Pointer))^;

  //set memory access rights so that we can read into this page
  if not VirtualProtect(PatchAddress, SizeOf(Pointer), PAGE_READONLY, @OldProtection) then
    raise EVMTError.CreateFmt(SMemoryWriteError, [SysErrorMessage(GetLastError)]);

  try
    Result := Pointer(PatchAddress);
  finally
    // restore memory access rights
    if not VirtualProtect(PatchAddress, SizeOf(Pointer), OldProtection, @DummyProtection) then
      raise EVMTError.CreateFmt(SMemoryWriteError, [SysErrorMessage(GetLastError)]);
  end;
end;

procedure SetVirtualMethod(AClass: TClass; const Index: Cardinal; const Method: Pointer);
var
  PatchAddress: Pointer;
  DummyProtection: DWORD;
  OldProtection: DWORD;
begin
  PatchAddress := PPointer(Cardinal(AClass) + Index * SizeOf(Pointer));

  //set memory access rights so that we can write into this page
  if not VirtualProtect(PatchAddress, SizeOf(Pointer), PAGE_READWRITE, @OldProtection) then
    raise EVMTError.CreateFmt(SMemoryWriteError, [SysErrorMessage(GetLastError)]);

  try
    // write method into VMT
    Pointer(PatchAddress^) := Method;

  finally
    // restore memory access rights
    if not VirtualProtect(PatchAddress, SizeOf(Pointer), OldProtection, @DummyProtection) then
      raise EVMTError.CreateFmt(SMemoryWriteError, [SysErrorMessage(GetLastError)]);
  end;

  // make sure that everything keeps working in a dual processor setting
  FlushInstructionCache(GetCurrentProcess, PatchAddress, SizeOf(Pointer));
end;

procedure SetClassParent(AClass: TClass; NewClassParent: TClass);
var
  DummyProtection: DWORD;
  OldProtection: DWORD;
  PatchAddress: Pointer;
begin
  PatchAddress := PPointer(Integer(AClass) + vmtParent)^;

  // set memory access rights so that we can write into this page
  if not VirtualProtect(PatchAddress, SizeOf(TClass), PAGE_READWRITE, @OldProtection) then
    raise EVMTError.CreateFmt(SMemoryWriteError, [SysErrorMessage(GetLastError)]);
  try
    // write class parent into table
    TClass(PatchAddress^) := NewClassParent;

  finally
    // restore memory access rights
    if not VirtualProtect(PatchAddress, SizeOf(TClass), OldProtection, @DummyProtection) then
      raise EVMTError.CreateFmt(SMemoryWriteError, [SysErrorMessage(GetLastError)]);
  end;

  // make sure that everything keeps working in a dual processor setting
  FlushInstructionCache(GetCurrentProcess, PatchAddress, SizeOf(TClass));
end;

function GetClassParent(AClass: TClass): TClass;
begin
  Result := TClass(PPointer(Integer(AClass) + vmtParent)^^);
end;

end.
