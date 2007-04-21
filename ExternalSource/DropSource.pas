unit DropSource;
{$T-}
// -----------------------------------------------------------------------------
// Project:         Drag and Drop Source Components
// Component Names: TDropTextSource, TDropFileSource,
// Module:          DropSource
// Description:     Implements Dragging & Dropping of text, files
//                  FROM your application to another.
// Version:	       3.6
// Date:            21-APR-1999
// Target:          Win32, Delphi3, Delphi4, C++Builder 3, C++Builder 4
// Authors:         Angus Johnson,   ajohnson@rpi.net.au
//                  Anders Melander, anders@melander.dk
//                                   http://www.melander.dk
//                  Graham Wideman,  graham@sdsu.edu
//                                   http://www.wideman-one.com
// Copyright        ©1997-99 Angus Johnson, Anders Melander & Graham Wideman
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
// Acknowledgements:
// 1. Thanks to Jim O'Brien for some tips on Shortcuts and Scrap files.
// 2. Thanks to Zbysek Hlinka for sugestions on Copying to Clipboard.
// 3. Thanks to Jan Debis for spotting a small bug in TDropFileSource.
// 4. Thanks to 'Scotto the Unwise' for spotting a Delphi4 compatibility bug.
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
//  Compiler compatibility...
// -----------------------------------------------------------------------------

{$I GX_CondDefine.inc}
// -----------------------------------------------------------------------------

interface

uses
  Controls, Windows, ActiveX, Classes;

const
  MAXFORMATS = 20;

type

  TInterfacedComponent = class(TComponent, IUnknown)
  private
    fRefCount: Integer;
  protected
    function QueryInterface(const IID: TGuid; out Obj): HRESULT; reintroduce; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;

  TDragType = (dtCopy, dtMove, dtLink);
  TDragTypes = set of TDragType;

  TDragResult = (drDropCopy, drDropMove,
    drDropLink, drCancel, drOutMemory, drUnknown);

  TDropEvent = procedure(Sender: TObject;
    DragType: TDragType; var ContinueDrop: Boolean) of object;
  TFeedbackEvent = procedure(Sender: TObject;
    Effect: Longint; var UseDefaultCursors: Boolean) of object;

  //C++Builder compatibility...
  {$IFDEF GX_BCB}
  {$HPPEMIT 'typedef System::DelphiInterface<IEnumFORMATETC> _di_IEnumFORMATETC;' }
  {$ENDIF}

  TDropSource = class(TInterfacedComponent, IDropSource, IDataObject)
  private
    fDragTypes      : TDragTypes;
    fDropEvent      : TDropEvent;
    fFBEvent        : TFeedBackEvent;
    fDataFormats    : array[0..MAXFORMATS-1] of TFormatEtc;
    fDataFormatsCount: Integer;

    //drag images...
    fImages: TImageList;
    fShowImage: Boolean;
    fImageIndex: Integer;
    fImageHotSpot: TPoint;
    procedure SetShowImage(Value: Boolean);
  protected
    FeedbackEffect  : Longint;

    // IDropSource implementation
    function QueryContinueDrag(fEscapePressed: bool;
      grfKeyState: Longint): HRESULT; stdcall;
    function GiveFeedback(dwEffect: Longint): HRESULT; stdcall;

    // IDataObject implementation
    function GetData(const FormatEtcIn: TFormatEtc;
      out Medium: TStgMedium):HRESULT; stdcall;
    function GetDataHere(const FormatEtc: TFormatEtc;
      out Medium: TStgMedium):HRESULT; stdcall;
    function QueryGetData(const FormatEtc: TFormatEtc): HRESULT; stdcall;
    function GetCanonicalFormatEtc(const FormatEtc: TFormatEtc;
      out FormatEtcout: TFormatEtc): HRESULT; stdcall;
    function SetData(const FormatEtc: TFormatEtc; var Medium: TStgMedium;
      fRelease: Bool): HRESULT; stdcall;
    function EnumFormatEtc(dwDirection: Longint;
      out EnumFormatEtc: IEnumFormatEtc): HRESULT; stdcall;
    function dAdvise(const FormatEtc: TFormatEtc; advf: Longint;
      const advsink: IAdviseSink; out dwConnection: Longint): HRESULT; stdcall;
    function dUnadvise(dwConnection: Longint): HRESULT; stdcall;
    function EnumdAdvise(out EnumAdvise: IEnumStatData): HRESULT; stdcall;

    //New functions...
    function DoGetData(const FormatEtcIn: TFormatEtc;
             out Medium: TStgMedium):HRESULT; virtual; abstract;
    procedure AddFormatEtc(cfFmt: TClipFormat;
                pt: PDVTargetDevice; dwAsp, lInd, tym: Longint); virtual;
    function InternalCutCopyToClipboard(Effect: Integer): Boolean;
    function CutOrCopyToClipboard: Boolean; virtual;

    procedure SetImages(const Value: TImageList);
    procedure SetImageIndex(const Value: Integer);
    procedure SetPoint(Index: Integer; Value: Integer);
    function GetPoint(Index: Integer): Integer;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(aowner: TComponent); override;
    function Execute: TDragResult;
    function CutToClipboard: Boolean; virtual;
    function CopyToClipboard: Boolean; virtual;
  Published
    property Dragtypes: TDragTypes read fDragTypes write fDragTypes;
    property OnFeedback: TFeedbackEvent read fFBEvent write fFBEvent;
    property OnDrop: TDropEvent read fDropEvent write fDropEvent;
    //Drag Images...
    property Images: TImageList read fImages write SetImages;
    property ImageIndex: Integer read fImageIndex write SetImageIndex;
    property ShowImage: Boolean read fShowImage write SetShowImage;
    property ImageHotSpotX: Integer index 1 read GetPoint write SetPoint;
    property ImageHotSpotY: Integer index 2 read GetPoint write SetPoint;
  end;

  TDropTextSource = class(TDropSource)
  private
    fText: string;
  protected
    function DoGetData(const FormatEtcIn: TFormatEtc;
      out Medium: TStgMedium):HRESULT; override;
    function CutOrCopyToClipboard: Boolean; override;
  public
    constructor Create(aOwner: TComponent); override;
  published
    property Text: string read fText write fText;
  end;

  TDropFileSource = class(TDropSource)
  private
    fFiles: TStrings;
    fMappedNames: TStrings;
    procedure SetFiles(files: TStrings);
    procedure SetMappedNames(names: TStrings);
  protected
    function DoGetData(const FormatEtcIn: TFormatEtc;
      out Medium: TStgMedium):HRESULT; override;
    function CutOrCopyToClipboard: Boolean; override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Files: TStrings read fFiles write SetFiles;
    //MappedNames is only needed if files need to be renamed during a drag op
    //eg dragging from 'Recycle Bin'.
    property MappedNames: TStrings read fMappedNames write SetMappedNames;
  end;

  procedure Register;

  var
    CF_FILEGROUPDESCRIPTOR, CF_FILECONTENTS, CF_FILENAMEMAP, CF_FILENAMEMAPW,
    CF_IDLIST, CF_PREFERREDDROPEFFECT, CF_URL: UINT; //see initialization.
    ShellMalloc: IMalloc;

implementation

uses
  Clipbrd, CommCtrl, ShlObj, SysUtils;

// -----------------------------------------------------------------------------
//			Miscellaneous functions.
// -----------------------------------------------------------------------------

function GetSizeOfPidl(pidl: pItemIDList): Integer;
var
  i: Integer;
begin
  Result := SizeOf(Word);
  repeat
    i := pSHItemID(pidl)^.cb;
    Inc(Result, i);
    Inc(Longint(pidl), i);
  until i = 0;
end;
// -----------------------------------------------------------------------------

function GetShellFolderOfPath(FolderPath: TFileName): IShellFolder;
var
  DeskTopFolder: IShellFolder;
  PathPidl: pItemIDList;
  OlePath: array[0..MAX_PATH] of WideChar;
  dummy, pdwAttributes: ULONG;
begin
  Result := nil;
  StringToWideChar(FolderPath, OlePath, MAX_PATH);
  pdwAttributes := SFGAO_FOLDER;
  try
    if not (SHGetDesktopFolder(DeskTopFolder) = NOERROR) then Exit;
    if (DesktopFolder.ParseDisplayName(0,
          nil, OlePath, dummy, PathPidl, pdwAttributes) = NOERROR) and
          (pdwAttributes and SFGAO_FOLDER <> 0) then
      DesktopFolder.BindToObject(PathPidl, nil, IID_IShellFolder, Pointer(Result));
    ShellMalloc.Free(PathPidl);
  except
  end;
end;
// -----------------------------------------------------------------------------

function GetFullPIDLFromPath(Path: TFileName): pItemIDList;
var
   DeskTopFolder: IShellFolder;
   OlePath: array[0..MAX_PATH] of WideChar;
   dummy1, dummy2: ULONG;
begin
  Result := nil;
  StringToWideChar(Path, OlePath, MAX_PATH);
  try
    if (SHGetDesktopFolder(DeskTopFolder) = NOERROR) then
      DesktopFolder.ParseDisplayName(0, nil, OlePath, dummy1, Result, dummy2);
  except
  end;
end;
// -----------------------------------------------------------------------------

function GetSubPidl(Folder: IShellFolder; Sub: TFilename): pItemIDList;
var
  dummy1, dummy2: ULONG;
  OleFile: array[0..MAX_PATH] of WideChar;
begin
  Result := nil;
  try
    StringToWideChar(Sub, OleFile, MAX_PATH);
    Folder.ParseDisplayName(0, nil, OleFile, dummy1, Result, dummy2);
  except
  end;
end;
// -----------------------------------------------------------------------------

//See "Clipboard Formats for Shell Data Transfers" in Ole.hlp...
//(Needed to drag links (shortcuts).)

type
  POffsets = ^TOffsets;
  TOffsets = array[0..$FFFF] of UINT;

function ConvertFilesToShellIDList(path: string; files: TStrings): HGlobal;
var
  shf: IShellFolder;
  PathPidl, pidl: pItemIDList;
  Ida: PIDA;
  pOffset: POffsets;
  ptrByte: ^Byte;
  i, PathPidlSize, IdaSize, PreviousPidlSize: Integer;
begin
  Result := 0;
  shf := GetShellFolderOfPath(path);
  if shf = nil then Exit;
  //Calculate size of IDA structure ...
  // cidl: UINT ; Directory pidl offset: UINT ; all file pidl offsets
  IdaSize := (files.Count + 2) * SizeOf(UINT);

  PathPidl := GetFullPIDLFromPath(path);
  if PathPidl = nil then Exit;
  PathPidlSize := GetSizeOfPidl(PathPidl);

  //Add to IdaSize space for ALL pidls...
  IdaSize := IdaSize + PathPidlSize;
  for i := 0 to files.Count-1 do
  begin
    pidl := GetSubPidl(shf, files[i]);
    IdaSize := IdaSize + GetSizeOfPidl(Pidl);
    ShellMalloc.Free(pidl);
  end;

  //Allocate memory...
  Result := GlobalAlloc(GMEM_SHARE or GMEM_ZEROINIT, IdaSize);
  if (Result = 0) then
  begin
    ShellMalloc.Free(PathPidl);
    Exit;
  end;

  Ida := GlobalLock(Result);
  try
    FillChar(Ida^, IdaSize, 0);

    //Fill in offset and pidl data...
    Ida^.cidl := files.Count; //cidl = file Count
    pOffset := @(Ida^.aoffset);
    pOffset^[0] := (files.Count+2) * SizeOf(UINT); //offset of Path pidl

    ptrByte := Pointer(Ida);
    Inc(ptrByte, pOffset^[0]); //ptrByte now points to Path pidl
    move(PathPidl^, ptrByte^, PathPidlSize); //copy path pidl
    ShellMalloc.Free(PathPidl);

    PreviousPidlSize := PathPidlSize;
    for i := 1 to files.Count do
    begin
      pidl := GetSubPidl(shf, files[i-1]);
      pOffset^[i] := pOffset^[i-1] + UINT(PreviousPidlSize); //offset of pidl
      PreviousPidlSize := GetSizeOfPidl(Pidl);

      ptrByte := Pointer(Ida);
      Inc(ptrByte, pOffset^[i]); //ptrByte now points to current file pidl
      move(Pidl^, ptrByte^, PreviousPidlSize); //copy file pidl
                            //PreviousPidlSize = current pidl size here
      ShellMalloc.Free(pidl);
    end;
  finally
    GlobalUnLock(Result);
  end;
end;
// -----------------------------------------------------------------------------

procedure Register;
begin
  RegisterComponents('DragDrop', [TDropFileSource, TDropTextSource]);
end;

// -----------------------------------------------------------------------------
//			TInterfacedComponent
// -----------------------------------------------------------------------------

function TInterfacedComponent.QueryInterface(const IID: TGuid; out Obj): HRESULT;
begin
  if GetInterface(IID, Obj) then Result := 0 else Result := E_NOINTERFACE;
end;
// -----------------------------------------------------------------------------

function TInterfacedComponent._AddRef: Integer;
begin
  Result := InterlockedIncrement(fRefCount);
end;
// -----------------------------------------------------------------------------

function TInterfacedComponent._Release: Integer;
begin
  Result := InterlockedDecrement(fRefCount);
  if (Result = 0) then
    Free;
end;

// -----------------------------------------------------------------------------
//			TEnumFormatEtc
// -----------------------------------------------------------------------------

type

pFormatList = ^TFormatList;
TFormatList = array[0..255] of TFormatEtc;

TEnumFormatEtc = class(TInterfacedObject, IEnumFormatEtc)
private
  fFormatList: pFormatList;
  fFormatCount: Integer;
  fIndex: Integer;
public
  constructor Create(FormatList: pFormatList; FormatCount, Index: Integer);
  { IEnumFormatEtc }
  function Next(Celt: Longint; out Elt; pCeltFetched: pLongint): HRESULT; stdcall;
  function Skip(Celt: Longint): HRESULT; stdcall;
  function Reset: HRESULT; stdcall;
  function Clone(out Enum: IEnumFormatEtc): HRESULT; stdcall;
end;
// -----------------------------------------------------------------------------

constructor TEnumFormatEtc.Create(FormatList: pFormatList;
            FormatCount, Index: Integer);
begin
  inherited Create;
  fFormatList := FormatList;
  fFormatCount := FormatCount;
  fIndex := Index;
end;
// -----------------------------------------------------------------------------

function TEnumFormatEtc.Next(Celt: Longint;
  out Elt; pCeltFetched: pLongint): HRESULT;
var
  i: Integer;
begin
  i := 0;
  WHILE (i < Celt) and (fIndex < fFormatCount) do
  begin
    TFormatList(Elt)[i] := fFormatList[fIndex];
    Inc(fIndex);
    Inc(i);
  end;
  if pCeltFetched <> NIL then pCeltFetched^ := i;
  if i = Celt then Result := S_OK else Result := S_FALSE;
end;
// -----------------------------------------------------------------------------

function TEnumFormatEtc.Skip(Celt: Longint): HRESULT;
begin
  if Celt <= fFormatCount - fIndex then
  begin
    fIndex := fIndex + Celt;
    Result := S_OK;
  end else
  begin
    fIndex := fFormatCount;
    Result := S_FALSE;
  end;
end;
// -----------------------------------------------------------------------------

function TEnumFormatEtc.ReSet: HRESULT;
begin
  fIndex := 0;
  Result := S_OK;
end;
// -----------------------------------------------------------------------------

function TEnumFormatEtc.Clone(out Enum: IEnumFormatEtc): HRESULT;
begin
  enum := TEnumFormatEtc.Create(fFormatList, fFormatCount, fIndex);
  Result := S_OK;
end;

// -----------------------------------------------------------------------------
//			TDropSource
// -----------------------------------------------------------------------------

constructor TDropSource.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  DragTypes := [dtCopy]; //default to Copy.
  //to avoid premature release ...
  _AddRef;
  fDataFormatsCount := 0;
  fImageHotSpot := Point(16, 16);
  fImages := nil;
end;
// -----------------------------------------------------------------------------

function TDropSource.GiveFeedback(dwEffect: Longint): HRESULT; stdcall;
var
  UseDefaultCursors: Boolean;
begin
  UseDefaultCursors := True;
  FeedbackEffect := dwEffect;
  if Assigned(OnFeedback) then
    OnFeedback(Self, dwEffect, UseDefaultCursors);
  if UseDefaultCursors then
    Result := DRAGDROP_S_USEDEFAULTCURSORS else
    Result := S_OK;
end;
// -----------------------------------------------------------------------------

function TDropSource.GetCanonicalFormatEtc(const FormatEtc: TFormatEtc;
         out FormatEtcout: TFormatEtc): HRESULT;
begin
  Result := DATA_S_SAMEFORMATETC;
end;
// -----------------------------------------------------------------------------

function TDropSource.SetData(const FormatEtc: TFormatEtc; var Medium: TStgMedium;
         fRelease: Bool): HRESULT;
begin
  Result := E_NOTIMPL;
end;
// -----------------------------------------------------------------------------

function TDropSource.DAdvise(const FormatEtc: TFormatEtc; advf: Longint;
         const advSink: IAdviseSink; out dwConnection: Longint): HRESULT;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;
// -----------------------------------------------------------------------------

function TDropSource.DUnadvise(dwConnection: Longint): HRESULT;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;
// -----------------------------------------------------------------------------

function TDropSource.EnumDAdvise(out EnumAdvise: IEnumStatData): HRESULT;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;
// -----------------------------------------------------------------------------

function TDropSource.GetData(const FormatEtcIn: TFormatEtc;
  out Medium: TStgMedium):HRESULT; stdcall;
begin
  Result := DoGetData(FormatEtcIn, Medium);
end;
// -----------------------------------------------------------------------------

function TDropSource.GetDataHere(const FormatEtc: TFormatEtc;
  out Medium: TStgMedium):HRESULT; stdcall;
begin
  Result := E_NOTIMPL;
end;
// -----------------------------------------------------------------------------

function TDropSource.QueryGetData(const FormatEtc: TFormatEtc): HRESULT; stdcall;
var
  i: Integer;
begin
  Result:= S_OK;
  for i := 0 to fDataFormatsCount-1 do
    with fDataFormats[i] do
    begin
      if (FormatEtc.cfFormat = cfFormat) and
         (FormatEtc.dwAspect = dwAspect) and
         (FormatEtc.tymed and tymed <> 0) then Exit; //Result:= S_OK;
    end;
  Result:= E_FAIL;
end;
// -----------------------------------------------------------------------------

function TDropSource.EnumFormatEtc(dwDirection: Longint;
  out EnumFormatEtc:IEnumFormatEtc): HRESULT; stdcall;
begin
  if (dwDirection = DATADIR_GET) then
  begin
    EnumFormatEtc :=
      TEnumFormatEtc.Create(@fDataFormats, fDataFormatsCount, 0);
    Result := S_OK;
  end else if (dwDirection = DATADIR_SET) then
    Result := E_NOTIMPL
  else Result := E_INVALIDARG;
end;
// -----------------------------------------------------------------------------

function TDropSource.QueryContinueDrag(fEscapePressed: bool;
  grfKeyState: Longint): HRESULT; stdcall;
var
  ContinueDrop: Boolean;
  dragtype: TDragType;
begin
  if fEscapePressed then
    Result := DRAGDROP_S_CANCEL
  // will now allow drag and drop with either mouse button.
  else if (grfKeyState and (MK_LBUTTON or MK_RBUTTON) = 0) then
  begin
    ContinueDrop := True;
    dragtype := dtCopy;
    if (FeedbackEffect and DROPEFFECT_COPY <> 0) then DragType := dtCopy
    else if (FeedbackEffect and DROPEFFECT_MOVE <> 0) then dragtype := dtMove
    else if (FeedbackEffect and DROPEFFECT_LINK <> 0) then dragtype := dtLink
    else ContinueDrop := False;

    if not (DragType in dragtypes) then ContinueDrop := False;
    //if a valid drop then do OnDrop event if assigned...
    if ContinueDrop and Assigned(OnDrop) then
      OnDrop(Self, dragtype, ContinueDrop);

    if ContinueDrop then Result := DRAGDROP_S_DROP
    else Result := DRAGDROP_S_CANCEL;
  end else
    Result := NOERROR;
end;
// -----------------------------------------------------------------------------

function TDropSource.Execute: TDragResult;
var
  res: HRESULT;
  okeffect, effect: Longint;
  IsDraggingImage: Boolean;
begin
  Result := drUnknown;
  okeffect := DROPEFFECT_NONE;
  if (dtCopy in fDragTypes) then okeffect := okeffect or DROPEFFECT_COPY;
  if (dtMove in fDragTypes) then okeffect := okeffect or DROPEFFECT_MOVE;
  if (dtLink in fDragTypes) then okeffect := okeffect or DROPEFFECT_LINK;

  if fShowImage and (fImages <> nil) and
        ImageList_BeginDrag(fImages.Handle,
             fImageIndex, fImageHotSpot.X, fImageHotSpot.Y) then
  begin
    IsDraggingImage := True;
    //Windows.ShowCursor(False);
  end
  else IsDraggingImage := False;

  res := DoDragDrop(Self as IDataObject, Self as IDropSource, okeffect, effect);

  if IsDraggingImage then
  begin
    ImageList_EndDrag;
    //Windows.ShowCursor(True);
  end;

  case res of
    DRAGDROP_S_DROP:
      begin
        if (okeffect and effect <> 0) then
        begin
          if (effect and DROPEFFECT_COPY <> 0) then Result := drDropCopy
          else if (effect and DROPEFFECT_MOVE <> 0) then Result := drDropMove
          else Result := drDropLink;
        end else
          Result := drCancel;
      end;
    DRAGDROP_S_CANCEL: Result := drCancel;
    E_OUTOFMEMORY:     Result := drOutMemory;
  end;
end;
// -----------------------------------------------------------------------------

procedure TDropSource.AddFormatEtc(cfFmt: TClipFormat;
  pt: PDVTargetDevice; dwAsp, lInd, tym: Longint);
begin
  if fDataFormatsCount = MAXFORMATS then Exit;

  fDataFormats[fDataFormatsCount].cfFormat := cfFmt;
  fDataFormats[fDataFormatsCount].ptd := pt;
  fDataFormats[fDataFormatsCount].dwAspect := dwAsp;
  fDataFormats[fDataFormatsCount].lIndex := lInd;
  fDataFormats[fDataFormatsCount].tymed := tym;
  Inc(fDataFormatsCount);
end;
// -----------------------------------------------------------------------------

function TDropSource.CutToClipboard: Boolean;
begin
  //sets CF_PREFERREDDROPEFFECT...
  Result := InternalCutCopyToClipboard(DROPEFFECT_MOVE);
end;
// -----------------------------------------------------------------------------

function TDropSource.CopyToClipboard: Boolean;
begin
  //sets CF_PREFERREDDROPEFFECT...
  Result := InternalCutCopyToClipboard(DROPEFFECT_COPY);
end;
// -----------------------------------------------------------------------------

function TDropSource.CutOrCopyToClipboard: Boolean;
begin
  Result := False;
end;
// -----------------------------------------------------------------------------

//1. Renders the CF_PREFERREDDROPEFFECT clipboard dataobject.
//2. Encloses all calls to Clipboard.SetAsHandle() between Clipboard.Open and
//Clipboard.Close so multiple clipboard formats can be rendered in CutOrCopyToClipboard().
function TDropSource.InternalCutCopyToClipboard(Effect: Longint): Boolean;
var
  FormatEtc: TFormatEtc;
  Medium: TStgMedium;
begin
  Clipboard.Open;
  try
    Result := CutOrCopyToClipboard;
    if not Result then Exit;

    FeedbackEffect := Effect;
    FormatEtc.cfFormat := CF_PREFERREDDROPEFFECT;
    FormatEtc.dwAspect := DVASPECT_CONTENT;
    FormatEtc.tymed := TYMED_HGLOBAL;

    if GetData(FormatEtc, Medium) = S_OK then
    begin
      Clipboard.SetAsHandle(CF_PREFERREDDROPEFFECT, Medium.hGlobal);
    end else
      Result := False;
  finally
    Clipboard.Close;
  end;
end;
// -----------------------------------------------------------------------------

procedure TDropSource.SetImages(const Value: TImageList);
begin
  if fImages = Value then Exit;
  fImages := Value;
  if (csLoading in ComponentState) then Exit;

  fImageIndex := 0;
  if (fImages <> nil) then
    fShowImage := True else
    fShowImage := False;
end;
// -----------------------------------------------------------------------------

procedure TDropSource.SetImageIndex(const Value: Integer);
begin
  if (csLoading in ComponentState) then fImageIndex := Value
  else
  if (Value < 0) or (FImages.Count = 0) or (FImages = nil) then
  begin
    fImageIndex := 0;
    fShowImage := False;
  end
  else if (Value < fImages.Count) then
    fImageIndex := Value;
end;
// -----------------------------------------------------------------------------

procedure TDropSource.SetPoint(Index: Integer; Value: Integer);
begin
  if (Index = 1) then
    fImageHotSpot.x := Value
  else
    fImageHotSpot.y := Value;
end;
// -----------------------------------------------------------------------------

function TDropSource.GetPoint(Index: Integer): Integer;
begin
  if (Index = 1) then
    Result := fImageHotSpot.x
  else
    Result := fImageHotSpot.y;
end;
// -----------------------------------------------------------------------------

procedure TDropSource.SetShowImage(Value: Boolean);
begin
  fShowImage := Value;
  if (csLoading in ComponentState) then Exit;
  if (fImages = nil) then fShowImage := False;
end;
// -----------------------------------------------------------------------------

procedure TDropSource.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = fImages) then
  begin
    fImages := nil;
    fImageIndex := 0;
    fShowImage := False;
  end;
end;

// -----------------------------------------------------------------------------
//			TDropTextSource
// -----------------------------------------------------------------------------

constructor TDropTextSource.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  fText := '';

  AddFormatEtc(CF_TEXT, NIL, DVASPECT_CONTENT, -1, TYMED_HGLOBAL);
  //These next two formats have been commented out (for the time being)
  //as they interfer with text drag and drop in Word97.
  //AddFormatEtc(CF_FILEGROUPDESCRIPTOR, NIL, DVASPECT_CONTENT, -1, TYMED_HGLOBAL);
  //AddFormatEtc(CF_FILECONTENTS, NIL, DVASPECT_CONTENT, 0, TYMED_HGLOBAL);
end;
// -----------------------------------------------------------------------------

function TDropTextSource.CutOrCopyToClipboard: Boolean;
var
  FormatEtcIn: TFormatEtc;
  Medium: TStgMedium;
begin
  FormatEtcIn.cfFormat := CF_TEXT;
  FormatEtcIn.dwAspect := DVASPECT_CONTENT;
  FormatEtcIn.tymed := TYMED_HGLOBAL;
  if fText = '' then Result := False
  else if GetData(formatetcIn, Medium) = S_OK then
  begin
    Clipboard.SetAsHandle(CF_TEXT, Medium.hGlobal);
    Result := True;
  end else Result := False;
end;
// -----------------------------------------------------------------------------

function TDropTextSource.DoGetData(const FormatEtcIn: TFormatEtc;
  out Medium: TStgMedium):HRESULT;
var
  pFGD: PFileGroupDescriptor;
  pText: PChar;
begin

  Medium.tymed := 0;
  Medium.UnkForRelease := NIL;
  Medium.hGlobal := 0;

  //--------------------------------------------------------------------------
  if ((FormatEtcIn.cfFormat = CF_TEXT) or
    (FormatEtcIn.cfFormat = CF_FILECONTENTS)) and
    (FormatEtcIn.dwAspect = DVASPECT_CONTENT) and
    (FormatEtcIn.tymed and TYMED_HGLOBAL <> 0) then
  begin
    Medium.hGlobal := GlobalAlloc(GMEM_SHARE or GHND, Length(fText)+1);
    if (Medium.hGlobal = 0) then
      Result := E_outOFMEMORY
    else
    begin
      medium.tymed := TYMED_HGLOBAL;
      pText := PChar(GlobalLock(Medium.hGlobal));
      try
        StrCopy(pText, PChar(fText));
      finally
        GlobalUnlock(Medium.hGlobal);
      end;
      Result := S_OK;
    end;
  end
  //--------------------------------------------------------------------------
  else if (FormatEtcIn.cfFormat = CF_FILEGROUPDESCRIPTOR) and
    (FormatEtcIn.dwAspect = DVASPECT_CONTENT) and
    (FormatEtcIn.tymed and TYMED_HGLOBAL <> 0) then
  begin
    Medium.hGlobal := GlobalAlloc(GMEM_SHARE or GHND, SizeOf(TFileGroupDescriptor));
    if (Medium.hGlobal = 0) then
    begin
      Result := E_outOFMEMORY;
      Exit;
    end;
    medium.tymed := TYMED_HGLOBAL;
    pFGD := Pointer(GlobalLock(Medium.hGlobal));
    try
      with pFGD^ do
      begin
        cItems := 1;
        fgd[0].dwFlags := FD_LINKUI;
        fgd[0].cFileName := 'Text Scrap File.txt';
      end;
    finally
      GlobalUnlock(Medium.hGlobal);
    end;
    Result := S_OK;
  end else
    Result := DV_E_FORMATETC;
end;

// -----------------------------------------------------------------------------
//			TDropFileSource
// -----------------------------------------------------------------------------

constructor TDropFileSource.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  fFiles := TStringList.Create;
  fMappedNames := TStringList.Create;

  AddFormatEtc(CF_HDROP, NIL, DVASPECT_CONTENT, -1, TYMED_HGLOBAL);
  AddFormatEtc(CF_IDLIST, NIL, DVASPECT_CONTENT, -1, TYMED_HGLOBAL);
  AddFormatEtc(CF_PREFERREDDROPEFFECT, NIL, DVASPECT_CONTENT, -1, TYMED_HGLOBAL);
  AddFormatEtc(CF_FILENAMEMAP, NIL, DVASPECT_CONTENT, -1, TYMED_HGLOBAL);
  AddFormatEtc(CF_FILENAMEMAPW, NIL, DVASPECT_CONTENT, -1, TYMED_HGLOBAL);
end;
// -----------------------------------------------------------------------------

destructor TDropFileSource.Destroy;
begin
  fFiles.Free;
  fMappedNames.Free;
  inherited Destroy;
end;
// -----------------------------------------------------------------------------

procedure TDropFileSource.SetFiles(files: TStrings);
begin
  fFiles.assign(files);
end;
// -----------------------------------------------------------------------------

procedure TDropFileSource.SetMappedNames(names: TStrings);
begin
  fMappedNames.assign(names);
end;
// -----------------------------------------------------------------------------

function TDropFileSource.CutOrCopyToClipboard: Boolean;
var
  FormatEtcIn: TFormatEtc;
  Medium: TStgMedium;
begin
  FormatEtcIn.cfFormat := CF_HDROP;
  FormatEtcIn.dwAspect := DVASPECT_CONTENT;
  FormatEtcIn.tymed := TYMED_HGLOBAL;
  if (Files.Count = 0) then Result := False
  else if GetData(formatetcIn, Medium) = S_OK then
  begin
    Clipboard.SetAsHandle(CF_HDROP, Medium.hGlobal);
    Result := True;
  end else Result := False;
end;
// -----------------------------------------------------------------------------

function TDropFileSource.DoGetData(const FormatEtcIn: TFormatEtc;
         out Medium: TStgMedium):HRESULT;
var
  i: Integer;
  dropfiles: pDropFiles;
  pFile: PChar;
  pFileW: PWideChar;
  DropEffect: ^DWORD;
  strlength: Integer;
  tmpFilenames: TStringList;
begin
  Medium.tymed := 0;
  Medium.UnkForRelease := NIL;
  Medium.hGlobal := 0;

  if fFiles.Count = 0 then Result := E_UNEXPECTED
  //--------------------------------------------------------------------------
  else if (FormatEtcIn.cfFormat = CF_HDROP) and
    (FormatEtcIn.dwAspect = DVASPECT_CONTENT) and
    (FormatEtcIn.tymed and TYMED_HGLOBAL <> 0) then
  begin
    strlength := 0;
    for i := 0 to fFiles.Count-1 do
      Inc(strlength, Length(fFiles[i])+1);
    Medium.hGlobal :=
      GlobalAlloc(GMEM_SHARE or GMEM_ZEROINIT, SizeOf(TDropFiles)+strlength+1);
    if (Medium.hGlobal = 0) then
      Result:=E_OUTOFMEMORY
    else
    begin
      Medium.tymed := TYMED_HGLOBAL;
      dropfiles := GlobalLock(Medium.hGlobal);
      try
        dropfiles^.pfiles := SizeOf(TDropFiles);
        dropfiles^.fwide := False;
        Longint(pFile) := Longint(dropfiles)+SizeOf(TDropFiles);
        for i := 0 to fFiles.Count-1 do
        begin
          StrPCopy(pFile, fFiles[i]);
          Inc(pFile, Length(fFiles[i])+1);
        end;
        pFile^ := #0;
      finally
        GlobalUnlock(Medium.hGlobal);
      end;
      Result := S_OK;
    end;
  end
  //--------------------------------------------------------------------------
  else if (FormatEtcIn.cfFormat = CF_FILENAMEMAP) and
    (FormatEtcIn.dwAspect = DVASPECT_CONTENT) and
    (FormatEtcIn.tymed and TYMED_HGLOBAL <> 0) and
    //make sure there is a Mapped Name for each filename...
    (fMappedNames.Count = fFiles.Count) then
  begin
    strlength := 0;
    for i := 0 to fMappedNames.Count-1 do
      Inc(strlength, Length(fMappedNames[i])+1);

    Medium.hGlobal :=
      GlobalAlloc(GMEM_SHARE or GMEM_ZEROINIT, strlength+1);
    if (Medium.hGlobal = 0) then
      Result:=E_OUTOFMEMORY
    else
    begin
      Medium.tymed := TYMED_HGLOBAL;
      pFile := GlobalLock(Medium.hGlobal);
      try
        for i := 0 to fMappedNames.Count-1 do
        begin
          StrPCopy(pFile, fMappedNames[i]);
          Inc(pFile, Length(fMappedNames[i])+1);
        end;
        pFile^ := #0;
      finally
        GlobalUnlock(Medium.hGlobal);
      end;
      Result := S_OK;
    end;
  end
  //--------------------------------------------------------------------------
  else if (FormatEtcIn.cfFormat = CF_FILENAMEMAPW) and
    (FormatEtcIn.dwAspect = DVASPECT_CONTENT) and
    (FormatEtcIn.tymed and TYMED_HGLOBAL <> 0) and
    //make sure there is a Mapped Name for each filename...
    (fMappedNames.Count = fFiles.Count) then
  begin
    strlength := 2;
    for i := 0 to fMappedNames.Count-1 do
      Inc(strlength, (Length(fMappedNames[i])+1)*2);

    Medium.hGlobal := GlobalAlloc(GMEM_SHARE or GMEM_ZEROINIT, strlength);
    if (Medium.hGlobal = 0) then
      Result:=E_OUTOFMEMORY
    else
    begin
      Medium.tymed := TYMED_HGLOBAL;
      pFileW := GlobalLock(Medium.hGlobal);
      try
        for i := 0 to fMappedNames.Count-1 do
        begin
          StringToWideChar(fMappedNames[i], pFileW,
            (Length(fMappedNames[i])+1)*2);
          Inc(pFileW, Length(fMappedNames[i])+1);
        end;
        pFileW^ := #0;
      finally
        GlobalUnlock(Medium.hGlobal);
      end;
      Result := S_OK;
    end;
  end
  //--------------------------------------------------------------------------
  else if (FormatEtcIn.cfFormat = CF_IDLIST) and
    (FormatEtcIn.dwAspect = DVASPECT_CONTENT) and
    (FormatEtcIn.tymed and TYMED_HGLOBAL <> 0) then
  begin
    tmpFilenames := TStringList.Create;
    try
      Medium.tymed := TYMED_HGLOBAL;
      for i := 0 to fFiles.Count-1 do
        tmpFilenames.Add(ExtractFileName(fFiles[i]));
      Medium.hGlobal :=
          ConvertFilesToShellIDList(ExtractFilePath(fFiles[0]), tmpFilenames);
      if Medium.hGlobal = 0 then
        Result:=E_outOFMEMORY else
        Result := S_OK;
    finally
      tmpFilenames.Free;
    end;
  end
  //--------------------------------------------------------------------------
  //This next format does not work for Win95 but should for Win98, WinNT ...
  //It stops the shell from prompting (with a popup menu) for the choice of
  //Copy/Move/Shortcut when performing a file 'Shortcut' onto Desktop or Explorer.
  else if (FormatEtcIn.cfFormat = CF_PREFERREDDROPEFFECT) and
    (FormatEtcIn.dwAspect = DVASPECT_CONTENT) and
    (FormatEtcIn.tymed and TYMED_HGLOBAL <> 0) then
  begin
    Medium.tymed := TYMED_HGLOBAL;
    Medium.hGlobal := GlobalAlloc(GMEM_SHARE or GMEM_ZEROINIT, SizeOf(DWORD));
    if Medium.hGlobal = 0 then
      Result:=E_outOFMEMORY
    else
    begin
      DropEffect := GlobalLock(Medium.hGlobal);
      try
        DropEffect^ := DWORD(FeedbackEffect);
      finally
        GlobalUnLock(Medium.hGlobal);
      end;
      Result := S_OK;
    end;
  end
  else
    Result := DV_E_FORMATETC;
end;
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

// TODO: IFNDEF LINUX -> IFDEF MSWINDOWS
// TODO: Disable this unit in its entirety for Linux?
{$IFNDEF LINUX}
initialization
  OleInitialize(NIL);

  CF_FILECONTENTS := RegisterClipboardFormat(CFSTR_FILECONTENTS);
  CF_FILEGROUPDESCRIPTOR := RegisterClipboardFormat(CFSTR_FILEDESCRIPTOR);
  CF_IDLIST := RegisterClipboardFormat(CFSTR_SHELLIDLIST);
  CF_PREFERREDDROPEFFECT := RegisterClipboardFormat('Preferred DropEffect');
  CF_URL := RegisterClipboardFormat('UniformResourceLocator');
  CF_FILENAMEMAP := RegisterClipboardFormat(CFSTR_FILENAMEMAPA);
  CF_FILENAMEMAPW := RegisterClipboardFormat(CFSTR_FILENAMEMAPW);

  ShGetMalloc(ShellMalloc);
finalization
  OleUninitialize;
{$ENDIF LINUX}

end.
