unit GX_Zipper;

interface

{$I GX_CondDefine.inc}

{$WARN SYMBOL_PLATFORM OFF}

uses
  Classes,
  // The Abbrevia units below are in the Comps\Abbrevia directory in the GExperts
  // source code.  See http://sourceforge.net/projects/tpabbrevia/ for more info.
  AbArcTyp, AbZipTyp;

type
  TGXZipItem = TAbZipItem;

  TGXZipper = class(TAbZipArchive)
  private
    function GetIncludePath: Boolean;
    procedure SetIncludePath(const Value: Boolean);
    procedure ZipProc(Sender: TObject; Item: TAbArchiveItem; OutStream: TStream);
    procedure ZipFromStreamProc(Sender: TObject; Item: TAbArchiveItem; OutStream, InStream: TStream);
  public
    function CreateItem(const FileSpec: string): TAbArchiveItem; override;
    constructor Create(const FileName: string; Mode: Word ); override;
    procedure AddFiles(Files: TStrings);
    property IncludePath: Boolean read GetIncludePath write SetIncludePath;
  end;

implementation

uses
  SysUtils, AbZipPrc, GX_GenericUtils;

{ TGXZipper }

procedure TGXZipper.AddFiles(Files: TStrings);
var
  i: Integer;
begin
  for i := 0 to Files.Count - 1 do
  begin
    if FileNameHasWildcards(Files[i]) then
      AddFilesEx(Files[i], '', faHidden)
    else
      Add(CreateItem(Files[i]));
  end;
end;

constructor TGXZipper.Create(const FileName: string; Mode: Word);
begin
  inherited Create(FileName, Mode);
  InsertHelper           := ZipProc;
  InsertFromStreamHelper := ZipFromStreamProc;
end;

function TGXZipper.CreateItem(const FileSpec: string): TAbArchiveItem;
begin
  Result := inherited CreateItem(FileSpec);
  Result.DiskFileName := FileSpec;
end;

function TGXZipper.GetIncludePath: Boolean;
begin
  Result := not (soStripPath in StoreOptions);
end;

procedure TGXZipper.SetIncludePath(const Value: Boolean);
begin
  if Value then
    Exclude(FStoreOptions, soStripPath)
  else
    Include(FStoreOptions, soStripPath);
end;

procedure TGXZipper.ZipFromStreamProc(Sender: TObject; Item: TAbArchiveItem; OutStream, InStream: TStream);
begin
  AbZipFromStream(TAbZipArchive(Sender), TAbZipItem(Item), OutStream, InStream);
end;

procedure TGXZipper.ZipProc(Sender: TObject; Item: TAbArchiveItem; OutStream: TStream);
begin
  AbZip(TAbZipArchive(Sender), TAbZipItem(Item), OutStream);
end;

end.
