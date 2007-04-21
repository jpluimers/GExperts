unit GX_FavUtil;

{$I GX_CondDefine.inc}

interface

uses
  Classes;

type
  TFolderType = (ftNormal, ftSource, ftBitmap, ftGlyph, ftDocs);
  TExecType = (etLoadInIDE, etShell, etCustom, etProject);

  TGXFile = class; // forward
  TGXFolder = class; // forward

  TGXFavItem = class
  private
    FOwner: TGXFolder;
    function GetContainingList: TList; virtual; abstract;
    procedure Detach;
    procedure AttachTo(AOwner: TGXFolder);
    procedure SetOwner(const Value: TGXFolder);
  public
    constructor Create(AOwner: TGXFolder); virtual;
    destructor Destroy; override;
    property Owner: TGXFolder read FOwner write SetOwner;
  end;

  TGXFolder = class(TGXFavItem)
  private
    FFolderName: string;
    FFolderType: TFolderType;
    FFileList: TList;
    FFolderList: TList;
    function GetFileCount: Integer;
    function GetFile(Index: Integer): TGXFile;
    function GetFolder(Index: Integer): TGXFolder;
    function GetFolderCount: Integer;
    procedure ClearList(AList: TList);
    function GetContainingList: TList; override;
  public
    constructor Create(AOwner: TGXFolder); override;
    destructor Destroy; override;

    property FolderName: string read FFolderName write FFolderName;
    property FolderType: TFolderType read FFolderType write FFolderType;

    property FolderCount: Integer read GetFolderCount;
    property Folders[Index: Integer]: TGXFolder read GetFolder;
    property FileCount: Integer read GetFileCount;
    property Files[Index: Integer]: TGXFile read GetFile;
  end;

  TGXFile = class(TGXFavItem)
  private
    FDName: string;
    FFileName: string;
    FDescription: string;
    FExecType: TExecType;
    FExecProg: string;
  public
    constructor Create(AOwner: TGXFolder); override;
    function GetContainingList: TList; override;

    property Description: string read FDescription write FDescription;
    property FileName: string read FFileName write FFileName;
    property DName: string read FDName write FDName;
    property ExecType: TExecType read FExecType write FExecType;
    property ExecProg: string read FExecProg write FExecProg;
  end;

const
  FolderNames: array[TFolderType] of string = ('Normal', 'Source', 'Bitmaps', 'Glyphs', 'Documentation');
  ExecTypeNames: array[TExecType] of string = ('Load In IDE', 'Shell Execute', 'Custom', 'Project');

var
  Root: TGXFolder;

implementation

uses
  SysUtils;

{ TGXFavItem }

constructor TGXFavItem.Create(AOwner: TGXFolder);
begin
  inherited Create;
  AttachTo(AOwner);
end;

destructor TGXFavItem.Destroy;
begin
  Detach;
  inherited Destroy;
end;

procedure TGXFavItem.Detach;
begin
  if Owner <> nil then
    GetContainingList.Remove(Self);
  FOwner := nil;
end;

procedure TGXFavItem.AttachTo(AOwner: TGXFolder);
begin
  FOwner := AOwner;
  if Owner <> nil then
    GetContainingList.Add(Self);
end;

procedure TGXFavItem.SetOwner(const Value: TGXFolder);
begin
  if Value <> Owner then
  begin
    Detach;
    AttachTo(Value);
  end;
end;

{ TGXFolder }

constructor TGXFolder.Create(AOwner: TGXFolder);
begin
  inherited Create(AOwner);
  FFileList := TList.Create;
  FFolderList := TList.Create;
end;

destructor TGXFolder.Destroy;
begin
  ClearList(FFileList);
  FreeAndNil(FFileList);
  ClearList(FFolderList);
  FreeAndNil(FFolderList);

  inherited Destroy;
end;

procedure TGXFolder.ClearList(AList: TList);
var
  i: Integer;
  CurrItem: TGXFavItem;
begin
  for i := 0 to AList.Count - 1 do
  begin
    CurrItem := TGXFavItem(AList[i]);
    CurrItem.FOwner := nil; // Avoid CurrItem removing itself from list
    CurrItem.Free;
  end;
  AList.Clear;
end;

function TGXFolder.GetContainingList: TList;
begin
  Result := Owner.FFolderList;
end;

function TGXFolder.GetFolderCount: Integer;
begin
  Result := FFolderList.Count;
end;

function TGXFolder.GetFolder(Index: Integer): TGXFolder;
begin
  Result := FFolderList[Index];
end;

function TGXFolder.GetFileCount: Integer;
begin
  Result := FFileList.Count;
end;

function TGXFolder.GetFile(Index: Integer): TGXFile;
begin
  Result := FFileList[Index];
end;

{ TGXFile }

constructor TGXFile.Create(AOwner: TGXFolder);
begin
  inherited Create(AOwner);
end;

function TGXFile.GetContainingList: TList;
begin
  Result := Owner.FFileList;
end;

initialization
  Root := TGXFolder.Create(nil);

finalization
  FreeAndNil(Root);

end.

