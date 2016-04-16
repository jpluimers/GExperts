unit GX_BaseExpert;

interface

uses
  Classes, Graphics;

type
  TGX_BaseExpert = class(TObject)
  private
    FBitmap: TBitmap;
  protected
    function GetShortCut: TShortCut; virtual; abstract;
    procedure SetShortCut(Value: TShortCut); virtual; abstract;
    // Return the file name of an icon associated with
    // the expert. Do not specify a path.
    // This bitmap must be included in the
    // GXIcons.rc file which in turn can be created
    // from all .bmp files located in the Images
    // directory by calling the _CreateGXIconsRc.bat
    // script located in that directory.
    // It is possible to return an empty string. This
    // signals that no icon file is available.
    function GetBitmapFileName: string; virtual;
  public
    // Internal name of expert for expert identification.
    class function GetName: string; virtual;
    destructor Destroy; override;
    // Get a reference to the bitmap for menu items, buttons, etc.
    // Note that this bitmap is loaded and unloaded on demand;
    // you will have to .Assign the bitmap to get a permanent copy.
    function GetBitmap: Graphics.TBitmap; virtual;
    // Name to be displayed for the expert in the GExperts
    // *configuration* dialog; this is a different entry than
    // the action caption (GetActionCaption)
    function GetDisplayName: string; virtual;
    procedure Execute(Sender: TObject); virtual; abstract;
    function GetDefaultShortCut: TShortCut; virtual;
    function HasConfigOptions: Boolean; virtual;
    property ShortCut: TShortCut read GetShortCut write SetShortCut;
  end;

implementation

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  SysUtils, GX_GxUtils, GX_MessageBox, GX_IconMessageBox;

{ TGX_BaseExpert }

destructor TGX_BaseExpert.Destroy;
begin
  FreeAndNil(FBitmap);
  inherited;
end;

function TGX_BaseExpert.GetBitmap: Graphics.TBitmap;
var
  BitmapFile: string;
begin
  if not Assigned(FBitmap) then
  begin
    // Locate and load the bitmap for the expert if it exists.
    BitmapFile := GetBitmapFileName;
    if BitmapFile <> '' then
    begin
      if not GxLoadBitmapForExpert(BitmapFile, FBitmap) then
      begin
        {$IFOPT D+} SendDebugError('Missing bitmap ' + BitmapFile + ' for ' + Self.ClassName); {$ENDIF}
        ShowGxMessageBox(TShowMissingIconMessage, ChangeFileExt(BitmapFile, '.bmp'));
      end;
    end
    else
    begin
      {$IFOPT D+} SendDebugError('Bitmap missing for expert ' + Self.ClassName); {$ENDIF D+}
    end;
  end;

  Result := FBitmap;
end;

function TGX_BaseExpert.GetBitmapFileName: string;
begin
  Result := GetName;
end;

function TGX_BaseExpert.GetDefaultShortCut: TShortCut;
begin
  Result := 0;
end;

function TGX_BaseExpert.GetDisplayName: string;
begin
  {$IFOPT D+} SendDebugError('The expert ' + Self.ClassName + ' does not provide a human-readable name'); {$ENDIF D+}
  Result := Self.ClassName;
end;

class function TGX_BaseExpert.GetName: string;
begin
  {$IFOPT D+} SendDebugError('The expert ' + Self.ClassName + ' does not provide a Name'); {$ENDIF D+}
  Result := Self.ClassName;
end;

function TGX_BaseExpert.HasConfigOptions: Boolean;
begin
  Result := True;
end;

end.
