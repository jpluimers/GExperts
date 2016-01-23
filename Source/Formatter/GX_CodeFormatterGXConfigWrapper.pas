// implements the IConfigReader and IConfigWriter interfaces using a TGExpertsSettings instance
// Original Author:     Thomas Mueller (http://www.dummzeuch.de)
unit GX_CodeFormatterGXConfigWrapper;

{$I GX_CondDefine.inc}

interface

uses
  Classes,
  GX_ConfigurationInfo,
  GX_CodeFormatterConfigHandler;

type
  TGxConfigWrapper = class(TInterfacedObject, IConfigReader, IConfigWriter)
  private
    FGxSettings: TGExpertsSettings;
    FSection: string;
  protected // implementation of IConfigReader
    function ReadBool(const AName: string; ADefault: Boolean): Boolean;
    function ReadInteger(const AName: string; ADefault: Integer): Integer;
    function ReadString(const AName, ADefault: string): string;
    procedure ReadStrings(const ASection: string; const AList: TStrings);
  protected // implementatio of IConfigWriter
    procedure WriteBool(const AName: string; AValue: Boolean);
    procedure WriteInteger(const AName: string; AValue: Integer);
    procedure WriteString(const AName: string; const AValue: string);
    procedure WriteStrings(const ASection: string; const AList: TStrings);
  public
    constructor Create(AGxSettings: TGExpertsSettings; const ASection: string);
  end;

implementation

{ TGxConfigWrapper }

constructor TGxConfigWrapper.Create(AGxSettings: TGExpertsSettings; const ASection: string);
begin
  inherited Create;
  FGxSettings := AGxSettings;
  FSection := ASection;
end;

function TGxConfigWrapper.ReadBool(const AName: string; ADefault: Boolean): Boolean;
begin
  Result := FGxSettings.ReadBool(FSection, AName, ADefault);
end;

function TGxConfigWrapper.ReadInteger(const AName: string; ADefault: Integer): Integer;
begin
  Result := FGxSettings.ReadInteger(FSection, AName, ADefault);
end;

function TGxConfigWrapper.ReadString(const AName, ADefault: string): string;
begin
  Result := FGxSettings.ReadString(FSection, AName, ADefault);
end;

procedure TGxConfigWrapper.ReadStrings(const ASection: string; const AList: TStrings);
begin
  FGxSettings.ReadStrings(AList, FSection + '\' + ASection, 'List');
end;

procedure TGxConfigWrapper.WriteBool(const AName: string; AValue: Boolean);
begin
  FGxSettings.WriteBool(FSection, AName, AValue);
end;

procedure TGxConfigWrapper.WriteInteger(const AName: string; AValue: Integer);
begin
  FGxSettings.WriteInteger(FSection, AName, AValue);
end;

procedure TGxConfigWrapper.WriteString(const AName, AValue: string);
begin
  FGxSettings.WriteString(FSection, AName, AValue);
end;

procedure TGxConfigWrapper.WriteStrings(const ASection: string; const AList: TStrings);
begin
  FGxSettings.WriteStrings(AList, FSection + '\' + ASection, 'List');
end;

end.

