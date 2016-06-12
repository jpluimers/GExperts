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
    FSettings: TExpertSettings;
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
    constructor Create(ASettings: TExpertSettings);
  end;

implementation

{ TGxConfigWrapper }

constructor TGxConfigWrapper.Create(ASettings: TExpertSettings);
begin
  inherited Create;
  FSettings := ASettings;
end;

function TGxConfigWrapper.ReadBool(const AName: string; ADefault: Boolean): Boolean;
begin
  Result := FSettings.ReadBool(AName, ADefault);
end;

function TGxConfigWrapper.ReadInteger(const AName: string; ADefault: Integer): Integer;
begin
  Result := FSettings.ReadInteger(AName, ADefault);
end;

function TGxConfigWrapper.ReadString(const AName, ADefault: string): string;
begin
  Result := FSettings.ReadString(AName, ADefault);
end;

procedure TGxConfigWrapper.ReadStrings(const ASection: string; const AList: TStrings);
begin
  FSettings.ReadStrings(ASection, AList, 'List');
end;

procedure TGxConfigWrapper.WriteBool(const AName: string; AValue: Boolean);
begin
  FSettings.WriteBool(AName, AValue);
end;

procedure TGxConfigWrapper.WriteInteger(const AName: string; AValue: Integer);
begin
  FSettings.WriteInteger(AName, AValue);
end;

procedure TGxConfigWrapper.WriteString(const AName, AValue: string);
begin
  FSettings.WriteString(AName, AValue);
end;

procedure TGxConfigWrapper.WriteStrings(const ASection: string; const AList: TStrings);
begin
  FSettings.WriteStrings(ASection, AList, 'List');
end;

end.

