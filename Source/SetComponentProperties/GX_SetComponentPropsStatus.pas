// Set component properties progress/status form
// Original Quthor: Robert Wachtel (rwachtel@gmx.de)

unit GX_SetComponentPropsStatus;

interface

uses
  Forms, StdCtrls, ExtCtrls, Classes, Controls;

type
  TfmSetComponentPropsStatus = class(TForm)
    grpbxStatus: TGroupBox;
    pnlProcessedFile: TPanel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    procedure SetProcessedFile(const Value: string);
  public
    class function GetInstance: TfmSetComponentPropsStatus;
    class procedure ReleaseMe;
    property ProcessedFile: string write SetProcessedFile;
  end;

implementation

{$R *.dfm}

var
  myfmSetComponentPropsStatus: TfmSetComponentPropsStatus = nil;

// Hide the form when closing it
procedure TfmSetComponentPropsStatus.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caHide;
end;

// Class function to get a form singleton
class function TfmSetComponentPropsStatus.GetInstance: TfmSetComponentPropsStatus;
begin
  if not Assigned(myfmSetComponentPropsStatus) then
  begin
    try
      myfmSetComponentPropsStatus := TfmSetComponentPropsStatus.Create(nil);
    except
      myfmSetComponentPropsStatus := nil;
    end;
  end;
  Result := myfmSetComponentPropsStatus;
end;

// Class function to release the form singleton
class procedure TfmSetComponentPropsStatus.ReleaseMe;
begin
  if Assigned(myfmSetComponentPropsStatus) then
  begin
    myfmSetComponentPropsStatus.Release;
    myfmSetComponentPropsStatus := nil;
  end;
end;

// Show the file being processed on the form
procedure TfmSetComponentPropsStatus.SetProcessedFile(const Value: string);
begin
  pnlProcessedFile.Caption := Value;
  Application.ProcessMessages;
end;

end.

