unit GX_BaseForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs;

type
  TfmBaseForm = class(TForm)
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  GX_GxUtils;

{$R *.dfm}

constructor TfmBaseForm.Create(AOwner: TComponent);
begin
  inherited;
  GxSetDefaultFont(Self);
end;

end.
