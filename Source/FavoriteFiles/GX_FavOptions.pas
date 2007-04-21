unit GX_FavOptions;

interface

uses
  Classes, Controls, Forms, StdCtrls;

type
  TfmFavOptions = class(TForm)
    gbxFavOptions: TGroupBox;
    chkConfirmFolderDelete: TCheckBox;
    chkExpandAllOnLoad: TCheckBox;
    btnOK: TButton;
    btnCancel: TButton;
    chkHideOnExecute: TCheckBox;
    chkShowPreview: TCheckBox;
  end;

implementation

{$R *.dfm}

end.
