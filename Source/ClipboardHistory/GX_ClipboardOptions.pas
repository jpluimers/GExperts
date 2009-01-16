unit GX_ClipboardOptions;

interface

uses
  Classes, StdCtrls, Controls, Forms, GX_BaseForm;

type
  TfmClipboardOptions = class(TfmBaseForm)
    gbxClipboardOptions: TGroupBox;
    lblMaxEntries: TLabel;
    edtMaxClip: TEdit;
    btnOK: TButton;
    btnCancel: TButton;
    chkAutoStart: TCheckBox;
    chkAutoClose: TCheckBox;
  end;

implementation

{$R *.dfm}

end.
