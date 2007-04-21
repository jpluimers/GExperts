unit GX_ClipboardOptions;

interface

uses
  Classes, StdCtrls, Controls, Forms;

type
  TfmClipboardOptions = class(TForm)
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
