unit w_GExpertsFormatterMain;

// Calls GExperts dll for formatting
// stand alone version by Ulrich Gerhardt

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  ComCtrls,
  Forms,
  ShellApi,
  Dialogs,
  StdCtrls,
  w_GExpertsFormatterAbout;

type
  Tf_GExpertsFormatterMain = class(TForm)
    ed_FileToFormat: TEdit;
    l_: TLabel;
    od_File: TOpenDialog;
    b_SelectFile: TButton;
    b_Format: TButton;
    b_Exit: TButton;
    b_Settings: TButton;
    b_About: TButton;
    TheStatusBar: TStatusBar;
    procedure b_ExitClick(Sender: TObject);
    procedure ed_FileToFormatChange(Sender: TObject);
    procedure b_SelectFileClick(Sender: TObject);
    procedure b_FormatClick(Sender: TObject);
    procedure b_SettingsClick(Sender: TObject);
    procedure b_AboutClick(Sender: TObject);
  private
    FOldLBWindowProc: TWndMethod;
    procedure NewWindowProc(var Message: TMessage);
    procedure InstallWindowProc;
    procedure UninstallWindowProc;
    procedure WMDROPFILES(var Msg: TMessage);
  public
    constructor Create(_Onwer: TComponent); override;
    destructor Destroy; override;
  end;

procedure Main;

implementation

{$R *.dfm}

uses
  GX_StandAloneLoadDLL;

type
  TFormatFileFunc = function(_FileName: PChar): Boolean;
  TFormatFilesFunc = function(_FileNames: PChar): Boolean;
  TConfigureFormatterProc = procedure;

const
  EntryPoint_FormatFile = 'FormatFile';
  EntryPoint_FormatFiles = 'FormatFiles';
  EntryPoint_ConfigureFormatter = 'ConfigureFormatter';
  EntryPoint_AboutFormatter = 'AboutFormatter';

var
  FormatFile: TFormatFileFunc = nil;
  FormatFiles: TFormatFilesFunc = nil;
  ConfigureFormatter: TConfigureFormatterProc = nil;
  AboutFormatter: TAboutFormatterProc = nil;
  Dll: IGExpertsDll;

function LoadGExperts(out _DllName: string): Boolean;
var
  Path: string;
begin
  try
    Dll := LoadAnyGExpertsDLL;
  except
    on e: exception do begin
      Application.ShowException(e);
      Result := False;
      Exit; //==>
    end;
  end;

  _DllName := Dll.DllName;
  Result := True;

  FormatFile := Dll.GetProcAddress(EntryPoint_FormatFile);
  FormatFiles := Dll.GetProcAddress(EntryPoint_FormatFiles);
  ConfigureFormatter := Dll.GetProcAddress(EntryPoint_ConfigureFormatter);
  AboutFormatter := Dll.GetProcAddress(EntryPoint_AboutFormatter);
end;

procedure Interactive(const _DllName: string);
var
  frm: Tf_GExpertsFormatterMain;
begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(Tf_GExpertsFormatterMain, frm);
  frm.TheStatusBar.SimpleText := _DllName + ' loaded.';
  Application.Run;
end;

procedure doFormatFile(FileName: string);
begin
  FileName := ExpandFileName(FileName);
  FormatFile(PChar(FileName));
end;

procedure Batch;
var
  FileName: string;
  FileList: TStringList;
  i: Integer;
begin
  FileList := TStringList.Create;
  try
    for i := 1 to ParamCount do begin
      FileName := ParamStr(i);
      FileName := ExpandFileName(FileName);
      FileList.Add(FileName);
    end;
    FileList.StrictDelimiter := True;
    FileList.Delimiter := ';';
    FormatFiles(PChar(FileList.DelimitedText));
  finally
    FileList.Free;
  end;
end;

procedure Main;
var
  DllName: string;
begin
  if LoadGExperts(DllName) then begin
    if ParamCount = 0 then begin
      Interactive(DllName);
    end else begin
      Batch;
    end;
  end;
end;

constructor Tf_GExpertsFormatterMain.Create(_Onwer: TComponent);
begin
  inherited;
  InstallWindowProc;
  b_Settings.Visible := Assigned(@ConfigureFormatter);
  b_About.Visible := Assigned(@AboutFormatter);
end;

destructor Tf_GExpertsFormatterMain.Destroy;
begin
  UninstallWindowProc;
  inherited;
end;

procedure Tf_GExpertsFormatterMain.b_SelectFileClick(Sender: TObject);
begin
  if od_File.Execute then
    ed_FileToFormat.Text := od_File.FileName;
end;

procedure Tf_GExpertsFormatterMain.b_SettingsClick(Sender: TObject);
begin
  ConfigureFormatter;
end;

procedure Tf_GExpertsFormatterMain.b_AboutClick(Sender: TObject);
begin
  Tf_GExpertsFormatterAbout.Execute(Self, @AboutFormatter);
end;

procedure Tf_GExpertsFormatterMain.b_ExitClick(Sender: TObject);
begin
  Close;
end;

procedure Tf_GExpertsFormatterMain.b_FormatClick(Sender: TObject);
begin
  doFormatFile(ed_FileToFormat.Text);
end;

procedure Tf_GExpertsFormatterMain.ed_FileToFormatChange(Sender: TObject);
var
  fn: string;
begin
  fn := ed_FileToFormat.Text;
  b_Format.Enabled := FileExists(fn);
end;

procedure Tf_GExpertsFormatterMain.InstallWindowProc;
var
  frm: TForm;
begin
  frm := Self as TForm;
  FOldLBWindowProc := frm.WindowProc;
  frm.WindowProc := NewWindowProc;
  DragAcceptFiles(frm.Handle, True);
end;

procedure Tf_GExpertsFormatterMain.UninstallWindowProc;
var
  frm: TForm;
begin
  frm := Self as TForm;
  DragAcceptFiles(frm.Handle, False);
  frm.WindowProc := FOldLBWindowProc;
end;

procedure Tf_GExpertsFormatterMain.NewWindowProc(var Message: TMessage);
begin
  if Message.Msg = WM_DROPFILES then
    WMDROPFILES(Message);
  FOldLBWindowProc(Message);
end;

procedure Tf_GExpertsFormatterMain.WMDROPFILES(var Msg: TMessage);
var
  pcFileName: array[0..255] of Char;
  fn: string;
  i, iSize, iFileCount: Integer;
begin
  iFileCount := DragQueryFile(Msg.wParam, $FFFFFFFF, nil, 255);
  for i := 0 to iFileCount - 1 do begin
    iSize := DragQueryFile(Msg.wParam, i, nil, 0) + 1;
    DragQueryFile(Msg.wParam, i, pcFileName, iSize);
    fn := pcFileName;
    fn := PChar(fn);
    if FileExists(fn) then
      ed_FileToFormat.Text := fn;
  end;
  DragFinish(Msg.wParam);
end;

end.

