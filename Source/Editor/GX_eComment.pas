unit GX_eComment;

{$I GX_CondDefine.inc}

interface

uses
  GX_eSelectionEditorExpert, GX_ConfigurationInfo,
  Classes, StdCtrls, Controls, Forms;

type
  TCommentType = (ctSlash, ctC, ctPascal, ctCpp);

type
  TCommentExpert = class(TSelectionEditorExpert)
  protected
    function GetDisplayName: string; override;
    class function GetName: string; override;
    procedure InternalSaveSettings(Settings: TGExpertsSettings); override;
    procedure InternalLoadSettings(Settings: TGExpertsSettings); override;
    function ProcessSelected(Lines: TStrings): Boolean; override;
  public
    constructor Create; override;
    procedure Configure; override;
    procedure GetHelpString(List: TStrings); override;
  end;

  TUnCommentExpert = class(TSelectionEditorExpert)
  protected
    function GetDisplayName: string; override;
    class function GetName: string; override;
    function ProcessSelected(Lines: TStrings): Boolean; override;
  public
    constructor Create; override;
    procedure GetHelpString(List: TStrings); override;
    function HasConfigOptions: Boolean; override;
  end;

  TSortExpert = class(TSelectionEditorExpert)
  protected
    function GetDisplayName: string; override;
    class function GetName: string; override;
    function ProcessSelected(Lines: TStrings): Boolean; override;
  public
    procedure GetHelpString(List: TStrings); override;
    function HasConfigOptions: Boolean; override;
  end;

  TfmCommentConfig = class(TForm)
    GroupBox1: TGroupBox;
    rbSlash: TRadioButton;
    rbC: TRadioButton;
    rbPascal: TRadioButton;
    btnOK: TButton;
    btnCancel: TButton;
    rbCpp: TRadioButton;
    chkInsertSpace: TCheckBox;
  end;

implementation

{$R *.dfm}

uses
  SysUtils, GX_EditorExpert;

var
  // This is *local* and used by both the comment
  // and the uncomment expert...
  CommentType: TCommentType = ctSlash;
  InsertRemoveSpace: Boolean = False;

{ TCommentExpert }

procedure TCommentExpert.Configure;
var
  Dlg: TfmCommentConfig;
begin
  Dlg := TfmCommentConfig.Create(nil);
  try
    case CommentType of
      ctSlash: Dlg.rbSlash.Checked := True;
      ctC: Dlg.rbC.Checked := True;
      ctPascal: Dlg.rbPascal.Checked := True;
      ctCpp: Dlg.rbCpp.Checked := True;
    end;
    Dlg.chkInsertSpace.Checked := InsertRemoveSpace;

    if Dlg.ShowModal = mrOk then
    begin
      if Dlg.rbSlash.Checked then
        CommentType := ctSlash
      else if Dlg.rbC.Checked then
        CommentType := ctC
      else if Dlg.rbCpp.Checked then
        CommentType := ctCpp
      else
        CommentType := ctPascal;

      InsertRemoveSpace := Dlg.chkInsertSpace.Checked;

      SaveSettings;
    end;
  finally
    FreeAndNil(Dlg);
  end;
end;

constructor TCommentExpert.Create;
const
  VK_OEM_PERIOD = $BE; // '.' any country
begin
  inherited Create;

  ShortCut := scCtrl + scAlt + VK_OEM_PERIOD;
end;

procedure AddBracketing(const CodeList: TStrings; const LeftBracket, RightBracket: string);
begin
  if InsertRemoveSpace then
  begin
    CodeList[0] := LeftBracket + ' ' + CodeList[0];
    CodeList[CodeList.Count - 1] := CodeList[CodeList.Count - 1] + ' ' + RightBracket;
  end
  else
  begin
    CodeList[0] := LeftBracket + CodeList[0];
    CodeList[CodeList.Count - 1] := CodeList[CodeList.Count - 1] + RightBracket;
  end;
end;

function TCommentExpert.GetDisplayName: string;
resourcestring
  SCommentName = 'Comment Code';
begin
  Result := SCommentName;
end;

procedure TCommentExpert.GetHelpString(List: TStrings);
resourcestring
  SCommentHelp =
    '  This expert comments out a selected block of code. ' +
    'To use it, select a block in the Delphi editor and ' +
    'activate this expert.' + sLineBreak +
    sLineBreak +
    '  You can configure this expert to use different comment styles.';
begin
  List.Text := SCommentHelp;
end;

class function TCommentExpert.GetName: string;
begin
  Result := 'Comment';
end;

procedure TCommentExpert.InternalLoadSettings(Settings: TGExpertsSettings);
begin
  inherited InternalLoadSettings(Settings);
  // Do not localize any of the below items.
  InsertRemoveSpace := Settings.ReadBool('Comment', 'InsertRemoveSpace', InsertRemoveSpace);
  CommentType := TCommentType(Settings.ReadEnumerated('Comment', 'CommentType', TypeInfo(TCommentType), Ord(ctSlash)));
end;

procedure TCommentExpert.InternalSaveSettings(Settings: TGExpertsSettings);
begin
  inherited InternalSaveSettings(Settings);
  // Do not localize any of the below items.
  Settings.WriteBool('Comment', 'InsertRemoveSpace', InsertRemoveSpace);
  Settings.WriteEnumerated('Comment', 'CommentType', TypeInfo(TCommentType), Ord(CommentType));
end;

function TCommentExpert.ProcessSelected(Lines: TStrings): Boolean;
var
  i: Integer;
  CommentPrefix: string;
begin
  Assert(Assigned(Lines));

  // Do not localize any of the below lines.
  case CommentType of
    ctSlash:
      begin
        if InsertRemoveSpace then
          CommentPrefix := '// '
        else
          CommentPrefix := '//';

        for i := 0 to Lines.Count - 1 do
          Lines[i] := CommentPrefix + Lines[i];
      end;

    ctC:
      AddBracketing(Lines, '(*', '*)');

    ctCpp:
      AddBracketing(Lines, '/*', '*/');

    ctPascal:
      AddBracketing(Lines, '{', '}');
  else
    Assert(False);
  end;
  Result := True;
end;

{ TUncommentExpert }

constructor TUnCommentExpert.Create;
const
  VK_OEM_COMMA = $BC; // ',' any country
begin
  inherited Create;

  ShortCut := scCtrl + scAlt + VK_OEM_COMMA;
end;

function TUnCommentExpert.GetDisplayName: string;
resourcestring
  SUncommentName = 'Uncomment Code';
begin
  Result := SUncommentName;
end;

procedure TUnCommentExpert.GetHelpString(List: TStrings);
resourcestring
  SUncommentHelp = '  This expert uncomments a selected block of code.  ' +
    'To use it, select a block in the IDE code editor and ' +
    'activate this expert.' +
    sLineBreak +
    '  Uncommenting is performed using the comment style that ' +
    'you selected for the Comment Code editor expert.';
begin
  List.Text := SUncommentHelp;
end;

class function TUnCommentExpert.GetName: string;
begin
  Result := 'UnComment';
end;

function TUnCommentExpert.HasConfigOptions: Boolean;
begin
  Result := False;
end;

function TUnCommentExpert.ProcessSelected(Lines: TStrings): Boolean;

  function RemoveFirstString(const SubString, InString: string): string;
  var
    Success: Boolean;

    function RemoveFirstInternal(const SubString, InString: string): string;
    var
      SubStringPos: Integer;
    begin
      if StrLComp(PChar(Trim(InString)), PChar(SubString), Length(SubString)) = 0 then
      begin
        SubStringPos := Pos(SubString, InString);
        if SubStringPos > 1 then
        begin
          Result := Copy(InString, 1, SubStringPos - 1) +
            Copy(InString, SubStringPos + Length(SubString), MaxInt)
        end
        else
          Result := Copy(InString, Length(SubString) + 1, MaxInt);

        Success := True;
      end
      else
        Result := InString;
    end;

  begin
    Success := False;
    // If spaces are to be removed, try to first kill
    // WITH space, otherwise continue with default
    // comment removal.
    if InsertRemoveSpace then
    begin
      Result := RemoveFirstInternal(SubString + ' ', InString);
      if Success then
        Exit;
    end;

    Result := RemoveFirstInternal(SubString, InString);
  end;

  function RemoveLastString(const SubString, InString: string): string;
  var
    Success: Boolean;

    function RemoveLastInternal(const SubString, InString: string): string;
    var
      SubStringStartPos: Integer;
      TempString: string;
    begin
      TempString := TrimRight(InString);

      SubStringStartPos := Length(TempString) - Length(SubString) + 1;

      if SubString = Copy(TempString, SubStringStartPos, Length(SubString)) then
      begin
        Result := Copy(TempString, 1, SubStringStartPos - 1);
        Success := True;
      end
      else
        Result := InString;
    end;

  begin
    Success := False;
    // If spaces are to be removed, try to first kill
    // WITH space, otherwise continue with default
    // comment removal.
    if InsertRemoveSpace then
    begin
      Result := RemoveLastInternal(' ' + SubString, InString);
      if Success then
        Exit;
    end;

    Result := RemoveLastInternal(SubString, InString);
  end;

var
  i: Integer;
begin
  Assert(Assigned(Lines));

  case CommentType of
    ctSlash:
      for i := 0 to Lines.Count - 1 do
        Lines[i] := RemoveFirstString('//', Lines[i]);
    ctC:
      begin
        Lines[0] := RemoveFirstString('(*', Lines[0]);
        Lines[Lines.Count - 1] := RemoveLastString('*)', Lines[Lines.Count - 1]);
      end;
    ctCpp:
      begin
        Lines[0] := RemoveFirstString('/*', Lines[0]);
        Lines[Lines.Count - 1] := RemoveLastString('*/', Lines[Lines.Count - 1]);
      end;
    ctPascal:
      begin
        Lines[0] := RemoveFirstString('{', Lines[0]);
        Lines[Lines.Count - 1] := RemoveLastString('}', Lines[Lines.Count - 1]);
      end;
  end;
  Result := True;
end;

{ TSortExpert }

function TSortExpert.GetDisplayName: string;
resourcestring
  SSortName = 'Sort Selected Lines';
begin
  Result := SSortName;
end;

procedure TSortExpert.GetHelpString(List: TStrings);
resourcestring
  SSortHelp = '  This expert sorts the lines in a selected block of code.  ' +
    'To use it, select several lines in the IDE code editor and ' +
    'activate this expert.';
begin
  List.Text := SSortHelp;
end;

class function TSortExpert.GetName: string;
begin
  Result := 'SortLines';
end;

function TSortExpert.HasConfigOptions: Boolean;
begin
  Result := False;
end;

function TSortExpert.ProcessSelected(Lines: TStrings): Boolean;
var
  TrimList, SortedList: TStringList;
  i: Integer;
begin
  Result := False;
  if Lines.Count > 1 then
  begin
   // The trim mess here is so we can ignore whitespace when sorting
    TrimList := TStringList.Create;
    try
      SortedList := TStringList.Create;
      try
        for i := 0 to Lines.Count - 1 do
          TrimList.AddObject(TrimLeft(Lines[i]), TObject(i));
        TrimList.Sort;
        for i := 0 to TrimList.Count - 1 do
          SortedList.Add(Lines[Integer(TrimList.Objects[i])]);
        Lines.Clear;
        Lines.AddStrings(SortedList);
      finally
        FreeAndNil(SortedList);
      end;
    finally
      FreeAndNil(TrimList);
    end;
    Result := True;
  end;
end;

initialization
  RegisterEditorExpert(TCommentExpert);
  RegisterEditorExpert(TUnCommentExpert);
  RegisterEditorExpert(TSortExpert);
end.

