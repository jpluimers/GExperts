unit GX_eComment;

{$I GX_CondDefine.inc}

interface

uses
  GX_eSelectionEditorExpert, GX_ConfigurationInfo,
  Classes, StdCtrls, Controls, Forms, GX_BaseForm, ExtCtrls, ComCtrls;

type
//  //        ctSlash
//  (* *)     ctC
//  { }       ctPascal
//  /* */     ctCpp
//  -- (SQL)  ctSQL
  TCommentType = (ctSlash, ctC, ctPascal, ctCpp, ctSQL);

  TExtensionStyle = class(TObject)
  private
    FExtensions: String;
    FCommentType: TCommentType;
    FInsertRemoveSpace: Boolean;
    FIsDefault: Boolean;
  protected
  public
    constructor Create;
    property Extensions: String read FExtensions write FExtensions;
    property CommentType: TCommentType read FCommentType write FCommentType;
    property InsertRemoveSpace: Boolean read FInsertRemoveSpace write FInsertRemoveSpace;
    property IsDefault: Boolean read FIsDefault write FIsDefault;
  end ;

  TCommentStyles = class(TStringList)
  private
    procedure ClearStyles;
  protected
  public
    destructor Destroy; override;
    procedure Clear; override;
    function  GetStyle(AFileName: String): TExtensionStyle;
    procedure CopyStyles(AFromStyles: TCommentStyles);
  end;

  TCommentExpert = class(TSelectionEditorExpert)
  protected
    procedure InternalSaveSettings(Settings: TGExpertsSettings); override;
    procedure InternalLoadSettings(Settings: TGExpertsSettings); override;
    function ProcessSelected(Lines: TStrings): Boolean; override;
  public
    class function GetName: string; override;
    constructor Create; override;
    function GetDefaultShortCut: TShortCut; override;
    function GetDisplayName: string; override;
    procedure Configure; override;
    function GetHelpString: string; override;
  end;

  TUnCommentExpert = class(TSelectionEditorExpert)
  protected
    function ProcessSelected(Lines: TStrings): Boolean; override;
  public
    class function GetName: string; override;
    constructor Create; override;
    function GetDefaultShortCut: TShortCut; override;
    function GetDisplayName: string; override;
    function GetHelpString: string; override;
    function HasConfigOptions: Boolean; override;
  end;

  TSortExpert = class(TSelectionEditorExpert)
  protected
    function ProcessSelected(Lines: TStrings): Boolean; override;
  public
    class function GetName: string; override;
    function GetDisplayName: string; override;
    function GetHelpString: string; override;
    function HasConfigOptions: Boolean; override;
  end;

  TfmCommentConfig = class(TfmBaseForm)
    gbStyles: TGroupBox;
    lvStyles: TListView;
    gbStyle: TGroupBox;
    lblExtensions: TLabel;
    eExtensions: TEdit;
    rgStyle: TRadioGroup;
    chkInsertSpace: TCheckBox;
    btnAdd: TButton;
    btnDelete: TButton;
    btnOK: TButton;
    btnCancel: TButton;
    procedure lvStylesData(Sender: TObject; Item: TListItem);
    procedure lvStylesSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure eExtensionsChange(Sender: TObject);
    procedure rgStyleClick(Sender: TObject);
    procedure chkInsertSpaceClick(Sender: TObject);
  private
    FStyles: TCommentStyles;
    FCurrStyle: TExtensionStyle;
    FCurrIndex: Integer;
    function  ConvertStyleTypeToViewedType(AStyleType: TCommentType): Integer;
    function  ConvertViewedTypeToStyleType(AViewedType: Integer): TCommentType;
  protected
    procedure Initialize;
  end;

implementation

{$R *.dfm}

uses
  SysUtils, StrUtils, Math,
  GX_EditorExpert, GX_OtaUtils;

var
  // This is *local* and used by both the comment
  // and the uncomment expert...
  Styles: TCommentStyles;

const
  cNameDefaultStyle = 'Default style';
  cIniStyleCount = 'StyleCount';
  cIniExtensions = 'Extensions';
  cIniCommentType = 'CommentType';
  cIniInsertRemoveSpace = 'InsertRemoveSpace';

{ TCommentExpert }

procedure TCommentExpert.Configure;
var
  Dlg: TfmCommentConfig;
begin
  Dlg := TfmCommentConfig.Create(nil);
  try
    Dlg.Initialize;

    if Dlg.ShowModal = mrOk then
    begin
      Styles.CopyStyles(Dlg.FStyles);

      SaveSettings;
    end;
  finally
    FreeAndNil(Dlg);
  end;
end;

constructor TCommentExpert.Create;
begin
  inherited Create;
end;

function TCommentExpert.GetDefaultShortCut: TShortCut;
const
  VK_OEM_PERIOD = $BE; // '.' any country
begin
  Result := scCtrl + scAlt + VK_OEM_PERIOD;
end;

function TCommentExpert.GetDisplayName: string;
resourcestring
  SCommentName = 'Comment Code';
begin
  Result := SCommentName;
end;

function TCommentExpert.GetHelpString: string;
resourcestring
  SCommentHelp =
    '  This expert comments out a selected block of code. ' +
    'To use it, select a block in the Delphi editor and ' +
    'activate this expert.' + sLineBreak +
    sLineBreak +
    '  You can configure this expert to use different comment styles.';
begin
  Result := SCommentHelp;
end;

class function TCommentExpert.GetName: string;
begin
  Result := 'Comment';
end;

procedure TCommentExpert.InternalLoadSettings(Settings: TGExpertsSettings);
var
  I, ACount: Integer;
  AIndexText: String;
  AStyle: TExtensionStyle;
begin
  inherited InternalLoadSettings(Settings);
  // Do not localize any of the below items.
  Styles.Clear;
  ACount := Settings.ReadInteger(ConfigurationKey, cIniStyleCount, 1);  //1= default style is always created
  for I := 0 to ACount-1 do
  begin
    AStyle := TExtensionStyle.Create;
    AStyle.IsDefault := I = 0;
    AIndexText := IfThen(not AStyle.IsDefault, IntToStr(I));

    AStyle.Extensions := Settings.ReadString(ConfigurationKey, cIniExtensions + AIndexText, cNameDefaultStyle);
    AStyle.CommentType :=
      TCommentType(Settings.ReadEnumerated(ConfigurationKey, cIniCommentType + AIndexText, TypeInfo(TCommentType), Ord(ctSlash)));
    AStyle.InsertRemoveSpace := Settings.ReadBool(ConfigurationKey, cIniInsertRemoveSpace + AIndexText, False);

    Styles.AddObject(AStyle.Extensions, AStyle);
  end;
end;

procedure TCommentExpert.InternalSaveSettings(Settings: TGExpertsSettings);
var
  I, ACount: Integer;
  AIndexText: String;
  AStyle: TExtensionStyle;
begin
  inherited InternalSaveSettings(Settings);
  // Do not localize any of the below items.
  ACount := Settings.ReadInteger(ConfigurationKey, cIniStyleCount, 0);
  Settings.WriteInteger(ConfigurationKey, cIniStyleCount, Styles.Count);
  for I := 0 to Styles.Count-1 do
  begin
    AStyle := TExtensionStyle(Styles.Objects[I]);
    AIndexText := IfThen(not AStyle.IsDefault, IntToStr(I));

    if not AStyle.IsDefault then
      Settings.WriteString(ConfigurationKey, cIniExtensions + AIndexText, AStyle.Extensions);
    Settings.WriteEnumerated(ConfigurationKey, cIniCommentType + AIndexText, TypeInfo(TCommentType), Ord(AStyle.CommentType));
    Settings.WriteBool(ConfigurationKey, cIniInsertRemoveSpace + AIndexText, AStyle.InsertRemoveSpace);
  end;
  for I := Styles.Count to ACount-1 do
  begin
    AIndexText := IntToStr(I);
    Settings.DeleteKey(ConfigurationKey, cIniExtensions + AIndexText);
    Settings.DeleteKey(ConfigurationKey, cIniCommentType + AIndexText);
    Settings.DeleteKey(ConfigurationKey, cIniInsertRemoveSpace + AIndexText);
  end;
end;

function TCommentExpert.ProcessSelected(Lines: TStrings): Boolean;
var
  i: Integer;
  CommentPrefix: string;
  AStyle: TExtensionStyle;

  procedure AddBracketing(const CodeList: TStrings; const LeftBracket, RightBracket: string);
  begin
    if AStyle.InsertRemoveSpace then
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

begin
  Assert(Assigned(Lines));

  AStyle := Styles.GetStyle(GxOtaGetTopMostEditBufferFileName);

  // Do not localize any of the below lines.
  case AStyle.CommentType of
    ctSlash:
      begin
        CommentPrefix := '//' + IfThen(AStyle.InsertRemoveSpace, ' ');

        for i := 0 to Lines.Count - 1 do
          Lines[i] := CommentPrefix + Lines[i];
      end;

    ctSQL:
      begin
        CommentPrefix := '--' + IfThen(AStyle.InsertRemoveSpace, ' ');

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
begin
  inherited Create;
end;

function TUnCommentExpert.GetDefaultShortCut: TShortCut;
const
  VK_OEM_COMMA = $BC; // ',' any country
begin
  Result := scCtrl + scAlt + VK_OEM_COMMA;
end;

function TUnCommentExpert.GetDisplayName: string;
resourcestring
  SUncommentName = 'Uncomment Code';
begin
  Result := SUncommentName;
end;

function TUnCommentExpert.GetHelpString: string;
resourcestring
  SUncommentHelp = '  This expert uncomments a selected block of code.  ' +
    'To use it, select a block in the IDE code editor and ' +
    'activate this expert.' +
    sLineBreak +
    '  Uncommenting is performed using the comment style that ' +
    'you selected for the Comment Code editor expert.';
begin
  Result := SUncommentHelp;
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
var
  AStyle: TExtensionStyle;

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
    if AStyle.InsertRemoveSpace then
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
    if AStyle.InsertRemoveSpace then
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

  AStyle := Styles.GetStyle(GxOtaGetTopMostEditBufferFileName);

  case AStyle.CommentType of
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
    ctSQL:
      for i := 0 to Lines.Count - 1 do
        Lines[i] := RemoveFirstString('--', Lines[i]);
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

function TSortExpert.GetHelpString: string;
resourcestring
  SSortHelp = '  This expert sorts the lines in a selected block of code.  ' +
    'To use it, select several lines in the IDE code editor and ' +
    'activate this expert.';
begin
  Result := SSortHelp;
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

{ TExtensionStyle }

constructor TExtensionStyle.Create;
begin
  inherited Create;
  FExtensions := '';
  FCommentType := ctSlash;
  FInsertRemoveSpace := False;
end;

{ TCommentStyles }

destructor TCommentStyles.Destroy;
begin
  ClearStyles;
  inherited Destroy;
end;

procedure TCommentStyles.ClearStyles;
var
  I: Integer;
begin
  for I := 0 to Count-1 do
    Objects[I].Free;
end;

procedure TCommentStyles.Clear;
begin
  ClearStyles;
  inherited Clear;
end;

function TCommentStyles.GetStyle(AFileName: String): TExtensionStyle;
var
  I: Integer;
  AExt: String;
begin
  AExt := ExtractFileExt(AFileName);
  if LeftStr(AExt, 1) = '.' then
    AExt := Copy(AExt, 2, MaxInt);

  for I := 1 to Count-1 do
    if AnsiContainsText(Strings[I], AExt) then
    begin
      Result := TExtensionStyle(Objects[I]);
      Exit;
    end;

  Result := TExtensionStyle(Objects[0]);
end;

procedure TCommentStyles.CopyStyles(AFromStyles: TCommentStyles);
var
  I: Integer;
  AStyle, AFromStyle: TExtensionStyle;
begin
  Clear;
  for I := 0 to AFromStyles.Count-1 do
  begin
    AFromStyle := TExtensionStyle(AFromStyles.Objects[I]);
    AStyle := TExtensionStyle.Create;
    AStyle.Extensions := AFromStyle.Extensions;
    AStyle.CommentType := AFromStyle.CommentType;
    AStyle.InsertRemoveSpace := AFromStyle.InsertRemoveSpace;
    AStyle.IsDefault := AFromStyle.IsDefault;
    AddObject(AStyle.Extensions, AStyle);
  end;
end;

{ TfmCommentConfig }

procedure TfmCommentConfig.FormCreate(Sender: TObject);
begin
  FStyles := TCommentStyles.Create;
  FCurrIndex := -1;
  FCurrStyle := nil;
end;

procedure TfmCommentConfig.FormDestroy(Sender: TObject);
begin
  FStyles.Free;
end;

procedure TfmCommentConfig.Initialize;
begin
 FStyles.CopyStyles(Styles);
 lvStyles.Clear;
 lvStyles.Items.Count := FStyles.Count;
 lvStyles.ItemIndex := 0;
end;

//  //        ctSlash
//  { }       ctPascal
//  -- (SQL)  ctSQL
//  /* */     ctCpp
//  (* *)     ctC
//  TCommentType = (ctSlash, ctC, ctPascal, ctCpp, ctSQL);

function TfmCommentConfig.ConvertStyleTypeToViewedType(AStyleType: TCommentType): Integer;
const
  cCommentTypeArray: array[TCommentType] of Integer = (0, 4, 1, 3, 2);
begin
  Result := cCommentTypeArray[AStyleType];
end;

function TfmCommentConfig.ConvertViewedTypeToStyleType(AViewedType: Integer): TCommentType;
const
  cViewedArray: array[0..Integer(High(TCommentType))] of TCommentType = (ctSlash, ctPascal, ctSQL, ctCpp, ctC);
begin
  Result := cViewedArray[AViewedType];
end;

procedure TfmCommentConfig.lvStylesData(Sender: TObject; Item: TListItem);
var
  AStyle: TExtensionStyle;
begin
  AStyle := TExtensionStyle(FStyles.Objects[Item.Index]);
  Item.Caption := AStyle.Extensions;
  Item.SubItems.Add( rgStyle.Items[ ConvertStyleTypeToViewedType(AStyle.CommentType) ] );
  Item.SubItems.Add( BoolToStr(AStyle.InsertRemoveSpace, True) );
end;

procedure TfmCommentConfig.lvStylesSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  if not Selected then
    Exit;

  FCurrIndex := Item.Index;
  FCurrStyle := TExtensionStyle(FStyles.Objects[FCurrIndex]);
  eExtensions.Text := FCurrStyle.Extensions;
  eExtensions.ReadOnly := FCurrStyle.IsDefault;
  rgStyle.ItemIndex := ConvertStyleTypeToViewedType(FCurrStyle.CommentType);
  chkInsertSpace.Checked := FCurrStyle.InsertRemoveSpace;
  btnDelete.Enabled := not FCurrStyle.IsDefault;
end;

procedure TfmCommentConfig.btnAddClick(Sender: TObject);
begin
  FStyles.AddObject('Style' + IntToStr(FStyles.Count), TExtensionStyle.Create);
  lvStyles.Items.Count := FStyles.Count;
  lvStyles.ItemIndex := FStyles.Count-1;
  eExtensions.SetFocus;
end;

procedure TfmCommentConfig.btnDeleteClick(Sender: TObject);
begin
  lvStyles.SetFocus;
  FCurrStyle.Free;
  FStyles.Delete(FCurrIndex);
  lvStyles.Items.Count := FStyles.Count;
  lvStyles.ItemIndex := Min(FCurrIndex, FStyles.Count-1);
end;

procedure TfmCommentConfig.eExtensionsChange(Sender: TObject);
begin
  if eExtensions.ReadOnly then
    Exit;

  FCurrStyle.Extensions := eExtensions.Text;
  FStyles[FCurrIndex] := eExtensions.Text;
  lvStyles.Refresh;
end;

procedure TfmCommentConfig.rgStyleClick(Sender: TObject);
begin
  FCurrStyle.CommentType := ConvertViewedTypeToStyleType(rgStyle.ItemIndex);
  lvStyles.Refresh;
end;

procedure TfmCommentConfig.chkInsertSpaceClick(Sender: TObject);
begin
  FCurrStyle.InsertRemoveSpace := chkInsertSpace.Checked;
  lvStyles.Refresh;
end;

initialization
  Styles := TCommentStyles.Create;
  RegisterEditorExpert(TCommentExpert);
  RegisterEditorExpert(TUnCommentExpert);
  RegisterEditorExpert(TSortExpert);

finalization
  Styles.Free;

end.

