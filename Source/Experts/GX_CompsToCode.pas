unit GX_CompsToCode;

{$I GX_CondDefine.inc}

(*
  Copy Selected Component Creation Code to Clipboard
  Original author: Primoz Gabrijelcic <gabr@17slon.com>

 Notes:
  * Add configuration memo to specify properties to ignore (Name, Visible, etc.)
  * Config option: Remove component (default: no). Make a backup copy.
    Store it somewhere in memory, make a button on config
    "Restore component".
  * Special child component support works for TTabSheet and descendants, not
    other TabSheet equivalents (TTab95Sheet for example) or other strange
    components like TTeeChart
*)

interface

uses
  GX_Experts, GX_EnhancedEditor, GX_ConfigurationInfo,
  Classes, Controls, Forms, StdCtrls, ExtCtrls, ActnList, Buttons, GX_BaseForm;

type
  TComponentArray = array of TComponent;

  TBinProps = (bpSkip, bpComment, bpUncomment);
  TCToCLanguage = (lPascal, lCpp);

  TCToCSettings = record
    BinProps: TBinProps;
    Prepend: Boolean;
    UseDelphiWith: Boolean;
    CreateFreeCode: Boolean;
    Language: TCToCLanguage;
  end;

  TCompsToCodeExpert = class(TGX_Expert)
  protected
    procedure UpdateAction(Action: TCustomAction); override;
    procedure InternalLoadSettings(Settings: TExpertSettings); override;
    procedure InternalSaveSettings(Settings: TExpertSettings); override;
  private
    FSettings: TCToCSettings;
    function GetDesignerComps: TComponentArray;
  public
    constructor Create; override;
    function GetActionCaption: string; override;
    class function GetName: string; override;
    procedure Configure; override;
    procedure Execute(Sender: TObject); override;
    function HasDesignerMenuItem: Boolean; override;
  end;

  TfmCompsToCode = class(TfmBaseForm)
    rgpBinProps: TRadioGroup;
    rgpLanguage: TRadioGroup;
    gbxGenerated: TGroupBox;
    chkPrepend: TCheckBox;
    chkUseDelphiWith: TCheckBox;
    chkCreateFreeCode: TCheckBox;
    pnlMain: TPanel;
    pnlSettings: TPanel;
    pnlView: TPanel;
    ParentPanel: TPanel;
    Edit1: TEdit;
    SpeedButton1: TSpeedButton;
    pnlButtons: TPanel;
    pnlCenterButtons: TPanel;
    btnHelp: TButton;
    btnCancel: TButton;
    btnOK: TButton;
    procedure btnHelpClick(Sender: TObject);
    procedure SettingsChanged(Sender: TObject);
  private
    FSettings: TCToCSettings;
    FCodeText: TGxEnhancedEditor;
    procedure ShowSampleCode;
  public
    constructor Create(AOwner: TComponent); override;
    property Settings: TCToCSettings read FSettings;
    procedure InitSettings(const ASettings: TCToCSettings);
  end;

implementation

{$R *.dfm}

uses
  SysUtils, Clipbrd, ComCtrls, ToolsAPI, TypInfo, Menus, StrUtils,
  GX_GenericUtils, GX_GxUtils, GX_OtaUtils, GX_MessageBox, GX_IdeUtils;

type
  TCCOptions = (ccBinaryRemove, ccBinaryComment, ccBinaryUncomment,
    ccIncludeObjectText, ccUseWith, ccUseFree);
  TCCOptionSet = set of TCCOptions;

  TComponentCreate = class(TObject)
  private
    ccObj: TStringList;
    ccDecl: TStringList;
    ccCrea: TStringList;
    ccDele: TStringList;
    ccImpl: TStringList;
    ccDumped: TStringList;
    ccCompDef: TStringList;
    ccInpPos: Integer;
    ccLn: string;
    ccULn: string;
    ccIsDirty: Boolean;
    ccForm: string;
    ccOptions: TCCOptionSet;
    FHaveFormSelected: Boolean;
    function EOF: Boolean;
    procedure Readln;
    procedure DumpComponent(Comp: TComponent; const Obj, Decl, Crea, Dele, Impl: TStrings);
    procedure StreamAndParse(Comp: TComponent; const Obj, Decl, Crea, Dele, Impl, Sub: TStrings);
    procedure ParseComponent(Comp: TComponent; const Decl, Crea, Dele, Impl, Sub: TStrings);
    function GetDumped: TStrings;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Dump(Comp: TComponent; Options: TCCOptionSet);
    property Dumped: TStrings read GetDumped;
  end;

  TShowCodeOnClipboardMessage = class(TGxMsgBoxAdaptor)
  protected
    function GetMessage: string; override;
  end;

const
  CFmtCPPVar = '';
  CFmtCPPDeclaration = '    %1:s *%0:s;';
  CFmtCPPCreation = '    %s = new %s(%s);';
  CFmtCPPDeletion = '    delete[] %s;';
  CFmtCPPWith = '';
  CFmtCPPParent = 'Parent = %s;';
  CFmtCPPName = 'Name = "%s";';
  CFmtCPPPageControl = 'PageControl = %s;';
  CFmtCPPBinaryCmt = '// %s = // please assign';
  CFmtCPPBinaryUncmt = '%s = // please assign';
  CFmtCPPAdd = '%s->Add(%s);';
  CFmtCPPClear = '%s->Clear();';
  CFmtCPPAssign = '%s = %s;';
  CFmtCPPEnd = '';
  CFmtCPPWithAdd = '';
  CFmtCPPPointerDeclaration = '        %s *%s = %s->%s->Add();';
  CFmtCPPSelf = 'this';
  CFmtCPPPropertyAccess = '->';
  CFmtCPPIndent = '    ';

  CFmtPasVar = 'var';
  CFmtPasDeclaration = '  %s: %s;';
  CFmtPasCreation = '  %s := %s.Create(%s);';
  CFmtPasDeletion = '  %s.Free;';
  CFmtPasWith = '  with %s do' + sLineBreak + '  begin';
  CFmtPasParent = '    Parent := %s;';
  CFmtPasName = '    Name := ''%s'';';
  CFmtPasPageControl = '    PageControl := %s;';
  CFmtPasBinaryCmt = '// %s := // please assign';
  CFmtPasBinaryUncmt = '%s := // please assign';
  CFmtPasClear = '    %s.Clear;';
  CFmtPasAdd = '    %s.Add(%s);';
  CFmtPasAssign = '    %s := %s;';
  CFmtPasEnd = '  end;';
  CFmtPasWithAdd = '  with %s.Add do begin ';
  CFmtPasPointerDeclaration = '';
  CFmtPasSelf = 'Self';
  CFmtPasPropertyAccess = '.';
  CFmtPasIndent = '  ';
var
  CFmtVar: string;
  CFmtDeclaration: string;
  CFmtCreation: string;
  CFmtDeletion: string;
  CFmtWith: string;
  CFmtParent: string;
  CFmtName: string;
  CFmtPageControl: string;
  CFmtBinaryCmt: string;
  CFmtBinaryUncmt: string;
  CFmtAdd: string;
  CFmtClear: string;
  CFmtAssign: string;
  CFmtEnd: string;
  CFmtWithAdd: string;
  CFmtPointerDeclaration: string;
  CFmtSelf: string;
  CFmtOwner: string;
  CFmtPropertyAccess: string;
  CFmtIndent: string;

{ Global }

function DoGetCreationCode(const ASettings: TCToCSettings; const AComponents: array of TComponent): string;
var
  i: Integer;
  CompConv: TComponentCreate;
  DumpOptions: TCCOptionSet;
begin
  DumpOptions := [];
  case ASettings.BinProps of
    bpSkip: Include(DumpOptions, ccBinaryRemove);
    bpComment: Include(DumpOptions, ccBinaryComment);
    bpUncomment: Include(DumpOptions, ccBinaryUncomment);
  end;

  if ASettings.Prepend then
    Include(DumpOptions, ccIncludeObjectText);

  if ASettings.UseDelphiWith then
    Include(DumpOptions, ccUseWith);

  if ASettings.CreateFreeCode then
    Include(DumpOptions, ccUseFree);

  case ASettings.Language of
    lPascal: begin
        CFmtVar := CFmtPasVar;
        CFmtDeclaration := CFmtPasDeclaration;
        CFmtCreation := CFmtPasCreation;
        CFmtDeletion := CFmtPasDeletion;
        if ASettings.UseDelphiWith then
          CFmtWith := CFmtPasWith
        else
          CFmtWith := '';
        CFmtParent := CFmtPasParent;
        CFmtName := CFmtPasName;
        CFmtPageControl := CFmtPasPageControl;
        CFmtBinaryCmt := CFmtPasBinaryCmt;
        CFmtBinaryUncmt := CFmtPasBinaryUncmt;
        CFmtAdd := CFmtPasAdd;
        CFmtClear := CFmtPasClear;
        CFmtAssign := CFmtPasAssign;
        CFmtEnd := CFmtPasEnd;
        CFmtWithAdd := CFmtPasWithAdd;
        CFmtPointerDeclaration := CFmtPasPointerDeclaration;
        CFmtSelf := CFmtPasSelf;
        if (ccUseFree in DumpOptions) then
          CFmtOwner := 'nil'
        else
          CFmtOwner := CFmtPasSelf;
        CFmtPropertyAccess := CFmtPasPropertyAccess;
        CFmtIndent := CFmtPasIndent
      end;
    lCpp: begin
        CFmtVar := CFmtCPPVar;
        CFmtDeclaration := CFmtCPPDeclaration;
        CFmtCreation := CFmtCPPCreation;
        CFmtDeletion := CFmtCPPDeletion;
        CFmtWith := CFmtCPPWith;
        CFmtParent := CFmtCPPParent;
        CFmtName := CFmtCPPName;
        CFmtPageControl := CFmtCPPPageControl;
        CFmtBinaryCmt := CFmtCPPBinaryCmt;
        CFmtBinaryUncmt := CFmtCPPBinaryUncmt;
        CFmtAdd := CFmtCPPAdd;
        CFmtClear := CFmtCPPClear;
        CFmtAssign := CFmtCPPAssign;
        CFmtEnd := CFmtCPPEnd;
        CFmtWithAdd := CFmtCPPWithAdd;
        CFmtPointerDeclaration := CFmtCPPPointerDeclaration;
        CFmtSelf := CFmtCPPSelf;
        if (ccUseFree in DumpOptions) then
          CFmtOwner := '(TComponent *)NULL'
        else
          CFmtOwner := CFmtCPPSelf;
        CFmtPropertyAccess := CFmtCPPPropertyAccess;
        CFmtIndent := CFmtCPPIndent
      end;
  end;

  CompConv := TComponentCreate.Create;
  try
    for i := Low(AComponents) to High(AComponents) do
      CompConv.Dump(AComponents[i], DumpOptions);

    Result := CompConv.Dumped.Text;
  finally
    FreeAndNil(compConv);
  end;
end;

{ TfmCompsToCode }

constructor TfmCompsToCode.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCodeText := TGXEnhancedEditor.Create(Self);
  FCodeText.Highlighter := gxpNone;
  FCodeText.Align := alClient;
  FCodeText.Parent := pnlView;
  FCodeText.ReadOnly := True;
  FCodeText.WantTabs := True;
  FCodeText.TabWidth := GxOtaGetTabWidth;
end;

procedure TfmCompsToCode.btnHelpClick(Sender: TObject);
begin
  GxContextHelp(Self, 33);
end;

procedure TfmCompsToCode.InitSettings(const ASettings: TCToCSettings);
begin
  FSettings := ASettings;

  rgpBinProps.ItemIndex := Ord(ASettings.BinProps);
  chkPrepend.Checked := ASettings.Prepend;
  chkUseDelphiWith.Checked := ASettings.UseDelphiWith;
  chkCreateFreeCode.Checked := ASettings.CreateFreeCode;
  rgpLanguage.ItemIndex := Ord(ASettings.Language);

  ShowSampleCode;
end;

procedure TfmCompsToCode.SettingsChanged(Sender: TObject);
begin
  FSettings.BinProps := TBinProps(rgpBinProps.ItemIndex);
  Assert(FSettings.BinProps in [Low(TBinProps)..High(TBinProps)]);
  FSettings.Prepend := chkPrepend.Checked;
  FSettings.UseDelphiWith := chkUseDelphiWith.Checked;
  FSettings.CreateFreeCode := chkCreateFreeCode.Checked;
  FSettings.Language := TCToCLanguage(rgpLanguage.ItemIndex);
  Assert(FSettings.Language in [Low(TCToCLanguage)..High(TCToCLanguage)]);

  ShowSampleCode;
end;

procedure TfmCompsToCode.ShowSampleCode;
begin
  case FSettings.Language of
    lPascal:
      FCodeText.Highlighter := gxpPAS;
    lCpp:
      FCodeText.Highlighter := gxpCPP;
    else
      FCodeText.Highlighter := gxpNone;
  end;
  FCodeText.Text := DoGetCreationCode(FSettings, [Edit1, SpeedButton1]);
end;

{ TCompsToCodeExpert }

constructor TCompsToCodeExpert.Create;
begin
  inherited;
  if RunningCPPBuilder then
    FSettings.Language := lCpp
  else
    FSettings.Language := lPascal; // This is only a default and is configurable in the dialog
end;

function TCompsToCodeExpert.GetActionCaption: string;
resourcestring
  SMenuCaption = '&Components to Code';
begin
  Result := SMenuCaption;
end;

class function TCompsToCodeExpert.GetName: string;
begin
  Result := 'ComponentsToCode';
end;

procedure TCompsToCodeExpert.Execute(Sender: TObject);
var
  Comps: TComponentArray;
begin
  // No way to get a "native" component for VCL.NET components
  if GxOtaActiveDesignerIsNFM then
    raise Exception.Create('Components to Code does not support VCL.NET forms.');

  Comps := GetDesignerComps;
  if Length(Comps) > 0 then begin
    Clipboard.AsText := DoGetCreationCode(FSettings, Comps);
    ShowGxMessageBox(TShowCodeOnClipboardMessage);

    IncCallCount;
  end;
end;

procedure TCompsToCodeExpert.InternalLoadSettings(Settings: TExpertSettings);
begin
  inherited InternalLoadSettings(Settings);
  // Do not localize any of the below items.
  FSettings.BinProps := TBinProps(Settings.ReadEnumerated('BinaryProperties', TypeInfo(TBinProps), Ord(bpComment)));
  FSettings.Prepend := Settings.ReadBool('PrependWithComponent', False);
  FSettings.UseDelphiWith := Settings.ReadBool('UseDelphiWith', False);
  FSettings.CreateFreeCode := Settings.ReadBool('CreateFreeCode', False);
  FSettings.Language := TCToCLanguage(Settings.ReadEnumerated('Language', TypeInfo(TCToCLanguage), Ord(FSettings.Language)));
end;

procedure TCompsToCodeExpert.InternalSaveSettings(Settings: TExpertSettings);
begin
  inherited InternalSaveSettings(Settings);
  // Do not localize any of the below items.
  Settings.WriteEnumerated('BinaryProperties', TypeInfo(TBinProps), Ord(FSettings.BinProps));
  Settings.WriteBool('PrependWithComponent', FSettings.Prepend);
  Settings.WriteBool('UseDelphiWith', FSettings.UseDelphiWith);
  Settings.WriteBool('CreateFreeCode', FSettings.CreateFreeCode);
  Settings.WriteEnumerated('Language', TypeInfo(TCToCLanguage), Ord(FSettings.Language));
end;

procedure TCompsToCodeExpert.Configure;
var
  Dlg: TfmCompsToCode;
begin
  Dlg := TfmCompsToCode.Create(nil);
  try
    Dlg.InitSettings(FSettings);
    if Dlg.ShowModal = mrOk then
    begin
      FSettings := Dlg.Settings;
      SaveSettings;
    end;
  finally
    FreeAndNil(Dlg);
  end;
end;

{ TODO 4 -cCleanup -oAnyone: This needs to be broken up and simplified }

// Only export component if it is not a child of another
// component also selected for export.
// Primarily used to remove duplicates when user selects
// Edit.Select All, GExperts.Copy Component Creation.
function ShouldDumpComponent(AFormEditor: IOTAFormEditor; AComponent: TComponent;
  SelIndex: Integer): Boolean;
var
  AContainedComponent: IOTAComponent;
  compParent: TControl;
  j: Integer;
begin
  Result := True;
  if (AComponent is TWinControl) or (AComponent is TGraphicControl) then
  begin
    if AComponent is TWinControl then
      compParent := (AComponent as TWinControl).Parent
    else
      compParent := (AComponent as TGraphicControl).Parent;

    for j := 0 to AFormEditor.GetSelCount - 1 do
    begin
      if SelIndex = j then
        Continue;

      AContainedComponent := AFormEditor.GetSelComponent(j);
      Assert(Assigned(AContainedComponent));

      if compParent = GxOtaGetNativeComponent(AContainedComponent) then
      begin
        Result := False;
        Break;
      end;
    end;
  end;
end;

function TCompsToCodeExpert.GetDesignerComps: TComponentArray;
var
  FormEditor: IOTAFormEditor;
  AComponent: IOTAComponent;
  i, Count: Integer;
  Comp: TComponent;
begin
  Result := nil;

  if not GxOtaTryGetCurrentFormEditor(FormEditor) then
    Exit;

  SetLength(Result, FormEditor.GetSelCount);
  if Length(Result) = 0 then
    Exit;

  Count := 0;
  for i := 0 to Length(Result) - 1 do
  begin
    AComponent := FormEditor.GetSelComponent(i);
    Assert(Assigned(AComponent));

    Comp := GxOtaGetNativeComponent(AComponent);
    if not Assigned(Comp) then
      raise Exception.Create('Native component casting not allowed for this component: ' + GxOtaGetComponentName(AComponent));
    if ShouldDumpComponent(FormEditor, Comp, i) then
    begin
      Result[Count] := Comp;
      Inc(Count);
    end;
  end;
  SetLength(Result, Count);
end;

procedure TCompsToCodeExpert.UpdateAction(Action: TCustomAction);
begin
  Action.Enabled := GxOtaCurrentlyEditingForm;
end;

function TCompsToCodeExpert.HasDesignerMenuItem: Boolean;
begin
  Result := True;
end;

{ TComponentCreate }

constructor TComponentCreate.Create;
begin
  inherited Create;

  ccDumped := TStringList.Create;
  ccObj := TStringList.Create;
  ccDecl := TStringList.Create;
  ccCrea := TStringList.Create;
  ccDele := TStringList.Create;
  ccImpl := TStringList.Create;
  FHaveFormSelected := False;
end;

destructor TComponentCreate.Destroy;
begin
  FreeAndNil(ccDumped);
  FreeAndNil(ccObj);
  FreeAndNil(ccDecl);
  FreeAndNil(ccDele);
  FreeAndNil(ccCrea);
  FreeAndNil(ccImpl);

  inherited Destroy;
end;

procedure TComponentCreate.Dump(Comp: TComponent; Options: TCCOptionSet);
begin
  ccOptions := Options;
  DumpComponent(Comp, ccObj, ccDecl, ccCrea, ccDele, ccImpl);
  ccIsDirty := True;
end; { TComponentCreate.Dump }

function FindMenuItem(Menu: TMenu; const cName: string): TMenuItem;
var
  i: integer;
begin
  for i := 0 to Menu.Items.Count-1 do begin
    Result := Menu.Items[i];
    if SameText(Result.Name, cName) then
      Exit;
  end;
  Result := nil;
end;

procedure TComponentCreate.DumpComponent(Comp: TComponent;
  const Obj, Decl, Crea, Dele, Impl: TStrings);

  procedure InternalDumpComponent(Comp: TComponent; const Obj, Decl, Crea, Dele, Impl: TStrings);
  var
    i: Integer;
    Sub: TStringList;
  begin
    Sub := TStringList.Create;
    try
      StreamAndParse(Comp, Obj, Decl, Crea, Dele, Impl, Sub);
      if Comp is TWinControl then
        with Comp as TWinControl do
        begin
          for i := 0 to ControlCount - 1 do
            if Sub.IndexOf(Controls[i].Name) < 0 then
              InternalDumpComponent(Controls[i], Obj, Decl, Crea, Dele, Impl);
        end;
    finally
      FreeAndNil(Sub);
    end;
  end;

begin
  if Assigned(Comp.Owner) then
    ccForm := Comp.Owner.Name
  else begin
    FHaveFormSelected := True;
    ccForm := Comp.Name;
  end;

  InternalDumpComponent(Comp, Obj, Decl, Crea, Dele, Impl);
end;

function TComponentCreate.EOF: Boolean;
begin
  Result := (ccInpPos >= ccCompDef.Count);
end;

function TComponentCreate.GetDumped: TStrings;
begin
  if ccIsDirty then
  begin
    ccDumped.Assign(ccObj);

    if CFmtVar <> '' then
      ccDumped.Add(CFmtVar);
    AppendStrings(ccDumped, ccDecl);
    ccDumped.Add('');
    AppendStrings(ccDumped, ccCrea);

    ccDumped.Add('');
    AppendStrings(ccDumped, ccImpl);

    if ccUseFree in ccOptions then
    begin
      ccDumped.Add('');
      AppendStrings(ccDumped, ccDele);
    end;

    ccIsDirty := False;
  end;
  Result := ccDumped;
end;

// This is one huge method - 180 lines!
procedure TComponentCreate.ParseComponent(Comp: TComponent;
  const Decl, Crea, Dele, Impl, Sub: TStrings);
var
  p: Integer;
  compSub: TStringList;
  compName: string;
  compClass: string;
  cName: string;
  propVal: string;
  propName: string;
  childComp: TComponent;
  Parent: string;
  tmps: string;

  function PropType(propName: string): string;

    // Why the hell are these named the same?
    function PropType(TypeInfo: PTypeInfo; propName: string): string;
    var
      count: Integer;
      size: Integer;
      i: Integer;
      p: Integer;
      PropList: PPropList;
      propName2: string;
    begin
      // Get string properties
      Count := GetPropList(TypeInfo, [tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString, tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString, tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray{$IFDEF GX_VER200_up}, tkUString{$ENDIF}], nil);
      Size := Count * SizeOf(Pointer);
      GetMem(PropList, Size);
      GetPropList(TypeInfo, [tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkString, tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString, tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray{$IFDEF GX_VER200_up}, tkUString{$ENDIF}], PropList);

      p := Pos('.', propName);
      if p <> 0 then
      begin
        propName2 := propName;
        Delete(propName2, 1, p);
        propName := Copy(propName, 1, p - 1);
      end
      else
        propName2 := '';

      Result := '';

      for I := 0 to Count - 1 do
      begin
        if string(PropList^[I]^.Name) = propName then
        begin
          if propName2 = '' then
            Result := string(PropList^[I]^.PropType^.Name)
          else
            Result := PropType(PropList^[I]^.PropType^, propName2);
          Break;
        end;
      end;
      FreeMem(PropList, Size);
    end;

  begin
    Result := PropType(PTypeInfo(Comp.ClassInfo), propName);
  end;

  procedure Log(List: TStrings; const Fmt: string; const Values: array of const);
  begin
    List.Add(Format(Fmt, Values));
  end;

  // Performs the same function as Log, but inserts the given strings to the top
  // of the specified List. Used when building "Component.Free" code, which must be
  // the reverse order of the "Component.Create()" code that is generated..
  procedure LogInsert(List: TStrings; const Fmt: string; const Values: array of const);
  begin
    List.Insert(0, Format(Fmt, Values));
  end;

  function WithoutS(pType: string): string;
  begin
    Result := '';
    if pType = '' then
      Exit;
    if pType[Length(pType)] = 's' then
      Result := Copy(pType, 1, Length(pType) - 1)
    else
      Result := pType;
  end;

  function PropC(propVal: string; propName: string): string;
  begin
    if propVal = 'False' then
      Result := 'false'
    else
      if propVal = 'True' then
        Result := 'true'
      else
        if (Length(propVal) >= 2) or StartsStr(propVal, '#39''') then
          if StartsStr(propVal, '#39''') or (propVal[1] = Chr(39)) and (propVal[Length(propVal)] = Chr(39)) then
          begin
            propVal := StringReplace(propVal, '#39''', '''', [rfReplaceAll]);
            propVal := Copy(propVal, 2, Length(propVal) - 2);
            propVal := StringReplace(propVal, '''''', '''', [rfReplaceAll]);
            propVal := StringReplace(propVal, '"', '\"', [rfReplaceAll]);
            propVal := StringReplace(propVal, '\\', '\\\\', [rfReplaceAll]);
            Result := '"' + propVal + '"';
          end
          else if (propVal[1] = '[') and (propVal[Length(propVal)] = ']') then
          begin
            if propVal = '[]' then
              propVal := PropType(propName) + '()'
            else
            begin
              propVal := StringReplace(propVal, '[', PropType(propName) + '() << ', []);
              propVal := StringReplace(propVal, ', ', ' << ', [rfReplaceAll]);
              propVal := StringReplace(propVal, ',', ' << ', [rfReplaceAll]);
              propVal := StringReplace(propVal, ']', '', []);
            end;
            Result := propVal;
          end
          else
            Result := propVal
        else
          Result := propVal;
  end;

  procedure ProcGlyph(propName: string; compName: string);
  var
    Indent: string;
  begin
    if ([ccBinaryComment, ccBinaryUncomment] * ccOptions) <> [] then
    begin
      p := LastDelimiter('.', propName);
      if p > 0 then
        propName := Copy(propName, 1, p - 1);
      if CFmtWith = '' then
        Indent := CFmtIndent
      else
        Indent := CFmtIndent + CFmtIndent; //FI:W510

      if ccBinaryComment in ccOptions then
        Log(impl, Indent + CFmtBinaryCmt, [propName])
      else
        Log(impl, Indent + CFmtBinaryUncmt, [propName]);
    end;
    while not EOF do
    begin
      Readln;
      if ccLn[Length(ccLn)] = '}' then
        Break;
    end; //while
  end;

  procedure ProcessMultilineString(propName: string; compName: string);
  var
    LastLineInString: Boolean;
    LineSuffix: string;
  begin
    if CFmtWith = '' then
      Log(Impl, '%s%s%s%s := ', [CFmtIndent, compName, CFmtPropertyAccess, PropName])
    else
      Log(Impl, CFmtIndent + CFmtIndent + '%s := ', [PropName]);
    while not Eof do
    begin
      Readln;
      LastLineInString := not StrEndsWith('+', ccLn);
      if LastLineInString then
        LineSuffix := ';'
      else
        LineSuffix := '';
      if CFmtWith = '' then
        Log(Impl, CFmtIndent + CFmtIndent + '%s%s', [ccLn, LineSuffix])
      else
        Log(Impl, CFmtIndent + CFmtIndent + CFmtIndent + '%s%s', [ccLn, LineSuffix]);
      if LastLineInString then
        Exit;
    end;
  end;

  procedure ProcessStringList(propName: string; compName: string);
  var
    p: Integer;
    last: Boolean;
    first: Boolean;
  begin
    first := True;
    p := LastDelimiter('.', propName);
    if p > 0 then
    begin
      propName := Copy(propName, 1, p - 1);
      last := False;
      while not (EOF or last) do
      begin
        Readln;
        if (Length(ccLn) > 0) and (ccLn[Length(ccLn)] = ')') then
        begin
          ccLn := Copy(ccLn, 1, Length(ccLn) - 1);
          last := True;
        end;
        // D5 (at least) sometimes inserts completely blank lines where the
        // next line has a ' +' in it at the end, which should be ignored.  One
        // example is TUpdateSQL with IDE generated SQL from DBDEMOS.biolife.db
        if (Length(ccLn) > 2) and (Copy(ccLn, Length(ccLn) - 1, 2) = ' +') then
          Delete(ccLn, Length(ccLn) - 1, 2);
        if (Length(ccLn) > 0) then
        begin
          if First then
          begin
            First := False;
            if compName = '' then
              Log(impl, CFmtClear, [propName])
            else
              Log(impl, CFmtIndent + compName + CFmtPropertyAccess + TrimLeft(CFmtClear), [propName])
          end;
        end;
        if compName = '' then
          Log(impl, CFmtAdd, [propName, ccLn])
        else
          if CFmtPropertyAccess = '.' then
            Log(impl, CFmtIndent + compName + CFmtPropertyAccess + TrimLeft(CFmtAdd), [propName, ccLn])
          else
            Log(impl, CFmtIndent + compName + CFmtPropertyAccess + TrimLeft(CFmtAdd), [propName, propC(ccLn, propName)]);
      end; //while
    end;
  end;

  procedure ProcessCollectionItems(const propName: string; compName: string);
  var
    pVal: string;
    pName: string;
    last: Boolean;
    p: Integer;
  begin
    last := False;
    while not (EOF or last) do
    begin
      Readln;
      if ccULn = 'ITEM' then
      begin
        if compName = '' then
          Log(impl, '  ' + CFmtWithAdd, [propName])
        else
          if CFmtWithAdd <> '' then
            Log(impl, CFmtWithAdd, [compName + CFmtPropertyAccess + propName])
          else
          begin
            Log(impl, CFmtIndent + '{', [nil]);
            Log(impl, CFmtPointerDeclaration, [WithoutS(PropType(propName)), WithoutS(propName), compName, propName]);
          end;

        while not (EOF or last) do
        begin
          Readln;
          if (ccULn = 'END') or (ccULn = 'END>') then
          begin
            if CFmtWithAdd <> '' then
              if compName = '' then
                Log(impl, '  ' + CFmtEnd, [nil])
              else
                Log(impl, CFmtEnd, [nil])
            else
              Log(impl, CFmtIndent + '}', [nil]);

            if ccLn[Length(ccLn)] = '>' then
              last := True;
            Break;
          end
          else
          begin
            p := Pos('=', ccLn);
            if p > 0 then
            begin
              pName := TrimRight(Copy(ccLn, 1, p - 1));
              pVal := TrimLeft(Copy(ccLn, p + 1, Length(ccLn) - p));
              if (pName <> 'WidthType') or (WithoutS(PropType(propName)) <> 'TListColumn') then
                if CFmtWithAdd <> '' then
                  if compName = '' then
                    Log(impl, '  ' + CFmtAssign, [pName, pVal])
                  else
                    Log(impl, CFmtAssign, [pName, pVal])
                else
                  Log(impl, CFmtIndent + CFmtIndent + WithoutS(propName) + CFmtPropertyAccess + CFmtAssign, [pName, propC(pVal, propName)]);
            end;
          end;
        end; //while
      end;
    end; //while
  end;

begin
  compSub := TStringList.Create;
  try
    ccLn := Copy(ccLn, Length('object') + 2, Length(ccLn) - Length('object') - 1);
    p := Pos(':', ccLn);
    if p > 0 then
    begin
      compName := TrimRight(Copy(ccLn, 1, p - 1));
      compClass := TrimLeft(Copy(ccLn, p + 1, Length(ccLn) - p));
      Log(decl, CFmtDeclaration, [compName, compClass]);
      if FHaveFormSelected and (not SameText(Comp.Name, ccForm)) then
      begin
        LogInsert(Dele, CFmtDeletion, [compName]);
        Log(crea, CFmtCreation, [compName, compClass, ccForm])
      end
      else
      begin
        LogInsert(Dele, CFmtDeletion, [compName]);
        Log(crea, CFmtCreation, [compName, compClass, CFmtOwner]);
      end;

      if CFmtWith <> '' then
        Log(impl, CFmtWith, [compName]);

      Parent := '';
      if Comp is TControl then
      begin
        if ((Comp as TControl).Parent is TCustomForm) and (not FHaveFormSelected) then
          Parent := CFmtSelf
        else if Assigned((Comp as TControl).Parent) then
          Parent := (Comp as TControl).Parent.Name;
      end;

      if CFmtWith = '' then
      begin
        Log(impl, CFmtIndent + compName + CFmtPropertyAccess + TrimLeft(CFmtName), [compName]);
        if (Comp is TControl) and (Parent <> '') then
          Log(impl, CFmtIndent + compName + CFmtPropertyAccess + TrimLeft(CFmtParent), [Parent]);
        if Comp is TTabSheet then
          Log(impl, CFmtIndent + compName + CFmtPropertyAccess + TrimLeft(CFmtPageControl), [Parent]);
      end
      else
      begin
        Log(impl, CFmtName, [compName]);
        compName := '';
        if (Comp is TControl) and (Parent <> '') then
          Log(impl, CFmtParent, [Parent]);
        if Comp is TTabSheet then
          Log(impl, CFmtPageControl, [Parent]);
      end;

      while not EOF do
      begin
        Readln;
        if ccULn = 'END' then
          Break
        else if Copy(ccULn, 1, Length('object')) = 'OBJECT' then
        begin
          tmps := Copy(ccLn, Length('object') + 2, Length(ccLn) - Length('object') - 1);
          p := Pos(':', tmps);
          if p > 0 then
          begin
            cName := TrimRight(Copy(tmps, 1, p - 1));
            sub.Add(cName);
            childComp := Comp;
            if Comp is TWinControl then
            begin
              childComp := FindChild(Comp as TWinControl, cName);
              if not Assigned(childComp) then
                childComp := Comp;
            end
            else if Comp is TMenu then
            begin
              childComp := FindMenuItem(TMenu(Comp), cName);
              if not Assigned(childComp) then
                childComp := Comp;
            end;
            ParseComponent(childComp, decl, crea, Dele, compSub, sub);
          end;
        end
        else
        begin
          p := Pos('=', ccLn);
          if p > 0 then
          begin
            propName := TrimRight(Copy(ccLn, 1, p - 1));
            propVal := TrimLeft(Copy(ccLn, p + 1, Length(ccLn) - p));
            p := Pos('.', propVal);
            if p > 0 then
            begin
              if SameText(Copy(propVal, 1, p - 1), ccForm) then
                propVal := Copy(propVal, p + 1, Length(propVal) - p);
            end;
            if propVal = '{' then // glyphs etc - skip
              ProcGlyph(propName, compName)
            else if propVal = '(' then // string lists
              ProcessStringList(propName, compName)
            else if propVal = '<' then // ListView columns etc
              ProcessCollectionItems(propName, compName)
            else if propVal = '' then // Multi-line strings where the value starts on the next line (TADOConnection.ConnectionString, a long TLabel.Caption, etc.)
              ProcessMultilineString(propName, compName)
            else if (propName = 'Top') or (propName = 'Left') then
            begin
              if propType(propName) <> '' then
                if CFmtWith <> '' then
                  Log(impl, CFmtAssign, [propName, propVal])
                else
                  if CFmtPropertyAccess = '.' then
                    Log(impl, CFmtIndent + compName + CFmtPropertyAccess + TrimLeft(CFmtAssign), [propName, propVal])
                  else
                    Log(impl, CFmtIndent + compName + CFmtPropertyAccess + TrimLeft(CFmtAssign), [StringReplace(propName, '.', CFmtPropertyAccess, [rfReplaceAll]), PropC(propVal, propName)]);
            end
            else if (Comp is TCustomForm) and (propName = 'TextHeight') then
            begin
              // Ignore this, it is a "virtual" property
            end
            else
            begin
              if IsPropWriteable(Comp, propName) then
              begin
                if CFmtWith <> '' then
                  Log(impl, CFmtAssign, [propName, propVal])
                else
                  if CFmtPropertyAccess = '.' then
                    Log(impl, CFmtIndent + compName + CFmtPropertyAccess + TrimLeft(CFmtAssign), [propName, propVal])
                  else
                    Log(impl, CFmtIndent + compName + CFmtPropertyAccess + TrimLeft(CFmtAssign), [StringReplace(propName, '.', CFmtPropertyAccess, [rfReplaceAll]), PropC(propVal, propName)]);
              end;
            end;
          end; //if p > 0
        end; //else if not skip
      end; //while not EOF
      if CFmtWith <> '' then
        Log(impl, CFmtEnd, [nil]);
    end;
    AppendStrings(impl, compSub);
  finally
    FreeAndNil(compSub);
  end;
end;

procedure TComponentCreate.Readln;
begin
  ccLn := Trim(ccCompDef[ccInpPos]);
  ccULn := AnsiUpperCase(ccLn);
  Inc(ccInpPos);
end;

procedure TComponentCreate.StreamAndParse(Comp: TComponent;
  const obj, decl, crea, Dele, impl, sub: TStrings);
var
  i: Integer;
  TempWriter: TWriter;
  iStream: TMemoryStream;
  oStream: TMemoryStream;
begin
  ccCompDef := nil;
  oStream := nil;
  iStream := TMemoryStream.Create;
  try
    // Suggested by John Hansen:
    TempWriter := TWriter.Create(iStream, 4096);
    try
      if Comp.Owner = nil then
        TempWriter.Root := Comp
      else
        TempWriter.Root := Comp.Owner;
      TempWriter.WriteSignature;
      TempWriter.WriteComponent(Comp);
      TempWriter.WriteListEnd;
      TempWriter.WriteListEnd;
    finally
      FreeAndNil(TempWriter);
    end;
    iStream.Position := 0;
    oStream := TMemoryStream.Create;
    ObjectBinaryToText(iStream, oStream);
    ccCompDef := TStringList.Create;
    oStream.Position := 0;
    ccCompDef.LoadFromStream(oStream);
    if ccIncludeObjectText in ccOptions then
    begin
      for i := 0 to ccCompDef.Count - 1 do
        obj.Add('//' + ccCompDef[i]);
      obj.Add('');
    end;
    ccInpPos := 0;
    while not EOF do
    begin
      Readln;
      if StartsText('object', ccULn) then
        ParseComponent(Comp, decl, crea, Dele, impl, sub);
    end;
  finally
    FreeAndNil(ccCompDef);
    FreeAndNil(oStream);
    FreeAndNil(iStream);
  end;
end;

{ TShowCodeOnClipboardMessage }

function TShowCodeOnClipboardMessage.GetMessage: string;
resourcestring
  SCopyToClipboardComplete =
    'The code to create the selected components has been copied to the clipboard.' + sLineBreak +
    sLineBreak +
    'You can now paste the generated code into the IDE ' +
    'editor at the appropriate position.';
begin
  Result := SCopyToClipboardComplete;
end;

initialization
  RegisterGX_Expert(TCompsToCodeExpert);

end.

