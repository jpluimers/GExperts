unit GX_eWarn;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Controls,
  Forms,
  Graphics,
  StdCtrls,
  GX_EditorExpert,
  GX_ConfigurationInfo,
  GX_dzCompilerAndRtlVersions,
  GX_BaseForm,
  GX_BaseExpert;

type
  TWarnExpert = class(TEditorExpert)
  private
  public
    class function GetName: string; override;
    constructor Create; override;
    function GetDisplayName: string; override;
    procedure Execute(Sender: TObject); override;
    function GetHelpString: string; override;
    function HasConfigOptions: Boolean; override;
  end;

type
  TWarnStatusEnum = (wseON, wseOFF, wseDEFAULT
{$IF CompilerVersion >= CompilerVersionDelphi2009}
    , wseERROR
{$IFEND}
    );

type
  TfmConfigureWarning = class(TfmBaseForm)
    lb_Warn: TListBox;
    b_ON: TButton;
    b_OFF: TButton;
    b_Default: TButton;
    b_Cancel: TButton;
    ed_Filter: TEdit;
    l_Filter: TLabel;
    b_ERROR: TButton;
    procedure b_ONClick(Sender: TObject);
    procedure b_OFFClick(Sender: TObject);
    procedure b_DefaultClick(Sender: TObject);
    procedure b_ERRORClick(Sender: TObject);
    procedure ed_FilterKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ed_FilterChange(Sender: TObject);
  private
    FAvailable: TStringList;
    FStatus: TWarnStatusEnum;
    function GetMessage: string;
    function GetStatus: TWarnStatusEnum;
    procedure SelectBestItem;
    procedure InitWarnings;
  public
    class function Execute(_bmp: TBitmap;
      out _Message: string; out _Status: TWarnStatusEnum): Boolean;
    constructor Create(_Owner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

uses
  Messages,
  GX_OtaUtils,
  GX_dzVclUtils,
  GX_GenericUtils;

{ TWarnExpert }

constructor TWarnExpert.Create;
begin
  inherited Create;
end;

function WarnStatusToStr(_Status: TWarnStatusEnum): string;
begin
  case _Status of
{$IF CompilerVersion >= CompilerVersionDelphi2009}
    wseERROR: Result := 'ERROR';
{$IFEND}
    wseOFF: Result := 'OFF';
    wseDEFAULT: Result := 'DEFAULT';
  else // wseON:
    Result := 'ON';
  end;
end;

procedure TWarnExpert.Execute(Sender: TObject);
var
  InsertString: string;
  Msg: string;
  Status: TWarnStatusEnum;
begin
  if not TfmConfigureWarning.Execute(GetBitmap, Msg, Status) then
    Exit; //==>
  InsertString := Format('{$WARN %s %s}', [Msg, WarnStatusToStr(Status)]);
  GxOtaInsertLineIntoEditor(InsertString);
  IncCallCount;
end;

function TWarnExpert.GetDisplayName: string;
resourcestring
  SWarnExpertName = 'WARN Directive';
begin
  Result := SWarnExpertName;
end;

function TWarnExpert.GetHelpString: string;
resourcestring
  SWarnExpertHelp =
    '  This expert inserts a {$WARN xxxx ON/OFF} directive into the source code';
begin
  Result := SWarnExpertHelp;
end;

class function TWarnExpert.GetName: string;
begin
  Result := 'Warn';
end;

function TWarnExpert.HasConfigOptions: Boolean;
begin
  Result := False;
end;

{ TfmConfigureWarning }

class function TfmConfigureWarning.Execute(_bmp: TBitmap;
  out _Message: string; out _Status: TWarnStatusEnum): Boolean;
var
  frm: TfmConfigureWarning;
begin
  frm := TfmConfigureWarning.Create(Application);
  try
    ConvertBitmapToIcon(_bmp, frm.Icon);
    Result := frm.ShowModal = mrOk;
    if Result then begin
      _Message := frm.GetMessage;
      _Status := frm.GetStatus;
    end;
  finally
    FreeAndNil(frm);
  end;
end;

{$IF CompilerVersion >= CompilerVersionDelphiX103}

procedure TfmConfigureWarning.InitWarnings;
begin
  FAvailable.Add('ASG_TO_TYPED_CONST');
  FAvailable.Add('BAD_GLOBAL_SYMBOL');
  FAvailable.Add('BOUNDS_ERROR');
  FAvailable.Add('CASE_LABEL_RANGE');
  FAvailable.Add('COMBINING_SIGNED_UNSIGNED');
  FAvailable.Add('COMPARING_SIGNED_UNSIGNED');
  FAvailable.Add('COMPARISON_FALSE');
  FAvailable.Add('COMPARISON_TRUE');
  FAvailable.Add('CONSTRUCTING_ABSTRACT');
  FAvailable.Add('CVT_ACHAR_TO_WCHAR');
  FAvailable.Add('CVT_NARROWING_STRING_LOST');
  FAvailable.Add('CVT_WCHAR_TO_ACHAR');
  FAvailable.Add('CVT_WIDENING_STRING_LOST');
  FAvailable.Add('DUPLICATE_CTOR_DTOR');
  FAvailable.Add('DUPLICATES_IGNORED');
  FAvailable.Add('EXPLICIT_STRING_CAST');
  FAvailable.Add('EXPLICIT_STRING_CAST_LOSS');
  FAvailable.Add('FILE_OPEN');
  FAvailable.Add('FILE_OPEN_UNITSRC');
  FAvailable.Add('FOR_LOOP_VAR_UNDEF');
  FAvailable.Add('FOR_LOOP_VAR_VARPAR');
  FAvailable.Add('FOR_VARIABLE');
  FAvailable.Add('GARBAGE');
  FAvailable.Add('HIDDEN_VIRTUAL');
  FAvailable.Add('HIDING_MEMBER');
  FAvailable.Add('HPPEMIT_IGNORED');
  FAvailable.Add('HRESULT_COMPAT');
  FAvailable.Add('IMAGEBASE_MULTIPLE');
  FAvailable.Add('IMMUTABLE_STRINGS');
  FAvailable.Add('IMPLICIT_IMPORT');
  FAvailable.Add('IMPLICIT_STRING_CAST');
  FAvailable.Add('IMPLICIT_STRING_CAST_LOSS');
  FAvailable.Add('IMPLICIT_VARIANTS');
  FAvailable.Add('INVALID_DIRECTIVE');
  FAvailable.Add('LNKDFM_NOTFOUND');
  FAvailable.Add('LOCAL_PINVOKE');
  FAvailable.Add('LOCALE_TO_UNICODE');
  FAvailable.Add('LOST_EXTENDED_PRECISION');
  FAvailable.Add('MESSAGE_DIRECTIVE');
  FAvailable.Add('MOBILE_DELPHI');
  FAvailable.Add('NO_CFG_FILE_FOUND');
  FAvailable.Add('NO_RETVAL');
  FAvailable.Add('NON_PORTABLE_TYPECAST');
  FAvailable.Add('OPTION_TRUNCATED');
  FAvailable.Add('PACKAGE_NO_LINK');
  FAvailable.Add('PACKAGED_THREADVAR');
  FAvailable.Add('PRIVATE_PROPACCESSOR');
  FAvailable.Add('RLINK_WARNING');
  FAvailable.Add('STRING_CONST_TRUNCED');
  FAvailable.Add('SUSPICIOUS_TYPECAST');
  FAvailable.Add('SYMBOL_DEPRECATED');
  FAvailable.Add('SYMBOL_EXPERIMENTAL');
  FAvailable.Add('SYMBOL_LIBRARY');
  FAvailable.Add('SYMBOL_PLATFORM');
  FAvailable.Add('TYPED_CONST_VARPAR');
  FAvailable.Add('TYPEINFO_IMPLICITLY_ADDED');
  FAvailable.Add('UNICODE_TO_LOCALE');
  FAvailable.Add('UNIT_DEPRECATED');
  FAvailable.Add('UNIT_EXPERIMENTAL');
  FAvailable.Add('UNIT_INIT_SEQ');
  FAvailable.Add('UNIT_LIBRARY');
  FAvailable.Add('UNIT_NAME_MISMATCH');
  FAvailable.Add('UNIT_PLATFORM');
  // new in Delphi 10.3 Rio
  FAvailable.Add('UNKNOWN_CUSTOM_ATTRIBUTE');
  FAvailable.Add('UNSAFE_CAST');
  FAvailable.Add('UNSAFE_CODE');
  FAvailable.Add('UNSAFE_TYPE');
  FAvailable.Add('UNSAFE_VOID_POINTER');
  FAvailable.Add('UNSUPPORTED_CONSTRUCT');
  FAvailable.Add('USE_BEFORE_DEF');
  FAvailable.Add('WIDECHAR_REDUCED');
  FAvailable.Add('XML_CREF_NO_RESOLVE');
  FAvailable.Add('XML_EXPECTED_CHARACTER');
  FAvailable.Add('XML_INVALID_NAME');
  FAvailable.Add('XML_INVALID_NAME_START');
  FAvailable.Add('XML_NO_MATCHING_PARM');
  FAvailable.Add('XML_NO_PARM');
  FAvailable.Add('XML_UNKNOWN_ENTITY');
  FAvailable.Add('XML_WHITESPACE_NOT_ALLOWED');
  FAvailable.Add('ZERO_NIL_COMPAT');
end;

{$ELSE}
{$IF CompilerVersion >= CompilerVersionDelphiXE3}

procedure TfmConfigureWarning.InitWarnings;
begin
  FAvailable.Add('SYMBOL_DEPRECATED');
  FAvailable.Add('SYMBOL_LIBRARY');
  FAvailable.Add('SYMBOL_PLATFORM');
  FAvailable.Add('SYMBOL_EXPERIMENTAL');
  FAvailable.Add('UNIT_LIBRARY');
  FAvailable.Add('UNIT_PLATFORM');
  FAvailable.Add('UNIT_DEPRECATED');
  FAvailable.Add('UNIT_EXPERIMENTAL');
  FAvailable.Add('HRESULT_COMPAT');
  FAvailable.Add('HIDING_MEMBER');
  FAvailable.Add('HIDDEN_VIRTUAL');
  FAvailable.Add('GARBAGE');
  FAvailable.Add('BOUNDS_ERROR');
  FAvailable.Add('ZERO_NIL_COMPAT');
  FAvailable.Add('STRING_CONST_TRUNCED');
  FAvailable.Add('FOR_LOOP_VAR_VARPAR');
  FAvailable.Add('TYPED_CONST_VARPAR');
  FAvailable.Add('ASG_TO_TYPED_CONST');
  FAvailable.Add('CASE_LABEL_RANGE');
  FAvailable.Add('FOR_VARIABLE');
  FAvailable.Add('CONSTRUCTING_ABSTRACT');
  FAvailable.Add('COMPARISON_FALSE');
  FAvailable.Add('COMPARISON_TRUE');
  FAvailable.Add('COMPARING_SIGNED_UNSIGNED');
  FAvailable.Add('COMBINING_SIGNED_UNSIGNED');
  FAvailable.Add('UNSUPPORTED_CONSTRUCT');
  FAvailable.Add('FILE_OPEN');
  FAvailable.Add('FILE_OPEN_UNITSRC');
  FAvailable.Add('BAD_GLOBAL_SYMBOL');
  FAvailable.Add('DUPLICATE_CTOR_DTOR');
  FAvailable.Add('INVALID_DIRECTIVE');
  FAvailable.Add('PACKAGE_NO_LINK');
  FAvailable.Add('PACKAGED_THREADVAR');
  FAvailable.Add('IMPLICIT_IMPORT');
  FAvailable.Add('HPPEMIT_IGNORED');
  FAvailable.Add('NO_RETVAL');
  FAvailable.Add('USE_BEFORE_DEF');
  FAvailable.Add('FOR_LOOP_VAR_UNDEF');
  FAvailable.Add('UNIT_NAME_MISMATCH');
  FAvailable.Add('NO_CFG_FILE_FOUND');
  FAvailable.Add('IMPLICIT_VARIANTS');
  FAvailable.Add('UNICODE_TO_LOCALE');
  FAvailable.Add('LOCALE_TO_UNICODE');
  FAvailable.Add('IMAGEBASE_MULTIPLE');
  FAvailable.Add('SUSPICIOUS_TYPECAST');
  FAvailable.Add('PRIVATE_PROPACCESSOR');
  FAvailable.Add('UNSAFE_TYPE');
  FAvailable.Add('UNSAFE_CODE');
  FAvailable.Add('UNSAFE_CAST');
  FAvailable.Add('OPTION_TRUNCATED');
  FAvailable.Add('WIDECHAR_REDUCED');
  FAvailable.Add('DUPLICATES_IGNORED');
  FAvailable.Add('UNIT_INIT_SEQ');
  FAvailable.Add('LOCAL_PINVOKE');
  FAvailable.Add('MESSAGE_DIRECTIVE');
  FAvailable.Add('TYPEINFO_IMPLICITLY_ADDED');
  FAvailable.Add('RLINK_WARNING');
  FAvailable.Add('IMPLICIT_STRING_CAST');
  FAvailable.Add('IMPLICIT_STRING_CAST_LOSS');
  FAvailable.Add('EXPLICIT_STRING_CAST');
  FAvailable.Add('EXPLICIT_STRING_CAST_LOSS');
  FAvailable.Add('CVT_WCHAR_TO_ACHAR');
  FAvailable.Add('CVT_NARROWING_STRING_LOST');
  FAvailable.Add('CVT_ACHAR_TO_WCHAR');
  FAvailable.Add('CVT_WIDENING_STRING_LOST');
  FAvailable.Add('NON_PORTABLE_TYPECAST');
  FAvailable.Add('LOST_EXTENDED_PRECISION');
  FAvailable.Add('LNKDFM_NOTFOUND');
  FAvailable.Add('IMMUTABLE_STRINGS');
  FAvailable.Add('MOBILE_DELPHI');
  FAvailable.Add('UNSAFE_VOID_POINTER');
  FAvailable.Add('XML_WHITESPACE_NOT_ALLOWED');
  FAvailable.Add('XML_UNKNOWN_ENTITY');
  FAvailable.Add('XML_INVALID_NAME_START');
  FAvailable.Add('XML_INVALID_NAME');
  FAvailable.Add('XML_EXPECTED_CHARACTER');
  FAvailable.Add('XML_CREF_NO_RESOLVE');
  FAvailable.Add('XML_NO_PARM');
  FAvailable.Add('XML_NO_MATCHING_PARM');
end;
{$ELSE}
{$IF CompilerVersion >= CompilerVersionDelphiXE2}

procedure TfmConfigureWarning.InitWarnings;
begin
  FAvailable.Add('SYMBOL_DEPRECATED');
  FAvailable.Add('SYMBOL_LIBRARY');
  FAvailable.Add('SYMBOL_PLATFORM');
  FAvailable.Add('SYMBOL_EXPERIMENTAL');
  FAvailable.Add('UNIT_LIBRARY');
  FAvailable.Add('UNIT_PLATFORM');
  FAvailable.Add('UNIT_DEPRECATED');
  FAvailable.Add('UNIT_EXPERIMENTAL');
  FAvailable.Add('HRESULT_COMPAT');
  FAvailable.Add('HIDING_MEMBER');
  FAvailable.Add('HIDDEN_VIRTUAL');
  FAvailable.Add('GARBAGE');
  FAvailable.Add('BOUNDS_ERROR');
  FAvailable.Add('ZERO_NIL_COMPAT');
  FAvailable.Add('STRING_CONST_TRUNCED');
  FAvailable.Add('FOR_LOOP_VAR_VARPAR');
  FAvailable.Add('TYPED_CONST_VARPAR');
  FAvailable.Add('ASG_TO_TYPED_CONST');
  FAvailable.Add('CASE_LABEL_RANGE');
  FAvailable.Add('FOR_VARIABLE');
  FAvailable.Add('CONSTRUCTING_ABSTRACT');
  FAvailable.Add('COMPARISON_FALSE');
  FAvailable.Add('COMPARISON_TRUE');
  FAvailable.Add('COMPARING_SIGNED_UNSIGNED');
  FAvailable.Add('COMBINING_SIGNED_UNSIGNED');
  FAvailable.Add('UNSUPPORTED_CONSTRUCT');
  FAvailable.Add('FILE_OPEN');
  FAvailable.Add('FILE_OPEN_UNITSRC');
  FAvailable.Add('BAD_GLOBAL_SYMBOL');
  FAvailable.Add('DUPLICATE_CTOR_DTOR');
  FAvailable.Add('INVALID_DIRECTIVE');
  FAvailable.Add('PACKAGE_NO_LINK');
  FAvailable.Add('PACKAGED_THREADVAR');
  FAvailable.Add('IMPLICIT_IMPORT');
  FAvailable.Add('HPPEMIT_IGNORED');
  FAvailable.Add('NO_RETVAL');
  FAvailable.Add('USE_BEFORE_DEF');
  FAvailable.Add('FOR_LOOP_VAR_UNDEF');
  FAvailable.Add('UNIT_NAME_MISMATCH');
  FAvailable.Add('NO_CFG_FILE_FOUND');
  FAvailable.Add('IMPLICIT_VARIANTS');
  FAvailable.Add('UNICODE_TO_LOCALE');
  FAvailable.Add('LOCALE_TO_UNICODE');
  FAvailable.Add('IMAGEBASE_MULTIPLE');
  FAvailable.Add('SUSPICIOUS_TYPECAST');
  FAvailable.Add('PRIVATE_PROPACCESSOR');
  FAvailable.Add('UNSAFE_TYPE');
  FAvailable.Add('UNSAFE_CODE');
  FAvailable.Add('UNSAFE_CAST');
  FAvailable.Add('OPTION_TRUNCATED');
  FAvailable.Add('WIDECHAR_REDUCED');
  FAvailable.Add('DUPLICATES_IGNORED');
  FAvailable.Add('UNIT_INIT_SEQ');
  FAvailable.Add('LOCAL_PINVOKE');
  FAvailable.Add('MESSAGE_DIRECTIVE');
  FAvailable.Add('TYPEINFO_IMPLICITLY_ADDED');
  FAvailable.Add('RLINK_WARNING');
  FAvailable.Add('IMPLICIT_STRING_CAST');
  FAvailable.Add('IMPLICIT_STRING_CAST_LOSS');
  FAvailable.Add('EXPLICIT_STRING_CAST');
  FAvailable.Add('EXPLICIT_STRING_CAST_LOSS');
  FAvailable.Add('CVT_WCHAR_TO_ACHAR');
  FAvailable.Add('CVT_NARROWING_STRING_LOST');
  FAvailable.Add('CVT_ACHAR_TO_WCHAR');
  FAvailable.Add('CVT_WIDENING_STRING_LOST');
  FAvailable.Add('NON_PORTABLE_TYPECAST');
  FAvailable.Add('LOST_EXTENDED_PRECISION');
  FAvailable.Add('LNKDFM_NOTFOUND');
  FAvailable.Add('XML_WHITESPACE_NOT_ALLOWED');
  FAvailable.Add('XML_UNKNOWN_ENTITY');
  FAvailable.Add('XML_INVALID_NAME_START');
  FAvailable.Add('XML_INVALID_NAME');
  FAvailable.Add('XML_EXPECTED_CHARACTER');
  FAvailable.Add('XML_CREF_NO_RESOLVE');
  FAvailable.Add('XML_NO_PARM');
  FAvailable.Add('XML_NO_MATCHING_PARM');
end;
{$ELSE}
{$IF CompilerVersion >= CompilerVersionDelphiXE}

procedure TfmConfigureWarning.InitWarnings;
begin
  FAvailable.Add('SYMBOL_DEPRECATED');
  FAvailable.Add('SYMBOL_LIBRARY');
  FAvailable.Add('SYMBOL_PLATFORM');
  FAvailable.Add('SYMBOL_EXPERIMENTAL');
  FAvailable.Add('UNIT_LIBRARY');
  FAvailable.Add('UNIT_PLATFORM');
  FAvailable.Add('UNIT_DEPRECATED');
  FAvailable.Add('UNIT_EXPERIMENTAL');
  FAvailable.Add('HRESULT_COMPAT');
  FAvailable.Add('HIDING_MEMBER');
  FAvailable.Add('HIDDEN_VIRTUAL');
  FAvailable.Add('GARBAGE');
  FAvailable.Add('BOUNDS_ERROR');
  FAvailable.Add('ZERO_NIL_COMPAT');
  FAvailable.Add('STRING_CONST_TRUNCED');
  FAvailable.Add('FOR_LOOP_VAR_VARPAR');
  FAvailable.Add('TYPED_CONST_VARPAR');
  FAvailable.Add('ASG_TO_TYPED_CONST');
  FAvailable.Add('CASE_LABEL_RANGE');
  FAvailable.Add('FOR_VARIABLE');
  FAvailable.Add('CONSTRUCTING_ABSTRACT');
  FAvailable.Add('COMPARISON_FALSE');
  FAvailable.Add('COMPARISON_TRUE');
  FAvailable.Add('COMPARING_SIGNED_UNSIGNED');
  FAvailable.Add('COMBINING_SIGNED_UNSIGNED');
  FAvailable.Add('UNSUPPORTED_CONSTRUCT');
  FAvailable.Add('FILE_OPEN');
  FAvailable.Add('FILE_OPEN_UNITSRC');
  FAvailable.Add('BAD_GLOBAL_SYMBOL');
  FAvailable.Add('DUPLICATE_CTOR_DTOR');
  FAvailable.Add('INVALID_DIRECTIVE');
  FAvailable.Add('PACKAGE_NO_LINK');
  FAvailable.Add('PACKAGED_THREADVAR');
  FAvailable.Add('IMPLICIT_IMPORT');
  FAvailable.Add('HPPEMIT_IGNORED');
  FAvailable.Add('NO_RETVAL');
  FAvailable.Add('USE_BEFORE_DEF');
  FAvailable.Add('FOR_LOOP_VAR_UNDEF');
  FAvailable.Add('UNIT_NAME_MISMATCH');
  FAvailable.Add('NO_CFG_FILE_FOUND');
  FAvailable.Add('IMPLICIT_VARIANTS');
  FAvailable.Add('UNICODE_TO_LOCALE');
  FAvailable.Add('LOCALE_TO_UNICODE');
  FAvailable.Add('IMAGEBASE_MULTIPLE');
  FAvailable.Add('SUSPICIOUS_TYPECAST');
  FAvailable.Add('PRIVATE_PROPACCESSOR');
  FAvailable.Add('UNSAFE_TYPE');
  FAvailable.Add('UNSAFE_CODE');
  FAvailable.Add('UNSAFE_CAST');
  FAvailable.Add('OPTION_TRUNCATED');
  FAvailable.Add('WIDECHAR_REDUCED');
  FAvailable.Add('DUPLICATES_IGNORED');
  FAvailable.Add('UNIT_INIT_SEQ');
  FAvailable.Add('LOCAL_PINVOKE');
  FAvailable.Add('MESSAGE_DIRECTIVE');
  FAvailable.Add('TYPEINFO_IMPLICITLY_ADDED');
  FAvailable.Add('RLINK_WARNING');
  FAvailable.Add('IMPLICIT_STRING_CAST');
  FAvailable.Add('IMPLICIT_STRING_CAST_LOSS');
  FAvailable.Add('EXPLICIT_STRING_CAST');
  FAvailable.Add('EXPLICIT_STRING_CAST_LOSS');
  FAvailable.Add('CVT_WCHAR_TO_ACHAR');
  FAvailable.Add('CVT_NARROWING_STRING_LOST');
  FAvailable.Add('CVT_ACHAR_TO_WCHAR');
  FAvailable.Add('CVT_WIDENING_STRING_LOST');
  FAvailable.Add('XML_WHITESPACE_NOT_ALLOWED');
  FAvailable.Add('XML_UNKNOWN_ENTITY');
  FAvailable.Add('XML_INVALID_NAME_START');
  FAvailable.Add('XML_INVALID_NAME');
  FAvailable.Add('XML_EXPECTED_CHARACTER');
  FAvailable.Add('XML_CREF_NO_RESOLVE');
  FAvailable.Add('XML_NO_PARM');
  FAvailable.Add('XML_NO_MATCHING_PARM');
end;
{$ELSE}
{$IF CompilerVersion >= CompilerVersionDelphi2010}

procedure TfmConfigureWarning.InitWarnings;
begin
  FAvailable.Add('SYMBOL_DEPRECATED');
  FAvailable.Add('SYMBOL_LIBRARY');
  FAvailable.Add('SYMBOL_PLATFORM');
  FAvailable.Add('SYMBOL_EXPERIMENTAL');
  FAvailable.Add('UNIT_LIBRARY');
  FAvailable.Add('UNIT_PLATFORM');
  FAvailable.Add('UNIT_DEPRECATED');
  FAvailable.Add('UNIT_EXPERIMENTAL');
  FAvailable.Add('HRESULT_COMPAT');
  FAvailable.Add('HIDING_MEMBER');
  FAvailable.Add('HIDDEN_VIRTUAL');
  FAvailable.Add('GARBAGE');
  FAvailable.Add('BOUNDS_ERROR');
  FAvailable.Add('ZERO_NIL_COMPAT');
  FAvailable.Add('STRING_CONST_TRUNCED');
  FAvailable.Add('FOR_LOOP_VAR_VARPAR');
  FAvailable.Add('TYPED_CONST_VARPAR');
  FAvailable.Add('ASG_TO_TYPED_CONST');
  FAvailable.Add('CASE_LABEL_RANGE');
  FAvailable.Add('FOR_VARIABLE');
  FAvailable.Add('CONSTRUCTING_ABSTRACT');
  FAvailable.Add('COMPARISON_FALSE');
  FAvailable.Add('COMPARISON_TRUE');
  FAvailable.Add('COMPARING_SIGNED_UNSIGNED');
  FAvailable.Add('COMBINING_SIGNED_UNSIGNED');
  FAvailable.Add('UNSUPPORTED_CONSTRUCT');
  FAvailable.Add('FILE_OPEN');
  FAvailable.Add('FILE_OPEN_UNITSRC');
  FAvailable.Add('BAD_GLOBAL_SYMBOL');
  FAvailable.Add('DUPLICATE_CTOR_DTOR');
  FAvailable.Add('INVALID_DIRECTIVE');
  FAvailable.Add('PACKAGE_NO_LINK');
  FAvailable.Add('PACKAGED_THREADVAR');
  FAvailable.Add('IMPLICIT_IMPORT');
  FAvailable.Add('HPPEMIT_IGNORED');
  FAvailable.Add('NO_RETVAL');
  FAvailable.Add('USE_BEFORE_DEF');
  FAvailable.Add('FOR_LOOP_VAR_UNDEF');
  FAvailable.Add('UNIT_NAME_MISMATCH');
  FAvailable.Add('NO_CFG_FILE_FOUND');
  FAvailable.Add('IMPLICIT_VARIANTS');
  FAvailable.Add('UNICODE_TO_LOCALE');
  FAvailable.Add('LOCALE_TO_UNICODE');
  FAvailable.Add('IMAGEBASE_MULTIPLE');
  FAvailable.Add('SUSPICIOUS_TYPECAST');
  FAvailable.Add('PRIVATE_PROPACCESSOR');
  FAvailable.Add('UNSAFE_TYPE');
  FAvailable.Add('UNSAFE_CODE');
  FAvailable.Add('UNSAFE_CAST');
  FAvailable.Add('OPTION_TRUNCATED');
  FAvailable.Add('WIDECHAR_REDUCED');
  FAvailable.Add('DUPLICATES_IGNORED');
  FAvailable.Add('UNIT_INIT_SEQ');
  FAvailable.Add('LOCAL_PINVOKE');
  FAvailable.Add('MESSAGE_DIRECTIVE');
  FAvailable.Add('TYPEINFO_IMPLICITLY_ADDED');
  FAvailable.Add('RLINK_WARNING');
  FAvailable.Add('IMPLICIT_STRING_CAST');
  FAvailable.Add('IMPLICIT_STRING_CAST_LOSS');
  FAvailable.Add('EXPLICIT_STRING_CAST');
  FAvailable.Add('EXPLICIT_STRING_CAST_LOSS');
  FAvailable.Add('CVT_WCHAR_TO_ACHAR');
  FAvailable.Add('CVT_NARROWING_STRING_LOST');
  FAvailable.Add('CVT_ACHAR_TO_WCHAR');
  FAvailable.Add('CVT_WIDENING_STRING_LOST');
  FAvailable.Add('XML_WHITESPACE_NOT_ALLOWED');
  FAvailable.Add('XML_UNKNOWN_ENTITY');
  FAvailable.Add('XML_INVALID_NAME_START');
  FAvailable.Add('XML_INVALID_NAME');
  FAvailable.Add('XML_EXPECTED_CHARACTER');
  FAvailable.Add('XML_CREF_NO_RESOLVE');
  FAvailable.Add('XML_NO_PARM');
  FAvailable.Add('XML_NO_MATCHING_PARM');
end;
{$ELSE}
{$IF CompilerVersion >= CompilerVersionDelphi2009}

procedure TfmConfigureWarning.InitWarnings;
begin
  FAvailable.Add('SYMBOL_DEPRECATED');
  FAvailable.Add('SYMBOL_LIBRARY');
  FAvailable.Add('SYMBOL_PLATFORM');
  FAvailable.Add('SYMBOL_EXPERIMENTAL');
  FAvailable.Add('UNIT_LIBRARY');
  FAvailable.Add('UNIT_PLATFORM');
  FAvailable.Add('UNIT_DEPRECATED');
  FAvailable.Add('UNIT_EXPERIMENTAL');
  FAvailable.Add('HRESULT_COMPAT');
  FAvailable.Add('HIDING_MEMBER');
  FAvailable.Add('HIDDEN_VIRTUAL');
  FAvailable.Add('GARBAGE');
  FAvailable.Add('BOUNDS_ERROR');
  FAvailable.Add('ZERO_NIL_COMPAT');
  FAvailable.Add('STRING_CONST_TRUNCED');
  FAvailable.Add('FOR_LOOP_VAR_VARPAR');
  FAvailable.Add('TYPED_CONST_VARPAR');
  FAvailable.Add('ASG_TO_TYPED_CONST');
  FAvailable.Add('CASE_LABEL_RANGE');
  FAvailable.Add('FOR_VARIABLE');
  FAvailable.Add('CONSTRUCTING_ABSTRACT');
  FAvailable.Add('COMPARISON_FALSE');
  FAvailable.Add('COMPARISON_TRUE');
  FAvailable.Add('COMPARING_SIGNED_UNSIGNED');
  FAvailable.Add('COMBINING_SIGNED_UNSIGNED');
  FAvailable.Add('UNSUPPORTED_CONSTRUCT');
  FAvailable.Add('FILE_OPEN');
  FAvailable.Add('FILE_OPEN_UNITSRC');
  FAvailable.Add('BAD_GLOBAL_SYMBOL');
  FAvailable.Add('DUPLICATE_CTOR_DTOR');
  FAvailable.Add('INVALID_DIRECTIVE');
  FAvailable.Add('PACKAGE_NO_LINK');
  FAvailable.Add('PACKAGED_THREADVAR');
  FAvailable.Add('IMPLICIT_IMPORT');
  FAvailable.Add('HPPEMIT_IGNORED');
  FAvailable.Add('NO_RETVAL');
  FAvailable.Add('USE_BEFORE_DEF');
  FAvailable.Add('FOR_LOOP_VAR_UNDEF');
  FAvailable.Add('UNIT_NAME_MISMATCH');
  FAvailable.Add('NO_CFG_FILE_FOUND');
  FAvailable.Add('IMPLICIT_VARIANTS');
  FAvailable.Add('UNICODE_TO_LOCALE');
  FAvailable.Add('LOCALE_TO_UNICODE');
  FAvailable.Add('IMAGEBASE_MULTIPLE');
  FAvailable.Add('SUSPICIOUS_TYPECAST');
  FAvailable.Add('PRIVATE_PROPACCESSOR');
  FAvailable.Add('UNSAFE_TYPE');
  FAvailable.Add('UNSAFE_CODE');
  FAvailable.Add('UNSAFE_CAST');
  FAvailable.Add('OPTION_TRUNCATED');
  FAvailable.Add('WIDECHAR_REDUCED');
  FAvailable.Add('DUPLICATES_IGNORED');
  FAvailable.Add('UNIT_INIT_SEQ');
  FAvailable.Add('LOCAL_PINVOKE');
  FAvailable.Add('MESSAGE_DIRECTIVE');
  FAvailable.Add('TYPEINFO_IMPLICITLY_ADDED');
  FAvailable.Add('RLINK_WARNING');
  FAvailable.Add('IMPLICIT_STRING_CAST');
  FAvailable.Add('IMPLICIT_STRING_CAST_LOSS');
  FAvailable.Add('EXPLICIT_STRING_CAST');
  FAvailable.Add('EXPLICIT_STRING_CAST_LOSS');
  FAvailable.Add('CVT_WCHAR_TO_ACHAR');
  FAvailable.Add('CVT_NARROWING_STRING_LOST');
  FAvailable.Add('CVT_ACHAR_TO_WCHAR');
  FAvailable.Add('CVT_WIDENING_STRING_LOST');
  FAvailable.Add('XML_WHITESPACE_NOT_ALLOWED');
  FAvailable.Add('XML_UNKNOWN_ENTITY');
  FAvailable.Add('XML_INVALID_NAME_START');
  FAvailable.Add('XML_INVALID_NAME');
  FAvailable.Add('XML_EXPECTED_CHARACTER');
  FAvailable.Add('XML_CREF_NO_RESOLVE');
  FAvailable.Add('XML_NO_PARM');
  FAvailable.Add('XML_NO_MATCHING_PARM');
end;
{$ELSE}
{$IF CompilerVersion >= CompilerVersionDelphi2007}

procedure TfmConfigureWarning.InitWarnings;
begin
  FAvailable.Add('SYMBOL_DEPRECATED');
  FAvailable.Add('SYMBOL_LIBRARY');
  FAvailable.Add('SYMBOL_PLATFORM');
  FAvailable.Add('SYMBOL_EXPERIMENTAL');
  FAvailable.Add('UNIT_LIBRARY');
  FAvailable.Add('UNIT_PLATFORM');
  FAvailable.Add('UNIT_DEPRECATED');
  FAvailable.Add('UNIT_EXPERIMENTAL');
  FAvailable.Add('HRESULT_COMPAT');
  FAvailable.Add('HIDING_MEMBER');
  FAvailable.Add('HIDDEN_VIRTUAL');
  FAvailable.Add('GARBAGE');
  FAvailable.Add('BOUNDS_ERROR');
  FAvailable.Add('ZERO_NIL_COMPAT');
  FAvailable.Add('STRING_CONST_TRUNCED');
  FAvailable.Add('FOR_LOOP_VAR_VARPAR');
  FAvailable.Add('TYPED_CONST_VARPAR');
  FAvailable.Add('ASG_TO_TYPED_CONST');
  FAvailable.Add('CASE_LABEL_RANGE');
  FAvailable.Add('FOR_VARIABLE');
  FAvailable.Add('CONSTRUCTING_ABSTRACT');
  FAvailable.Add('COMPARISON_FALSE');
  FAvailable.Add('COMPARISON_TRUE');
  FAvailable.Add('COMPARING_SIGNED_UNSIGNED');
  FAvailable.Add('COMBINING_SIGNED_UNSIGNED');
  FAvailable.Add('UNSUPPORTED_CONSTRUCT');
  FAvailable.Add('FILE_OPEN');
  FAvailable.Add('FILE_OPEN_UNITSRC');
  FAvailable.Add('BAD_GLOBAL_SYMBOL');
  FAvailable.Add('DUPLICATE_CTOR_DTOR');
  FAvailable.Add('INVALID_DIRECTIVE');
  FAvailable.Add('PACKAGE_NO_LINK');
  FAvailable.Add('PACKAGED_THREADVAR');
  FAvailable.Add('IMPLICIT_IMPORT');
  FAvailable.Add('HPPEMIT_IGNORED');
  FAvailable.Add('NO_RETVAL');
  FAvailable.Add('USE_BEFORE_DEF');
  FAvailable.Add('FOR_LOOP_VAR_UNDEF');
  FAvailable.Add('UNIT_NAME_MISMATCH');
  FAvailable.Add('NO_CFG_FILE_FOUND');
  FAvailable.Add('IMPLICIT_VARIANTS');
  FAvailable.Add('UNICODE_TO_LOCALE');
  FAvailable.Add('LOCALE_TO_UNICODE');
  FAvailable.Add('IMAGEBASE_MULTIPLE');
  FAvailable.Add('SUSPICIOUS_TYPECAST');
  FAvailable.Add('PRIVATE_PROPACCESSOR');
  FAvailable.Add('UNSAFE_TYPE');
  FAvailable.Add('UNSAFE_CODE');
  FAvailable.Add('UNSAFE_CAST');
  FAvailable.Add('OPTION_TRUNCATED');
  FAvailable.Add('WIDECHAR_REDUCED');
  FAvailable.Add('DUPLICATES_IGNORED');
  FAvailable.Add('UNIT_INIT_SEQ');
  FAvailable.Add('LOCAL_PINVOKE');
  FAvailable.Add('MESSAGE_DIRECTIVE');
  FAvailable.Add('TYPEINFO_IMPLICITLY_ADDED');
  FAvailable.Add('XML_WHITESPACE_NOT_ALLOWED');
  FAvailable.Add('XML_UNKNOWN_ENTITY');
  FAvailable.Add('XML_INVALID_NAME_START');
  FAvailable.Add('XML_INVALID_NAME');
  FAvailable.Add('XML_EXPECTED_CHARACTER');
  FAvailable.Add('XML_CREF_NO_RESOLVE');
  FAvailable.Add('XML_NO_PARM');
  FAvailable.Add('XML_NO_MATCHING_PARM');
end;
{$ELSE}
{$IF CompilerVersion >= CompilerVersionDelphi2006}

procedure TfmConfigureWarning.InitWarnings;
begin
  FAvailable.Add('SYMBOL_DEPRECATED');
  FAvailable.Add('SYMBOL_LIBRARY');
  FAvailable.Add('SYMBOL_PLATFORM');
  FAvailable.Add('SYMBOL_EXPERIMENTAL');
  FAvailable.Add('UNIT_LIBRARY');
  FAvailable.Add('UNIT_PLATFORM');
  FAvailable.Add('UNIT_DEPRECATED');
  FAvailable.Add('UNIT_EXPERIMENTAL');
  FAvailable.Add('HRESULT_COMPAT');
  FAvailable.Add('HIDING_MEMBER');
  FAvailable.Add('HIDDEN_VIRTUAL');
  FAvailable.Add('GARBAGE');
  FAvailable.Add('BOUNDS_ERROR');
  FAvailable.Add('ZERO_NIL_COMPAT');
  FAvailable.Add('STRING_CONST_TRUNCED');
  FAvailable.Add('FOR_LOOP_VAR_VARPAR');
  FAvailable.Add('TYPED_CONST_VARPAR');
  FAvailable.Add('ASG_TO_TYPED_CONST');
  FAvailable.Add('CASE_LABEL_RANGE');
  FAvailable.Add('FOR_VARIABLE');
  FAvailable.Add('CONSTRUCTING_ABSTRACT');
  FAvailable.Add('COMPARISON_FALSE');
  FAvailable.Add('COMPARISON_TRUE');
  FAvailable.Add('COMPARING_SIGNED_UNSIGNED');
  FAvailable.Add('COMBINING_SIGNED_UNSIGNED');
  FAvailable.Add('UNSUPPORTED_CONSTRUCT');
  FAvailable.Add('FILE_OPEN');
  FAvailable.Add('FILE_OPEN_UNITSRC');
  FAvailable.Add('BAD_GLOBAL_SYMBOL');
  FAvailable.Add('DUPLICATE_CTOR_DTOR');
  FAvailable.Add('INVALID_DIRECTIVE');
  FAvailable.Add('PACKAGE_NO_LINK');
  FAvailable.Add('PACKAGED_THREADVAR');
  FAvailable.Add('IMPLICIT_IMPORT');
  FAvailable.Add('HPPEMIT_IGNORED');
  FAvailable.Add('NO_RETVAL');
  FAvailable.Add('USE_BEFORE_DEF');
  FAvailable.Add('FOR_LOOP_VAR_UNDEF');
  FAvailable.Add('UNIT_NAME_MISMATCH');
  FAvailable.Add('NO_CFG_FILE_FOUND');
  FAvailable.Add('IMPLICIT_VARIANTS');
  FAvailable.Add('UNICODE_TO_LOCALE');
  FAvailable.Add('LOCALE_TO_UNICODE');
  FAvailable.Add('IMAGEBASE_MULTIPLE');
  FAvailable.Add('SUSPICIOUS_TYPECAST');
  FAvailable.Add('PRIVATE_PROPACCESSOR');
  FAvailable.Add('UNSAFE_TYPE');
  FAvailable.Add('UNSAFE_CODE');
  FAvailable.Add('UNSAFE_CAST');
  FAvailable.Add('OPTION_TRUNCATED');
  FAvailable.Add('WIDECHAR_REDUCED');
  FAvailable.Add('DUPLICATES_IGNORED');
  FAvailable.Add('UNIT_INIT_SEQ');
  FAvailable.Add('LOCAL_PINVOKE');
  FAvailable.Add('MESSAGE_DIRECTIVE');
end;
{$ELSE}
{$IF CompilerVersion >= CompilerVersionDelphi2005}

procedure TfmConfigureWarning.InitWarnings;
begin
  FAvailable.Add('SYMBOL_DEPRECATED');
  FAvailable.Add('SYMBOL_LIBRARY');
  FAvailable.Add('SYMBOL_PLATFORM');
  FAvailable.Add('SYMBOL_EXPERIMENTAL');
  FAvailable.Add('UNIT_LIBRARY');
  FAvailable.Add('UNIT_PLATFORM');
  FAvailable.Add('UNIT_DEPRECATED');
  FAvailable.Add('UNIT_EXPERIMENTAL');
  FAvailable.Add('HRESULT_COMPAT');
  FAvailable.Add('HIDING_MEMBER');
  FAvailable.Add('HIDDEN_VIRTUAL');
  FAvailable.Add('GARBAGE');
  FAvailable.Add('BOUNDS_ERROR');
  FAvailable.Add('ZERO_NIL_COMPAT');
  FAvailable.Add('STRING_CONST_TRUNCED');
  FAvailable.Add('FOR_LOOP_VAR_VARPAR');
  FAvailable.Add('TYPED_CONST_VARPAR');
  FAvailable.Add('ASG_TO_TYPED_CONST');
  FAvailable.Add('CASE_LABEL_RANGE');
  FAvailable.Add('FOR_VARIABLE');
  FAvailable.Add('CONSTRUCTING_ABSTRACT');
  FAvailable.Add('COMPARISON_FALSE');
  FAvailable.Add('COMPARISON_TRUE');
  FAvailable.Add('COMPARING_SIGNED_UNSIGNED');
  FAvailable.Add('COMBINING_SIGNED_UNSIGNED');
  FAvailable.Add('UNSUPPORTED_CONSTRUCT');
  FAvailable.Add('FILE_OPEN');
  FAvailable.Add('FILE_OPEN_UNITSRC');
  FAvailable.Add('BAD_GLOBAL_SYMBOL');
  FAvailable.Add('DUPLICATE_CTOR_DTOR');
  FAvailable.Add('INVALID_DIRECTIVE');
  FAvailable.Add('PACKAGE_NO_LINK');
  FAvailable.Add('PACKAGED_THREADVAR');
  FAvailable.Add('IMPLICIT_IMPORT');
  FAvailable.Add('HPPEMIT_IGNORED');
  FAvailable.Add('NO_RETVAL');
  FAvailable.Add('USE_BEFORE_DEF');
  FAvailable.Add('FOR_LOOP_VAR_UNDEF');
  FAvailable.Add('UNIT_NAME_MISMATCH');
  FAvailable.Add('NO_CFG_FILE_FOUND');
  FAvailable.Add('IMPLICIT_VARIANTS');
  FAvailable.Add('UNICODE_TO_LOCALE');
  FAvailable.Add('LOCALE_TO_UNICODE');
  FAvailable.Add('IMAGEBASE_MULTIPLE');
  FAvailable.Add('SUSPICIOUS_TYPECAST');
  FAvailable.Add('PRIVATE_PROPACCESSOR');
  FAvailable.Add('UNSAFE_TYPE');
  FAvailable.Add('UNSAFE_CODE');
  FAvailable.Add('UNSAFE_CAST');
  FAvailable.Add('OPTION_TRUNCATED');
  FAvailable.Add('WIDECHAR_REDUCED');
  FAvailable.Add('DUPLICATES_IGNORED');
  FAvailable.Add('MESSAGE_DIRECTIVE');
end;
{$ELSE}
{$IF CompilerVersion >= CompilerVersionDelphi7}

procedure TfmConfigureWarning.InitWarnings;
begin
  FAvailable.Add('SYMBOL_DEPRECATED');
  FAvailable.Add('SYMBOL_LIBRARY');
  FAvailable.Add('SYMBOL_PLATFORM');
  FAvailable.Add('UNIT_LIBRARY');
  FAvailable.Add('UNIT_PLATFORM');
  FAvailable.Add('UNIT_DEPRECATED');
  FAvailable.Add('HRESULT_COMPAT');
  FAvailable.Add('HIDING_MEMBER');
  FAvailable.Add('HIDDEN_VIRTUAL');
  FAvailable.Add('GARBAGE');
  FAvailable.Add('BOUNDS_ERROR');
  FAvailable.Add('ZERO_NIL_COMPAT');
  FAvailable.Add('STRING_CONST_TRUNCED');
  FAvailable.Add('FOR_LOOP_VAR_VARPAR');
  FAvailable.Add('TYPED_CONST_VARPAR');
  FAvailable.Add('ASG_TO_TYPED_CONST');
  FAvailable.Add('CASE_LABEL_RANGE');
  FAvailable.Add('FOR_VARIABLE');
  FAvailable.Add('CONSTRUCTING_ABSTRACT');
  FAvailable.Add('COMPARISON_FALSE');
  FAvailable.Add('COMPARISON_TRUE');
  FAvailable.Add('COMPARING_SIGNED_UNSIGNED');
  FAvailable.Add('COMBINING_SIGNED_UNSIGNED');
  FAvailable.Add('UNSUPPORTED_CONSTRUCT');
  FAvailable.Add('FILE_OPEN');
  FAvailable.Add('FILE_OPEN_UNITSRC');
  FAvailable.Add('BAD_GLOBAL_SYMBOL');
  FAvailable.Add('DUPLICATE_CTOR_DTOR');
  FAvailable.Add('INVALID_DIRECTIVE');
  FAvailable.Add('PACKAGE_NO_LINK');
  FAvailable.Add('PACKAGED_THREADVAR');
  FAvailable.Add('IMPLICIT_IMPORT');
  FAvailable.Add('HPPEMIT_IGNORED');
  FAvailable.Add('NO_RETVAL');
  FAvailable.Add('USE_BEFORE_DEF');
  FAvailable.Add('FOR_LOOP_VAR_UNDEF');
  FAvailable.Add('UNIT_NAME_MISMATCH');
  FAvailable.Add('NO_CFG_FILE_FOUND');
  FAvailable.Add('IMPLICIT_VARIANTS');
  FAvailable.Add('UNICODE_TO_LOCALE');
  FAvailable.Add('LOCALE_TO_UNICODE');
  FAvailable.Add('IMAGEBASE_MULTIPLE');
  FAvailable.Add('SUSPICIOUS_TYPECAST');
  FAvailable.Add('PRIVATE_PROPACCESSOR');
  FAvailable.Add('UNSAFE_TYPE');
  FAvailable.Add('UNSAFE_CODE');
  FAvailable.Add('UNSAFE_CAST');
  FAvailable.Add('MESSAGE_DIRECTIVE');
end;
{$ELSE}
{$IF CompilerVersion >= CompilerVersionDelphi6}

procedure TfmConfigureWarning.InitWarnings;
begin
  FAvailable.Add('SYMBOL_DEPRECATED');
  FAvailable.Add('SYMBOL_LIBRARY');
  FAvailable.Add('SYMBOL_PLATFORM');
  FAvailable.Add('UNIT_LIBRARY');
  FAvailable.Add('UNIT_PLATFORM');
  FAvailable.Add('UNIT_DEPRECATED');
end;
{$IFEND}
{$IFEND}
{$IFEND}
{$IFEND}
{$IFEND}
{$IFEND}
{$IFEND}
{$IFEND}
{$IFEND}
{$IFEND}
{$IFEND}

constructor TfmConfigureWarning.Create(_Owner: TComponent);
begin
  inherited;

{$IF CompilerVersion >= CompilerVersionDelphi2009}
  b_ERROR.Visible := False;
{$IFEND}

  TControl_SetMinConstraints(Self);

  FAvailable := TStringList.Create;
  InitWarnings;
  lb_Warn.Items.Assign(FAvailable);
end;

destructor TfmConfigureWarning.Destroy;
begin
  FreeAndNil(FAvailable);
  inherited;
end;

function TfmConfigureWarning.GetMessage: string;
begin
  if not TListBox_GetSelected(lb_Warn, Result) then
    Result := '';
end;

function TfmConfigureWarning.GetStatus: TWarnStatusEnum;
begin
  Result := FStatus;
end;

procedure TfmConfigureWarning.SelectBestItem;
var
  Filter: string;
  MatchIndex: Integer;
begin
  if lb_Warn.Items.Count > 0 then begin
    Filter := Trim(ed_Filter.Text);
    MatchIndex := lb_Warn.Items.IndexOf(Filter);
    if MatchIndex = -1 then
      MatchIndex := 0;
    lb_Warn.ItemIndex := MatchIndex;
  end;
end;

procedure TfmConfigureWarning.ed_FilterChange(Sender: TObject);
var
  Filter: string;
begin
  Filter := Trim(ed_Filter.Text);
  FilterStringList(FAvailable, lb_Warn.Items, Filter, False);
  SelectBestItem;
end;

procedure TfmConfigureWarning.ed_FilterKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key in [VK_DOWN, VK_UP, VK_NEXT, VK_PRIOR]) then begin
    lb_Warn.Perform(WM_KEYDOWN, Key, 0);
    Key := 0;
  end;
end;

procedure TfmConfigureWarning.b_DefaultClick(Sender: TObject);
begin
  FStatus := wseDEFAULT;
end;

procedure TfmConfigureWarning.b_ERRORClick(Sender: TObject);
begin
{$IF CompilerVersion >= CompilerVersionDelphi2009}
  FStatus := wseERROR;
{$IFEND}
end;

procedure TfmConfigureWarning.b_OFFClick(Sender: TObject);
begin
  FStatus := wseOFF;
end;

procedure TfmConfigureWarning.b_ONClick(Sender: TObject);
begin
  FStatus := wseON;
end;

initialization
  RegisterEditorExpert(TWarnExpert);
end.

