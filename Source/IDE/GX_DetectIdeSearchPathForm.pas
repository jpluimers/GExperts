unit GX_DetectIdeSearchPathForm;

interface

uses
  SysUtils,
  Forms;

// This function is defined and implemented here because we cannot include
// GX_IdeSearchPathEnhancer in the uses list of GX_IdeFormChangeManager
// without causing an AV.
// https://blog.dummzeuch.de/2018/11/24/found-the-cause-of-the-av-on-exiting-the-delphi-ide/
function IsSarchPathForm(_Form: TCustomForm): Boolean;

implementation


type
  ///<summary>
  /// defines the strings used to identify the search path edit dialog </summary>
  TSearchPathDlgStrings = record
    DialogClass: string;
    DialogName: string;
    DialogCaptionEn: string;
    DialogCaptionFr: string;
    DialogCaptionDe: string;
  end;

{$IFDEF GX_VER300_up}
// Delphi 10 and up
const
  ProjectSearchPathDlg: TSearchPathDlgStrings = (
    DialogClass: 'TInheritedListEditDlg';
    DialogName: 'InheritedListEditDlg';
    DialogCaptionEn: 'Search Path';
    DialogCaptionFr: 'Chemin de recherche';
    DialogCaptionDe: 'Verzeichnisse';
    );
const
  LibrarySearchPathDlg: TSearchPathDlgStrings = (
    DialogClass: 'TOrderedListEditDlg';
    DialogName: 'OrderedListEditDlg';
    DialogCaptionEn: 'Directories';
    DialogCaptionFr: 'Chemin de recherche';
    DialogCaptionDe: 'Verzeichnisse';
    );
{$ELSE GX_VER300_up}
{$IFDEF GX_VER220_up}
// Delphi XE and up
const
  ProjectSearchPathDlg: TSearchPathDlgStrings = (
    DialogClass: 'TInheritedListEditDlg';
    DialogName: 'InheritedListEditDlg';
    DialogCaptionEn: 'Search Path';
    DialogCaptionFr: 'Chemin de recherche';
    DialogCaptionDe: 'Verzeichnisse';
    );
const
  LibrarySearchPathDlg: TSearchPathDlgStrings = (
    DialogClass: 'TOrderedListEditDlg';
    DialogName: 'OrderedListEditDlg';
    DialogCaptionEn: 'Directories';
    DialogCaptionFr: 'Chemin de recherche';
    DialogCaptionDe: 'Verzeichnisse';
    );
{$ELSE GX_VER220_up}
{$IFDEF GX_VER200_up}
// Delphi 2009 and up
const
  ProjectSearchPathDlg: TSearchPathDlgStrings = (
    DialogClass: 'TInheritedListEditDlg';
    DialogName: 'InheritedListEditDlg';
    DialogCaptionEn: 'Search Path';
    DialogCaptionFr: 'Chemin de recherche';
    DialogCaptionDe: 'Verzeichnisse';
    );
const
  LibrarySearchPathDlg: TSearchPathDlgStrings = (
    DialogClass: 'TOrderedListEditDlg';
    DialogName: 'OrderedListEditDlg';
    DialogCaptionEn: 'Directories';
    DialogCaptionFr: 'Chemin de recherche';
    DialogCaptionDe: 'Verzeichnisse';
    );
{$ELSE GX_VER200_up}
// Delphi 2007 and earlier
const
  ProjectSearchPathDlg: TSearchPathDlgStrings = (
    DialogClass: 'TOrderedListEditDlg';
    DialogName: 'OrderedListEditDlg';
    DialogCaptionEn: 'Search Path';
    DialogCaptionFr: 'Chemin de recherche';
    DialogCaptionDe: 'Verzeichnisse';
    );
const
  LibrarySearchPathDlg: TSearchPathDlgStrings = (
    DialogClass: 'TOrderedListEditDlg';
    DialogName: 'OrderedListEditDlg';
    DialogCaptionEn: 'Directories';
    DialogCaptionFr: 'Chemin de recherche';
    DialogCaptionDe: 'Verzeichnisse';
    );
{$ENDIF GX_VER200_up}
{$ENDIF GX_VER220_up}
{$ENDIF GX_VER300_up}

function MatchesDlg(_Form: TCustomForm; _Strings: TSearchPathDlgStrings): Boolean;
begin
  Result := False;
  if not SameText(_Form.ClassName, _Strings.DialogClass) then
    Exit; //==>
  if not SameText(_Form.Name, _Strings.DialogName) then
    Exit; //==>
  if not SameText(_Form.Caption, _Strings.DialogCaptionEn)
    and not SameText(_Form.Caption, _Strings.DialogCaptionFr)
    and not SameText(_Form.Caption, _Strings.DialogCaptionDe) then
    Exit;
  Result := True;
end;

function IsSarchPathForm(_Form: TCustomForm): Boolean;
begin
  if Assigned(_Form) then begin
    Result := True;
    if MatchesDlg(_Form, ProjectSearchPathDlg) then
      Exit; //==>
    if MatchesDlg(_Form, LibrarySearchPathDlg) then
      Exit; //==>
  end;
  Result := False;
end;

end.
