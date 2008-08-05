unit GX_ProofreaderDefaults;

{ This unit contains the GExperts default proofreader
  entries to use if none are found on the system. }

{$I GX_CondDefine.inc}

interface

uses
  GX_ProofreaderData;

procedure AddDefaultDictionaryEntries(ProofreaderData: TProofreaderData);
procedure AddDefaultReplacementEntries(ProofreaderData: TProofreaderData);

implementation

uses
  GX_ProofreaderUtils;

type
  TGxDefReplacementItem = record
    Typed: string;
    Replace: string;
    WhereReplace: TGXWhereReplace;
    Language: TReplacementSource;
  end;

  TGxDefDictionaryItem = record
    Language: TReplacementSource;
    Word: string;
  end;

const
  GxDefaultReplacements: packed array[0..4] of TGxDefReplacementItem = (
    (
      Typed: ':+';
      Replace: ':=';
      WhereReplace: wrAnywhere;
      Language: rtPasSrc;
    ),
    (
      Typed: ';=';
      Replace: ':=';
      WhereReplace: wrAnywhere;
      Language: rtPasSrc;
    ),
    (
      Typed: ';+';
      Replace: ':=';
      WhereReplace: wrAnywhere;
      Language: rtPasSrc;
    ),
    (
      Typed: 'inci';
      Replace: 'Inc(i, |);';
      WhereReplace: wrAnywhere;
      Language: rtPasSrc;
    ),
    (
      Typed: 'smg';
      Replace: 'ShowMessage(''|'');';
      WhereReplace: wrAnywhere;
      Language: rtPasSrc;
    ));

  GxDefaultDictionary: packed array[0..606] of TGxDefDictionaryItem = (
    (
      Language: rtPasSrc;
      Word: 'Accepted';
    ),
    (
      Language: rtPasSrc;
      Word: 'Action';
    ),
    (
      Language: rtPasSrc;
      Word: 'Active';
    ),
    (
      Language: rtPasSrc;
      Word: 'ActivePage';
    ),
    (
      Language: rtPasSrc;
      Word: 'ActiveX';
    ),
    (
      Language: rtPasSrc;
      Word: 'Add';
    ),
    (
      Language: rtPasSrc;
      Word: 'Ancestor';
    ),
    (
      Language: rtPasSrc;
      Word: 'Anchors';
    ),
    (
      Language: rtPasSrc;
      Word: 'AnsiCompareStr';
    ),
    (
      Language: rtPasSrc;
      Word: 'AnsiCompareText';
    ),
    (
      Language: rtPasSrc;
      Word: 'AnsiString';
    ),
    (
      Language: rtPasSrc;
      Word: 'Append';
    ),
    (
      Language: rtPasSrc;
      Word: 'AppendRecord';
    ),
    (
      Language: rtPasSrc;
      Word: 'Application';
    ),
    (
      Language: rtPasSrc;
      Word: 'AsInteger';
    ),
    (
      Language: rtPasSrc;
      Word: 'AsString';
    ),
    (
      Language: rtPasSrc;
      Word: 'Assigned';
    ),
    (
      Language: rtPasSrc;
      Word: 'Boolean';
    ),
    (
      Language: rtPasSrc;
      Word: 'Bottom';
    ),
    (
      Language: rtPasSrc;
      Word: 'BoundsRect';
    ),
    (
      Language: rtPasSrc;
      Word: 'Brush';
    ),
    (
      Language: rtPasSrc;
      Word: 'Byte';
    ),
    (
      Language: rtPasSrc;
      Word: 'ByteBool';
    ),
    (
      Language: rtPasSrc;
      Word: 'Canvas';
    ),
    (
      Language: rtPasSrc;
      Word: 'Caption';
    ),
    (
      Language: rtPasSrc;
      Word: 'Cardinal';
    ),
    (
      Language: rtPasSrc;
      Word: 'Checked';
    ),
    (
      Language: rtPasSrc;
      Word: 'Clear';
    ),
    (
      Language: rtPasSrc;
      Word: 'Click';
    ),
    (
      Language: rtPasSrc;
      Word: 'ClientHeight';
    ),
    (
      Language: rtPasSrc;
      Word: 'ClientRect';
    ),
    (
      Language: rtPasSrc;
      Word: 'ClientWidth';
    ),
    (
      Language: rtPasSrc;
      Word: 'Close';
    ),
    (
      Language: rtPasSrc;
      Word: 'CloseKey';
    ),
    (
      Language: rtPasSrc;
      Word: 'Color';
    ),
    (
      Language: rtPasSrc;
      Word: 'ComponentCount';
    ),
    (
      Language: rtPasSrc;
      Word: 'ComponentState';
    ),
    (
      Language: rtPasSrc;
      Word: 'Components';
    ),
    (
      Language: rtPasSrc;
      Word: 'Component';
    ),
    (
      Language: rtPasSrc;
      Word: 'Constraints';
    ),
    (
      Language: rtPasSrc;
      Word: 'Continue';
    ),
    (
      Language: rtPasSrc;
      Word: 'ControlCount';
    ),
    (
      Language: rtPasSrc;
      Word: 'ControlStyle';
    ),
    (
      Language: rtPasSrc;
      Word: 'Controls';
    ),
    (
      Language: rtPasSrc;
      Word: 'Copy';
    ),
    (
      Language: rtPasSrc;
      Word: 'Count';
    ),
    (
      Language: rtPasSrc;
      Word: 'Create';
    ),
    (
      Language: rtPasSrc;
      Word: 'CreateFile';
    ),
    (
      Language: rtPasSrc;
      Word: 'Currency';
    ),
    (
      Language: rtPasSrc;
      Word: 'DataSource';
    ),
    (
      Language: rtPasSrc;
      Word: 'DatabaseName';
    ),
    (
      Language: rtPasSrc;
      Word: 'Dec';
    ),
    (
      Language: rtPasSrc;
      Word: 'Delete';
    ),
    (
      Language: rtPasSrc;
      Word: 'DisableControls';
    ),
    (
      Language: rtPasSrc;
      Word: 'Double';
    ),
    (
      Language: rtPasSrc;
      Word: 'Edit';
    ),
    (
      Language: rtPasSrc;
      Word: 'EnableControls';
    ),
    (
      Language: rtPasSrc;
      Word: 'Enabled';
    ),
    (
      Language: rtPasSrc;
      Word: 'Exception';
    ),
    (
      Language: rtPasSrc;
      Word: 'Exit';
    ),
    (
      Language: rtPasSrc;
      Word: 'Extended';
    ),
    (
      Language: rtPasSrc;
      Word: 'False';
    ),
    (
      Language: rtPasSrc;
      Word: 'FieldByName';
    ),
    (
      Language: rtPasSrc;
      Word: 'FieldName';
    ),
    (
      Language: rtPasSrc;
      Word: 'Field';
    ),
    (
      Language: rtPasSrc;
      Word: 'Fields';
    ),
    (
      Language: rtPasSrc;
      Word: 'FileName';
    ),
    (
      Language: rtPasSrc;
      Word: 'Filter';
    ),
    (
      Language: rtPasSrc;
      Word: 'Filtered';
    ),
    (
      Language: rtPasSrc;
      Word: 'FindKey';
    ),
    (
      Language: rtPasSrc;
      Word: 'FindNearest';
    ),
    (
      Language: rtPasSrc;
      Word: 'First';
    ),
    (
      Language: rtPasSrc;
      Word: 'Font';
    ),
    (
      Language: rtPasSrc;
      Word: 'Footer';
    ),
    (
      Language: rtPasSrc;
      Word: 'ForceDirectories';
    ),
    (
      Language: rtPasSrc;
      Word: 'Form';
    ),
    (
      Language: rtPasSrc;
      Word: 'Forms';
    ),
    (
      Language: rtPasSrc;
      Word: 'Format';
    ),
    (
      Language: rtPasSrc;
      Word: 'FormatDateTime';
    ),
    (
      Language: rtPasSrc;
      Word: 'Free';
    ),
    (
      Language: rtPasSrc;
      Word: 'GetDataSize';
    ),
    (
      Language: rtPasSrc;
      Word: 'GetLastError';
    ),
    (
      Language: rtPasSrc;
      Word: 'GetTickCount';
    ),
    (
      Language: rtPasSrc;
      Word: 'Handle';
    ),
    (
      Language: rtPasSrc;
      Word: 'Height';
    ),
    (
      Language: rtPasSrc;
      Word: 'High';
    ),
    (
      Language: rtPasSrc;
      Word: 'IDispatch';
    ),
    (
      Language: rtPasSrc;
      Word: 'IInterface';
    ),
    (
      Language: rtPasSrc;
      Word: 'IUnknown';
    ),
    (
      Language: rtPasSrc;
      Word: 'Inc';
    ),
    (
      Language: rtPasSrc;
      Word: 'Index';
    ),
    (
      Language: rtPasSrc;
      Word: 'Insert';
    ),
    (
      Language: rtPasSrc;
      Word: 'Int64';
    ),
    (
      Language: rtPasSrc;
      Word: 'IntToHex';
    ),
    (
      Language: rtPasSrc;
      Word: 'IntToStr';
    ),
    (
      Language: rtPasSrc;
      Word: 'Integer';
    ),
    (
      Language: rtPasSrc;
      Word: 'Invalidate';
    ),
    (
      Language: rtPasSrc;
      Word: 'ItemIndex';
    ),
    (
      Language: rtPasSrc;
      Word: 'Items';
    ),
    (
      Language: rtPasSrc;
      Word: 'Layout';
    ),
    (
      Language: rtPasSrc;
      Word: 'Left';
    ),
    (
      Language: rtPasSrc;
      Word: 'Length';
    ),
    (
      Language: rtPasSrc;
      Word: 'Lines';
    ),
    (
      Language: rtPasSrc;
      Word: 'Loaded';
    ),
    (
      Language: rtPasSrc;
      Word: 'Locate';
    ),
    (
      Language: rtPasSrc;
      Word: 'LongBool';
    ),
    (
      Language: rtPasSrc;
      Word: 'Longint';
    ),
    (
      Language: rtPasSrc;
      Word: 'Longword';
    ),
    (
      Language: rtPasSrc;
      Word: 'Low';
    ),
    (
      Language: rtPasSrc;
      Word: 'Margin';
    ),
    (
      Language: rtPasSrc;
      Word: 'MaxHeight';
    ),
    (
      Language: rtPasSrc;
      Word: 'MaxWidth';
    ),
    (
      Language: rtPasSrc;
      Word: 'MinHeight';
    ),
    (
      Language: rtPasSrc;
      Word: 'MinWidth';
    ),
    (
      Language: rtPasSrc;
      Word: 'Msg';
    ),
    (
      Language: rtPasSrc;
      Word: 'Name';
    ),
    (
      Language: rtPasSrc;
      Word: 'Next';
    ),
    (
      Language: rtPasSrc;
      Word: 'Node';
    ),
    (
      Language: rtPasSrc;
      Word: 'Objects';
    ),
    (
      Language: rtPasSrc;
      Word: 'OnChange';
    ),
    (
      Language: rtPasSrc;
      Word: 'OnMouseMove';
    ),
    (
      Language: rtPasSrc;
      Word: 'OnMouseUp';
    ),
    (
      Language: rtPasSrc;
      Word: 'Open';
    ),
    (
      Language: rtPasSrc;
      Word: 'OpenKey';
    ),
    (
      Language: rtPasSrc;
      Word: 'Options';
    ),
    (
      Language: rtPasSrc;
      Word: 'Ord';
    ),
    (
      Language: rtPasSrc;
      Word: 'Owner';
    ),
    (
      Language: rtPasSrc;
      Word: 'PChar';
    ),
    (
      Language: rtPasSrc;
      Word: 'ParamStr';
    ),
    (
      Language: rtPasSrc;
      Word: 'Parent';
    ),
    (
      Language: rtPasSrc;
      Word: 'Perform';
    ),
    (
      Language: rtPasSrc;
      Word: 'Pointer';
    ),
    (
      Language: rtPasSrc;
      Word: 'PopupMenu';
    ),
    (
      Language: rtPasSrc;
      Word: 'Pos';
    ),
    (
      Language: rtPasSrc;
      Word: 'Position';
    ),
    (
      Language: rtPasSrc;
      Word: 'PostMessage';
    ),
    (
      Language: rtPasSrc;
      Word: 'ProcessMessages';
    ),
    (
      Language: rtPasSrc;
      Word: 'Random';
    ),
    (
      Language: rtPasSrc;
      Word: 'Randomize';
    ),
    (
      Language: rtPasSrc;
      Word: 'ReadBool';
    ),
    (
      Language: rtPasSrc;
      Word: 'ReadString';
    ),
    (
      Language: rtPasSrc;
      Word: 'RecordCount';
    ),
    (
      Language: rtPasSrc;
      Word: 'RecreateWnd';
    ),
    (
      Language: rtPasSrc;
      Word: 'Result';
    ),
    (
      Language: rtPasSrc;
      Word: 'Right';
    ),
    (
      Language: rtPasSrc;
      Word: 'RowCount';
    ),
    (
      Language: rtPasSrc;
      Word: 'Screen';
    ),
    (
      Language: rtPasSrc;
      Word: 'Self';
    ),
    (
      Language: rtPasSrc;
      Word: 'SendMessage';
    ),
    (
      Language: rtPasSrc;
      Word: 'Sender';
    ),
    (
      Language: rtPasSrc;
      Word: 'SetBounds';
    ),
    (
      Language: rtPasSrc;
      Word: 'ShortInt';
    ),
    (
      Language: rtPasSrc;
      Word: 'ShortString';
    ),
    (
      Language: rtPasSrc;
      Word: 'ShowMessage';
    ),
    (
      Language: rtPasSrc;
      Word: 'ShowModal';
    ),
    (
      Language: rtPasSrc;
      Word: 'Single';
    ),
    (
      Language: rtPasSrc;
      Word: 'Size';
    ),
    (
      Language: rtPasSrc;
      Word: 'SizeOf';
    ),
    (
      Language: rtPasSrc;
      Word: 'Smallint';
    ),
    (
      Language: rtPasSrc;
      Word: 'Source';
    ),
    (
      Language: rtPasSrc;
      Word: 'StrToFloat';
    ),
    (
      Language: rtPasSrc;
      Word: 'Strings';
    ),
    (
      Language: rtPasSrc;
      Word: 'Style';
    ),
    (
      Language: rtPasSrc;
      Word: 'TButton';
    ),
    (
      Language: rtPasSrc;
      Word: 'TControl';
    ),
    (
      Language: rtPasSrc;
      Word: 'TCustomForm';
    ),
    (
      Language: rtPasSrc;
      Word: 'TCustomTransparentControl';
    ),
    (
      Language: rtPasSrc;
      Word: 'TDataModule';
    ),
    (
      Language: rtPasSrc;
      Word: 'TDataSet';
    ),
    (
      Language: rtPasSrc;
      Word: 'TDateTime';
    ),
    (
      Language: rtPasSrc;
      Word: 'TEdit';
    ),
    (
      Language: rtPasSrc;
      Word: 'TField';
    ),
    (
      Language: rtPasSrc;
      Word: 'TFlowPanel';
    ),
    (
      Language: rtPasSrc;
      Word: 'TForm';
    ),
    (
      Language: rtPasSrc;
      Word: 'TGridPanel';
    ),
    (
      Language: rtPasSrc;
      Word: 'THandle';
    ),
    (
      Language: rtPasSrc;
      Word: 'TImage';
    ),
    (
      Language: rtPasSrc;
      Word: 'TLabel';
    ),
    (
      Language: rtPasSrc;
      Word: 'TMargins';
    ),
    (
      Language: rtPasSrc;
      Word: 'TMemo';
    ),
    (
      Language: rtPasSrc;
      Word: 'TMemoryStream';
    ),
    (
      Language: rtPasSrc;
      Word: 'TMenuItem';
    ),
    (
      Language: rtPasSrc;
      Word: 'TMessage';
    ),
    (
      Language: rtPasSrc;
      Word: 'TObject';
    ),
    (
      Language: rtPasSrc;
      Word: 'TPadding';
    ),
    (
      Language: rtPasSrc;
      Word: 'TPanel';
    ),
    (
      Language: rtPasSrc;
      Word: 'TPoint';
    ),
    (
      Language: rtPasSrc;
      Word: 'TQuery';
    ),
    (
      Language: rtPasSrc;
      Word: 'TRect';
    ),
    (
      Language: rtPasSrc;
      Word: 'TRegistry';
    ),
    (
      Language: rtPasSrc;
      Word: 'TSpeedButton';
    ),
    (
      Language: rtPasSrc;
      Word: 'TStream';
    ),
    (
      Language: rtPasSrc;
      Word: 'TStringList';
    ),
    (
      Language: rtPasSrc;
      Word: 'TStrings';
    ),
    (
      Language: rtPasSrc;
      Word: 'TTable';
    ),
    (
      Language: rtPasSrc;
      Word: 'TTrayIcon';
    ),
    (
      Language: rtPasSrc;
      Word: 'TTreeNode';
    ),
    (
      Language: rtPasSrc;
      Word: 'TWinControl';
    ),
    (
      Language: rtPasSrc;
      Word: 'Tab';
    ),
    (
      Language: rtPasSrc;
      Word: 'Table';
    ),
    (
      Language: rtPasSrc;
      Word: 'TableName';
    ),
    (
      Language: rtPasSrc;
      Word: 'Tag';
    ),
    (
      Language: rtPasSrc;
      Word: 'Text';
    ),
    (
      Language: rtPasSrc;
      Word: 'Top';
    ),
    (
      Language: rtPasSrc;
      Word: 'True';
    ),
    (
      Language: rtPasSrc;
      Word: 'Update';
    ),
    (
      Language: rtPasSrc;
      Word: 'Value';
    ),
    (
      Language: rtPasSrc;
      Word: 'ValueExists';
    ),
    (
      Language: rtPasSrc;
      Word: 'Variant';
    ),
    (
      Language: rtPasSrc;
      Word: 'Variants';
    ),
    (
      Language: rtPasSrc;
      Word: 'Visible';
    ),
    (
      Language: rtPasSrc;
      Word: 'WaitForMultipleObjects';
    ),
    (
      Language: rtPasSrc;
      Word: 'WaitForSingleObject';
    ),
    (
      Language: rtPasSrc;
      Word: 'WideString';
    ),
    (
      Language: rtPasSrc;
      Word: 'Width';
    ),
    (
      Language: rtPasSrc;
      Word: 'Word';
    ),
    (
      Language: rtPasSrc;
      Word: 'WordBool';
    ),
    (
      Language: rtPasSrc;
      Word: 'Write';
    ),
    (
      Language: rtPasSrc;
      Word: 'WriteBool';
    ),
    (
      Language: rtPasSrc;
      Word: 'WriteInteger';
    ),
    (
      Language: rtPasSrc;
      Word: 'absolute';
    ),
    (
      Language: rtPasSrc;
      Word: 'abstract';
    ),
    (
      Language: rtPasSrc;
      Word: 'alBottom';
    ),
    (
      Language: rtPasSrc;
      Word: 'alLeft';
    ),
    (
      Language: rtPasSrc;
      Word: 'alNone';
    ),
    (
      Language: rtPasSrc;
      Word: 'alRight';
    ),
    (
      Language: rtPasSrc;
      Word: 'alTop';
    ),
    (
      Language: rtPasSrc;
      Word: 'and';
    ),
    (
      Language: rtPasSrc;
      Word: 'array';
    ),
    (
      Language: rtPasSrc;
      Word: 'asm';
    ),
    (
      Language: rtPasSrc;
      Word: 'automated';
    ),
    (
      Language: rtPasSrc;
      Word: 'begin';
    ),
    (
      Language: rtPasSrc;
      Word: 'case';
    ),
    (
      Language: rtPasSrc;
      Word: 'clBlack';
    ),
    (
      Language: rtPasSrc;
      Word: 'clBtnFace';
    ),
    (
      Language: rtPasSrc;
      Word: 'clWhite';
    ),
    (
      Language: rtPasSrc;
      Word: 'clWindow';
    ),
    (
      Language: rtPasSrc;
      Word: 'clWindowText';
    ),
    (
      Language: rtPasSrc;
      Word: 'const';
    ),
    (
      Language: rtPasSrc;
      Word: 'constructor';
    ),
    (
      Language: rtPasSrc;
      Word: 'continue';
    ),
    (
      Language: rtPasSrc;
      Word: 'csDesigning';
    ),
    (
      Language: rtPasSrc;
      Word: 'default';
    ),
    (
      Language: rtPasSrc;
      Word: 'destructor';
    ),
    (
      Language: rtPasSrc;
      Word: 'dispinterface';
    ),
    (
      Language: rtPasSrc;
      Word: 'div';
    ),
    (
      Language: rtPasSrc;
      Word: 'do';
    ),
    (
      Language: rtPasSrc;
      Word: 'downto';
    ),
    (
      Language: rtPasSrc;
      Word: 'dynamic';
    ),
    (
      Language: rtPasSrc;
      Word: 'else';
    ),
    (
      Language: rtPasSrc;
      Word: 'end';
    ),
    (
      Language: rtPasSrc;
      Word: 'except';
    ),
    (
      Language: rtPasSrc;
      Word: 'exports';
    ),
    (
      Language: rtPasSrc;
      Word: 'finalization';
    ),
    (
      Language: rtPasSrc;
      Word: 'finally';
    ),
    (
      Language: rtPasSrc;
      Word: 'for';
    ),
    (
      Language: rtPasSrc;
      Word: 'function';
    ),
    (
      Language: rtPasSrc;
      Word: 'goto';
    ),
    (
      Language: rtPasSrc;
      Word: 'if';
    ),
    (
      Language: rtPasSrc;
      Word: 'implementation';
    ),
    (
      Language: rtPasSrc;
      Word: 'in';
    ),
    (
      Language: rtPasSrc;
      Word: 'inherited';
    ),
    (
      Language: rtPasSrc;
      Word: 'initialization';
    ),
    (
      Language: rtPasSrc;
      Word: 'inline';
    ),
    (
      Language: rtPasSrc;
      Word: 'interface';
    ),
    (
      Language: rtPasSrc;
      Word: 'is';
    ),
    (
      Language: rtPasSrc;
      Word: 'lParam';
    ),
    (
      Language: rtPasSrc;
      Word: 'label';
    ),
    (
      Language: rtPasSrc;
      Word: 'library';
    ),
    (
      Language: rtPasSrc;
      Word: 'mod';
    ),
    (
      Language: rtPasSrc;
      Word: 'nil';
    ),
    (
      Language: rtPasSrc;
      Word: 'not';
    ),
    (
      Language: rtPasSrc;
      Word: 'object';
    ),
    (
      Language: rtPasSrc;
      Word: 'of';
    ),
    (
      Language: rtPasSrc;
      Word: 'or';
    ),
    (
      Language: rtPasSrc;
      Word: 'out';
    ),
    (
      Language: rtPasSrc;
      Word: 'override';
    ),
    (
      Language: rtPasSrc;
      Word: 'packed';
    ),
    (
      Language: rtPasSrc;
      Word: 'private';
    ),
    (
      Language: rtPasSrc;
      Word: 'procedure';
    ),
    (
      Language: rtPasSrc;
      Word: 'program';
    ),
    (
      Language: rtPasSrc;
      Word: 'property';
    ),
    (
      Language: rtPasSrc;
      Word: 'protected';
    ),
    (
      Language: rtPasSrc;
      Word: 'public';
    ),
    (
      Language: rtPasSrc;
      Word: 'published';
    ),
    (
      Language: rtPasSrc;
      Word: 'raise';
    ),
    (
      Language: rtPasSrc;
      Word: 'read';
    ),
    (
      Language: rtPasSrc;
      Word: 'record';
    ),
    (
      Language: rtPasSrc;
      Word: 'reintroduce';
    ),
    (
      Language: rtPasSrc;
      Word: 'repeat';
    ),
    (
      Language: rtPasSrc;
      Word: 'resourcestring';
    ),
    (
      Language: rtPasSrc;
      Word: 'set';
    ),
    (
      Language: rtPasSrc;
      Word: 'shl';
    ),
    (
      Language: rtPasSrc;
      Word: 'shr';
    ),
    (
      Language: rtPasSrc;
      Word: 'string';
    ),
    (
      Language: rtPasSrc;
      Word: 'then';
    ),
    (
      Language: rtPasSrc;
      Word: 'threadvar';
    ),
    (
      Language: rtPasSrc;
      Word: 'try';
    ),
    (
      Language: rtPasSrc;
      Word: 'type';
    ),
    (
      Language: rtPasSrc;
      Word: 'unit';
    ),
    (
      Language: rtPasSrc;
      Word: 'until';
    ),
    (
      Language: rtPasSrc;
      Word: 'uses';
    ),
    (
      Language: rtPasSrc;
      Word: 'var';
    ),
    (
      Language: rtPasSrc;
      Word: 'virtual';
    ),
    (
      Language: rtPasSrc;
      Word: 'wParam';
    ),
    (
      Language: rtPasSrc;
      Word: 'while';
    ),
    (
      Language: rtPasSrc;
      Word: 'with';
    ),
    (
      Language: rtPasSrc;
      Word: 'write';
    ),
    (
      Language: rtPasSrc;
      Word: 'xor';
    ),
    (
      Language: rtCPPSrc;
      Word: '__cdecl';
    ),
    (
      Language: rtCPPSrc;
      Word: '__declspec';
    ),
    (
      Language: rtCPPSrc;
      Word: '__except';
    ),
    (
      Language: rtCPPSrc;
      Word: '__fastcall';
    ),
    (
      Language: rtCPPSrc;
      Word: '__finally';
    ),
    (
      Language: rtCPPSrc;
      Word: '__inline';
    ),
    (
      Language: rtCPPSrc;
      Word: '__stdcall';
    ),
    (
      Language: rtCPPSrc;
      Word: '__try';
    ),
    (
      Language: rtCPPSrc;
      Word: 'auto';
    ),
    (
      Language: rtCPPSrc;
      Word: 'break';
    ),
    (
      Language: rtCPPSrc;
      Word: 'case';
    ),
    (
      Language: rtCPPSrc;
      Word: 'char';
    ),
    (
      Language: rtCPPSrc;
      Word: 'class';
    ),
    (
      Language: rtCPPSrc;
      Word: 'const';
    ),
    (
      Language: rtCPPSrc;
      Word: 'continue';
    ),
    (
      Language: rtCPPSrc;
      Word: 'default';
    ),
    (
      Language: rtCPPSrc;
      Word: 'dllexport';
    ),
    (
      Language: rtCPPSrc;
      Word: 'dllimport';
    ),
    (
      Language: rtCPPSrc;
      Word: 'double';
    ),
    (
      Language: rtCPPSrc;
      Word: 'else';
    ),
    (
      Language: rtCPPSrc;
      Word: 'enum';
    ),
    (
      Language: rtCPPSrc;
      Word: 'except';
    ),
    (
      Language: rtCPPSrc;
      Word: 'extern';
    ),
    (
      Language: rtCPPSrc;
      Word: 'finally';
    ),
    (
      Language: rtCPPSrc;
      Word: 'float';
    ),
    (
      Language: rtCPPSrc;
      Word: 'for';
    ),
    (
      Language: rtCPPSrc;
      Word: 'goto';
    ),
    (
      Language: rtCPPSrc;
      Word: 'long';
    ),
    (
      Language: rtCPPSrc;
      Word: 'naked';
    ),
    (
      Language: rtCPPSrc;
      Word: 'register';
    ),
    (
      Language: rtCPPSrc;
      Word: 'return';
    ),
    (
      Language: rtCPPSrc;
      Word: 'short';
    ),
    (
      Language: rtCPPSrc;
      Word: 'signed';
    ),
    (
      Language: rtCPPSrc;
      Word: 'sizeof';
    ),
    (
      Language: rtCPPSrc;
      Word: 'static';
    ),
    (
      Language: rtCPPSrc;
      Word: 'struct';
    ),
    (
      Language: rtCPPSrc;
      Word: 'switch';
    ),
    (
      Language: rtCPPSrc;
      Word: 'thread';
    ),
    (
      Language: rtCPPSrc;
      Word: 'try';
    ),
    (
      Language: rtCPPSrc;
      Word: 'typedef';
    ),
    (
      Language: rtCPPSrc;
      Word: 'union';
    ),
    (
      Language: rtCPPSrc;
      Word: 'unsigned';
    ),
    (
      Language: rtCPPSrc;
      Word: 'void';
    ),
    (
      Language: rtCPPSrc;
      Word: 'while';
    ),
    (
      Language: rtPreproc;
      Word: 'define';
    ),
    (
      Language: rtPreproc;
      Word: 'include';
    ),
    (
      Language: rtSQLSrc;
      Word: 'ACTION';
    ),
    (
      Language: rtSQLSrc;
      Word: 'ACTIVE';
    ),
    (
      Language: rtSQLSrc;
      Word: 'ADD';
    ),
    (
      Language: rtSQLSrc;
      Word: 'ADMIN';
    ),
    (
      Language: rtSQLSrc;
      Word: 'AFTER';
    ),
    (
      Language: rtSQLSrc;
      Word: 'ALL';
    ),
    (
      Language: rtSQLSrc;
      Word: 'ALTER';
    ),
    (
      Language: rtSQLSrc;
      Word: 'AND';
    ),
    (
      Language: rtSQLSrc;
      Word: 'ANY';
    ),
    (
      Language: rtSQLSrc;
      Word: 'AS';
    ),
    (
      Language: rtSQLSrc;
      Word: 'ASC';
    ),
    (
      Language: rtSQLSrc;
      Word: 'ASCENDING';
    ),
    (
      Language: rtSQLSrc;
      Word: 'AT';
    ),
    (
      Language: rtSQLSrc;
      Word: 'AUTO';
    ),
    (
      Language: rtSQLSrc;
      Word: 'AUTODDL';
    ),
    (
      Language: rtSQLSrc;
      Word: 'AVG';
    ),
    (
      Language: rtSQLSrc;
      Word: 'BASED';
    ),
    (
      Language: rtSQLSrc;
      Word: 'BASENAME';
    ),
    (
      Language: rtSQLSrc;
      Word: 'BEFORE';
    ),
    (
      Language: rtSQLSrc;
      Word: 'BEGIN';
    ),
    (
      Language: rtSQLSrc;
      Word: 'BETWEEN';
    ),
    (
      Language: rtSQLSrc;
      Word: 'BLOB';
    ),
    (
      Language: rtSQLSrc;
      Word: 'BLOBEDIT';
    ),
    (
      Language: rtSQLSrc;
      Word: 'BUFFER';
    ),
    (
      Language: rtSQLSrc;
      Word: 'BY';
    ),
    (
      Language: rtSQLSrc;
      Word: 'CACHE';
    ),
    (
      Language: rtSQLSrc;
      Word: 'CASCADE';
    ),
    (
      Language: rtSQLSrc;
      Word: 'CAST';
    ),
    (
      Language: rtSQLSrc;
      Word: 'CHAR';
    ),
    (
      Language: rtSQLSrc;
      Word: 'CHARACTER';
    ),
    (
      Language: rtSQLSrc;
      Word: 'CHARACTER_LENGTH';
    ),
    (
      Language: rtSQLSrc;
      Word: 'CHAR_LENGTH';
    ),
    (
      Language: rtSQLSrc;
      Word: 'CHECK';
    ),
    (
      Language: rtSQLSrc;
      Word: 'CHECK_POINT_LEN';
    ),
    (
      Language: rtSQLSrc;
      Word: 'CHECK_POINT_LENGTH';
    ),
    (
      Language: rtSQLSrc;
      Word: 'CLOSE';
    ),
    (
      Language: rtSQLSrc;
      Word: 'COLLATE';
    ),
    (
      Language: rtSQLSrc;
      Word: 'COLLATION';
    ),
    (
      Language: rtSQLSrc;
      Word: 'COLUMN';
    ),
    (
      Language: rtSQLSrc;
      Word: 'COMMIT';
    ),
    (
      Language: rtSQLSrc;
      Word: 'COMMITTED';
    ),
    (
      Language: rtSQLSrc;
      Word: 'COMPILETIME';
    ),
    (
      Language: rtSQLSrc;
      Word: 'COMPUTED';
    ),
    (
      Language: rtSQLSrc;
      Word: 'CONDITIONAL';
    ),
    (
      Language: rtSQLSrc;
      Word: 'CONNECT';
    ),
    (
      Language: rtSQLSrc;
      Word: 'CONSTRAINT';
    ),
    (
      Language: rtSQLSrc;
      Word: 'CONTAINING';
    ),
    (
      Language: rtSQLSrc;
      Word: 'CONTINUE';
    ),
    (
      Language: rtSQLSrc;
      Word: 'COUNT';
    ),
    (
      Language: rtSQLSrc;
      Word: 'CREATE';
    ),
    (
      Language: rtSQLSrc;
      Word: 'CSTRING';
    ),
    (
      Language: rtSQLSrc;
      Word: 'CURRENT';
    ),
    (
      Language: rtSQLSrc;
      Word: 'CURSOR';
    ),
    (
      Language: rtSQLSrc;
      Word: 'DATABASE';
    ),
    (
      Language: rtSQLSrc;
      Word: 'DATE';
    ),
    (
      Language: rtSQLSrc;
      Word: 'DEBUG';
    ),
    (
      Language: rtSQLSrc;
      Word: 'DEC';
    ),
    (
      Language: rtSQLSrc;
      Word: 'DECIMAL';
    ),
    (
      Language: rtSQLSrc;
      Word: 'DECLARE';
    ),
    (
      Language: rtSQLSrc;
      Word: 'DEFAULT';
    ),
    (
      Language: rtSQLSrc;
      Word: 'DELETE';
    ),
    (
      Language: rtSQLSrc;
      Word: 'DESC';
    ),
    (
      Language: rtSQLSrc;
      Word: 'DESCENDING';
    ),
    (
      Language: rtSQLSrc;
      Word: 'DESCRIBE';
    ),
    (
      Language: rtSQLSrc;
      Word: 'DESCRIPTOR';
    ),
    (
      Language: rtSQLSrc;
      Word: 'DISCONNECT';
    ),
    (
      Language: rtSQLSrc;
      Word: 'DISPLAY';
    ),
    (
      Language: rtSQLSrc;
      Word: 'DISTINCT';
    ),
    (
      Language: rtSQLSrc;
      Word: 'DO';
    ),
    (
      Language: rtSQLSrc;
      Word: 'DOMAIN';
    ),
    (
      Language: rtSQLSrc;
      Word: 'DOUBLE';
    ),
    (
      Language: rtSQLSrc;
      Word: 'DROP';
    ),
    (
      Language: rtSQLSrc;
      Word: 'ECHO';
    ),
    (
      Language: rtSQLSrc;
      Word: 'EDIT';
    ),
    (
      Language: rtSQLSrc;
      Word: 'ELSE';
    ),
    (
      Language: rtSQLSrc;
      Word: 'END';
    ),
    (
      Language: rtSQLSrc;
      Word: 'ENTRY_POINT';
    ),
    (
      Language: rtSQLSrc;
      Word: 'ESCAPE';
    ),
    (
      Language: rtSQLSrc;
      Word: 'EVENT';
    ),
    (
      Language: rtSQLSrc;
      Word: 'EXCEPTION';
    ),
    (
      Language: rtSQLSrc;
      Word: 'EXECUTE';
    ),
    (
      Language: rtSQLSrc;
      Word: 'EXISTS';
    ),
    (
      Language: rtSQLSrc;
      Word: 'EXIT';
    ),
    (
      Language: rtSQLSrc;
      Word: 'EXTERN';
    ),
    (
      Language: rtSQLSrc;
      Word: 'EXTERNAL';
    ),
    (
      Language: rtSQLSrc;
      Word: 'EXTRACT';
    ),
    (
      Language: rtSQLSrc;
      Word: 'FETCH';
    ),
    (
      Language: rtSQLSrc;
      Word: 'FILE';
    ),
    (
      Language: rtSQLSrc;
      Word: 'FILTER';
    ),
    (
      Language: rtSQLSrc;
      Word: 'FLOAT';
    ),
    (
      Language: rtSQLSrc;
      Word: 'FOR';
    ),
    (
      Language: rtSQLSrc;
      Word: 'FOREIGN';
    ),
    (
      Language: rtSQLSrc;
      Word: 'FOUND';
    ),
    (
      Language: rtSQLSrc;
      Word: 'FROM';
    ),
    (
      Language: rtSQLSrc;
      Word: 'FULL';
    ),
    (
      Language: rtSQLSrc;
      Word: 'FUNCTION';
    ),
    (
      Language: rtSQLSrc;
      Word: 'GENERATOR';
    ),
    (
      Language: rtSQLSrc;
      Word: 'GEN_ID';
    ),
    (
      Language: rtSQLSrc;
      Word: 'GLOBAL';
    ),
    (
      Language: rtSQLSrc;
      Word: 'GOTO';
    ),
    (
      Language: rtSQLSrc;
      Word: 'GRANT';
    ),
    (
      Language: rtSQLSrc;
      Word: 'GROUP';
    ),
    (
      Language: rtSQLSrc;
      Word: 'GROUP_COMMIT_WAIT';
    ),
    (
      Language: rtSQLSrc;
      Word: 'HAVING';
    ),
    (
      Language: rtSQLSrc;
      Word: 'HELP';
    ),
    (
      Language: rtSQLSrc;
      Word: 'IF';
    ),
    (
      Language: rtSQLSrc;
      Word: 'IMMEDIATE';
    ),
    (
      Language: rtSQLSrc;
      Word: 'IN';
    ),
    (
      Language: rtSQLSrc;
      Word: 'INACTIVE';
    ),
    (
      Language: rtSQLSrc;
      Word: 'INDEX';
    ),
    (
      Language: rtSQLSrc;
      Word: 'INDICATOR';
    ),
    (
      Language: rtSQLSrc;
      Word: 'INIT';
    ),
    (
      Language: rtSQLSrc;
      Word: 'INNER';
    ),
    (
      Language: rtSQLSrc;
      Word: 'INPUT';
    ),
    (
      Language: rtSQLSrc;
      Word: 'INPUT_TYPE';
    ),
    (
      Language: rtSQLSrc;
      Word: 'INSERT';
    ),
    (
      Language: rtSQLSrc;
      Word: 'INT';
    ),
    (
      Language: rtSQLSrc;
      Word: 'INTEGER';
    ),
    (
      Language: rtSQLSrc;
      Word: 'INTO';
    ),
    (
      Language: rtSQLSrc;
      Word: 'IS';
    ),
    (
      Language: rtSQLSrc;
      Word: 'ISOLATION';
    ),
    (
      Language: rtSQLSrc;
      Word: 'ISQL';
    ),
    (
      Language: rtSQLSrc;
      Word: 'JOIN';
    ),
    (
      Language: rtSQLSrc;
      Word: 'KEY';
    ),
    (
      Language: rtSQLSrc;
      Word: 'LEFT';
    ),
    (
      Language: rtSQLSrc;
      Word: 'LENGTH';
    ),
    (
      Language: rtSQLSrc;
      Word: 'LEV';
    ),
    (
      Language: rtSQLSrc;
      Word: 'LEVEL';
    ),
    (
      Language: rtSQLSrc;
      Word: 'LIKE';
    ),
    (
      Language: rtSQLSrc;
      Word: 'LOGFILE';
    ),
    (
      Language: rtSQLSrc;
      Word: 'LOG_BUFFER_SIZE';
    ),
    (
      Language: rtSQLSrc;
      Word: 'LOG_BUF_SIZE';
    ),
    (
      Language: rtSQLSrc;
      Word: 'LONG';
    ),
    (
      Language: rtSQLSrc;
      Word: 'MANUAL';
    ),
    (
      Language: rtSQLSrc;
      Word: 'MAX';
    ),
    (
      Language: rtSQLSrc;
      Word: 'MAXIMUM';
    ),
    (
      Language: rtSQLSrc;
      Word: 'MAXIMUM_SEGMENT';
    ),
    (
      Language: rtSQLSrc;
      Word: 'MAX_SEGMENT';
    ),
    (
      Language: rtSQLSrc;
      Word: 'MERGE';
    ),
    (
      Language: rtSQLSrc;
      Word: 'MESSAGE';
    ),
    (
      Language: rtSQLSrc;
      Word: 'MIN';
    ),
    (
      Language: rtSQLSrc;
      Word: 'MINIMUM';
    ),
    (
      Language: rtSQLSrc;
      Word: 'NAMES';
    ),
    (
      Language: rtSQLSrc;
      Word: 'NATIONAL';
    ),
    (
      Language: rtSQLSrc;
      Word: 'NATURAL';
    ),
    (
      Language: rtSQLSrc;
      Word: 'NCHAR';
    ),
    (
      Language: rtSQLSrc;
      Word: 'NO';
    ),
    (
      Language: rtSQLSrc;
      Word: 'NOAUTO';
    ),
    (
      Language: rtSQLSrc;
      Word: 'NOT';
    ),
    (
      Language: rtSQLSrc;
      Word: 'NULL';
    ),
    (
      Language: rtSQLSrc;
      Word: 'NUMERIC';
    ),
    (
      Language: rtSQLSrc;
      Word: 'OF';
    ),
    (
      Language: rtSQLSrc;
      Word: 'ON';
    ),
    (
      Language: rtSQLSrc;
      Word: 'ONLY';
    ),
    (
      Language: rtSQLSrc;
      Word: 'OPEN';
    ),
    (
      Language: rtSQLSrc;
      Word: 'OPTION';
    ),
    (
      Language: rtSQLSrc;
      Word: 'OR';
    ),
    (
      Language: rtSQLSrc;
      Word: 'ORDER';
    ),
    (
      Language: rtSQLSrc;
      Word: 'OUTER';
    ),
    (
      Language: rtSQLSrc;
      Word: 'OUTPUT';
    ),
    (
      Language: rtSQLSrc;
      Word: 'OVERFLOW';
    ),
    (
      Language: rtSQLSrc;
      Word: 'PAGE';
    ),
    (
      Language: rtSQLSrc;
      Word: 'PAGELENGTH';
    ),
    (
      Language: rtSQLSrc;
      Word: 'PAGES';
    ),
    (
      Language: rtSQLSrc;
      Word: 'PAGE_SIZE';
    ),
    (
      Language: rtSQLSrc;
      Word: 'PARAMETER';
    ),
    (
      Language: rtSQLSrc;
      Word: 'PASSWORD';
    ),
    (
      Language: rtSQLSrc;
      Word: 'PLAN';
    ),
    (
      Language: rtSQLSrc;
      Word: 'POSITION';
    ),
    (
      Language: rtSQLSrc;
      Word: 'POST_EVENT';
    ),
    (
      Language: rtSQLSrc;
      Word: 'PRECISION';
    ),
    (
      Language: rtSQLSrc;
      Word: 'PREPARE';
    ),
    (
      Language: rtSQLSrc;
      Word: 'PRIMARY';
    ),
    (
      Language: rtSQLSrc;
      Word: 'PRIVILEGES';
    ),
    (
      Language: rtSQLSrc;
      Word: 'PROCEDURE';
    ),
    (
      Language: rtSQLSrc;
      Word: 'PROTECTED';
    ),
    (
      Language: rtSQLSrc;
      Word: 'PUBLIC';
    ),
    (
      Language: rtSQLSrc;
      Word: 'QUIT';
    ),
    (
      Language: rtSQLSrc;
      Word: 'READ';
    ),
    (
      Language: rtSQLSrc;
      Word: 'REAL';
    ),
    (
      Language: rtSQLSrc;
      Word: 'REFERENCES';
    ),
    (
      Language: rtSQLSrc;
      Word: 'RELEASE';
    ),
    (
      Language: rtSQLSrc;
      Word: 'RESERV';
    ),
    (
      Language: rtSQLSrc;
      Word: 'RESERVING';
    ),
    (
      Language: rtSQLSrc;
      Word: 'RESTRICT';
    ),
    (
      Language: rtSQLSrc;
      Word: 'RETAIN';
    ),
    (
      Language: rtSQLSrc;
      Word: 'RETURN';
    ),
    (
      Language: rtSQLSrc;
      Word: 'RETURNS';
    ),
    (
      Language: rtSQLSrc;
      Word: 'REVOKE';
    ),
    (
      Language: rtSQLSrc;
      Word: 'RIGHT';
    ),
    (
      Language: rtSQLSrc;
      Word: 'ROLE';
    ),
    (
      Language: rtSQLSrc;
      Word: 'ROLLBACK';
    ),
    (
      Language: rtSQLSrc;
      Word: 'RUNTIME';
    ),
    (
      Language: rtSQLSrc;
      Word: 'SCHEMA';
    ),
    (
      Language: rtSQLSrc;
      Word: 'SEGMENT';
    ),
    (
      Language: rtSQLSrc;
      Word: 'SELECT';
    ),
    (
      Language: rtSQLSrc;
      Word: 'SET';
    ),
    (
      Language: rtSQLSrc;
      Word: 'SHADOW';
    ),
    (
      Language: rtSQLSrc;
      Word: 'SHARED';
    ),
    (
      Language: rtSQLSrc;
      Word: 'SHELL';
    ),
    (
      Language: rtSQLSrc;
      Word: 'SHOW';
    ),
    (
      Language: rtSQLSrc;
      Word: 'SINGULAR';
    ),
    (
      Language: rtSQLSrc;
      Word: 'SIZE';
    ),
    (
      Language: rtSQLSrc;
      Word: 'SMALLINT';
    ),
    (
      Language: rtSQLSrc;
      Word: 'SNAPSHOT';
    ),
    (
      Language: rtSQLSrc;
      Word: 'SOME';
    ),
    (
      Language: rtSQLSrc;
      Word: 'SORT';
    ),
    (
      Language: rtSQLSrc;
      Word: 'SQL';
    ),
    (
      Language: rtSQLSrc;
      Word: 'SQLCODE';
    ),
    (
      Language: rtSQLSrc;
      Word: 'SQLERROR';
    ),
    (
      Language: rtSQLSrc;
      Word: 'SQLWARNING';
    ),
    (
      Language: rtSQLSrc;
      Word: 'STABILITY';
    ),
    (
      Language: rtSQLSrc;
      Word: 'STARTING';
    ),
    (
      Language: rtSQLSrc;
      Word: 'STARTS';
    ),
    (
      Language: rtSQLSrc;
      Word: 'STATEMENT';
    ),
    (
      Language: rtSQLSrc;
      Word: 'STATIC';
    ),
    (
      Language: rtSQLSrc;
      Word: 'STATISTICS';
    ),
    (
      Language: rtSQLSrc;
      Word: 'SUB_TYPE';
    ),
    (
      Language: rtSQLSrc;
      Word: 'SUM';
    ),
    (
      Language: rtSQLSrc;
      Word: 'SUSPEND';
    ),
    (
      Language: rtSQLSrc;
      Word: 'TABLE';
    ),
    (
      Language: rtSQLSrc;
      Word: 'TERMINATOR';
    ),
    (
      Language: rtSQLSrc;
      Word: 'THEN';
    ),
    (
      Language: rtSQLSrc;
      Word: 'TO';
    ),
    (
      Language: rtSQLSrc;
      Word: 'TRANSACTION';
    ),
    (
      Language: rtSQLSrc;
      Word: 'TRANSLATE';
    ),
    (
      Language: rtSQLSrc;
      Word: 'TRANSLATION';
    ),
    (
      Language: rtSQLSrc;
      Word: 'TRIGGER';
    ),
    (
      Language: rtSQLSrc;
      Word: 'TRIM';
    ),
    (
      Language: rtSQLSrc;
      Word: 'UNCOMMITTED';
    ),
    (
      Language: rtSQLSrc;
      Word: 'UNION';
    ),
    (
      Language: rtSQLSrc;
      Word: 'UNIQUE';
    ),
    (
      Language: rtSQLSrc;
      Word: 'UPDATE';
    ),
    (
      Language: rtSQLSrc;
      Word: 'UPPER';
    ),
    (
      Language: rtSQLSrc;
      Word: 'USER';
    ),
    (
      Language: rtSQLSrc;
      Word: 'USING';
    ),
    (
      Language: rtSQLSrc;
      Word: 'VALUE';
    ),
    (
      Language: rtSQLSrc;
      Word: 'VALUES';
    ),
    (
      Language: rtSQLSrc;
      Word: 'VARCHAR';
    ),
    (
      Language: rtSQLSrc;
      Word: 'VARIABLE';
    ),
    (
      Language: rtSQLSrc;
      Word: 'VARYING';
    ),
    (
      Language: rtSQLSrc;
      Word: 'VERSION';
    ),
    (
      Language: rtSQLSrc;
      Word: 'VIEW';
    ),
    (
      Language: rtSQLSrc;
      Word: 'WAIT';
    ),
    (
      Language: rtSQLSrc;
      Word: 'WAIT_TIME';
    ),
    (
      Language: rtSQLSrc;
      Word: 'WHEN';
    ),
    (
      Language: rtSQLSrc;
      Word: 'WHENEVER';
    ),
    (
      Language: rtSQLSrc;
      Word: 'WHERE';
    ),
    (
      Language: rtSQLSrc;
      Word: 'WHILE';
    ),
    (
      Language: rtSQLSrc;
      Word: 'WITH';
    ),
    (
      Language: rtSQLSrc;
      Word: 'WORK';
    ),
    (
      Language: rtSQLSrc;
      Word: 'WRITE';
    ));

procedure AddDefaultDictionaryEntries(ProofreaderData: TProofreaderData);
var
  i: Integer;
begin
  for i := Low(GxDefaultDictionary) to High(GxDefaultDictionary) do
    ProofreaderData.AddToDictionary(GxDefaultDictionary[i].Language, GxDefaultDictionary[i].Word);
end;

procedure AddDefaultReplacementEntries(ProofreaderData: TProofreaderData);
var
  i: Integer;
begin
  for i := Low(GxDefaultReplacements) to High(GxDefaultReplacements) do
    with GxDefaultReplacements[i] do
      ProofreaderData.AddToReplacements(Language, Typed, WhereReplace, Replace);
end;

end.

