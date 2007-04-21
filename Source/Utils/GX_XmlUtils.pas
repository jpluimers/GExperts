unit GX_XmlUtils;

interface

uses
  OmniXML;

function CreateOrLoadFileToDom(const FileName, FileDescription: string;
  const RootName: string = ''): IXMLDocument;
procedure AddXMLHeader(Doc: IXMLDocument);
function GetChildNodeOfType(Node: IXMLNode; NodeType: TNodeType): IXMLNode;
function GetCDataSectionTextOrNodeText(Node: IXMLNode): string;
function EscapeCDataText(Text: string): string;
function UnEscapeCDataText(Text: string): string;

implementation

uses
  SysUtils;

function CreateOrLoadFileToDom(const FileName, FileDescription, RootName: string): IXMLDocument;
begin
  Result := nil;
end;
(*
  function CreateDom: TDomDocument;
  begin
    Result := TDomDocument.Create(TDomImplementation.Create(nil));
    Result.AppendChild(Result.CreateXmlDeclaration('1.0', '', ''));
    if RootName <> '' then
      Result.AppendChild(Result.CreateElement(RootName));
  end;

  function LoadDom: TDomDocument;
  const
    Question = 'Your %s XML storage file appears corrupt:'+#13+#10+'%s'+#13+#10+'Would you like to delete and then recreate it?';
  var
    Parser: TXmlToDomParser;
  begin
    Result := nil;
    Parser := TXmlToDomParser.Create(nil);
    Parser.DOMImpl := TDomImplementation.Create(nil);
    try
      try
        Result := Parser.fileToDom(FileName);
      except on E: Exception do
        begin
          if MessageDlg(Format(Question, [FileDescription, E.Message]), mtConfirmation, [mbYes,mbNo], 0) = mrYes then
          begin
            DeleteFile(FileName);
            Result := CreateDom;
          end;
        end;
      end;
    finally
      FreeAndNil(Parser);
    end;
  end;

begin
  if FileExists(FileName) then
  begin
    if GetFileSize(FileName) = 0 then
      Result := CreateDom
    else
      Result := LoadDom;
  end
  else
    Result := CreateDom;
end;
*)

procedure AddXMLHeader(Doc: IXMLDocument);
begin
  Doc.AppendChild(Doc.CreateProcessingInstruction('xml', Format('version="1.0" encoding="%s"', ['UTF-8'])));
end;

function GetChildNodeOfType(Node: IXMLNode; NodeType: TNodeType): IXMLNode;
var
  i: Integer;
begin
  Assert(Assigned(Node));
  Result := nil;
  for i := 0 to Node.ChildNodes.Length - 1 do
  begin
    if Node.ChildNodes.Item[i].NodeType = NodeType then
    begin
      Result := Node.ChildNodes.Item[i];
      Exit;
    end;
  end;
end;

function GetCDataSectionTextOrNodeText(Node: IXMLNode): string;
var
  CDataNode: IXMLCDATASection;
begin
  Assert(Assigned(Node));
  Result := '';
  if Node.ChildNodes.Length < 1 then
    Exit;

  CDataNode := (GetChildNodeOfType(Node, CDATA_SECTION_NODE) as IXMLCDataSection);
  if Assigned(CDataNode) then
    Result := UnEscapeCDataText(CDataNode.Data)
  else
    Result := Node.Text;
end;

const
  CDataEndMarker = ']]>';
  CDataEndMarkerEscape = '&GXCDataEndSectionEscape;';

function EscapeCDataText(Text: string): string;
begin
  Result := StringReplace(Text, CDataEndMarker, CDataEndMarkerEscape, [rfReplaceAll]);
end;

function UnEscapeCDataText(Text: string): string;
begin
  Result := StringReplace(Text, CDataEndMarkerEscape, CDataEndMarker, [rfReplaceAll]);
end;

end.

