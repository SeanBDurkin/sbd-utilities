unit uXmlRegistry;
interface

function DLL_ReadXMLdoc( FileName: PAnsiChar; // FileName is UTF-8
  Key: PAnsiChar): PAnsiChar; pascal;

exports DLL_ReadXMLdoc name 'ReadXMLdoc';



implementation












uses XMLdoc, xmldom, oxmldom, XMLIntf, Dialogs, SysUtils, TntSysUtils;
var
  ReferenceHolder: string;

procedure Test; 
begin
end;


function Native_ReadXMLdoc( const wFileName: widestring; const sKey: string): string; forward;

function DLL_ReadXMLdoc( FileName: PAnsiChar; Key: PAnsiChar): PAnsiChar;
var
  sFileName: UTF8String;
  wFileName: widestring;
  sKey: string;
begin
sFileName := FileName;
wFileName := UTF8Decode( sFileName);
sKey      := Key;
try
  if WideFileExists( wFileName) then
      ReferenceHolder := Native_ReadXMLdoc( wFileName, sKey)
    else
      ReferenceHolder := 'Error: file does not exist.' + wFileName
except on E:Exception do
  ReferenceHolder := 'Error: ' + E.Message
  end;
result := PAnsiChar( ReferenceHolder)
end;




function Native_ReadXMLdoc( const wFileName: widestring; const sKey: string): string;
var
  xmlDoc: IXMLDocument;
  Key: string;
  Parent, Node: IXMLNode;
  Name: string;
  isAttrib: boolean;
  Attrib, Value: string;
  isFound: boolean;

  function GetPart( var Key1: string): string;
  var
    P: integer;
  begin
  P := Pos( '\', Key1);
  result := Key1;
  if P > 0 then
      begin
      SetLength( result, P-1);
      Delete( Key1, 1, P)
      end
    else
      Key1 := ''
  end;

  function ExtractValue( const NameValue: string; var Name: string): string;
  var
    P, L: integer;
  begin
  P := Pos( '=', NameValue);
  Name := NameValue;
  if P > 0 then
      begin
      SetLength( Name, P - 1);
      result := NameValue;
      Delete( result, 1, P)
      end
    else
      result := '';
  L := Length( result);
  if (L >= 2) and (result[1]='"') and (result[L]='"') then
    begin
    Delete( result, 1, 1);
    SetLength( result, L - 2)
    end
  end;

begin
result := 'not yet implemented';
Key := sKey;

  DefaultDOMVendor := oxmldom.SOpenXML;
  xmlDoc := LoadXMLDocument( wFileName);
  Parent := xmlDoc.DocumentElement;
  Node   := Parent;
  isFound := False;
  while Key <> '' do
    begin
    Name := GetPart( Key);
    isAttrib := (Key <> '') and (Key[1]='\');
    if isAttrib then
      begin
      Delete( Key, 1, 1);
      Value := ExtractValue( GetPart( Key), Attrib);
      end;
    isFound := False;
    if Name = '' then break;
    while assigned( Node) do
      begin
      isFound := SameText( Node.NodeName, Name) and
        ((not isAttrib) or SameText( Node.Attributes[ Attrib], Value));
      if isFound then break;
      Node := Node.NextSibling
      end;
    if (not isFound) or (Key = '') then break;
    Node := Node.ChildNodes.First
    end;
if isFound and (Node.DOMNode.childNodes.length >= 1) and
    (Node.DOMNode.childNodes[0].nodeType in [TEXT_NODE, CDATA_SECTION_NODE]) then
    result := Node.Text
  else
    result := ''
end;



end.
