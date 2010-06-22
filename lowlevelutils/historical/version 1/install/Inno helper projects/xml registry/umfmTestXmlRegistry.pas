unit umfmTestXmlRegistry;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, XPStyleActnCtrls, ActnMan, StdCtrls, Buttons;

type
  TmfmTestXmlRegistry = class(TForm)
    lblXMLFile: TLabel;
    edtXMLFile: TEdit;
    dlgOpenXMLFile: TOpenDialog;
    btnBrowseXMLFile: TBitBtn;
    actmngrTestXML: TActionManager;
    actBrowseXMLFile: TAction;
    lblKey: TLabel;
    lblResult: TLabel;
    edtKey: TEdit;
    btnParse: TButton;
    actParse: TAction;
    procedure actParseExecute(Sender: TObject);
    procedure actBrowseXMLFileExecute(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  mfmTestXmlRegistry: TmfmTestXmlRegistry;

implementation













uses uXmlRegistry;

{$R *.dfm}

procedure TmfmTestXmlRegistry.actBrowseXMLFileExecute( Sender: TObject);
begin
dlgOpenXMLFile.FileName := edtXMLFile.Text;
if dlgOpenXMLFile.Execute then
  edtXMLFile.Text := dlgOpenXMLFile.FileName
end;



procedure TmfmTestXmlRegistry.actParseExecute( Sender: TObject);
var
  sFileName, sKey, sResult: string;
begin
sFileName := edtXMLFile.Text;
sKey := edtKey.Text;
sResult := DLL_ReadXMLdoc( PChar( sFileName), PChar( sKey));
if sResult = '' then
  sResult := '(empty)';
lblResult.Caption := sResult
end;

end.
