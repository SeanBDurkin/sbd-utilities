program TestXmlRegistry;

uses
  Forms,
  umfmTestXmlRegistry in 'umfmTestXmlRegistry.pas' {mfmTestXmlRegistry},
  uXmlRegistry in 'uXmlRegistry.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TmfmTestXmlRegistry, mfmTestXmlRegistry);
  Application.Run;
end.
