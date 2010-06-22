program CommandConsolePervert;

uses
  Forms,
  umfmCommandConsolePervert in 'umfmCommandConsolePervert.pas' {mfmCommandConsolePervert};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Command Console Pervert';
  Application.CreateForm(TmfmCommandConsolePervert, mfmCommandConsolePervert);
  Application.Run;
end.
