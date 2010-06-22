program StringReplacer;

uses
  Forms,
  umfmStringReplacer in 'umfmStringReplacer.pas' {mfmStringReplacer};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'String Replacer';
  Application.CreateForm(TmfmStringReplacer, mfmStringReplacer);
  Application.Run;
end.
