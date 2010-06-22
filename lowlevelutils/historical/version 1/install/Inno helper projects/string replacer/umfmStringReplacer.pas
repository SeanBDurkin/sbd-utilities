unit umfmStringReplacer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TntForms, StdCtrls, TntStdCtrls, AppEvnts;

type
  TmfmStringReplacer = class(TTntForm)
    edtTarget: TTntEdit;
    edtSubstitution: TTntEdit;
    lblTarget: TTntLabel;
    lblSubstitution: TTntLabel;
    btnProceed: TButton;
    appevMain: TApplicationEvents;
    procedure appevMainIdle(Sender: TObject; var Done: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure btnProceedClick(Sender: TObject);

  private
    procedure DoSubstitution;

  public
    { Public declarations }
  end;

var
  mfmStringReplacer: TmfmStringReplacer;

implementation

























{$I '..\..\..\..\..\Common files\inc\SBDCompilers.inc'}
{$R *.dfm}
uses TntSysUtils, TntSystem, TntDialogs, TntClasses, IniFiles, RTLConsts,
     TntWideStrUtils,
{$IFDEF COMPILER_9_UP}
     WideStrUtils,
{$ENDIF}
     StrUtils;

resourcestring
  sSect = 'Substitutions';
  sHelp = '/s<Substition ini filename> (section = [Substitutions])'#13#10 +
          '/t<Target filename>'#13#10 +
          '/h+ (hide main form)'#13#10 +
          '/r+ (report errors)';
   sTargetError         = 'Target file "%s" could not be opened.';
   sIniError            = 'Ini file "%s" could not be opened.';
   sGeneralReplaceError = 'Unknown error in replacement function.';

var
  SubstitutionFN: widestring;
  TargetFN: widestring;
  doHide1: boolean;
  doReportErrors: boolean;
  doShowHelp: boolean;

procedure InitParams_With_DefaultValues; forward;
procedure ReadCommandLineParameters; forward;




procedure TmfmStringReplacer.FormCreate( Sender: TObject);
begin
InitParams_With_DefaultValues;
ReadCommandLineParameters;
Application.ShowMainForm := not doHide1
end;



function WideCharToChar( Ch: widechar): char;
begin
if Hi( Word( Ch)) = 0 then
    result := Char( word( Ch))
  else
    result := #0
end;


procedure ReadCommandLineParameters;
var
  j: integer;
  s: widestring;
  Ch: char;
begin
for j := 1 to WideParamCount do
  begin
  s := WideParamStr( j);
  if (s = '') or ((s[1]<>'-') and (s[1]<>'/')) then continue;
  Delete( s, 1, 1);
  if s = '' then continue;
  Ch := Upcase( WideCharToChar( s[1]));
  Delete( s, 1, 1);
  case Ch of
    'S': // Substitution file
         SubstitutionFN := s;

    'T': // Target file
         TargetFN := s;

    'H': // Hide form
         doHide1 := (s = '+') or (s='');

    'R': // Hide form
         doReportErrors := s = '+';

    '?': // Show help
         doShowHelp := True
    end
  end
end;



procedure InitParams_With_DefaultValues;
begin
SubstitutionFN := WideChangeFileExt( TntApplication.ExeName, '.ini');
TargetFN       := '';
doHide1        := True;
doReportErrors := False;
doShowHelp     := False
end;


procedure TmfmStringReplacer.appevMainIdle(
  Sender: TObject; var Done: Boolean);
begin
appevMain.OnIdle := nil;
if not doHide1 then
  begin
  edtTarget      .Text := TargetFN;
  edtSubstitution.Text := SubstitutionFN
  end;
if doShowHelp then
  WideShowMessage( sHelp);
if doHide1 then
  begin
  DoSubstitution;
  Close
  end
end;







procedure TmfmStringReplacer.btnProceedClick( Sender: TObject);
begin
TargetFN       := edtTarget      .Text;
SubstitutionFN := edtSubstitution.Text;
DoSubstitution;
end;


type
  TWideIniFile = class( TIniFile)
  private
    FFileName: widestring;

  public
    constructor Create( const FileName: widestring);
    function ReadString(const Section, Ident, Default: string): string; override;
    procedure WriteString(const Section, Ident, Value: String); override;
    procedure ReadSection(const Section: string; Strings: TStrings); override;
    procedure ReadSections(Strings: TStrings); override;
    procedure EraseSection(const Section: string); override;
    procedure DeleteKey(const Section, Ident: String); override;
    procedure UpdateFile; override;
  end;


procedure TmfmStringReplacer.DoSubstitution;
var
 ErrorIdx: integer;
 s: widestring;
 Target: TStream;
 Ini: TIniFile;
 L: integer;
 Data: string;
 Subs: TStrings;
 j: integer;
 Key: string;
 Replacement: string;
 InitialData: string;
begin
ErrorIdx := 0;
if (TargetFN = '') or (not WideFileExists( TargetFN)) then
  ErrorIdx := 1;
if (ErrorIdx = 0) and ((SubstitutionFN = '') or (not WideFileExists( SubstitutionFN))) then
  ErrorIdx := 2;
if ErrorIdx = 0 then
try
  Target := TTnTFileStream.Create( TargetFN, fmOpenReadWrite);
  try
   Ini := TWideIniFile.Create( SubstitutionFN);
   Subs := TStringList.Create;
   try
     L := Target.Size;
     SetLength( Data, L);
     if L > 0 then
       Target.ReadBuffer( Data[1], L);
     InitialData := Data;
     Ini.ReadSection( sSect, Subs);
     for j := 0 to Subs.Count - 1 do
       begin
       Key := Subs[j];
       Replacement := Ini.ReadString( sSect, Key, '');
       Data := AnsiReplaceStr( Data, Key, Replacement)
       end;
     L := Length( Data);
     if Data <> InitialData then
       begin
       Target.Size := 0;
       if L > 0 then
         Target.WriteBuffer( Data[1], L)
       end
   finally
     Subs.Free;
     Ini.Free
     end
  finally
    Target.Free
    end
except
  ErrorIdx := 3;
  end;
if (ErrorIdx <> 0) and doReportErrors then
  begin
  case ErrorIdx of
    1: s := WideFormat( sTargetError, [TargetFN]);
    2: s := WideFormat( sIniError, [SubstitutionFN]);
    3: s := sGeneralReplaceError
    end;
  WideMessageDlg( s, mtError, [mbOk], 0)
  end
end;





{ TIniFile }

constructor TWideIniFile.Create( const FileName: widestring);
begin
FFileName := FileName
end;

procedure TWideIniFile.DeleteKey( const Section, Ident: String);
var
  wSection, wIdent: widestring;
begin
wSection := Section;
wIdent   := Ident;
WritePrivateProfileStringW(PWideChar(wSection), PWideChar(wIdent), nil, PWideChar(FFileName));
end;

procedure TWideIniFile.EraseSection(const Section: string);
var
  wSection: widestring;
begin
wSection := Section;
  if not WritePrivateProfileStringW(PWideChar(wSection), nil, nil, PWideChar(FFileName)) then
    raise EIniFileException.CreateResFmt(@SIniFileWriteError, [FFileName]);
end;

procedure TWideIniFile.ReadSection(const Section: string; Strings: TStrings);
const
  BufSize = 16384;
var
  Buffer, P: PWideChar;
  wSection: widestring;
begin
wSection := Section;
  GetMem(Buffer, BufSize * SizeOf( WideChar));
  try
    Strings.BeginUpdate;
    try
      Strings.Clear;
      if GetPrivateProfileStringW(PWideChar(wSection), nil, nil, Buffer, BufSize,
        PWideChar(FFileName)) <> 0 then
      begin
        P := Buffer;
        while P^ <> #0 do
        begin
          Strings.Add(P);
{$IFDEF COMPILER_9_UP}
          Inc(P, WideStrUtils.WStrLen(P) + 1);
{$ELSE}
          Inc(P, TntWideStrUtils.WStrLen(P) + 1);
{$ENDIF}
        end;
      end;
    finally
      Strings.EndUpdate;
    end;
  finally
    FreeMem(Buffer, BufSize * SizeOf( WideChar));
  end;
end;

procedure TWideIniFile.ReadSections(Strings: TStrings);
const
  BufSize = 16384;
var
  Buffer, P: PWideChar;
begin
  GetMem(Buffer, BufSize * SizeOf( WideChar));
  try
    Strings.BeginUpdate;
    try
      Strings.Clear;
      if GetPrivateProfileStringW(nil, nil, nil, Buffer, BufSize,
        PWideChar(FFileName)) <> 0 then
      begin
        P := Buffer;
        while P^ <> #0 do
        begin
          Strings.Add(P);
          Inc(P, WStrLen(P) + 1);
        end;
      end;
    finally
      Strings.EndUpdate;
    end;
  finally
    FreeMem(Buffer, BufSize * SizeOf( WideChar));
  end;
end;

function TWideIniFile.ReadString(
  const Section, Ident, Default: string): string;
var
  Buffer: array[0..2047] of WideChar;
  wSection, wIdent, wDefault, wResult: widestring;
begin
wSection := Section;
wIdent   := Ident;
wDefault := Default;
SetLength( wResult, GetPrivateProfileStringW(
  PWChar(wSection), PWideChar(wIdent), PWideChar(wDefault), Buffer,
  SizeOf(Buffer), PWideChar( FFileName)));
if Length( wResult) > 0 then
  Move( Buffer, wResult[1], Length( wResult) * SizeOf( widechar));
result := wResult
end;

procedure TWideIniFile.UpdateFile;
begin
  WritePrivateProfileStringW(nil, nil, nil, PWideChar(FFileName));
end;

procedure TWideIniFile.WriteString(const Section, Ident, Value: String);
var
  wSection, wIdent, wValue: widestring;
begin
wSection := Section;
wIdent := Ident;
wValue := Value;
  if not WritePrivateProfileStringW(PWideChar(wSection), PWideChar(wIdent),
                                   PWideChar(wValue), PWideChar(FFileName)) then
    raise EIniFileException.CreateResFmt(@SIniFileWriteError, [FFileName]);

end;

end.
