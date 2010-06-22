{*****************************************************************************}
{                                                                             }
{    SBD Low Level Utility library                                            }
{        Version: 1.0                                                         }
{                                                                             }
{    Copyright (c) 2003-2007, Sean B. Durkin (s.durkin@gridsense.net)         }
{ The public is hereby granted free and unrestricted license to use and       }
{ to copy this unit.                                                          }
{                                                                             }
{*****************************************************************************}

unit uSBD_Vers;

interface
uses Classes;
const
  MajorVersion_If_No_Version_Resource = 0;
  // An argument could be validly made that this value should be 1.

type
  TVers = class( TPersistent)
    private
      FMajor: Integer;
      FMinor: Integer;
      FRelease: Integer;
      FBuild: Integer;

      function GetAsString: string;

    public
      constructor Create( const FileName: string);
      function Compare( Reference: TVers): Integer;

    published
      property Major: Integer read FMajor;
      property Minor: Integer read FMinor;
      property Release: Integer read FRelease;
      property Build: Integer read FBuild;
      property AsString: string read GetAsString;
    end;

  TExeVers = class( TVers)
    public
      constructor Create;
    end;

implementation




















uses Types, Windows, SysUtils, Forms;
{ TVers }

{$WARNINGS OFF}
function TVers.Compare( Reference: TVers): Integer;
  function CompareInt( Subj, Ref: Integer): Integer;
  begin
  result := 0;
  if Subj = Ref then exit;
  if Subj > Ref then
      result := 1
    else
      result := -1
  end;

begin
result := CompareInt( FMajor  , Reference.Major  );
if result <> 0 then exit;
result := CompareInt( FMinor  , Reference.Minor  );
if result <> 0 then exit;
result := CompareInt( FRelease, Reference.Release);
if result <> 0 then exit;
result := CompareInt( FBuild  , Reference.Build  )
end;

type
P_VS_FIXEDFILEINFO = ^_VS_FIXEDFILEINFO;
_VS_FIXEDFILEINFO = record
    dwSignature       : DWord;
    dwStrucVersion    : DWord;
    dwFileVersionMS   : DWord;
    dwFileVersionLS   : DWord;
    dwProductVersionMS: DWord;
    dwProductVersionLS: DWord;
    dwFileFlagsMask   : DWord;
    dwFileFlags       : DWord;
    dwFileOS          : DWord;
    dwFileType        : DWord;
    dwFileSubtype     : DWord;
    dwFileDateMS      : DWord;
    dwFileDateLS      : DWord;
    end;

constructor TVers.Create( const FileName: string);
var
  InfoSize: Integer;
  InfoP: Pointer;
  Dummy: DWord;
  FileInfo: Pointer;
  FileInfoSize: UINT;

begin
InfoSize := GetFileVersionInfoSize( PChar(FileName), Dummy);
if InfoSize = 0 then // no version resource
  begin
  FMajor   := MajorVersion_If_No_Version_Resource;
  FMinor   := 0;
  FRelease := 0;
  FBuild   := 0;
  exit
  end;
GetMem( InfoP, InfoSize);
try
GetFileVersionInfo( PChar(FileName), 0, InfoSize, InfoP);
verQueryValue( InfoP, '\', FileInfo, FileInfoSize);
FMajor   := HiWord( P_VS_FIXEDFILEINFO(FileInfo)^.dwFileVersionMS);
FMinor   := LoWord( P_VS_FIXEDFILEINFO(FileInfo)^.dwFileVersionMS);
FRelease := HiWord( P_VS_FIXEDFILEINFO(FileInfo)^.dwFileVersionLS);
FBuild   := LoWord( P_VS_FIXEDFILEINFO(FileInfo)^.dwFileVersionLS);
finally
FreeMem( InfoP, InfoSize);
end end;
{$WARNINGS OFF}

function TVers.GetAsString: string;
begin
result := Format('%d.%d.%d.%d',[FMajor, FMinor, FRelease, FBuild])
end;

{ TExeVers }

constructor TExeVers.Create;
begin
inherited Create( Application.ExeName)
end;

end.
