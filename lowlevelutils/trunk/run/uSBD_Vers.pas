{*****************************************************************************}
{                                                                             }
{    SBD Low Level Utility library                                            }
{        Version: 2.0.0.X                                                     }
{                                                                             }
{    Copyright (c) 2003-2010, Sean B. Durkin (sean@seanbdurkin.id.au)         }
{                                                                             }
{*****************************************************************************}

{* ***** BEGIN LICENSE BLOCK *****
This file is part of SBD Low Level Utils.
SBD Low Level Utils is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

SBD Low Level Utils is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.

You should have received a copy of the Lesser GNU General Public License
along with SBD Low Level Utils.  If not, see <http://www.gnu.org/licenses/>.

 * ***** END LICENSE BLOCK ***** *}

unit uSBD_Vers;

interface
uses Classes;
type
  TVersionDisplayLevel = (
    vdlBuild,    // Display version number like N.N.N.N
    vdlRelease,  // Display version number like N.N.N
    vdlMinor,    // Display version number like N.N
    vdlMajor);   // Display version number like N

  TFileVersion = class( TPersistent)
    private
      FhasVersion: boolean;
      FMajor, FMinor, FRelease, FBuild: word;
      FisDebug, FisPreRel, FisPrivate, FisSpecial: boolean;
      FTrans: TStrings;
      FFN: string;
      FPrivateBuild, FSpecialBuild: string;

      function StrValueOfBlock(
         VerInfo: Pointer; TransIndex: integer; const StrName: string): string;

    public
      constructor CreateFromFile( const FN: string);
      constructor CreateSpecific( Major1, Minor1, Release1, Build1: word);
      constructor CreateSpecificUnversioned;

      function StrValue( TransIndex: integer; const StrName: string): string;

      destructor  Destroy; override;

      function Compare( Ref: TFileVersion): integer;
        // result > 0  ==> This is superior to Ref.
        // result = 0  ==> This is equal to Ref.
        // result < 0  ==> This is inferior to Ref.
{$WARNINGS OFF}
      function ToString( DisplayLevel: TVersionDisplayLevel): string;
{$WARNINGS ON}

      property hasVersion: boolean  read FhasVersion;

      property Major  : word  read FMajor;
      property Minor  : word  read FMinor;
      property Release: word  read FRelease;
      property Build  : word  read FBuild;

      property isDebug: boolean read FisDebug;        // Not implemented in Delphi 5!
      property isPreRel : boolean read FisPreRel;     // Not implemented in Delphi 5!
      property isPrivate: boolean read FisPrivate;
      property isSpecial: boolean read FisSpecial;
      property Translations: TStrings  read FTrans;
      property PrivateBuildDescription: string read FPrivateBuild;
      property SpecialBuildDescription: string read FSpecialBuild;
    end;



function Get_SBD_LowLevelUtils_HINSTANCE:  HMODULE;

function GetVersionOfModule_ByHandle(
  // Input:
  ModuleHandle: HMODULE;
  // Outputs:
  var LibName: string; var MajorV, MinorV, ReleaseV, BuildV: word;
  var sVersion: string): boolean;

implementation




















uses Types, Windows, SysUtils, Forms, uSBD_FileUtils, uSBD_RecallIntf;

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



function Get_SBD_LowLevelUtils_HINSTANCE:  HMODULE;
begin
result := HINSTANCE
end;





{ TFileVersion }

function TFileVersion.Compare( Ref: TFileVersion): integer;
// result > 0  ==> This is superior to Ref.
// result = 0  ==> This is equal to Ref.
// result < 0  ==> This is inferior to Ref.
begin
result := 0;
if (not Ref.FhasVersion) and (not FhasVersion) then exit;
if Ref.FhasVersion <> FhasVersion then
  begin
  if FhasVersion then
      result := 1
    else
      result := -1;
  exit
  end;
result := Ref.FMajor - FMajor;
if result <> 0 then exit;
result := Ref.FMinor - FMinor;
if result <> 0 then exit;
result := Ref.FRelease - FRelease;
if result <> 0 then exit;
result := Ref.FBuild - FBuild
end;





constructor TFileVersion.CreateFromFile( const FN: string);
type
  TTranslationPair = packed record
    Lang,
    CharSet: word;
  end;
  PTranslationIDList = ^TTranslationPair;

var
   VerInfoSize, VerValueSize, Dummy: DWORD;
   VerInfo: Pointer;
   VerValue: PVSFixedFileInfo;
   Worker, IDs: PTranslationIDList;
   IDsLen: UINT;
   j: integer;
   s: string;

{$WARNINGS OFF}
   function hasFlag( Flag: DWord): boolean;
   begin
   result := (VerValue^.dwFileFlagsMask and Flag) = Flag
   end;
{$WARNINGS ON}

begin
CreateSpecificUnversioned;
FFN := FN;
VerInfoSize := GetFileVersionInfoSize( PChar( FFN), Dummy);
if VerInfoSize = 0 then exit;
{$WARNINGS OFF}
GetMem( VerInfo, VerInfoSize);
{$WARNINGS ON}
try
  if GetFileVersionInfo( PChar( FFN), 0, VerInfoSize, VerInfo) and
     VerQueryValue( VerInfo, '\', pointer( VerValue), VerValueSize ) and
     (VerValueSize >= SizeOf( TVSFixedFileInfo)) then
      begin
      FhasVersion := True;
{$WARNINGS OFF}
      with VerValue^ do
        begin
        FMajor   := LongRec( dwFileVersionMS).Hi;
        FMinor   := LongRec( dwFileVersionMS).Lo;
        FRelease := LongRec( dwFileVersionLS).Hi;
        FBuild   := LongRec( dwFileVersionLS).Lo;
        FisDebug   := hasFlag( VS_FF_DEBUG);
        FisPreRel  := hasFlag( VS_FF_PRERELEASE);
        FisPrivate := hasFlag( VS_FF_PRIVATEBUILD);
        FisSpecial := hasFlag( VS_FF_SPECIALBUILD)
{$WARNINGS ON}
        end;
      if VerQueryValue( VerInfo, '\VarFileInfo\Translation', Pointer( IDs), IDsLen) then
        begin
        Worker := IDs;
        for j := 0 to (IDsLen div SizeOf( TTranslationPair)) - 1 do
          begin
{$WARNINGS OFF}
          s := Format('%.4x%.4x', [Worker^.Lang, Worker^.CharSet]);
{$WARNINGS ON}
          FTrans.Add( s);
          Inc( Worker)
          end
        end;
      if FTrans.Count = 0 then
          begin
          FisPrivate := False;
          FisSpecial := False
          end
        else
          begin
          FPrivateBuild := StrValueOfBlock( VerInfo, 0, 'PrivateBuild');
          FSpecialBuild := StrValueOfBlock( VerInfo, 0, 'SpecialBuild');
          FisPrivate := FisPrivate and (FPrivateBuild <> '');
          FisSpecial := FisSpecial and (FSpecialBuild <> '');
          end
      end
finally
{$WARNINGS OFF}
  FreeMem( VerInfo, VerInfoSize)
{$WARNINGS ON}
  end
end;





constructor TFileVersion.CreateSpecific(
  Major1, Minor1, Release1, Build1: word);
begin
CreateSpecificUnversioned;
FhasVersion := True;
FMajor := Major1;
FMinor := Minor1;
FRelease := Release1;
FBuild   := Build1
end;



constructor TFileVersion.CreateSpecificUnversioned;
begin
FFN := '';
FTrans := TStringList.Create;
FhasVersion := False;
FMajor   := 0;
FMinor   := 0;
FRelease := 0;
FBuild   := 0;
FisDebug   := False;
FisPreRel  := False;
FisPrivate := False;
FisSpecial := False;
FPrivateBuild := '';
FSpecialBuild := ''
end;


destructor TFileVersion.Destroy;
begin
FTrans.Free;
inherited
end;



function TFileVersion.StrValueOfBlock(
  VerInfo: Pointer; TransIndex: integer; const StrName: string): string;
var
  VerData: PChar;
  VerDataSize: DWORD;
  s: string;
  L: dword;
begin
result := '';
s := '\StringFileInfo\' + FTrans[TransIndex] + '\' + StrName;
if VerQueryValue( VerInfo, PChar( s), pointer( VerData), VerDataSize ) and
  (VerDataSize > 0) then
      begin
      L := SBD_StrLen( VerData);
      if L < VerDataSize then
        VerDataSize := L;
      if VerDataSize > 0 then
        begin
        SetLength( result, VerDataSize);
{$WARNINGS OFF}
        Move( VerData^, result[1], VerDataSize * SizeOf( Char))
{$WARNINGS ON}
        end
      end
end;




function TFileVersion.StrValue(
  TransIndex: integer; const StrName: string): string;
var
  VerInfoSize, Dummy: DWORD;
  VerInfo: Pointer;
begin
result := '';
if (not FhasVersion) or (FFN = '') or (StrName = '') and
   (TransIndex < 0) or (TransIndex >= FTrans.Count) then exit;
VerInfoSize := GetFileVersionInfoSize( PChar( FFN), Dummy);
if VerInfoSize = 0 then exit;
{$WARNINGS OFF}
GetMem( VerInfo, VerInfoSize);
try
  if GetFileVersionInfo( PChar( FFN), 0, VerInfoSize, VerInfo) then
    result := StrValueOfBlock( VerInfo, TransIndex, StrName)
finally
  FreeMem( VerInfo, VerInfoSize)
  end
{$WARNINGS ON}
end;





function TFileVersion.ToString(
  DisplayLevel: TVersionDisplayLevel): string;
begin
if FHasVersion then
  case DisplayLevel of
    vdlBuild   : result := Format( '%d.%d.%d.%d', [FMajor, FMinor, FRelease, FBuild]);
    vdlRelease : result := Format( '%d.%d.%d'   , [FMajor, FMinor, FRelease        ]);
    vdlMinor   : result := Format( '%d.%d'      , [FMajor, FMinor                  ]);
    vdlMajor   : result := IntToStr( FMajor)
    end
  else
    result := '(no version)'
end;


function GetVersionOfModule_ByHandle(
  // Input:
  ModuleHandle: HMODULE;
  // Outputs:
  var LibName: string; var MajorV, MinorV, ReleaseV, BuildV: word;
  var sVersion: string): boolean;

var
  Vers: TFileVersion;
begin
LibName := GetModuleName( ModuleHandle);
result := LibName <> '';
if not result then exit;
Vers := TFileVersion.CreateFromFile( LibName);
TLifeRecall.Store( Vers);
MajorV   := Vers.Major;
MinorV   := Vers.Minor;
ReleaseV := Vers.Release;
BuildV   := Vers.Build;
sVersion := Vers.ToString( vdlBuild);
LibName  := ExtractFileName( LibName)
end;


end.
