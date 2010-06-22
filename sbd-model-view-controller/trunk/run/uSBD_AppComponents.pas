{*****************************************************************************}
{                                                                             }
{    SBD Model-View-Controller Framework                                      }
{        Version: 1.0.0.X                                                     }
{                                                                             }
{    Copyright (c) 2010, Sean B. Durkin (sean@seanbdurkin.id.au)         }
{                                                                             }
{*****************************************************************************}

{* ***** BEGIN LICENSE BLOCK *****
This file is part of SBD Low Level Utils.
SBD Model-View-Controller is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

SBD Model-View-Controller is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.

You should have received a copy of the Lesser GNU General Public License
along with SBD Model-View-Controller.  If not, see <http://www.gnu.org/licenses/>.

 * ***** END LICENSE BLOCK ***** *}

unit uSBD_AppComponents;
interface
uses TypInfo, Classes
{$IFDEF SmartInspect}
, SmartInspect
{$EndIf}
;

const
  BaseKeyFmt = '\Software\%0:s\%1:s\%2:d.%3:d';
  // either something like ... '\Software\%0:s\%1:s\%2:d.%3:d'
  //    or                     '\Software\%0:s\%1:s\%2:d';
  //    or                     '\Software\%0:s\%1:s';


type
TAppInfo = class
  public
    FCompany: string;
    FAppId: string;
    FBaseKey: string;

    FAppExeDir: string;
    FAppSpecific_User_Dir: string;
    FAppSpecific_Machine_Dir: string;
    FDocument_User_Dir: string;
    FDocument_Machine_Dir: string;

    FMajorV, FMinorV, FReleaseV, FBuildV: integer;
    FisEngineering: boolean;
    FisAdmin: boolean;

    constructor Create( const ExeName, Company, AppId, DocDesc: string);
  end;


TOptionType = (otString, otEncrypted, otInteger, otBoolean, otFloat,
               otBinary, otEnum);


TTestPutResult = (putOk, putRequiresAdmin, putAccessDenied, putOtherError);

ISystemOptions = interface
  ['{8FF653B6-8FFC-4D5D-B748-F666DC08B4F0}']
    procedure DefineOption( const ProgId: string;
                            const StorageName: string;
                            OptionType: TOptionType;
                            EnumTypeInfo: PTypeInfo;
                            isUser: boolean);

    procedure UndefineOption( const ProgId: string);
    function  isUserOption( const ProgId: string): boolean;
    function  isValueDefined( const ProgId: string): boolean;

    function  GetStringOption ( const ProgId: string): string;
    function  GetIntegerOption( const ProgId: string): integer;
    function  GetBooleanOption( const ProgId: string): boolean;
    function  GetFloatOption  ( const ProgId: string): double;
    function  GetBinaryOption ( const ProgId: string): TStream;

    function  BeginUpdate: IInterface;

    procedure PutStringOption ( const ProgId: string; const Value :string);
    procedure PutIntegerOption( const ProgId: string;       Value :integer);
    procedure PutBooleanOption( const ProgId: string;       Value :boolean);
    procedure PutFloatOption  ( const ProgId: string;       Value :double);
    function  PutBinaryOption ( const ProgId: string): TStream;

    procedure PutDefaultStringOption ( const ProgId: string; const Value :string);
    procedure PutDefaultIntegerOption( const ProgId: string;       Value :integer);
    procedure PutDefaultBooleanOption( const ProgId: string;       Value :boolean);
    procedure PutDefaultFloatOption  ( const ProgId: string;       Value :double);
    function  PutDefaultBinaryOption ( const ProgId: string): TStream;

    function  TestPutFloatOption( const ProgId: string): TTestPutResult;

    procedure SetCryptoString( const CryptoKey1: string);
    procedure StartUp( PutDefaultsWhereUndefined: boolean);
  end;


ISystemOptions_Reg = interface
  // ISystemOptions that are implemented by the system Registry
  //  also implement this interface.
  ['{C5317654-5935-436D-8292-F857B979A9E4}']
    procedure SetBaseKey( const Key: string);
  end;

IApplicationComponent = interface
  ['{1B558A9F-FCA9-4D44-987B-5E0E4F8251F9}']

  {$IFDEF SmartInspect}
    procedure SetSmartInspect(
      Si1: TSmartInspect; Sess1: TSiSession);
   {$EndIf}

    procedure SetAppInfo( Info: TAppInfo);
    procedure DefineOptions( const Options: ISystemOptions);
    procedure StartUp;
    procedure ShutDown;
  end;

TEncryptFunc  = function (const Key, Plaintext: string): ansistring;
TDecryptFunc  = function (const Key: string; const Ciphertext: ansistring): string;
{$IFDEF UNICODE}
TEncryptFuncA = function (const Key, Plaintext: ansistring): ansistring;
TEncryptFuncW = TEncryptFunc;
TDecryptFuncA = function (const Key, Ciphertext: ansistring): ansistring;
TDecryptFuncW = TDecryptFunc;
{$ELSE}
TEncryptFuncA = TEncryptFunc;
TEncryptFuncW = function (const Key, Plaintext: widestring): ansistring;
TDecryptFuncA = TDecryptFunc;
TDecryptFuncW = function (const Key: widestring; const Ciphertext: ansistring): widestring;
{$ENDIF}


function GetVersionInfo(
  // Input:
  const LibName: string;
  // Outputs:
  var MajorV, MinorV, ReleaseV, BuildV: integer): boolean;

function Get_ModelViewController_Framework_HINSTANCE:  HMODULE;

const Library_RTLVersion_Name = 'RTLVersion';
function LibRTLVersion: double; exports LibRTLVersion name Library_RTLVersion_Name;

var
  EncryptFunc : TEncryptFunc;
  EncryptFuncA: TEncryptFuncA; // if not UNICODE, the this should be equal to EncryptFunc;
  EncryptFuncW: TEncryptFuncW; // if UNICODE, the this should be equal to EncryptFunc;
  DecryptFunc : TDecryptFunc;
  DecryptFuncA: TDecryptFuncA; // if not UNICODE, the this should be equal to DecryptFunc;
  DecryptFuncW: TDecryptFuncW; // if UNICODE, the this should be equal to DecryptFunc;

implementation





uses Windows, SysUtils, uSBD_WindowsSecurity,
{$IFDEF UNICODE} {$if CompilerVersion >= 17}
  WideStrUtils,  // WideStrUtils was only intoduced in Delphi 2005.
{$ifend} {$ENDIF}
uSBD_OS_Version, uSBD_FileUtils, uSBD_Vers, uSBD_RecallIntf;



function MakeRecommendedBaseKey(
  const CompanyName, AppId: string;
  MajorVersion, MinorVersion: integer): string;
begin
// Refer http://msdn.microsoft.com/en-us/library/ms724202(VS.85).aspx
// and also http://msdn.microsoft.com/en-us/library/bb756915.aspx
// eg. 'HKCU\Software\Borland\Delphi\7.0'
if Pos( '%3:d', BaseKeyFmt) <> 0 then
    result := Format( BaseKeyFmt,
      [CompanyName, AppId, MajorVersion, MinorVersion])

  else if Pos( '%2:d', BaseKeyFmt) <> 0 then
    result := Format( BaseKeyFmt,
      [CompanyName, AppId, MajorVersion])

  else
    result := Format( BaseKeyFmt,
      [CompanyName, AppId])
end;






function GetVersionInfo(
  // Input:
  const LibName: string;
  // Outputs:
  var MajorV, MinorV, ReleaseV, BuildV: integer): boolean;
var
  Vers: TFileVersion;
begin
Vers := TFileVersion.CreateFromFile( LibName);
TLifeRecall.Store( Vers);
result   := Vers.hasVersion;
MajorV   := Vers.Major;
MinorV   := Vers.Minor;
ReleaseV := Vers.Release;
BuildV   := Vers.Build
end;


{ TAppInfo }

constructor TAppInfo.Create( const ExeName, Company, AppId, DocDesc: string);
var
  sAppName: string;
  j: integer;
  s: string;
begin
FCompany := Company;
FAppId   := AppId;
sAppName := ExeName;
FAppExeDir := RemoveFinalBackSlash( ExtractFilePath( sAppName));
GetVersionInfo( sAppName, FMajorV, FMinorV, FReleaseV, FBuildV);

FAppSpecific_User_Dir    := SpecialPath( sfAppData_Local     , False) + '\' + Company + '\' + AppId;
FAppSpecific_Machine_Dir := SpecialPath( sfAppData_Common    , False) + '\' + Company + '\' + AppId;
FDocument_User_Dir       := SpecialPath( sfMyDocuments       , False);
if DocDesc <> '' then
  FDocument_User_Dir := FDocument_User_Dir + '\' + DocDesc;
FDocument_Machine_Dir    := SpecialPath( sfMyDocuments_Common, False);
if DocDesc <> '' then
  FDocument_Machine_Dir := FDocument_Machine_Dir + '\' + DocDesc;

FisEngineering := False;
for j := 1 to ParamCount do
  begin
  s := ParamStr( j);
  if (s='') or (s[1] <> '-') or (s[1] <> '/') then continue;
  Delete( s, 1, 1);
  FisEngineering := SameText( s, 'engineering');
  if FisEngineering then break
  end;
FBaseKey := MakeRecommendedBaseKey( FCompany, FAppId, FMajorV, FMinorV);
if ThisOS.FOS >= osNT then
    FisAdmin := uSBD_WindowsSecurity.IsAdmin
  else
    FisAdmin := True
end;



function Get_ModelViewController_Framework_HINSTANCE:  HMODULE;
begin
result := HINSTANCE
end;


function LibRTLVersion: double;
begin
result := System.RTLVersion
end;

end.
