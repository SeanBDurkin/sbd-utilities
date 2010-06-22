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

unit uSBD_FileUtils;

interface
type

TSpecialFolder = ( // See MSDN comments in implemenation section for definitions.
    sfMyDocuments,
    sfMyDocuments_Common,
    sfMyPictures,
    sfAdminTools,
    sfAdminTools_Common,
    sfAppData,
    sfAppData_Common,
    sfAppData_Local,
    sfCookies,
    sfHistory,
    sfInternetCache,
    sfProgramFiles,
    sfProgramFiles_Common,
    sfSystem,
    sfWindows,
    sfTemp );

TVersionCompareResult = (
    vcSubject_NotExists,
    vcIncumbent_NotExits,
    vcSubject_LaterThan_Incumbent,
    vcSubject_EqualTo_Incumbent,
    vcSubject_EarlierThan_Incumbent);

function CompareFileVersions( const Incumbent, Subject: string): TVersionCompareResult;

function CopyFile( const Source, Destin: string): boolean;

function EraseFile( const Target: string): boolean;

function SpecialPath( Folder: TSpecialFolder; CanCreate: Boolean): string;

function GetTempFileName( const prefix: string): string;

function SearchPath( const FileName: string; var FullFN: string): boolean;

function Set_DllDirectory( const PathName: string):boolean;

function RemoveCurrent_DllDirectory:boolean;

function RestoreDefault_DllDirectory:boolean;

function SBD_StrLen( Str: PChar): Cardinal;

function RemoveFinalBackSlash( const Value: string): string;

implementation
































uses uSBD_Vers, SysUtils, Classes, Windows
// Don't use the ShlObj unit because its way of getting special folders
//  has been superseded in Win2000.
{$IFNDEF UNICODE}
  , SHFolder
{$ENDIF}
;

function CompareFileVersions( const Incumbent, Subject: string): TVersionCompareResult;
var
  SubjectVer, IncumbentVer: TFileVersion;

begin
result := vcSubject_NotExists;
if not FileExists( Subject) then exit;
result := vcIncumbent_NotExits;
if not FileExists( Incumbent) then exit;
SubjectVer := TFileVersion.CreateFromFile( Subject);
IncumbentVer := TFileVersion.CreateFromFile( Incumbent);
try
case SubjectVer.Compare( IncumbentVer) of
  -1: result := vcSubject_EarlierThan_Incumbent;
   0: result := vcSubject_EqualTo_Incumbent;
   1: result := vcSubject_LaterThan_Incumbent;
   end // case
finally
SubjectVer.Free;
IncumbentVer.Free
end end;

function CopyFile( const Source, Destin: string): boolean;
var
  SourceStream, DestinStream: TStream;
  Mode: integer;

begin
try // except
SourceStream := TFileStream.Create( Source, fmOpenRead);
try // finally
if FileExists( Destin) then
    Mode := fmOpenWrite
  else
    Mode := fmCreate;
DestinStream := TFileStream.Create( Destin, Mode);
try // finally
if Mode = fmOpenWrite then
  DestinStream.Size := 0;
DestinStream.CopyFrom( SourceStream, 0)
finally // inner try
DestinStream.Free;
end;
finally
SourceStream.Free;
end; // middle try
result := True
except on E: EFOpenError do // outer try
  begin
  result := False;
  end;
end end;


//function WideFileExists( const Name: widestring): Boolean;
//var
//  Handle: THandle;
//  FindData: TWin32FindDataW;
//begin
//Handle := FindFirstFileW( PWideChar(Name), FindData);
//result := (Handle <> INVALID_HANDLE_VALUE) and
//          ((FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0);
//if (Handle <> INVALID_HANDLE_VALUE) and (Handle <> 0) then
//  Windows.FindClose( Handle)
//end;



function EraseFile( const Target: string): boolean;
begin
result := (not FileExists( Target)) or
          Windows.DeleteFile( PChar( Target))
end;


function SHGetFolderPathW(
// http://msdn.microsoft.com/library/default.asp?url=/library/en-us/shellcc/
//   platform/shell/reference/functions/shgetfolderpath.asp
  hwndOwner: HWND;
  nFolder: Integer;
  hToken : THandle;
  dwFlags: DWord;
  pszPath: LPWSTR): HRESULT; stdcall;
  external 'shell32.dll' name 'SHGetFolderPathW'

// Requires shell32.dll version 5.0+
// Don't use SHGetSpecialFolderPath, because with Microsoft Windows 2000,
//   SHGetSpecialFolderPath is superseded by ShGetFolderPath.
// SHGetSpecialFolderPath required shell32.dll version 4.71 up until 5.0


function Succeeded( Res: HResult): Boolean;
begin
result := Res and $80000000 = 0;
end;

const
  CSIDL_FLAG_CREATE                   = $8000;
  SHGFP_TYPE_CURRENT                  = $0000; // Value not documented in MSDN. Found by trial and error.
  SHGFP_TYPE_DEFAULT                  = $0001; // Value not documented in MSDN. Found by trial and error.


FolderNumbers: array [ TSpecialFolder] of DWord = (
  { sfMyDocuments          ==> } $0005,  // CSIDL_PERSONAL
  { sfMyDocuments_Common   ==> } $002E,  // CSIDL_COMMON_DOCUMENTS
  { sfMyPictures           ==> } $0027,  // CSIDL_MYPICTURES
  { sfAdminTools           ==> } $0030,  // CSIDL_ADMINTOOLS
  { sfAdminTools_Common    ==> } $002F,  // CSIDL_COMMON_ADMINTOOLS
  { sfAppData              ==> } $001A,  // CSIDL_APPDATA
  { sfAppData_Common       ==> } $002F,  // CSIDL_COMMON_APPDATA
  { sfAppData_Local        ==> } $001C,  // CSIDL_LOCAL_APPDATA
  { sfCookies              ==> } $0021,  // CSIDL_COOKIES
  { sfHistory              ==> } $0022,  // CSIDL_HISTORY
  { sfInternetCache        ==> } $0020,  // CSIDL_INTERNET_CACHE
  { sfProgramFiles         ==> } $0026,  // CSIDL_PROGRAM_FILES
  { sfProgramFiles_Common  ==> } $002B,  // CSIDL_PROGRAM_FILES_COMMON
  { sfSystem               ==> } $0025,  // CSIDL_SYSTEM
  { sfWindows              ==> } $0024,  // CSIDL_WINDOWS
  { sfTemp                 ==> } $0000); // No CIDL. Use GetTempPath









{$IFNDEF COMPILER_9_UP}
function WStrEnd( Str: PWideChar): PWideChar;
begin
  // returns a pointer to the end of a null terminated string
  Result := Str;
  While Result^ <> #0 do
    Inc(Result);
end;

function WStrLen( Str: PWideChar): Cardinal;
begin
  Result := WStrEnd(Str) - Str;
end;
{$ENDIF}

function SBD_StrLen( Str: PChar): Cardinal;
begin
{$IFDEF UNICODE}
result := WStrLen( Str)
{$ELSE}
result :=  StrLen( Str)
{$ENDIF}
end;


function GetTempFileName( const prefix: string): string;
var
  Path: string;
begin
SetLength( Path, MAX_PATH);
SetLength( Path, GetTempPath( MAX_PATH, PChar( Path)));
SetLength( result, MAX_PATH);
windows.GetTempFileName( PChar( Path), PChar( Prefix), 0, PChar( result));
SetLength( result, SBD_StrLen( PChar( result)))
end;


function RemoveFinalBackSlash( const Value: string): string;
begin
result := Value;
if (result <> '') and (result[ length( result)] = '\') then
  SetLength( result, length( result) - 1)
end;


function SpecialPath( Folder: TSpecialFolder; CanCreate: Boolean): string;
var
  szPath: array [0..MAX_PATH] of char;
  nFolder: DWord;
  Ok: boolean;

begin
result  := '';
Ok      := False;
nFolder := FolderNumbers[ Folder];
if Folder = sfTemp then
  begin
  SetLength( result, MAX_PATH);
{$IFDEF UNICODE}
  SetLength( result, GetTempPathW( MAX_PATH, PWideChar( result)));
{$ELSE}
  SetLength( result, GetTempPathA( MAX_PATH, PAnsiChar( result)));
{$ENDIF}
  exit
  end;
if CanCreate then
  nFolder := nFolder or CSIDL_FLAG_CREATE;
try
{$IFDEF UNICODE}
  Ok := SUCCEEDED( SHGetFolderPathW( 0, nFolder, 0, SHGFP_TYPE_CURRENT, szPath))
{$ELSE}
  Ok := SUCCEEDED( SHGetFolderPathA( 0, nFolder, 0, SHGFP_TYPE_CURRENT, szPath))
{$ENDIF}
  except
  end;
if not Ok then exit;
SetLength( result, MAX_PATH);
Move( szPath, result[1], MAX_PATH * SizeOf( char));
SetLength( result, SBD_StrLen( PChar( result)));
result := RemoveFinalBackSlash( result)
end;





function SearchPath( const FileName: string; var FullFN: string): boolean;
var
  L: DWORD;
  lpFilePart: PChar;
begin
SetLength( FullFN, MAX_PATH);
L := windows.SearchPath( nil, PChar( FileName), nil, Length( FullFN), PChar( FullFN), lpFilePart);
SetLength( FullFN, L);
result := L > 0
end;


function SetDllDirectory_API( lpPathName: LPCTSTR): BOOL; stdcall;
{$IFDEF UNICODE}
 external kernel32 name 'SetDllDirectoryW';
{$ELSE}
 external kernel32 name 'SetDllDirectoryA';
{$ENDIF}

function Set_DllDirectory( const PathName: string):boolean;
begin
result := SetDllDirectory_API( LPCTSTR( PathName))
end;

function RemoveCurrent_DllDirectory:boolean;
const Empty = '';
begin
result := SetDllDirectory_API( LPCTSTR( Empty))
end;

function RestoreDefault_DllDirectory:boolean;
begin
result := SetDllDirectory_API( nil)
end;

end.
