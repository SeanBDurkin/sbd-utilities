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

unit uSBD_FileUtils;

interface
{$WARNINGS OFF}
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

function CompareFileVersions( const Incumbent, Subject: widestring): TVersionCompareResult;

function CopyFile( const Source, Destin: widestring): boolean;

function EraseFile( const Target: widestring): boolean;

{$IFDEF MSWINDOWS}
function GetResourceString( const FileName: widestring; Ordinal: Integer): widestring;
// first resourcestring is Ordinal=0, second is Ordinal=1 etc.

function SpecialPath( Folder: TSpecialFolder; CanCreate: Boolean): widestring;

function GetTempFileName( const prefix: string): widestring;

function SearchEnviroPathW( const FileName: widestring; var QualifiedFileName: widestring): boolean;
{$ENDIF}

implementation































{$I 'SBDCompilers.inc'}

uses uSBD_Vers, SysUtils, Classes
{$IFDEF MSWINDOWS}
  , Windows
{$ENDIF}
  ;
// Don't use the ShlObj unit because its way of getting special folders
//  has been superseded in Win2000.


{$IFDEF MSWINDOWS}
function GetResourceString( const FileName: widestring; Ordinal: Integer): widestring;
var
  Buffer: array [0..1023] of widechar;
  L: integer;
  Ident: UINT;
  MHandle: LongInt;
begin
try
Ident := UINT( -Ordinal);
MHandle := LoadLibraryExW( PWideChar( FileName), 0, LOAD_LIBRARY_AS_DATAFILE);
if MHandle <> 0 then
  try
    L := LoadStringW( MHandle, Ident, Buffer, sizeof( Buffer));
    SetLength( result, L);
    if L > 0 then
      Move( Buffer, result[1], L*SizeOf( widechar))
  finally
  FreeLibrary( MHandle)
  end
except on E:Exception do
 result := '';
end end;
{$ENDIF}




function CompareFileVersions( const Incumbent, Subject: widestring): TVersionCompareResult;
var
  SubjectVer, IncumbentVer: TVers;

begin
result := vcSubject_NotExists;
if not FileExists( Subject) then exit;
result := vcIncumbent_NotExits;
if not FileExists( Incumbent) then exit;
SubjectVer := TVers.Create( Subject);
IncumbentVer := TVers.Create( Incumbent);
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

function CopyFile( const Source, Destin: widestring): boolean;
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


{$IFDEF MSWINDOWS}
function WideFileExists( const Name: WideString): Boolean;
var
  Handle: THandle;
  FindData: TWin32FindDataW;
begin
Handle := FindFirstFileW( PWideChar(Name), FindData);
result := (Handle <> INVALID_HANDLE_VALUE) and
          ((FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0);
if (Handle <> INVALID_HANDLE_VALUE) and (Handle <> 0) then
  Windows.FindClose( Handle)
end;
{$ENDIF}



function EraseFile( const Target: widestring): boolean;
{$IFDEF LINUX}
var
  sTarget: string;
{$ENDIF}
begin
if not WideFileExists( Target) then exit;

{$IFDEF MSWINDOWS}
  result := Windows.DeleteFileW( PWideChar( Target))
{$ENDIF}

{$IFDEF LINUX}
  sTarget := Target;
  result := unlink( PWideChar( sTarget)) <> -1;
{$ENDIF}
end;


{$IFDEF MSWINDOWS}
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







{ From MSDN:
CSIDL_ADMINTOOLS (0x0030)
Version 5.0. The file system directory that is used to store administrative
tools for an individual user. The Microsoft Management Console (MMC) will save
customized consoles to this directory, and it will roam with the user.

CSIDL_APPDATA (0x001a)
Version 4.71. The file system directory that serves as a common repository for
application-specific data. A typical path is C:\Documents and Settings\username
\Application Data. This CSIDL is supported by the redistributable Shfolder.dll
for systems that do not have the Microsoft® Internet Explorer 4.0 integrated
Shell installed.

CSIDL_COMMON_ADMINTOOLS (0x002f)
Version 5.0. The file system directory containing administrative tools for
all users of the computer.

CSIDL_COMMON_APPDATA (0x0023)
Version 5.0. The file system directory containing application data for all
users. A typical path is C:\Documents and Settings\All Users\Application Data.

CSIDL_COMMON_DOCUMENTS (0x002e)
The file system directory that contains documents that are common to all users.
A typical paths is C:\Documents and Settings\All Users\Documents. Valid for
Windows NT systems and Microsoft Windows® 95 and Windows 98 systems with
Shfolder.dll installed.

CSIDL_COOKIES (0x0021)
The file system directory that serves as a common repository for Internet
cookies. A typical path is C:\Documents and Settings\username\Cookies.

CSIDL_HISTORY (0x0022)
The file system directory that serves as a common repository for Internet
history items.

CSIDL_INTERNET_CACHE (0x0020)
Version 4.72. The file system directory that serves as a common repository for
temporary Internet files. A typical path is C:\Documents and Settings\username
\Local Settings\Temporary Internet Files.

CSIDL_LOCAL_APPDATA (0x001c)
Version 5.0. The file system directory that serves as a data repository for
local (nonroaming) applications. A typical path is C:\Documents and Settings
\username\Local Settings\Application Data.

CSIDL_MYPICTURES (0x0027)
Version 5.0. The file system directory that serves as a common repository
for image files. A typical path is C:\Documents and Settings\username
\My Documents\My Pictures.

CSIDL_PERSONAL (0x0005)
Version 6.0. The virtual folder representing the My Documents desktop item.
This is equivalent to CSIDL_MYDOCUMENTS.
Previous to Version 6.0. The file system directory used to physically store a
user's common repository of documents. A typical path is C:\Documents and
Settings\username\My Documents. This should be distinguished from the virtual
My Documents folder in the namespace. To access that virtual folder, use
SHGetFolderLocation, which returns the ITEMIDLIST for the virtual location,
or refer to the technique described in Managing the File System.

CSIDL_PROGRAM_FILES (0x0026)
Version 5.0. The Program Files folder. A typical path is C:\Program Files.

CSIDL_PROGRAM_FILES_COMMON (0x002b)
Version 5.0. A folder for components that are shared across applications. A
typical path is C:\Program Files\Common. Valid only for Windows NT, Windows
2000, and Windows XP systems. Not valid for Windows Millennium Edition
(Windows Me).

CSIDL_SYSTEM (0x0025)
Version 5.0. The Windows System folder. A typical path is C:\Windows\System32.

CSIDL_WINDOWS (0x0024)
Version 5.0. The Windows directory or SYSROOT. This corresponds to the %windir%
or %SYSTEMROOT% environment variables. A typical path is C:\Windows.
}



{$IFNDEF COMPILER_9_UP}
function WStrEnd(Str: PWideChar): PWideChar;
begin
  // returns a pointer to the end of a null terminated string
  Result := Str;
  While Result^ <> #0 do
    Inc(Result);
end;

function WStrLen(Str: PWideChar): Cardinal;
begin
  Result := WStrEnd(Str) - Str;
end;
{$ENDIF}

function GetTempFileName( const prefix: string): widestring;
var
  Path: widestring;
  wPrefix: widestring;
begin
wPrefix := prefix;
SetLength( Path, MAX_PATH);
SetLength( Path, GetTempPathW( MAX_PATH, PWideChar( Path)));
SetLength( result, MAX_PATH);
windows.GetTempFileNameW( PWideChar( Path), PWideChar( wPrefix), 0, PWideChar( result));
SetLength( result, WStrLen( PWideChar( result)))
end;

function SpecialPath( Folder: TSpecialFolder; CanCreate: Boolean): widestring;
var
  szPath: array [0..MAX_PATH] of widechar;
  nFolder: DWord;
  Ok: boolean;

begin
result  := '';
Ok      := False;
nFolder := FolderNumbers[ Folder];
if Folder = sfTemp then
  begin
  SetLength( result, MAX_PATH);
  SetLength( result, GetTempPathW( MAX_PATH, PWideChar( result)));
  exit
  end;
if CanCreate then
  nFolder := nFolder or CSIDL_FLAG_CREATE;
try
  Ok := SUCCEEDED( SHGetFolderPathW( 0, nFolder, 0, SHGFP_TYPE_CURRENT, szPath))
  except
  end;
if not Ok then exit;
SetLength( result, MAX_PATH);
Move( szPath, result[1], MAX_PATH * SizeOf( widechar));
SetLength( result, WStrLen( PWideChar( result)))
end;





function SearchEnviroPathW( const FileName: widestring; var QualifiedFileName: widestring): boolean;
var
  lpBuffer: packed array[ 0..999 ] of byte;
  lpFilePart      : PWideChar;
  FoundFileLen    : dword;
  SecondBuffer    : Pointer;
  SecondBufferLen : integer;

begin
lpFilePart   := nil;
FoundFileLen := windows.SearchPathW(
 { lpPath        : PAnsiChar = } nil,
 { lpFileName    : PAnsiChar = } PWideChar( FileName),
 { lpExtension   : PAnsiChar = } nil,
 { nBufferLength : DWORD     = } SizeOf( lpBuffer) div 2,
 { lpBuffer      : PAnsiChar = } PWideChar( @lpBuffer),
 { var lpFilePart: PAnsiChar = } lpFilePart); { returns: DWORD}

if FoundFileLen <= 0 then
    QualifiedFileName := ''

  else if FoundFileLen > SizeOf( lpBuffer) then
    begin
    SecondBufferLen := (FoundFileLen + 1) * 2;
    GetMem( SecondBuffer, SecondBufferLen);
    try
    FoundFileLen := windows.SearchPathW(
 { lpPath        : PAnsiChar = } nil,
 { lpFileName    : PAnsiChar = } PWideChar( FileName),
 { lpExtension   : PAnsiChar = } nil,
 { nBufferLength : DWORD     = } SecondBufferLen div 2,
 { lpBuffer      : PAnsiChar = } SecondBuffer,
 { var lpFilePart: PAnsiChar = } lpFilePart); { returns: DWORD}
    SetLength( QualifiedFileName, FoundFileLen);
    if FoundFileLen > 0 then
      Move( SecondBuffer^, QualifiedFileName[1], FoundFileLen * 2);
    finally
    if SecondBufferLen > 0 then
      FreeMem( SecondBuffer, SecondBufferLen);
    end end

  else
    begin
    SetLength( QualifiedFileName, FoundFileLen);
    Move( lpBuffer, QualifiedFileName[1], FoundFileLen * 2)
    end;
result := QualifiedFileName <> ''
end;
{$ENDIF}


end.
