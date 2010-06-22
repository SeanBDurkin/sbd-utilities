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

unit uSBD_OSVersion;
// Computes operating system and details with much more precision
//  and accuracy than SysUtils. This unit also provides operating system
//  comparison support that is typically need in installers and install engines.
// Special thanks goes to Marius Bancila
//  (http://www.codeguru.com/cpp/w-p/system/systeminformation/article.php/c8973__1/)
//  for inspiration.

interface
uses uSBD_FundamentTypeSupport;

type

TOperatingSystem = (
  osUnrecognised,  // Any thing, not on the following list.
  osArchaic,       // Windows 95 (including OSR2 and OSR2.5), Windows NT3.50 and Windows NT3.51
  osWin98,      // Windows 98
  osWin98SE,    // Windows 98 SE
  osWinME,      // Windows ME
  osNT,         // Windows NT4.0
  os2000,       // Windows 2000 (Including Pro, Server and Advanced Server)
  osXP,         // Windows XP
  os2003,       // Windows Server 2003
  os2003R2,     // Windows Server 2003 R2
  osVista,      // Windows Vista
  os2008,       // Windows Server 2008
  osPost2008);  // Any client later than Vista, or any server later than 2008.

  TTier = (tClient, tServer);

  TProcessorArchitecture = (
    paIntel,    //x86
    paAMD64,    // x64 (AMD or Intel)
    paIA64,     // Intel Itanium Processor Family (IPF)
    paUnknown); // Unknown processor.

  TEdition = (
    edWin98,
    edWin98_SE,
    edNT_35,
    edWinNT_40_Workstation,
    edWinNT_40_Server_Enterprise,
    edWinNT_40_Server,
    edWin95,
    edWin95_OSR2,
    edWinME,
    edWin2000_Professional,
    edWin2000_Server_Datacenter,
    edWin2000_Server_Advanced,
    edWin2000_Server,
    edWinXP_Home,
    edWinXP_Professional,
    edWinXP_x64,
    edWinXP_MediaCenter,
    edWinXP_Starter,
    edWinXP_TabletPC,
    edWinServer2003_Datacenter,
    edWinServer2003_Enterprise,
    edWinServer2003_WebEdition,
    edWinServer2003,
    edVistaHome,
		edWinServer2003_I64_Datacenter,
		edWinServer2003_I64_Enterprise,
		edWinServer2003_I64,
		edWinServer2003_AMD64_Datacenter,
		edWinServer2003_AMD64_Enterprise,
		edWinServer2003_AMD64,
    edWinServer2008_Datacenter,
    edWinServer2008_Enterprise,
    edWinServer2008,
		edWinVistaBusiness,
		edWinVistaUltimate,
		edWinVistaEnterprise,
		edWinVistaHomeBasic,
		edWinVistaHomePremium,
		edWinVistaStarter,
		edWinServer2008_Datacenter_Full,
		edWinServer2008_Datacenter_Core,
		edWinServer2008_Enterprise_Full,
		edWinServer2008_Enterprise_Core,
		edWinServer2008_Enterprise_IA64,
		edWinServer2008_Standard_Full,
		edWinServer2008_Standard_Core,
		edWinServer2008_WebServer,
		edWinServer2008_SS_Enterprise,
		edWinServer2008_SS_Express,
		edWinServer2008_SS_Standard,
		edWinServer2008_SS_WorkGroup,
		edWinServer2008_SBE,
		edWinServer2008_SBS,
		edWinServer2008_SBS_Premium,
		edWinServer2008_Cluster,
    edUnknown);


type
// IOperatingSystemGrade coursely represents an operating system version.
// This object is suitable for use by installer script engines when
//  comparing the host operating system against a particular valid range
//  of supported system.
// The object can be encoded as a string. For example
//   '6.0' means vista or
//   '5.1.2' means XP with Service Pack 2.
// Note that XP x64 is encoded as '5.1' like other XP's even though the
//  windows API (GetVersion) will report 5.2 . This is not a bug. It is
//  deliberate.
// Only sufficient information is stored in this object for the basic
//  needs of install engines. For example, architecture and edition
//  are not part of an IOperatingSystemGrade object.

  IOperatingSystemGrade = interface
  ['{FA04FF89-50E6-4181-93F7-90E90DD2763C}']
      function GetOp: TOperatingSystem;
      function GetSP: integer;
      function GetTier: TTier;
      function GetAsString: string;

      function isComparable( const Ref: IOperatingSystemGrade): boolean;

      function CompareWith( const Ref: IOperatingSystemGrade): TComparisonResult;
      // You can't compare a client type with a server type for example.
      // Some comparisons are meaningless.
      // Returns crGreater if this object's system is superior to the reference

      property Op: TOperatingSystem   read GetOp;
      property ServicePack: integer   read GetSP;
      property Tier: TTier            read GetTier;
      property AsString: string       read GetAsString;  // Encode into a string memento.
    end;


// The following global variables store information about this system,
//  however don't use them until you have called procedure CheckOperatingSystem
//  at least once.

var
  Op: TOperatingSystem;
  SP: integer;
  Tier: TTier;
  Arch: TProcessorArchitecture;
  Edition: TEdition;



function ThisSystem: IOperatingSystemGrade;  // About 'this' system.

// The following function can be used
function SystemGrade_From_String( const Memento: string; Tier1: TTier): IOperatingSystemGrade;


procedure CheckOperatingSystem; // Initialises the above global variables.


{ EXAMPLE USE
  ===========

Suppose that we want to check that we are running XP or superior.
We can achieve this with the following function.

function AreWeRunningXPorBetter: boolean;
var
  ReferenceSystem: string;
  Ref: IOperatingSystemGrade;
begin                          isComparable
ReferenceSystem := '5.1';
CheckOperatingSystem;
Ref := SystemGrade_From_String( ReferenceSystem, Tier);
result := ThisSystem.isComparable( Ref) and  // Unrecognised o/s !
          ThisSystem.CompareWith ( Ref) in [crGreater, crEquivalent]
end;

}



implementation

















uses Classes, Windows, SysUtils, Math, Registry;

type
  TOperatingSystemGrade = class( TInterfacedObject, IOperatingSystemGrade)
    private
      FOp: TOperatingSystem;
      FSP: integer;
      FTier: TTier;

      function GetOp: TOperatingSystem;
      function GetSP: integer;
      function GetTier: TTier;
      function GetAsString: string;
      function isComparable( const Ref: IOperatingSystemGrade): boolean;
      function CompareWith( const Ref: IOperatingSystemGrade): TComparisonResult;

    public
      constructor CreateFromString( const Value: string; Tier1: TTier);
      constructor CreateThisSystem;
    end;

var
  hasCheckedOperatingSystem: boolean = False;

procedure Classify; forward;

procedure CheckOperatingSystem;
begin
if hasCheckedOperatingSystem then exit;
hasCheckedOperatingSystem := True;
// Only do this once, and only do if necessary!
Classify
end;



procedure InitUnit_OSVersion;
begin
hasCheckedOperatingSystem := False;
Op      := osUnrecognised;
SP      := -1;
Tier    := tClient;
Arch    := paUnknown;
Edition := edUnknown
end;


type
  TOSVersionInfoExA = record
    dwOSVersionInfoSize: DWord;
    dwMajorVersion     : DWord;
    dwMinorVersion     : DWord;
    dwBuildNumber      : DWord;
    dwPlatformId       : DWord;
    szCSDVersion       : array[ 0..127] of AnsiChar;
    wServicePackMajor  : Word;
    wServicePackMinor  : Word;
    wSuiteMask         : Word;
    wProductType       : Byte;
    wReserved          : Byte;
  end;

// http://msdn2.microsoft.com/en-us/library/ms724429.aspx

function GetVersionExA(var lpVersionInformation: TOSVersionInfoExA): BOOL;
  stdcall; external kernel32 name 'GetVersionExA';

type
TGetNativeSystemInfo = procedure( var lpSystemInfo: TSystemInfo); stdcall;


const
// This table is used for a preliminary (course grain) determination of the
//  operating system
Versions: array[0..9] of record
  FPltfm, FMajor: word; FMinor: integer; FOp: TOperatingSystem end = (
  (FPltfm: VER_PLATFORM_WIN32_WINDOWS; FMajor: 4; FMinor: 0; FOp:osArchaic), // Win 95, Win 95 OSR2, Win 95 OSR2.5
  (FPltfm: VER_PLATFORM_WIN32_NT     ; FMajor: 3; FMinor:51; FOp:osArchaic), // NT 3.51
  (FPltfm: VER_PLATFORM_WIN32_NT     ; FMajor: 3; FMinor:-1; FOp:osArchaic), // NT 3.5 ?
  (FPltfm: VER_PLATFORM_WIN32_WINDOWS; FMajor: 4; FMinor:10; FOp:osWin98),   // Win 98, Win 98 SE
  (FPltfm: VER_PLATFORM_WIN32_WINDOWS; FMajor: 4; FMinor:90; FOp:osWinME),   // Win ME
  (FPltfm: VER_PLATFORM_WIN32_NT     ; FMajor: 4; FMinor: 0; FOp:osNT),      // NT
  (FPltfm: VER_PLATFORM_WIN32_NT     ; FMajor: 5; FMinor: 0; FOp:os2000),    // Windows 2000
  (FPltfm: VER_PLATFORM_WIN32_NT     ; FMajor: 5; FMinor: 1; FOp:osXP),      // XP
  (FPltfm: VER_PLATFORM_WIN32_NT     ; FMajor: 5; FMinor: 2; FOp:os2003),    // Server 2003, Server 2003 R2 or XP x64
  (FPltfm: VER_PLATFORM_WIN32_NT     ; FMajor: 6; FMinor: 0; FOp:osVista));  // Vista, Longhorn Server or Server 2008


// This table is use for string memento representation of the operating system
//  level, at a course grain.
MementoVersions: array[ TOperatingSystem ] of record
  FMajor, FMinor: integer end = (
  { osUnrecognised ==> } (FMajor: -1; FMinor:  0),  // -1 means unknown
  { osArchaic      ==> } (FMajor: -1; FMinor:  0),
  { osWin98        ==> } (FMajor:  4; FMinor: 10),
  { osWin98SE      ==> } (FMajor:  4; FMinor: 10),
  { osWinME        ==> } (FMajor:  4; FMinor: 90),
  { osNT           ==> } (FMajor:  4; FMinor:  0),
  { os2000         ==> } (FMajor:  5; FMinor:  0),
  { osXP           ==> } (FMajor:  5; FMinor:  1),
  { os2003         ==> } (FMajor:  5; FMinor:  2),
  { os2003R2       ==> } (FMajor:  5; FMinor:  2),
  { osVista        ==> } (FMajor:  6; FMinor:  0),
  { os2008         ==> } (FMajor:  5; FMinor:  0),
  { osPost2008     ==> } (FMajor: -1; FMinor:  0));


function PreliminaryClassification(
 const Info: TOSVersionInfoExA): TOperatingSystem;
var
  j: integer;
  p,m,n: word;
begin
p := Info.dwPlatformId;
m := Info.dwMajorVersion;
n := Info.dwMinorVersion;
result := osUnrecognised;
for j := Low( Versions) to High( Versions) do
  begin
  if (Versions[j].FPltfm <> p) or
     (Versions[j].FMajor <> m) or
     ((Versions[j].FMinor <> -1) and (Versions[j].FMinor <> n)) then
        continue;
  result := Versions[j].FOp;
  break
  end;
j := High( Versions);
if (result = osUnrecognised) and (Versions[j].FPltfm = p) and (
  ((m = Versions[j].FMajor) and (n > Versions[j].FMinor)) or
   (m > Versions[j].FMajor)) then
      result := osPost2008
end;




function GetBasicVersionInfo(
  var Info: TOSVersionInfoExA; var isExtended: boolean): boolean;
begin
ZeroMemory( @Info, SizeOf( Info));
Info.dwOSVersionInfoSize := SizeOf( Info);
result     := GetVersionExA( Info);
isExtended := result;
if not result then
  begin
  Info.dwOSVersionInfoSize := SizeOf( windows.TOSVersionInfoA);
  result := GetVersionExA( Info);
  end
end;


function Extract_CDS_Version( const Info: TOSVersionInfoExA): string;
var
  L: integer;
begin
L := Min( StrLen( @Info.szCSDVersion), SizeOf( Info.szCSDVersion));
SetLength( result, L);
if L > 0 then
  Move( Info.szCSDVersion, result[1], L);
result := Trim( result)
end;


function ComputeServicePack( const CDS: string): integer;
const
  Prefix = 'Service Pack ';
var
  s: string;
  V: integer;
  Code: integer;
begin
result := 0;
if Pos( Prefix, CDS) = 1 then
    begin
    s := CDS;
    Delete( s, 1, Length( Prefix));
    Val( s, V, Code);
    if (s <> '') and (Code = 0) then
      result := V
    end
end;


function CDS_FirstLetter( const Info: TOSVersionInfoExA): char;
var
  s: string;
begin
s := Extract_CDS_Version( Info);
if s = '' then
    result := ' '
  else
    result := s[1]
end;




function isNT_ServicePack6a: boolean;
// Assume that we have already detected that this is NT SP6 or SP6a
var
  Reg: TRegistry;
begin
Reg := TRegistry.Create;
try
  Reg.RootKey := HKEY_LOCAL_MACHINE;
  result := Reg.OpenKeyReadOnly(
    '\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Hotfix\Q246009')
finally
  Reg.Free
  end
end;

const
  PROCESSOR_ARCHITECTURE_AMD64 = 9; // x64 (AMD or Intel)
  PROCESSOR_ARCHITECTURE_IA64 = 6; // Intel Itanium Processor Family (IPF)
  PROCESSOR_ARCHITECTURE_INTEL = 0; //x86
  PROCESSOR_ARCHITECTURE_UNKNOWN = $FFFF;  // Unknown processor.

  Architectures: array[ 0..2 ] of record
  FCode: word; FArch: TProcessorArchitecture end = (
   (FCode: PROCESSOR_ARCHITECTURE_INTEL; FArch: paIntel),
   (FCode: PROCESSOR_ARCHITECTURE_AMD64; FArch: paAMD64),
   (FCode: PROCESSOR_ARCHITECTURE_IA64 ; FArch: paIA64));

function Architecture: TProcessorArchitecture;
var
  KernelLib: integer;
  GetNativeSystemInfoProc: TGetNativeSystemInfo;
  Ok: boolean;
  SystemInfo: TSystemInfo;
  j: integer;
begin
result := paUnknown;
GetNativeSystemInfoProc := nil;
KernelLib := SafeLoadLibrary( 'Kernel32.dll');
try
  if KernelLib <> 0 then
    GetNativeSystemInfoProc := GetProcAddress(
      KernelLib, 'GetNativeSystemInfoA');
  Ok := assigned( GetNativeSystemInfoProc);
  if Ok then
    GetNativeSystemInfoProc( SystemInfo)
finally
  if KernelLib <> 0 then
    FreeLibrary( KernelLib)
  end;
if not Ok then
  windows.GetSystemInfo( SystemInfo);
for j := Low( Architectures) to High( Architectures) do
  begin
  if Architectures[j].FCode <> SystemInfo.wProcessorArchitecture then
    continue;
  result := Architectures[j].FArch;
  break
  end;
end;



function isMSLU: boolean;
// Has the MicroSoft Layer for Unicode been installed?
// Assume this is Windows 95/98/ME
var
  lpWndClass: TWndClassW;
  ClassName1: widestring;
  Atom: TAtom;
begin
ClassName1 := 'Random234f4';
with lpWndClass do
  begin
  style := 0;
  //lpfnWndProc: TFNWndProc;
  cbClsExtra := 0;
  //cbWndExtra: Integer;
  //  hInstance: HINST;
 //   hIcon: HICON;
  hCursor := 0;
  hbrBackground := 0;
  lpszMenuName  := nil;
  lpszClassName := PWideChar( ClassName1)
  end;
Atom := RegisterClassW( lpWndClass);
result := (Atom <> 0) and
          UnregisterClassW( PWideChar( ClassName1), lpWndClass.hInstance)
end;


function hasSuite( const Info: TOSVersionInfoExA; Mask: word): boolean;
begin
result := (Info.wSuiteMask and Mask) <> 0
end;

const
  VER_NT_DOMAIN_CONTROLLER = $0000002; //	The system is a domain controller.
  VER_NT_SERVER = $0000003; // The system is a server.
    // Note that a server that is also a domain controller is reported as
    //  VER_NT_DOMAIN_CONTROLLER, not VER_NT_SERVER.
  VER_NT_WORKSTATION = $0000001; // The operating system is Windows Vista,
    // Windows XP Professional, Windows XP Home Edition, Windows 2000
    //  Professional, or Windows NT Workstation 4.0.

  SM_SERVERR2 = 89;


function RegProductType: string;
var
  Reg: TRegistry;
begin
Reg := TRegistry.Create;
try
  Reg.RootKey := HKEY_LOCAL_MACHINE;
  if Reg.OpenKeyReadOnly(
    '\SYSTEM\CurrentControlSet\Control\ProductOptions') then
      result := Reg.ReadString( 'ProductType')
    else
      result := ''
finally
  Reg.Free
  end
end;

type TGetProductInfoProc = function (
  dwOSMajorVersion, dwOSMinorVersion: DWORD;
  dwSpMajorVersion, dwSpMinorVersion: DWORD;
  var pdwReturnedProductType: DWORD): BOOL; stdcall;


function GetProductType( const Info: TOSVersionInfoExA): DWord;
// Assumption: This may only be called if Vista or Server 2008
//  and isExtended.
var
  Proc: TGetProductInfoProc;
  KernelLib: cardinal;
begin
result := 0;
KernelLib := SafeLoadLibrary( 'Kernel32.dll');
try
  Proc := GetProcAddress( KernelLib, 'GetProductInfo');
  if assigned( Proc) then
    Proc( 6, 0, Info.wServicePackMajor, Info.wServicePackMinor, result)
finally
  if KernelLib <> 0 then
    FreeLibrary( KernelLib)
  end
end;



const
  VER_SUITE_ENTERPRISE = $00000002;
  VER_SUITE_DATACENTER = $00000080;
  VER_SUITE_PERSONAL   = $00000200;
  VER_SUITE_BLADE      = $00000400;
  SM_MEDIACENTER = 87;  // Detects XP Media Center Edition
  SM_TABLETPC = 86;     // Detects XP Tablet Edition
  SM_STARTER = 88;

PRODUCT_BUSINESS = $00000006; // 	Business Edition
PRODUCT_BUSINESS_N = $00000010; // 	Business Edition
PRODUCT_CLUSTER_SERVER = $00000012; // 	Cluster Server Edition
PRODUCT_DATACENTER_SERVER = $00000008; // 	Server Datacenter Edition (full installation)
PRODUCT_DATACENTER_SERVER_CORE = $0000000C; // 	Server Datacenter Edition (core installation)
PRODUCT_ENTERPRISE = $00000004; // 	Enterprise Edition
PRODUCT_ENTERPRISE_N = $0000001B; // 	Enterprise Edition
PRODUCT_ENTERPRISE_SERVER = $0000000A; // 	Server Enterprise Edition (full installation)
PRODUCT_ENTERPRISE_SERVER_CORE = $0000000E; // 	Server Enterprise Edition (core installation)
PRODUCT_ENTERPRISE_SERVER_IA64 = $0000000F; // 	Server Enterprise Edition for Itanium-based Systems
PRODUCT_HOME_BASIC = $00000002; // 	Home Basic Edition
PRODUCT_HOME_BASIC_N = $00000005; // 	Home Basic Edition
PRODUCT_HOME_PREMIUM = $00000003; // 	Home Premium Edition
PRODUCT_HOME_PREMIUM_N = $0000001A; // 	Home Premium Edition
PRODUCT_HOME_SERVER = $00000013; // 	Home Server Edition
PRODUCT_SERVER_FOR_SMALLBUSINESS = $00000018; // 	Server for Small Business Edition
PRODUCT_SMALLBUSINESS_SERVER = $00000009; // 	Small Business Server
PRODUCT_SMALLBUSINESS_SERVER_PREMIUM = $00000019; // 	Small Business Server Premium Edition
PRODUCT_STANDARD_SERVER = $00000007; // 	Server Standard Edition (full installation)
PRODUCT_STANDARD_SERVER_CORE = $0000000D; // 	Server Standard Edition (core installation)
PRODUCT_STARTER = $0000000B; // 	Starter Edition
PRODUCT_STORAGE_ENTERPRISE_SERVER = $00000017; // 	Storage Server Enterprise Edition
PRODUCT_STORAGE_EXPRESS_SERVER = $00000014; // 	Storage Server Express Edition
PRODUCT_STORAGE_STANDARD_SERVER = $00000015; // 	Storage Server Standard Edition
PRODUCT_STORAGE_WORKGROUP_SERVER = $00000016; // 	Storage Server Workgroup Edition
PRODUCT_UNDEFINED = $00000000; // 	An unknown product
PRODUCT_ULTIMATE = $00000001; // 	Ultimate Edition
PRODUCT_ULTIMATE_N = $0000001C; // 	Ultimate Edition
PRODUCT_WEB_SERVER = $00000011;



function hasSystemMetrics( nIndex: Integer): boolean;
begin
result := GetSystemMetrics( nIndex) <> 0
end;



type TEditionTest = record
  FOS: TOperatingSystem;
  FTier: TTier;
  FCDS: Char;
  FisExt: boolean;
  FSuite: word;
  FsRegProd: string;
  FMetric: integer;
  FArch: TProcessorArchitecture;
  FProdType: cardinal;
  FEdition: TEdition;
  end;








function RefineArchaic( const Info: TOSVersionInfoExA; FirstChar: Char;
  var isTierKnown1: boolean): TEdition; forward;

function RefineWin98( FirstChar: Char; var Op1: TOperatingSystem;
  var isTierKnown1: boolean): TEdition; forward;

function RefineNT( const Info: TOSVersionInfoExA; isExtended: boolean;
  var SP1: integer): TEdition; forward;

function Refine2000( const Info: TOSVersionInfoExA; isExtended: boolean): TEdition; forward;

function RefineXP( const Info: TOSVersionInfoExA; isExtended: boolean): TEdition; forward;

function Refine2003( const Info: TOSVersionInfoExA; isExtended: boolean;
  Tier1: TTier; Arch1: TProcessorArchitecture; var Op1: TOperatingSystem)
  : TEdition; forward;

function RefineVista( const Info: TOSVersionInfoExA; isExtended: boolean;
  Tier1: TTier; var Op1: TOperatingSystem): TEdition; forward;





procedure Classify;
var
  Info: TOSVersionInfoExA;
  isExtended: boolean;
  FirstChar: char;
  isTierKnown: boolean;
begin
if not GetBasicVersionInfo( Info, isExtended) then
  begin
  Op := osUnrecognised;
  exit
  end;
SP := ComputeServicePack( Extract_CDS_Version( Info));
Op := PreliminaryClassification( Info);
if Op = osUnrecognised then exit;
Tier := tClient;
isTierKnown := False;
if isExtended then
    case Info.wProductType of
      VER_NT_WORKSTATION:
        begin
        Tier := tClient;
        isTierKnown := True
        end;

      VER_NT_DOMAIN_CONTROLLER, VER_NT_SERVER:
        begin
        Tier := tServer;
        isTierKnown := True
        end
      end;
Arch := Architecture;
FirstChar := CDS_FirstLetter( Info);
Edition := edUnknown;
case Op of
  osArchaic:
    Edition := RefineArchaic( Info, FirstChar, isTierKnown);

  osWin98, osWin98SE:
    Edition := RefineWin98( FirstChar, Op, isTierKnown);

  osWinME:
    begin
    isTierKnown := True;
    Edition := edWinME
    end;

  osNT:
    Edition := RefineNT( Info, isExtended, SP);

  os2000:
    Edition := Refine2000( Info, isExtended);

  osXP:
    Edition := RefineXP( Info, isExtended);

  os2003, os2003R2:
    Edition := Refine2003( Info, isExtended, Tier, Arch, Op);

  osVista, os2008:
    Edition := RefineVista( Info, isExtended, Tier, Op);
  end;

if (not isTierKnown) and (Op in [os2003, os2003R2, os2008]) then
  begin
  isTierKnown := True;
  Tier := tServer
  end;
if (not isTierKnown) and (Edition in [
      edWinNT_40_Server_Enterprise, edWinNT_40_Server,
      edWin2000_Server_Datacenter,  edWin2000_Server_Advanced,
      edWin2000_Server]) then
  Tier := tServer
end;







function RefineArchaic( const Info: TOSVersionInfoExA; FirstChar: Char;
  var isTierKnown1: boolean): TEdition;
begin
// Windows 95 (including OSR2 and OSR2.5), Windows NT3.50 and Windows NT3.51
    if Info.dwPlatformId = VER_PLATFORM_WIN32_WINDOWS then
        begin // Win95
        isTierKnown1 := True;
        if (FirstChar= 'C') or (FirstChar = 'B') then
            result := edWin95_OSR2
          else
            result := edWin95
        end
      else
        result := edNT_35
end;



function RefineWin98( FirstChar: Char; var Op1: TOperatingSystem;
  var isTierKnown1: boolean): TEdition;
begin
// Windows 98, Windows 98 SE
isTierKnown1 := True;
if FirstChar = 'A' then
    begin
    Op1     := osWin98;
    result  := edWin98
    end
  else
    begin
    Op1     := osWin98SE;
    result  := edWin98_SE
    end
end;



function RefineNT( const Info: TOSVersionInfoExA; isExtended: boolean;
  var SP1: integer): TEdition;
var
  sRegProductType: string;
begin
// Windows NT4.0
if (SP1 = 6) and isNT_ServicePack6a then
  SP1 := 7;
if isExtended then
    begin
    if Info.wProductType = VER_NT_WORKSTATION then
        result := edWinNT_40_Workstation
      else if hasSuite( Info, VER_SUITE_ENTERPRISE) then
        result := edWinNT_40_Server_Enterprise
      else
        result := edWinNT_40_Server
    end
  else
    begin
    sRegProductType := RegProductType;
    if sRegProductType = 'WINNT' then
        result := edWinNT_40_Workstation
      else if sRegProductType = 'SERVERNT' then
        result := edWinNT_40_Server_Enterprise
      else if sRegProductType = 'LANMANNT' then
        result := edWinNT_40_Server
      else
        result := edUnknown
    end
end;



function Refine2000( const Info: TOSVersionInfoExA; isExtended: boolean): TEdition;
var
  sRegProductType: string;
begin
// Windows 2000 (Including Pro, Server and Advanced Server)
if isExtended then
        begin
        if Info.wProductType = VER_NT_WORKSTATION then
            result := edWin2000_Professional
          else if hasSuite( Info, VER_SUITE_DATACENTER) then
            // wProductType = VER_NT_SERVER || VER_NT_DOMAIN_CONTROLLER
            result := edWin2000_Server_Datacenter
          else if hasSuite( Info, VER_SUITE_ENTERPRISE) then
            result := edWin2000_Server_Advanced
          else
            result := edWin2000_Server
        end
      else
        begin
        sRegProductType := RegProductType;
        if sRegProductType = 'SERVERNT' then
            result := edWin2000_Server_Advanced
          else if sRegProductType = 'LANMANNT' then
            result := edWin2000_Server
          else
            result := edUnknown
        end
end;



function RefineXP( const Info: TOSVersionInfoExA; isExtended: boolean): TEdition;
begin
result := edUnknown;
// Windows XP
if hasSystemMetrics( SM_MEDIACENTER) then
    result := edWinXP_MediaCenter
  else if hasSystemMetrics( SM_STARTER) then
    result := edWinXP_Starter
  else if hasSystemMetrics( SM_TABLETPC) then
    result := edWinXP_TabletPC
  else if isExtended then
    begin
    if hasSuite( Info, VER_SUITE_PERSONAL) then
        result := edWinXP_Home
      else
        result := edWinXP_Professional
    end
end;



function Refine2003( const Info: TOSVersionInfoExA; isExtended: boolean;
  Tier1: TTier; Arch1: TProcessorArchitecture; var Op1: TOperatingSystem)
  : TEdition; 
begin
// Windows Server 2003, Windows Server 2003 R2
result := edUnknown;
    if hasSystemMetrics( SM_SERVERR2) then
        Op1 := os2003R2
      else if (Tier1 = tClient) and (Arch1 = paAMD64) then
        begin
        // If we are os2003, then isTierKnown must be True.
        Op1 := osXP;
        result := edWinXP_x64
        end
      else
        Op1 := os2003;
    if (Op1 <> osXP) and isExtended then
      begin
      if Arch1 = paIA64 then
          begin
          if hasSuite( Info, VER_SUITE_DATACENTER) then
							result := edWinServer2003_I64_Datacenter
						else if hasSuite( Info, VER_SUITE_ENTERPRISE) then
							result := edWinServer2003_I64_Enterprise
						else
							result := edWinServer2003_I64
          end
        else if Arch1 = paAMD64 then
          begin
          if hasSuite( Info, VER_SUITE_DATACENTER) then
							result := edWinServer2003_AMD64_Datacenter
						else if hasSuite( Info, VER_SUITE_ENTERPRISE) then
							result := edWinServer2003_AMD64_Enterprise
						else
							result := edWinServer2003_AMD64
          end
        else if hasSuite( Info, VER_SUITE_DATACENTER) then
          result := edWinServer2003_Datacenter
        else if hasSuite( Info, VER_SUITE_ENTERPRISE) then
          result := edWinServer2003_Enterprise
        else if hasSuite( Info, VER_SUITE_BLADE) then
          result := edWinServer2003_WebEdition
        else
          result := edWinServer2003
      end
end;



function RefineVista( const Info: TOSVersionInfoExA; isExtended: boolean;
  Tier1: TTier; var Op1: TOperatingSystem): TEdition;
var
  ProductType: Cardinal;
begin
result := edUnknown;
    if isExtended then
        ProductType := GetProductType( Info)
      else
        ProductType := 0;
    case Tier1 of
      tClient:  Op1 := osVista;
      tServer:  Op1 := os2008
      end;
    if (Op1 = osVista) and isExtended and
        hasSuite( Info, VER_SUITE_PERSONAL) then
      result := edVistaHome;
    if (Op1 = os2008) and isExtended then
      begin
      if hasSuite( Info, VER_SUITE_DATACENTER) then
          result := edWinServer2008_Datacenter
        else if hasSuite( Info, VER_SUITE_ENTERPRISE) then
          result := edWinServer2008_Enterprise
        else
					result := edWinServer2008
      end;

case ProductType of
			PRODUCT_BUSINESS, PRODUCT_BUSINESS_N:
				result := edWinVistaBusiness;
			PRODUCT_ULTIMATE, PRODUCT_ULTIMATE_N:
				result := edWinVistaUltimate;
			PRODUCT_ENTERPRISE, PRODUCT_ENTERPRISE_N:
				result := edWinVistaEnterprise;
		  PRODUCT_HOME_BASIC, PRODUCT_HOME_BASIC_N:
				result := edWinVistaHomeBasic;
			PRODUCT_HOME_PREMIUM, PRODUCT_HOME_PREMIUM_N:
				result := edWinVistaHomePremium;
			PRODUCT_STARTER:
				result := edWinVistaStarter;

			PRODUCT_DATACENTER_SERVER:
				result := edWinServer2008_Datacenter_Full;
			PRODUCT_DATACENTER_SERVER_CORE:
				result := edWinServer2008_Datacenter_Core;
			PRODUCT_ENTERPRISE_SERVER:
				result := edWinServer2008_Enterprise_Full;
			PRODUCT_ENTERPRISE_SERVER_CORE:
				result := edWinServer2008_Enterprise_Core;
			PRODUCT_ENTERPRISE_SERVER_IA64:
				result := edWinServer2008_Enterprise_IA64;
			PRODUCT_STANDARD_SERVER:
				result := edWinServer2008_Standard_Full;
			PRODUCT_STANDARD_SERVER_CORE:
				result := edWinServer2008_Standard_Core;
			PRODUCT_WEB_SERVER:
				result := edWinServer2008_WebServer;
      PRODUCT_STORAGE_ENTERPRISE_SERVER:
				result := edWinServer2008_SS_Enterprise;
      PRODUCT_STORAGE_EXPRESS_SERVER:
				result := edWinServer2008_SS_Express;
      PRODUCT_STORAGE_STANDARD_SERVER:
				result := edWinServer2008_SS_Standard;
      PRODUCT_STORAGE_WORKGROUP_SERVER:
				result := edWinServer2008_SS_WorkGroup;
      PRODUCT_SERVER_FOR_SMALLBUSINESS:
				result := edWinServer2008_SBE;
      PRODUCT_SMALLBUSINESS_SERVER:
				result := edWinServer2008_SBS;
      PRODUCT_SMALLBUSINESS_SERVER_PREMIUM:
				result := edWinServer2008_SBS_Premium;
      PRODUCT_CLUSTER_SERVER:
				result := edWinServer2008_Cluster;
      end
end;



function ThisSystem: IOperatingSystemGrade;
begin
result := TOperatingSystemGrade.CreateThisSystem
end;





function SystemGrade_From_String( const Memento: string; Tier1: TTier): IOperatingSystemGrade;
begin
result := TOperatingSystemGrade.CreateFromString( Memento, Tier1)
end;






{ TOperatingSystemGrade }

function TOperatingSystemGrade.CompareWith(
  const Ref: IOperatingSystemGrade): TComparisonResult;
begin
Assert( isComparable( Ref), 'Operating systems are not comparable!');
if (FOp = Ref.Op) and (FSP = Ref.ServicePack) then
    result := crEquivalent
  else if (FOp > Ref.Op) or ((FOp = Ref.Op) and (FSP > Ref.ServicePack)) then
    result := crGreater
  else
    result := crLess
end;





constructor TOperatingSystemGrade.CreateFromString( const Value: string; Tier1: TTier);
// Read a string like '4.1.2' and convert to our native format.
// The last two places (minor & SP) are optional.
var
  Maj, Min, P: integer;
  s, s2: string;
  j: TOperatingSystem;
  v, c: integer;

  function ReadInt( var sInt: string): integer;
  begin
  P := Pos('.',sInt);
  if P > 0 then
    SetLength( sInt, P - 1);
  // s is now the minor version as string
  Val( sInt, v, c);
  if sInt = '' then
      result := 0   // default to 0
    else if (c > 0) and (v >= 0) then
      result := v
    else
      result := -1 // Its how we represent a conversion error.
  end;

begin
s := Value;
P := Pos('.',s);
if P > 0 then
  SetLength( s, P - 1);
// s is now the major version as string.
Val( s, v, c);
if (s <> '') and (c > 0) and (v > 0) then
    Maj := v
  else
    Maj := -1;
// now to extract the minor version
s  := Value;
s2 := s;
if P > 0 then
  Delete( s, 1, P);
Min := ReadInt( s);
if Min < 0 then
  begin
  Min := 0;
  Maj := -1
  end;
s := s2;
if P > 0 then
  Delete( s, 1, P);
SP := ReadInt( s);
if SP < 0 then
  begin
  SP  := 0;
  Maj := -1
  end;
FOp := osUnrecognised;
FSP := 0;
FTier := Tier1;
if Maj <> -1 then
    for j := Low( MementoVersions) to High( MementoVersions) do
      begin
      if (MementoVersions[j].FMajor <> Maj) or
         (MementoVersions[j].FMinor <> Min) then continue;
      FOp := j;
      FSP := SP;
      break
      end
end;




constructor TOperatingSystemGrade.CreateThisSystem;
begin
CheckOperatingSystem;
FOp   := Op;
FSP   := SP;
FTier := Tier
end;





function TOperatingSystemGrade.GetAsString: string;
begin
if (FOp in [osUnrecognised, osArchaic, osPost2008]) or
   (MementoVersions[ FOp].FMajor = -1) then
    result := 'error'
  else
    result := Format( '%d.%d.%d', [
      MementoVersions[ FOp].FMajor,
      MementoVersions[ FOp].FMinor, FSP])
end;





function TOperatingSystemGrade.GetOp: TOperatingSystem;
begin
result := FOp
end;





function TOperatingSystemGrade.GetSP: integer;
begin
result := FSP
end;




function TOperatingSystemGrade.GetTier: TTier;
begin
result := FTier
end;





function TOperatingSystemGrade.isComparable( const Ref: IOperatingSystemGrade): boolean;
begin
result :=
  (   FOp <> osUnrecognised) and
  (Ref.Op <> osUnrecognised) and
  ((FOp <> osArchaic ) or (Ref.Op <> osArchaic )) and
  ((FOp <> osPost2008) or (Ref.Op <> osPost2008)) and
  (FTier <> Ref.Tier)
end;





initialization
InitUnit_OSVersion;

end.
