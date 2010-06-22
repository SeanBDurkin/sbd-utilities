{*****************************************************************************}
{                                                                             }
{    SBD Low Level Utility library                                            }
{        Version: 2.0                                                         }
{                                                                             }
{ Parts of this unit are copied from sources as indicated. For these          }
{ parts the copyright belongs to the original author.                         }
{ In relation to the balance of this unit:                                    }
{    Copyright (c) 2003-2010, Sean B. Durkin (sean@seanbdurkin.id.au)         }
{ The public is hereby granted free and unrestricted license to use and       }
{ to copy this unit.                                                          }
{                                                                             }
{*****************************************************************************}

unit uSBD_OS_Version;
// This unit will sense the operating system related stuff, including:
//   * Operating System
//   * The TRUE operating system, if this is being run under a compatibility mode.
//   * version numbers
//   * Edition
//   * Service pack
//   * Architecture
//   * 32 or 64 bit
//   * Virtual or native machine
//   * Whether or not the system supports UCS-2 (wide strings)
// If the application is running under a compatibility layer, then
//  these compute the simulated operating system.
// Works from Delphi 7 through to Delphi 2010.
// Should work on wintel operating systems from Win95 through to Windows 7.
// Very little testing done. Use at your own risk.
interface
type

TOS_Tier = ( // Operating System Tier, also known as product type.
  tWorkStation,  // WorkStation product type. Eg: XP
  tServer        // Either Server or Domain Controller product type. Eg: Server 2008
  );

TProcessorArchitecture = (
  paIntel,    // x86. 32 bit.
  paAMD64,    // x64 (AMD or Intel). 64 bit.
  paIA64,     // Intel Itanium Processor Family (IPF). 64 bit.
  paUnknown); // Unknown processor.

TNativeWordSize = (
  nw32,    // 32 bit operating system.
  nw64);   // 64 bit operating system.

TVirtualizationMode = (
  vmNative,        // This is a true physical machine.
  vmVMware,        // This is a VMware virtual machine.
  vmMSVirtualPC,   // This is a Windows Virtual PC virtual machine.
  vmWine,          // Using Wine.
  vmVirtualBox);   // This is running under Virtual Box.


TOperatingSystem = (
  osUnrecognised,  // Any thing, not on the following list.
  osArchaic,       // Windows 95 (including OSR2 and OSR2.5), Windows NT3.50 and Windows NT3.51
  osWin98,         // Windows 98 and Windows 98SE
  osWinME,         // Windows ME
  osNT,            // Windows NT4.0
  os2000,          // Windows 2000 (Including Pro, Server and Advanced Server)
  osXP,            // Windows XP
  os2003,          // Windows Server 2003 and Windows Server 2003 R2
  osVista,         // Windows Vista
  os2008,          // Windows Server 2008
  osWindows7,      // Windows 7
  osPost2010);     // Any client later than Windows 7, or any server later than 2008.

TOS_Edition = (
// Archaic editions
    edWin95,
    edWin95_OSR2,
    edNT_35,

// Win98 editions
   edWin98,
   edWin98_SE,

// WinME editions
    edWinME,

// NT editions
    edWinNT_40_Workstation,
    edWinNT_40_Server_Enterprise,
    edWinNT_40_Server,

// Windows 2000 editions
    edWin2000_Professional,
    edWin2000_Server_Datacenter,
    edWin2000_Server_Advanced,
    edWin2000_Server,

// XP editions
    edWinXP_Home,
    edWinXP_Professional,
    edWinXP_64bit,
    edWinXP_MediaCenter,
    edWinXP_Starter,
    edWinXP_TabletPC,

// Server 2003 editions
    edWinServer2003_Datacenter,
    edWinServer2003_Enterprise,
    edWinServer2003_WebEdition,
    edWinServer2003_Standard,
		edWinServer2003_64bit,
    edWinServer2003_HomeServer,

// Vista editions
		edVista_Business,
		edVista_Ultimate,
		edVista_Enterprise,
		edVista_HomeBasic,
		edVista_HomePremium,
		edVista_Starter,

// Server 2008 editions
    edServer2008_Datacenter,
    edServer2008_Enterprise,
		edServer2008_Enterprise_IA64,
    edServer2008,
		edServer2008_WebServer,
		edServer2008_SS_Enterprise,
		edServer2008_SS_Express,
		edServer2008_SS_Standard,
		edServer2008_SS_WorkGroup,
		edServer2008_SBE,
		edServer2008_SBS,
		edServer2008_SBS_Premium,
		edServer2008_Cluster,

// Windows 7 editions
    edWin7_Ultimate,
    edWin7_Business,
    edWin7_Enterprise,
    edWin7_HomeBasic,
    edWin7_Premium,
    edWin7_Starter,

// Other
    edUnknown);


TOS_LocalityBasedVariety = (
  varStandard,   // Like Windows 7 Ultimate
  varN_Type);    // Like Windows 7 Ultimate N


TOS_Datum = class
  public
    FOS: TOperatingSystem;
    FTrueSystem: TOperatingSystem;
    FEdition: TOS_Edition;
    FTier: TOS_Tier;
    FArch: TProcessorArchitecture;
    FBits: TNativeWordSize;
    FVMode: TVirtualizationMode;
    FVariety: TOS_LocalityBasedVariety;
    FMajorVersion, FMinorVersion: cardinal; // 0 means unknown.
    FMajorRelease: cardinal;  // 0 means standard
                              // 2 means release 2. Eg: Windows Server 2008 R2 or Win95 OSR2
                              // 3 means release 3. Eg: Win95 OSR2.5
    FServicePack: cardinal; // 0 means no service packs.
                            // 1 means Service Pack 1 (SP1) applied.
                            // 2 means SP2 or Release 2 (R2). Eg: Windows Server 2008 R2
    FBuildNumber: cardinal;
    FUCS2: boolean;  // False ==> Only Ansi character set available.
                     // True  ==> UCS-2 character set is available.
                     //   The 'W' or 'Wide' versions of windows API calls are available.
                     //   This is often incorrectly called 'unicode'.
  end;


resourcestring
  SOperatingSystem_osUnrecognised = 'Unrecognised';
  SOperatingSystem_osArchaic      = 'Windows 95, Windows NT3.50 or Windows NT3.51 ';
  SOperatingSystem_osWin98        = 'Windows 98 or Windows 98SE';
  SOperatingSystem_osWinME        = 'Windows ME';
  SOperatingSystem_osNT           = 'Windows NT4';
  SOperatingSystem_os2000         = 'Windows 2000';
  SOperatingSystem_osXP           = 'Windows XP';
  SOperatingSystem_os2003         = 'Windows Server 2003';
  SOperatingSystem_osVista        = 'Windows Vista';
  SOperatingSystem_os2008         = 'Windows Server 2008';
  SOperatingSystem_osWindows7     = 'Windows 7';
  SOperatingSystem_osPost2010     = 'Unrecognised system higher than Windows 7/ Windows Server 2008.';

  STier_tWorkStation = 'WorkStation';
  STier_tServer      = 'Server';

  SProcessorArchitecture_paIntel   = 'Intel x86, 32 bit';
  SProcessorArchitecture_paAMD64   = 'AMD, 64 bit';
  SProcessorArchitecture_paIA64    = 'Intel Itanium, 64 bit';
  SProcessorArchitecture_paUnknown = 'Unrecognised processor';

  SNativeWordSize_nw32  = '32 bit';
  SNativeWordSize_nw64  = '64 bit';

  SVirtualizationMode_vmNative      = 'Real machine';
  SVirtualizationMode_vmVMware      = 'VMware virtual machine';
  SVirtualizationMode_vmMSVirtualPC = 'Windows Virtual PC';
  SVirtualizationMode_vmWine        = 'Wine virtual machine';
  SVirtualizationMode_vmVirtualBox  = 'Virtual Box';

  SOS_Edition_edWin95 = 'Win 95 (original)';
  SOS_Edition_edWin95_OSR2= 'Win 95 OS2';
  SOS_Edition_edNT_35= 'NT 3.5';
  SOS_Edition_edWin98= 'Win 98';
  SOS_Edition_edWin98_SE= 'Win 98SE';
  SOS_Edition_edWinME= 'Win ME';
  SOS_Edition_edWinNT_40_Workstation= 'NT4 Workstation';
  SOS_Edition_edWinNT_40_Server_Enterprise= 'NT4 Server Enterprise';
  SOS_Edition_edWinNT_40_Server= 'NT 4 Server Standard';
  SOS_Edition_edWin2000_Professional= 'Win2000 Professional';
  SOS_Edition_edWin2000_Server_Datacenter= 'Win2000 Server Datacenter';
  SOS_Edition_edWin2000_Server_Advanced= 'Win2000 Server Advanced';
  SOS_Edition_edWin2000_Server= 'Win2000 Server Standard';
  SOS_Edition_edWinXP_Home= 'XP Home';
  SOS_Edition_edWinXP_Professional= 'XP Professional';
  SOS_Edition_edWinXP_64bit= 'XP 64 bit';
  SOS_Edition_edWinXP_MediaCenter= 'XP MediaCenter';
  SOS_Edition_edWinXP_Starter= 'XP Starter';
  SOS_Edition_edWinXP_TabletPC= 'XP Tablet PC';
  SOS_Edition_edWinServer2003_Datacenter= 'Server 2003 Datacenter';
  SOS_Edition_edWinServer2003_Enterprise= 'Server 2003 Enterprise';
  SOS_Edition_edWinServer2003_WebEdition= 'Server 2003 WebEdition';
  SOS_Edition_edWinServer2003_Standard= 'Server 2003 Standard';
  SOS_Edition_edWinServer2003_64bit= 'Server 2003 64 bit';
  SOS_Edition_edWinServer2003_HomeServer= 'Server 2003 Home Server';
  SOS_Edition_edVista_Business= 'Vista Business';
  SOS_Edition_edVista_Ultimate= 'Vista Ultimate';
  SOS_Edition_edVista_Enterprise= 'Vista Enterprise';
  SOS_Edition_edVista_HomeBasic= 'Vista Home Basic';
  SOS_Edition_edVista_HomePremium= 'Vista Home Premium';
  SOS_Edition_edVista_Starter= 'Vista Starter';
  SOS_Edition_edServer2008_Datacenter= 'Server 2008 Datacenter';
  SOS_Edition_edServer2008_Enterprise= 'Server 2008 Enterprise';
  SOS_Edition_edServer2008_Enterprise_IA64= 'Server 2008 Enterprise IA64';
  SOS_Edition_edServer2008= 'Server 2008 Standard';
  SOS_Edition_edServer2008_WebServer= 'Server 2008 WebServer';
  SOS_Edition_edServer2008_SS_Enterprise= 'Server 2008 SS Enterprise';
  SOS_Edition_edServer2008_SS_Express= 'Server 2008 SS Express';
  SOS_Edition_edServer2008_SS_Standard= 'Server 2008 SS Standard';
  SOS_Edition_edServer2008_SS_WorkGroup= 'Server 2008 SS WorkGroup';
  SOS_Edition_edServer2008_SBE= 'Server 2008 SBE';
  SOS_Edition_edServer2008_SBS= 'Server 2008 Small Business System, Standard';
  SOS_Edition_edServer2008_SBS_Premium= 'Server 2008 Small Business System, Premium';
  SOS_Edition_edServer2008_Cluster= 'Server 2008 Cluster';
  SOS_Edition_edWin7_Ultimate= 'Windows 7 Utimate';
  SOS_Edition_edWin7_Business= 'Windows 7 Business';
  SOS_Edition_edWin7_Enterprise= 'Windows 7 Enterprise';
  SOS_Edition_edWin7_HomeBasic= 'Windows 7 HomeBasic';
  SOS_Edition_edWin7_Premium= 'Windows 7 Premium';
  SOS_Edition_edWin7_Starter= 'Windows 7 Starter';
  SOS_Edition_edUnknown= 'unrecognised edition';

  SOS_LocalityBasedVariety_varStandard = '' ;   // Like Windows 7 Ultimate
  SOS_LocalityBasedVariety_varN_Type   = 'N';   // Like Windows 7 Ultimate N


const
  OperatingSystem_DisplayNames: array[ TOperatingSystem] of string = (
    SOperatingSystem_osUnrecognised,
    SOperatingSystem_osArchaic,
    SOperatingSystem_osWin98,
    SOperatingSystem_osWinME,
    SOperatingSystem_osNT,
    SOperatingSystem_os2000,
    SOperatingSystem_osXP,
    SOperatingSystem_os2003,
    SOperatingSystem_osVista,
    SOperatingSystem_os2008,
    SOperatingSystem_osWindows7,
    SOperatingSystem_osPost2010);

  OS_Tier_DisplayNames: array[ TOS_Tier] of string = (
    STier_tWorkStation,
    STier_tServer);

ProcessorArchitecture_DisplayNames: array[ TProcessorArchitecture] of string = (
  SProcessorArchitecture_paIntel,
  SProcessorArchitecture_paAMD64,
  SProcessorArchitecture_paIA64,
  SProcessorArchitecture_paUnknown);

NativeWordSize_DisplayNames: array[ TNativeWordSize] of string = (
  SNativeWordSize_nw32,
  SNativeWordSize_nw64);

VirtualizationMode_DisplayNames: array[ TVirtualizationMode] of string = (
  SVirtualizationMode_vmNative,
  SVirtualizationMode_vmVMware,
  SVirtualizationMode_vmMSVirtualPC,
  SVirtualizationMode_vmWine,
  SVirtualizationMode_vmVirtualBox);


TOS_Edition_DisplayNames: array[ TOS_Edition] of string = (
  SOS_Edition_edWin95,
  SOS_Edition_edWin95_OSR2,
  SOS_Edition_edNT_35,
  SOS_Edition_edWin98,
  SOS_Edition_edWin98_SE,
  SOS_Edition_edWinME,
  SOS_Edition_edWinNT_40_Workstation,
  SOS_Edition_edWinNT_40_Server_Enterprise,
  SOS_Edition_edWinNT_40_Server,
  SOS_Edition_edWin2000_Professional,
  SOS_Edition_edWin2000_Server_Datacenter,
  SOS_Edition_edWin2000_Server_Advanced,
  SOS_Edition_edWin2000_Server,
  SOS_Edition_edWinXP_Home,
  SOS_Edition_edWinXP_Professional,
  SOS_Edition_edWinXP_64bit,
  SOS_Edition_edWinXP_MediaCenter,
  SOS_Edition_edWinXP_Starter,
  SOS_Edition_edWinXP_TabletPC,
  SOS_Edition_edWinServer2003_Datacenter,
  SOS_Edition_edWinServer2003_Enterprise,
  SOS_Edition_edWinServer2003_WebEdition,
  SOS_Edition_edWinServer2003_Standard,
  SOS_Edition_edWinServer2003_64bit,
  SOS_Edition_edWinServer2003_HomeServer,
  SOS_Edition_edVista_Business,
  SOS_Edition_edVista_Ultimate,
  SOS_Edition_edVista_Enterprise,
  SOS_Edition_edVista_HomeBasic,
  SOS_Edition_edVista_HomePremium,
  SOS_Edition_edVista_Starter,
  SOS_Edition_edServer2008_Datacenter,
  SOS_Edition_edServer2008_Enterprise,
  SOS_Edition_edServer2008_Enterprise_IA64,
  SOS_Edition_edServer2008,
  SOS_Edition_edServer2008_WebServer,
  SOS_Edition_edServer2008_SS_Enterprise,
  SOS_Edition_edServer2008_SS_Express,
  SOS_Edition_edServer2008_SS_Standard,
  SOS_Edition_edServer2008_SS_WorkGroup,
  SOS_Edition_edServer2008_SBE,
  SOS_Edition_edServer2008_SBS,
  SOS_Edition_edServer2008_SBS_Premium,
  SOS_Edition_edServer2008_Cluster,
  SOS_Edition_edWin7_Ultimate,
  SOS_Edition_edWin7_Business,
  SOS_Edition_edWin7_Enterprise,
  SOS_Edition_edWin7_HomeBasic,
  SOS_Edition_edWin7_Premium,
  SOS_Edition_edWin7_Starter,
  SOS_Edition_edUnknown);


OS_LocalityBasedVariety_DisplayNames: array[ TOS_LocalityBasedVariety] of string = (
  SOS_LocalityBasedVariety_varStandard,
  SOS_LocalityBasedVariety_varN_Type);


function ThisOS: TOS_Datum;

implementation








uses SysUtils, Windows, Math, TlHelp32, Registry;

const
// This table is used for a preliminary (course grain) determination of the
//  operating system
MatchAny = $FFFFFFFF;
OSVersions: array[0..10] of record
  FPltfm, FMajor: word; FMinor: cardinal; FOp: TOperatingSystem end = (
  // NT 3
  (FPltfm: VER_PLATFORM_WIN32_NT     ; FMajor: 3; FMinor:51; FOp:osArchaic), // NT 3.51
  (FPltfm: VER_PLATFORM_WIN32_NT     ; FMajor: 3; FMinor: MatchAny; FOp:osArchaic), // NT 3.X

  // Win 95, 98 & ME
  (FPltfm: VER_PLATFORM_WIN32_WINDOWS; FMajor: 4; FMinor: 0; FOp:osArchaic), // Win 95, Win 95 OSR2, Win 95 OSR2.5
  (FPltfm: VER_PLATFORM_WIN32_WINDOWS; FMajor: 4; FMinor:10; FOp:osWin98),   // Win 98, Win 98 SE
  (FPltfm: VER_PLATFORM_WIN32_WINDOWS; FMajor: 4; FMinor:90; FOp:osWinME),   // Win ME

  // NT 4
  (FPltfm: VER_PLATFORM_WIN32_NT     ; FMajor: 4; FMinor: MatchAny; FOp:osNT),      // NT

  // Windows 2000, XP, Server 2003 and Home Server
  (FPltfm: VER_PLATFORM_WIN32_NT     ; FMajor: 5; FMinor: 0; FOp:os2000),    // Windows 2000
  (FPltfm: VER_PLATFORM_WIN32_NT     ; FMajor: 5; FMinor: 1; FOp:osXP),      // XP
  (FPltfm: VER_PLATFORM_WIN32_NT     ; FMajor: 5; FMinor: 2; FOp:os2003),    // Server 2003, Server 2003 R2 or XP x64

  // Windows Vista, Server 2008 and Windows 7
  (FPltfm: VER_PLATFORM_WIN32_NT     ; FMajor: 6; FMinor: 0; FOp:osVista),   // Vista or Server 2008 original
  (FPltfm: VER_PLATFORM_WIN32_NT     ; FMajor: 6; FMinor: 1; FOp:os2008));  // Server 2008 R2 or Windows 7

  VER_NT_WORKSTATION = $0000001; // The operating system is Windows Vista,
    // Windows XP Professional, Windows XP Home Edition, Windows 2000
    //  Professional, or Windows NT Workstation 4.0.
  VER_NT_DOMAIN_CONTROLLER = $0000002; //	The system is a domain controller.
  VER_NT_SERVER = $0000003; // The system is a server.
    // Note that a server that is also a domain controller is reported as
    //  VER_NT_DOMAIN_CONTROLLER, not VER_NT_SERVER.

  PROCESSOR_ARCHITECTURE_AMD64 = 9; // x64 (AMD or Intel)
  PROCESSOR_ARCHITECTURE_IA64 = 6; // Intel Itanium Processor Family (IPF)
  PROCESSOR_ARCHITECTURE_INTEL = 0; //x86
  PROCESSOR_ARCHITECTURE_UNKNOWN = $FFFF;  // Unknown processor.

  Architectures: array[ 0..2 ] of record
  FCode: word; FArch: TProcessorArchitecture end = (
   (FCode: PROCESSOR_ARCHITECTURE_INTEL; FArch: paIntel),
   (FCode: PROCESSOR_ARCHITECTURE_AMD64; FArch: paAMD64),
   (FCode: PROCESSOR_ARCHITECTURE_IA64 ; FArch: paIA64));

  VER_SUITE_ENTERPRISE = $00000002;
  VER_SUITE_DATACENTER = $00000080;
  VER_SUITE_PERSONAL   = $00000200;
  VER_SUITE_BLADE      = $00000400;
  VER_SUITE_HOMESERVER = $00008000;

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

  SM_SERVERR2 = 89;

type
TGetNativeSystemInfo = procedure( var lpSystemInfo: TSystemInfo); stdcall;

var This: TOS_Datum = nil;

function Sense_OS: TOS_Datum; forward;
function ThisOS: TOS_Datum;
begin
if not assigned( This) then
  This := Sense_OS;
result := This
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

function GetVersionExA( var lpVersionInformation: TOSVersionInfoExA): BOOL;
  stdcall; external kernel32 name 'GetVersionExA';

function GetBasicVersionInfo(
  var Info: TOSVersionInfoExA; var isExtended: boolean): boolean;
begin
FillChar( Info, SizeOf( Info), 0);
Info.dwOSVersionInfoSize := SizeOf( Info);
result     := GetVersionExA( Info);
isExtended := result;
if not result then
  begin // Try one more time, but on a smaller scale.
  Info.dwOSVersionInfoSize := SizeOf( windows.TOSVersionInfoA);
  result := GetVersionExA( Info)
  end
end;


{$IFDEF UNICODE}
function AnsiTrim(const S: ansistring): ansistring;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] <= ' ') do Inc(I);
  if I > L then Result := '' else
  begin
    while S[L] <= ' ' do Dec(L);
    Result := Copy(S, I, L - I + 1);
  end;
end;
{$ENDIF}

{$WARNINGS OFF}
function Extract_CDS_Version( const Info: TOSVersionInfoExA): ansistring;
var
  L, j: integer;
  P: PAnsiChar;
begin
P := @Info.szCSDVersion;
L := SizeOf( Info.szCSDVersion);
for j := 0 to L - 1 do
  begin
  if P <> #0 then
    begin
    Inc( P);
    continue
    end;
  L := j;
  break
  end;
SetLength( result, L);
if L > 0 then
  Move( Info.szCSDVersion, result[1], L);
{$IFDEF UNICODE}
result := AnsiTrim( result)
{$ELSE}
result := Trim( result)
{$ENDIF}
end;
{$WARNINGS ON}

{$IFDEF UNICODE}
function TrueAnsiPos(const Substr, S: ansistring): Integer;
var
  P: PAnsiChar;
begin
result := 0;
P := AnsiStrPos( PAnsiChar( S), PAnsiChar( SubStr));
if P <> nil then
  result := (Integer( P) - Integer( PAnsiChar( S))) + 1
end;
{$ENDIF}

function ComputeServicePack( const CDS: ansistring): integer;
const
  Prefix: ansistring = 'Service Pack ';
var
  s: string;
  V: integer;
  Code: integer;
begin
result := 0;
{$IFDEF UNICODE}
if TrueAnsiPos( Prefix, CDS) = 1 then
{$ELSE}
if AnsiPos( Prefix, CDS) = 1 then
{$ENDIF}
    begin
    {$IFDEF UNICODE}
    s := UTF8ToString( CDS);
    {$ELSE}
    s := CDS;
    {$ENDIF}
    Delete( s, 1, Length( Prefix));
    Val( s, V, Code);
    if (s <> '') and (Code = 0) then
      result := V
    end
end;


function PreliminaryClassification(
 const Info: TOSVersionInfoExA): TOperatingSystem;
var
  j: integer;
  p,m,n: cardinal;
begin
p := Info.dwPlatformId;
m := Info.dwMajorVersion;
n := Info.dwMinorVersion;
result := osUnrecognised;
for j := Low( OSVersions) to High( OSVersions) do
  begin
  if (OSVersions[j].FPltfm <> p) or
     (OSVersions[j].FMajor <> m) or
     ((OSVersions[j].FMinor <> MatchAny) and (OSVersions[j].FMinor <> n)) then
        continue;
  result := OSVersions[j].FOp;
  break
  end;
j := High( OSVersions);
if (result = osUnrecognised) and (OSVersions[j].FPltfm = p) and (
  ((m = OSVersions[j].FMajor) and (n > OSVersions[j].FMinor)) or
   (m > OSVersions[j].FMajor)) then
      // Version is greater than 6.1
      result := osPost2010
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


function CDS_FirstLetter( const Info: TOSVersionInfoExA): ansichar;
var
  s: ansistring;
begin
s := Extract_CDS_Version( Info);
if s = '' then
    result := ' '
  else
    result := s[1]
end;


function SenseArchitecture: TProcessorArchitecture;
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


{$WARNINGS OFF}
function isRunningVMware: boolean;
// This from http://www.delphi3000.com/articles/article_4135.asp
// It appears that the author is Thomas Stutz.
begin
try
asm
mov     eax, 564D5868h   // 'VMXh'
mov     ebx, 00000000h
mov     ecx, 0000000Ah   // get VMWare version
mov     edx, 00005658h   // Port number 'VX'
in      eax, dx
cmp     ebx, 564D5868h
jne     @@exit
mov     result, True
@@exit:
end;
except
  result := False
end end;
{$WARNINGS ON}



{$WARNINGS OFF}
function IsRunningVirtualPC: boolean;
// This from http://ruminatedrumblings.blogspot.com/2008/04/detecting-virtual-pc.html
// He attributes credit to Dennis Pasamore.
asm
  push ebp;
  mov ebp, esp;

  mov ecx, offset @exception_handler;

  push ebx;
  push ecx;

  push dword ptr fs:[0];
  mov dword ptr fs:[0], esp;

  mov ebx, 0; // Flag
  mov eax, 1; // VPC function number

  // call VPC
  db $0F, $3F, $07, $0B

  mov eax, dword ptr ss:[esp];
  mov dword ptr fs:[0], eax;

  add esp, 8;

  test ebx, ebx;

  setz al;

  lea esp, dword ptr ss:[ebp-4];
  mov ebx, dword ptr ss:[esp];
  mov ebp, dword ptr ss:[esp+4];

  add esp, 8;

  jmp @ret1;

@exception_handler:
  mov ecx, [esp+0Ch];
  mov dword ptr [ecx+0A4h], -1; // EBX = -1 ->; not running, ebx = 0 -> running
  add dword ptr [ecx+0B8h], 4;  // ->; skip past the call to VPC
  xor eax, eax;                 // exception is handled

@ret1:
end;
{$WARNINGS ON}




function IsRunningWine: boolean;
var
  hnd:THandle;
  wine_get_version: function : pansichar; stdcall;
  wine_unix2fn: procedure ( p1:pointer; p2:pointer); stdcall;
begin
result := False;
hnd := LoadLibrary('ntdll.dll');
if hnd > 32 then
  begin
  wine_get_version := GetProcAddress( hnd, 'wine_get_version');
  wine_unix2fn:= GetProcAddress( hnd, 'wine_nt_to_unix_file_name');
  result := assigned( wine_get_version) or assigned( wine_unix2fn);
  FreeLibrary( hnd)
  end
end;


//type
//TProcessEntry32A = packed record
//    dwSize: DWORD;
//    cntUsage: DWORD;
//    th32ProcessID: DWORD;
//    th32DefaultHeapID: DWORD;
//    th32ModuleID: DWORD;
//    cntThreads: DWORD;
//    th32ParentProcessID: DWORD;
//    pcPriClassBase: Longint;
//    dwFlags: DWORD;
//    szExeFile: array[0..MAX_PATH - 1] of AnsiChar;
//    end;
//
//TCreateToolhelp32Snapshot = function (dwFlags, th32ProcessID: DWORD): THandle stdcall;

function isRunningVirtualBox: boolean;
var
  handle:THandle;
  procinfo: TProcessEntry32;
//  KernelHandle: HModule;
//  _CreateToolhelp32Snapshot: TCreateToolhelp32Snapshot;

begin
result := False;
//KernelHandle := GetModuleHandleA( kernel32);
//if KernelHandle <> 0 then
//    @_CreateToolhelp32Snapshot := GetProcAddress(KernelHandle, 'CreateToolhelp32Snapshot')
//  else
//    @_CreateToolhelp32Snapshot := nil;
handle := CreateToolhelp32Snapshot( TH32CS_SNAPPROCESS, 0);
procinfo.dwSize := SizeOf( procinfo);
while Process32Next( handle, procinfo) do
  begin
  if Pos( 'VBoxService.exe', procinfo.szExeFile) > 0 then
    begin
    CloseHandle(handle);
    result := True;
    exit
    end;
  end;
CloseHandle( handle);
end;


procedure RefineArchaic( var OS: TOS_Datum; const Info: TOSVersionInfoExA; FirstChar: AnsiChar);
begin
// Windows 95 (including OSR2 and OSR2.5), Windows NT3.50 and Windows NT3.51
if Info.dwPlatformId = VER_PLATFORM_WIN32_WINDOWS then
    begin // Win95
    if FirstChar = 'B' then
        begin
        OS.FEdition := edWin95_OSR2;
        OS.FMajorRelease := 2 // Meaning OSR2
        end
      else if FirstChar= 'C' then
        begin
        OS.FEdition := edWin95_OSR2;
        OS.FMajorRelease := 3 // Meaning OSR2.5
        end
      else
        OS.FEdition := edWin95
    end
  else
    OS.FEdition := edNT_35
end;


procedure RefineWin98( var OS: TOS_Datum; const Info: TOSVersionInfoExA; FirstChar: AnsiChar);
begin
// Windows 98, Windows 98 SE
if FirstChar = 'A' then
    OS.FEdition := edWin98
  else
    begin
    OS.FEdition := edWin98_SE;
    OS.FServicePack := 2 // Meaning 2nd edition
    end;
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


function hasSuite( const Info: TOSVersionInfoExA; Mask: word): boolean;
begin
result := (Info.wSuiteMask and Mask) <> 0
end;


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


procedure RefineNT4( var OS: TOS_Datum; const Info: TOSVersionInfoExA; isExtended: boolean);
var
  sRegProductType: string;
begin
// Windows NT4.0
if (OS.FServicePack = 6) and isNT_ServicePack6a then
  OS.FServicePack := 7;
if isExtended then
    begin
    if Info.wProductType = VER_NT_WORKSTATION then
        OS.FEdition := edWinNT_40_Workstation
      else if hasSuite( Info, VER_SUITE_ENTERPRISE) then
        OS.FEdition := edWinNT_40_Server_Enterprise
      else
        OS.FEdition := edWinNT_40_Server
    end
  else
    begin
    sRegProductType := RegProductType;
    if sRegProductType = 'WINNT' then
        OS.FEdition := edWinNT_40_Workstation
      else if sRegProductType = 'SERVERNT' then
        OS.FEdition := edWinNT_40_Server_Enterprise
      else if sRegProductType = 'LANMANNT' then
        OS.FEdition := edWinNT_40_Server
      else
        OS.FEdition := edUnknown
    end
end;



procedure Refine2000( var OS: TOS_Datum; const Info: TOSVersionInfoExA; isExtended: boolean);
var
  sRegProductType: string;
begin
// Windows 2000 (Including Pro, Server and Advanced Server)
if isExtended then
    begin
    if Info.wProductType = VER_NT_WORKSTATION then
        OS.FEdition := edWin2000_Professional
      else if hasSuite( Info, VER_SUITE_DATACENTER) then
            // wProductType = VER_NT_SERVER || VER_NT_DOMAIN_CONTROLLER
        OS.FEdition := edWin2000_Server_Datacenter
      else if hasSuite( Info, VER_SUITE_ENTERPRISE) then
        OS.FEdition := edWin2000_Server_Advanced
      else
        OS.FEdition := edWin2000_Server
    end
  else
    begin
    sRegProductType := RegProductType;
    if sRegProductType = 'SERVERNT' then
        OS.FEdition := edWin2000_Server_Advanced
      else if sRegProductType = 'LANMANNT' then
        OS.FEdition := edWin2000_Server
      else
        OS.FEdition := edUnknown
    end
end;



function hasSystemMetrics( nIndex: Integer): boolean;
begin
result := GetSystemMetrics( nIndex) <> 0
end;


procedure RefineXP( var OS: TOS_Datum; const Info: TOSVersionInfoExA; isExtended: boolean);
begin
if hasSystemMetrics( SM_MEDIACENTER) then
    OS.FEdition := edWinXP_MediaCenter
  else if hasSystemMetrics( SM_STARTER) then
    OS.FEdition := edWinXP_Starter
  else if hasSystemMetrics( SM_TABLETPC) then
    OS.FEdition := edWinXP_TabletPC
  else if isExtended then
    begin
    if hasSuite( Info, VER_SUITE_PERSONAL) then
        OS.FEdition := edWinXP_Home
      else
        OS.FEdition := edWinXP_Professional
    end
  else
    OS.FEdition := edUnknown
end;




procedure Refine2003( var OS: TOS_Datum; const Info: TOSVersionInfoExA; isExtended: boolean);
begin
// XP 64, Windows Server 2003, Windows Server 2003 R2
// Version 5.2
OS.FEdition := edWinServer2003_Standard;
if hasSystemMetrics( SM_SERVERR2) then
    OS.FMajorRelease := 2; // Windows Server 2003 R2
if (OS.FTier = tWorkStation) and (OS.FArch = paAMD64) then
    begin
    OS.FOS := osXP;
    OS.FEdition := edWinXP_64bit;
    exit
    end;
if not isExtended then exit;
if hasSuite( Info, VER_SUITE_DATACENTER) then
    OS.FEdition := edWinServer2003_Datacenter
  else if hasSuite( Info, VER_SUITE_ENTERPRISE) then
    OS.FEdition := edWinServer2003_Enterprise
  else if hasSuite( Info, VER_SUITE_BLADE) then
    OS.FEdition := edWinServer2003_WebEdition
  else if hasSuite( Info, VER_SUITE_HOMESERVER) then
    OS.FEdition := edWinServer2003_HomeServer
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


procedure RefineOS2008_Primitive( var OS: TOS_Datum; const Info: TOSVersionInfoExA; ProductType: Cardinal);
// Windows Server 2008 (v6.0) or Windows Server 2008 R2 (v6.1)
var
  doUseSuite: boolean;
begin
// Possible editions are ...
//    edServer2008_Datacenter
//    edServer2008_Enterprise
//		edServer2008_Enterprise_IA64
//    edServer2008
//		edServer2008_WebServer
//		edServer2008_SS_Enterprise
//		edServer2008_SS_Express
//		edServer2008_SS_Standard
//		edServer2008_SS_WorkGroup
//		edServer2008_SBE
//		edServer2008_SBS
//		edServer2008_SBS_Premium
//		edServer2008_Cluster
OS.FEdition := edServer2008;
if OS.FMinorVersion = 1 then
    OS.FMajorRelease := 2; // Windows Server 2008 R2

doUseSuite := False;
case ProductType of
			PRODUCT_DATACENTER_SERVER:
				OS.FEdition := edServer2008_Datacenter;
			PRODUCT_DATACENTER_SERVER_CORE:
				OS.FEdition := edServer2008_Datacenter;
			PRODUCT_ENTERPRISE_SERVER:
				OS.FEdition := edServer2008_Enterprise;
			PRODUCT_ENTERPRISE_SERVER_CORE:
				OS.FEdition := edServer2008_Enterprise;
			PRODUCT_ENTERPRISE_SERVER_IA64:
				OS.FEdition := edServer2008_Enterprise_IA64;
			PRODUCT_STANDARD_SERVER:
				OS.FEdition := edServer2008;
			PRODUCT_STANDARD_SERVER_CORE:
				OS.FEdition := edServer2008;
			PRODUCT_WEB_SERVER:
				OS.FEdition := edServer2008_WebServer;
      PRODUCT_STORAGE_ENTERPRISE_SERVER:
				OS.FEdition := edServer2008_SS_Enterprise;
      PRODUCT_STORAGE_EXPRESS_SERVER:
				OS.FEdition := edServer2008_SS_Express;
      PRODUCT_STORAGE_STANDARD_SERVER:
				OS.FEdition := edServer2008_SS_Standard;
      PRODUCT_STORAGE_WORKGROUP_SERVER:
				OS.FEdition := edServer2008_SS_WorkGroup;
      PRODUCT_SERVER_FOR_SMALLBUSINESS:
				OS.FEdition := edServer2008_SBE;
      PRODUCT_SMALLBUSINESS_SERVER:
				OS.FEdition := edServer2008_SBS;
      PRODUCT_SMALLBUSINESS_SERVER_PREMIUM:
				OS.FEdition := edServer2008_SBS_Premium;
      PRODUCT_CLUSTER_SERVER:
				OS.FEdition := edServer2008_Cluster;
      else
        doUseSuite := True
      end;
if not doUseSuite then exit;
// Assume isExtended.
if hasSuite( Info, VER_SUITE_DATACENTER) then
    OS.FEdition := edServer2008_Datacenter
  else if hasSuite( Info, VER_SUITE_ENTERPRISE) then
    OS.FEdition := edServer2008_Enterprise
end;


procedure RefineVista( var OS: TOS_Datum; const Info: TOSVersionInfoExA; isExtended: boolean);
var
  ProductType: Cardinal;
begin
// Version 6.0
OS.FEdition := edUnknown;
if isExtended then
    ProductType := GetProductType( Info)
  else
    ProductType := 0;
case OS.FTier of
  tWorkStation:  OS.FOS := osVista;
  tServer:       begin
                 OS.FOS := os2008;
                 RefineOS2008_Primitive( OS, Info, ProductType);
                 exit
                 end
  end;
if not isExtended then exit;
case ProductType of
      PRODUCT_BUSINESS:
				OS.FEdition := edVista_Business;
      PRODUCT_BUSINESS_N:
        begin
				OS.FEdition := edVista_Business;
        OS.FVariety := varN_Type
        end;
			PRODUCT_ULTIMATE:
				OS.FEdition := edVista_Ultimate;
			PRODUCT_ULTIMATE_N:
        begin
				OS.FEdition := edVista_Ultimate;
        OS.FVariety := varN_Type
        end;
			PRODUCT_ENTERPRISE:
				OS.FEdition := edVista_Enterprise;
			PRODUCT_ENTERPRISE_N:
        begin
				OS.FEdition := edVista_Enterprise;
        OS.FVariety := varN_Type
        end;
		  PRODUCT_HOME_BASIC:
				OS.FEdition := edVista_HomeBasic;
		  PRODUCT_HOME_BASIC_N:
        begin
				OS.FEdition := edVista_HomeBasic;
        OS.FVariety := varN_Type
        end;
			PRODUCT_HOME_PREMIUM:
				OS.FEdition := edVista_HomePremium;
			PRODUCT_HOME_PREMIUM_N:
        begin
				OS.FEdition := edVista_HomePremium;
        OS.FVariety := varN_Type
        end;
			PRODUCT_STARTER:
				OS.FEdition := edVista_Starter;
      end
end;


procedure RefineWindows7( var OS: TOS_Datum; const Info: TOSVersionInfoExA; ProductType: Cardinal);
// Windows 7 (v6.1)
begin
// OS.FOS = osWindows7;
OS.FEdition := edUnknown;
case ProductType of
      PRODUCT_BUSINESS:
				OS.FEdition := edWin7_Business;
      PRODUCT_BUSINESS_N:
        begin
				OS.FEdition := edWin7_Business;
        OS.FVariety := varN_Type
        end;
			PRODUCT_ULTIMATE:
				OS.FEdition := edWin7_Ultimate;
			PRODUCT_ULTIMATE_N:
        begin
				OS.FEdition := edWin7_Ultimate;
        OS.FVariety := varN_Type
        end;
			PRODUCT_ENTERPRISE:
				OS.FEdition := edWin7_Enterprise;
			PRODUCT_ENTERPRISE_N:
        begin
				OS.FEdition := edWin7_Enterprise;
        OS.FVariety := varN_Type
        end;
		  PRODUCT_HOME_BASIC:
				OS.FEdition := edWin7_HomeBasic;
		  PRODUCT_HOME_BASIC_N:
        begin
				OS.FEdition := edWin7_HomeBasic;
        OS.FVariety := varN_Type
        end;
			PRODUCT_HOME_PREMIUM:
				OS.FEdition := edWin7_Premium;
			PRODUCT_HOME_PREMIUM_N:
        begin
				OS.FEdition := edWin7_Premium;
        OS.FVariety := varN_Type
        end;
			PRODUCT_STARTER:
				OS.FEdition := edWin7_Starter;
      end
end;


procedure Refine2008( var OS: TOS_Datum; const Info: TOSVersionInfoExA; isExtended: boolean);
// Version 6.1 .  Either Server 2008 R2 or Windows 7
var
  ProductType: cardinal;
begin
OS.FEdition := edUnknown;
if isExtended then
    ProductType := GetProductType( Info)
  else
    ProductType := 0;
case OS.FTier of
  tWorkStation:  begin
                 OS.FOS := osWindows7;
                 RefineWindows7( OS, Info, ProductType);
                 end;

  tServer:
                 // OS.FOS = os2008;
                 RefineOS2008_Primitive( OS, Info, ProductType)
  end
end;


function DSiGetTrueWindowsVersion( Tier: TOS_Tier): TOperatingSystem; forward;

function Sense_OS: TOS_Datum;
var
  Info: TOSVersionInfoExA;
  isExtended: boolean;
  FirstChar: ansichar;
  CDS_Version: ansistring;
begin
result := TOS_Datum.Create;
with result do begin
// Initialize everything to unknown.
FOS := osUnrecognised;
FTrueSystem := osUnrecognised;
FEdition := edUnknown;
FTier := tWorkStation;
FArch := paUnknown;
FBits := nw32;
FVMode := vmNative;
FVariety := varStandard;
FMajorVersion := 0;
FMinorVersion := 0;
FServicePack  := 0;
FBuildNumber  := 0;
FMajorRelease := 0;
FUCS2 := False;
if not GetBasicVersionInfo( Info, isExtended) then exit;
CDS_Version := Extract_CDS_Version( Info);
FServicePack := ComputeServicePack( CDS_Version);
FOS := PreliminaryClassification( Info);
FMajorVersion := Info.dwMajorVersion;
FMinorVersion := Info.dwMinorVersion;
FBuildNumber  := Info.dwBuildNumber;
if isExtended then
  case Info.wProductType of
    VER_NT_WORKSTATION: begin end;
    VER_NT_DOMAIN_CONTROLLER, VER_NT_SERVER:
      FTier := tServer
    end;
FirstChar := CDS_FirstLetter( Info);
FArch := SenseArchitecture;
if FArch in [paAMD64, paIA64] then
    result.FBits := nw64;

if FOS in [ osNT, os2000, osXP, os2003, osVista, os2008, osWindows7, osPost2010] then
    FUCS2 := True
  else if (FOS in [osArchaic, osWin98, osWinME]) and
          (Info.dwPlatformId  = VER_PLATFORM_WIN32_WINDOWS) then
    FUCS2 := isMSLU;

if isRunningVMware then
    FVMode := vmVMware
  else if isRunningVirtualPC then
    FVMode := vmMSVirtualPC
  else if isRunningWine then
    FVMode := vmWine
  else if isRunningVirtualBox then
    FVMode := vmVirtualBox;

case FOS of
  osUnrecognised: begin end;
  osArchaic:       // Windows 95 (including OSR2 and OSR2.5), Windows NT3.50 and Windows NT3.51
    RefineArchaic( result, Info, FirstChar);
  osWin98:         // Windows 98 and Windows 98SE
    RefineWin98( result, Info, FirstChar);
  osWinME:         // Windows ME
    FEdition := edWinME;
  osNT:            // Windows NT4.0
    RefineNT4( result, Info, isExtended);
  os2000:          // Windows 2000 (Including Pro, Server and Advanced Server)
    Refine2000( result, Info, isExtended);
  osXP:            // Windows XP
    RefineXP( result, Info, isExtended);
  os2003:          // Windows Server 2003 and Windows Server 2003 R2
    Refine2003( result, Info, isExtended);
  osVista:         // Windows Vista or original 2008
    RefineVista( result, Info, isExtended);
  os2008:          // Windows Server 2008 R2 or Windows 7
    Refine2008( result, Info, isExtended);
  osPost2010: begin end;
  end;

if FOS <> osUnrecognised then
    FTrueSystem := DSiGetTrueWindowsVersion( FTier)
  else
    FTrueSystem := FOS;
end end;



function ExportsAPI( module: HMODULE; const apiName: ansistring): boolean;
begin
result := GetProcAddress( module, PAnsiChar( apiName)) <> nil
end;

// This from http://17slon.com/blogs/gabr/2007/02/four-ways-to-detect-vista.html
  {:Tries to return a true OS version when application has compatibility flags set.
    @since   2007-02-11
    @author  Miha-R, gabr
  }
function DSiGetTrueWindowsVersion( Tier: TOS_Tier): TOperatingSystem;
var
  hKernel32: HMODULE;

begin { DSiGetTrueWindowsVersion }
    hKernel32 := GetModuleHandle('kernel32');
    if ExportsAPI(hKernel32, 'CreateRemoteThreadEx') then
      begin
      if Tier = tWorkStation then
          Result := osWindows7// wvWin7OrServer2008R2
        else
          Result := os2008// wvWin7OrServer2008R2
      end
    else if ExportsAPI(hKernel32, 'AddSecureMemoryCacheCallback') then
      begin
      if Tier = tWorkStation then
          Result := osVista// wvWinServer2008OrVistaSP1
        else
          Result := os2008// wvWinServer2008OrVistaSP1
      end
    else if ExportsAPI(hKernel32, 'GetLocaleInfoEx') then
      Result := osVista
    else if ExportsAPI(hKernel32, 'GetLargePageMinimum') then
      Result := os2003
//    else if ExportsAPI(hKernel32, 'GetDLLDirectory') then
//      Result := wvWinXPSP1
    else if ExportsAPI(hKernel32, 'GetNativeSystemInfo') then
      Result := osXP
    else if ExportsAPI(hKernel32, 'ReplaceFile') then
      Result := os2000
    else if ExportsAPI(hKernel32, 'OpenThread') then
      Result := osWinME
    else if ExportsAPI(hKernel32, 'GetThreadPriorityBoost') then
      Result := osNT
    else if ExportsAPI(hKernel32, 'IsDebuggerPresent') then  // is also in NT4!
      Result := osWin98
    else if ExportsAPI(hKernel32, 'GetDiskFreeSpaceEx') then  // is also in NT4!
      Result := osArchaic   // 95 OSR2
    else if ExportsAPI(hKernel32, 'ConnectNamedPipe') then
      Result := osArchaic // NT3
    else if ExportsAPI(hKernel32, 'Beep') then
      Result := osArchaic // wvWin95
    else // we have no idea
      Result := osUnrecognised
  end; { DSiGetTrueWindowsVersion }


initialization
This := nil;

finalization
This.Free;
This := nil
end.
