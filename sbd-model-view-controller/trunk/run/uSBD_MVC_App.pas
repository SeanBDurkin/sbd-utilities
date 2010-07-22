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

unit uSBD_MVC_App;
interface
uses Contnrs, uSBD_AppComponents, uSBD_PresentationStrata_Framework,
     uSBD_PresentationStrata_Skins, uSBD_ModelStrata_Framework
{$IFDEF SmartInspect}
, SmartInspect
{$ENDIF}
, uSBD_PersistenceStrata_Framework
;

type

TMVC_Option = ( opHasPersistenceStrata,
                osPrstncHasAsychronousEvents,
                osUseSmartInspect,
                osCanUsePlugInSkins,
                osAllowHijackMainForm,
                osUsesPreview,
                osSetInitialOptionDefaultsWhereUndefined);

TMVC_OptionSet = set of TMVC_Option;

TSkinChoice = class
  public
    FisInbuilt: boolean;
    FSkinProgId: string;
    FSkinDisplayName: string;
    FSkin: ISkin;
    FPreview: IView;
    FRTLVersion: double;
    FFileName: string;
    FFileVersion: string;
    FModuleHandle: HMODULE;
  end;

TSkinChoiceList = class( TObjectList)
  private
    function  GetSkin( Idx: integer): TSkinChoice;
    procedure SetSkin( Idx: integer; Value: TSkinChoice);
  public
    property Skins[ Idx: integer]: TSkinChoice   read GetSkin write SetSkin; default;
  end;

TMVC_App = class
  private
    MVC_Binder: TObject;

  protected
    FisAdmin: boolean;

    function  ApplicationDisplayName: string;        virtual; abstract;
    function  Options: TMVC_OptionSet;               virtual;
    procedure SetTheCryptoFunctionPointers;          virtual;
    function  GetCryptoKey: string;                  virtual;
    procedure GetSkinLocation(
      AppInfo: TAppInfo; const BootStrapSysOptions: ISystemOptions;
      var PlugInDir, Pattern: string);               virtual;

    function  AcquireSystemOptions(
      AppInfo: TAppInfo; isBootStrap: boolean;
      var SysObj: TObject): ISystemOptions;

{$IFDEF SmartInspect}
    procedure AcquireSmartInspect( var Si1: TSmartInspect; var Sess1: TSiSession);    virtual;
    procedure ReleaseSmartInspect( Si1: TSmartInspect; Sess1: TSiSession);            virtual;
    procedure SmartInspectBootStrapReport( Sess1: TSiSession; AppInfo: TAppInfo); virtual;
{$EndIf}

    function  GetExeName: string; virtual; abstract;
    procedure GetAppStrings( var Company, AppId, DocDesc: string);              virtual; abstract;
    procedure DefineBootStrapOptions( const BootStrapOptions: ISystemOptions);  virtual; // For SI and Skin selection
    function  AcquirePersistenceStrata( var PrstncObj: TObject): IPersistenceStrata;     virtual;
    function  AcquireModelStrata      ( var ModelObj: TObject): IModel;     virtual; abstract;
    function  Inbuilt_MainForm: TObject;                                    virtual;
      // Application.MainForm
    procedure SetMainForm( NewForm: TObject); virtual;  // Application.MainForm := NewForm
    procedure CreateMainForm( MainFormClass: TClass); virtual;
          // Application.ShowMainForm := True;
          // Application.CreateForm( MainFormClass, Reference)
    procedure HijackMainForm( OldForm: TObject); virtual;
      // OldForm.OnClose := nil;
      // OldForm.OnCanClose := nil;
    procedure ClearApplicationEvents( ApplicationEventsSender: TObject); virtual;
      // ApplicationEventsSender is TApplicationEvents

    function  Acquire_InBuiltController( var Ctrl: TObject): IController;           virtual; abstract;
    // ^ If the controller is reference counted, the Ctrl shall return nil.
    // ^ If the controller is non-counted, then Ctrl should be the object to
    //     be destroyed upon shut-down or library release.
    function  Acquire_InBuiltViews: TObjectList; virtual; // Owning ObjectList of TViewInfo
      // Ownership of the list is also transferred.
    function  Acquire_InBuiltPreviewMain(
      var Vw: TObject; const ConstructionContext: IInterface): IView;             virtual;

    function  PreviewConstructionContext: IInterface; virtual;
    function  SelectSkin(
      AppInfo: TAppInfo; const BootStrapSysOptions: ISystemOptions;
       ViewChoices: TSkinChoiceList): integer;    virtual;
    procedure Breathe; virtual;

  public
    constructor EarlyInitiation;                                   virtual;
    procedure   LateInitiation( ApplicationEventsSender: TObject); virtual;
  end;

TMVC_AppClass = class of TMVC_App;






implementation







uses Classes, SysUtils, TypInfo, Windows, uSBD_SystemOptions_Reg,
     uSBD_OS_Version, uSBD_WindowsSecurity, uSBD_FileUtils
{$IFDEF SmartInspect}
  , SiAuto
{$ENDIF}
;



type
TBinder = class( TInterfacedPersistent, ISkin, IViewIterator,
                  IControlServices, ISubSystemViewManager)
  private
    FApp: TMVC_App;
    FdoUseSI: boolean;
    FAppInfo: TAppInfo;
{$IFDEF SmartInspect}
    FSi: TSmartInspect;
    FSess: TSiSession;
{$ENDIF}
    FOptions: ISystemOptions;
    FOptionsObj: TObject;
    FPrstncObj: TObject;
    FPer: IPersistenceStrata;
    FModelObj: TObject;
    FModel: IModel;
    FLibHandle: HMODULE;
    FSkinGateWay: TSkinGateway;
    FCtrl: IController;
    FCtrlObj: TObject;
    FViewInfos: TObjectList; // of TViewInfo;
    FControllerIsView: boolean;
    FisIteratingViews: boolean;
    FViewSubsystems: IInterfaceList;
    Skins: TSkinChoiceList;
    BootStrapOptions: ISystemOptions;
    BootStrapObj: TObject;
    OpenedExternalSkinData: TObjectList; // of TOpenedExternalSkinDatum
    FisShuttingDown: boolean;
    hasClosedMainForm: boolean;

    constructor Create( App: TMVC_App);
    procedure   LateInitiation;

    function  ScanDirectory( const Dir, Pattern: string): TStrings;

    // ISkin
    function InBuilt_SkinProgId: string;
    function ISkin.SkinProgId = InBuilt_SkinProgId;

    function InBuilt_SkinDisplayName: string;
    function ISkin.SkinDisplayName = InBuilt_SkinDisplayName;

    function InBuilt_CreateController( var Ctrl: TObject): IController;
    function ISkin.CreateController = InBuilt_CreateController;

    function InBuilt_CreateViews: TObjectList;
    function ISkin.CreateViews = InBuilt_CreateViews;

    function InBuilt_CreatePreviewMain(
      var Vw: TObject; const ConstructionContext: IInterface): IView;
    function ISkin.CreatePreviewMain = InBuilt_CreatePreviewMain;

    function OpenSkin(
      const FN:string; var LibHandle: HMODULE;
      var SkinGateWay: TSkinGateway
      ): boolean;

    // IViewIterator
    procedure IterateViews( Method: TExecuteViewProc; var Datum; Ascending: boolean);

  //IControlServices
    procedure ShutDown( ForceShutDown: boolean; OriginatingForm: TObject);
      // ^ Will shut-down the application if either:
      //    (1) ForceShutDown is True; or
      //    (2) OriginatingForm is either nil or it is the Application MainForm.
      // A View's main form will call the above when it closes, with itself as
      //  the OriginatingForm parameter.
      // UPSHOT: A main-view will shut-down the application.
      //         A secondary view will just hide itself.

    function  WillShutDownFromForm( OriginatingForm: TObject): boolean;
      // ^ Will return True iff:
      //    (1) Model & Persistence strata agree to it; and
      //    (2) Either OriginatingForm is nil or it is the Application MainForm.
      // A View's main form CanClose event will call the above, with itself as
      //  the OriginatingForm parameter.

    function  CanShutDown: boolean;
      // ^ Will return True iff Model & Persistence strata agree to it.

    procedure Breathe;
      // ^ Application level idle processing.

    // ISubSystemViewManager
    procedure Subscribe  ( const Observer: IView); // Must be a reference
      // counted Subsystem of the presentation layer, such as a page.
    procedure Unsubscribe( const Observer: IView);

  public
    destructor Destroy; override;
  end;



TOpenedExternalSkinDatum = class
  public
    FFN:string;
    FLibHandle: HMODULE;
    FSkinGateWay: TSkinGateway;
  end;


{ TSkinChoiceList }

function TSkinChoiceList.GetSkin( Idx: integer): TSkinChoice;
begin
result := Items[ Idx] as TSkinChoice
end;

procedure TSkinChoiceList.SetSkin( Idx: integer; Value: TSkinChoice);
begin
Items[ Idx] := Value
end;

{ TMVC_App }

function TMVC_App.AcquirePersistenceStrata(
  var PrstncObj: TObject): IPersistenceStrata;
begin
PrstncObj := nil;
result    := nil
end;

{$IFDEF SmartInspect}
procedure TMVC_App.AcquireSmartInspect(
  var Si1: TSmartInspect;
  var Sess1: TSiSession);
begin
Si1   := nil;
Sess1 := nil
end;
{$ENDIF}


function TMVC_App.AcquireSystemOptions(
  AppInfo: TAppInfo; isBootStrap: boolean; var SysObj: TObject): ISystemOptions;
begin
if isBootStrap then
    SysObj := TSystemOptions_Reg.Create( '', AppInfo)
  else
    SysObj := TSystemOptions_Reg.Create( GetCryptoKey, AppInfo);
Supports( SysObj, ISystemOptions, result)
end;



function TMVC_App.Acquire_InBuiltPreviewMain(
  var Vw: TObject; const ConstructionContext: IInterface): IView;
begin
Vw     := nil;
result := nil
end;


function TMVC_App.Acquire_InBuiltViews: TObjectList;
begin
result := TObjectList.Create( True)
end;


procedure TMVC_App.Breathe;
begin
end;

procedure TMVC_App.ClearApplicationEvents( ApplicationEventsSender: TObject);
begin
end;

procedure TMVC_App.CreateMainForm( MainFormClass: TClass);
begin
end;

procedure TMVC_App.DefineBootStrapOptions(
  const BootStrapOptions: ISystemOptions);
begin
end;



constructor TMVC_App.EarlyInitiation;
begin
MVC_Binder := TBinder.Create( self)
end;


function TMVC_App.GetCryptoKey: string;
begin
result := ''
end;



procedure TMVC_App.GetSkinLocation(
  AppInfo: TAppInfo; const BootStrapSysOptions: ISystemOptions;
  var PlugInDir, Pattern: string);
begin
PlugInDir := AppInfo.FAppExeDir;
Pattern   := '*.skin'
end;


procedure TMVC_App.HijackMainForm( OldForm: TObject);
begin
end;

function TMVC_App.Inbuilt_MainForm: TObject;
begin
result := nil
end;

procedure TMVC_App.LateInitiation( ApplicationEventsSender: TObject);
begin
ClearApplicationEvents( ApplicationEventsSender);
//if ApplicationEventsSender is TApplicationEvents then
//  TApplicationEvents( ApplicationEventsSender).OnIdle := nil;
TBinder(MVC_Binder).LateInitiation
end;



function TMVC_App.Options: TMVC_OptionSet;
begin
result := []
end;

function TMVC_App.PreviewConstructionContext: IInterface;
begin
result := nil
end;

{$IFDEF SmartInspect}
procedure TMVC_App.ReleaseSmartInspect( Si1: TSmartInspect; Sess1: TSiSession);
begin
end;
{$ENDIF}


function TMVC_App.SelectSkin(
  AppInfo: TAppInfo; const BootStrapSysOptions: ISystemOptions;
  ViewChoices: TSkinChoiceList): integer;
begin
result := -1
end;


procedure TMVC_App.SetMainForm( NewForm: TObject);
begin
end;

procedure TMVC_App.SetTheCryptoFunctionPointers;
begin
end;


{$IFDEF SmartInspect}
procedure TMVC_App.SmartInspectBootStrapReport( Sess1: TSiSession; AppInfo: TAppInfo);
var
  Options1: TMVC_OptionSet;
  s: string;
  j: TMVC_Option;
  Card: integer;
  Company, AppId, DocDesc: string;
begin
Sess1.ClearAll;
Sess1.LogMessage( 'Welcome to %s.', [ApplicationDisplayName]);
Sess1.LogMessage( 'A program using the SBD Model-View-Controller Framework library.');
Sess1.LogSystem( lvDebug);
Sess1.LogMemoryStatistic( lvDebug);
Sess1.LogDouble( lvDebug, 'MVC Framwork RTL Version', uSBD_AppComponents.LibRTLVersion);
Options1 := Options;
s := '[';
Card := 0;
for j in Options1 do
//for j := Low( TMVC_Option) to High( TMVC_Option) do
  begin
  if not (j in Options1) then continue;
  Inc( Card);
  if Card >= 2 then
    s := s + ',';
  s := s + GetEnumName( TypeInfo( TMVC_Option), Ord(j))
  end;
s := s + ']';
Sess1.LogString( lvDebug, 'MVC Options', s);
Sess1.LogString( lvDebug, 'Exe', GetExeName);
GetAppStrings( Company, AppId, DocDesc);
Sess1.LogString( lvDebug, 'Company', Company);
Sess1.LogString( lvDebug, 'AppId', AppId);
Sess1.LogString( lvDebug, 'DocDesc', DocDesc);

Sess1.LogString( lvDebug, 'BaseKey', AppInfo.FBaseKey);
Sess1.LogString( lvDebug, 'AppSpecific User Dir', AppInfo.FAppSpecific_User_Dir);
Sess1.LogString( lvDebug, 'AppSpecific Machine Dir', AppInfo.FAppSpecific_Machine_Dir);
Sess1.LogString( lvDebug, 'Document User Dir', AppInfo.FDocument_User_Dir);
Sess1.LogString( lvDebug, 'Document Machine Dir', AppInfo.FDocument_Machine_Dir);
Sess1.LogString( lvDebug, 'Version', Format('%d.%d.%d.%d',
  [AppInfo.FMajorV, AppInfo.FMinorV, AppInfo.FReleaseV, AppInfo.FBuildV]));
Sess1.LogBoolean( lvDebug, 'Engineering mode', AppInfo.FisEngineering);
Sess1.LogBoolean( lvDebug, 'Administrator', FisAdmin);
if ThisOS.FOS = ThisOS.FTrueSystem then
    Sess1.LogString( lvDebug, 'Operating system', OperatingSystem_DisplayNames[ ThisOS.FOS])
  else
    begin
    Sess1.LogString( lvDebug, 'Apparant Operating system', OperatingSystem_DisplayNames[ ThisOS.FOS]);
    Sess1.LogString( lvDebug, 'True Operating system', OperatingSystem_DisplayNames[ ThisOS.FTrueSystem])
    end;
Sess1.LogString( lvDebug, 'Edition', TOS_Edition_DisplayNames[ ThisOS.FEdition]);
Sess1.LogString( lvDebug, 'Tier', OS_Tier_DisplayNames[ ThisOS.FTier]);
Sess1.LogString( lvDebug, 'Architecture', ProcessorArchitecture_DisplayNames[ ThisOS.FArch]);
Sess1.LogString( lvDebug, 'Native word size', NativeWordSize_DisplayNames[ ThisOS.FBits]);
Sess1.LogString( lvDebug, 'Virtualization mode', VirtualizationMode_DisplayNames[ ThisOS.FVMode]);
Sess1.LogString( lvDebug, 'Locality based variety', OS_LocalityBasedVariety_DisplayNames[ ThisOS.FVariety]);
Sess1.LogString( lvDebug, 'OS version', Format( '%d.%d', [ThisOS.FMajorVersion, ThisOS.FMinorVersion]));
if ThisOS.FMajorRelease <> 0 then
  Sess1.LogInteger( lvDebug, 'OS release (R)', ThisOS.FMajorRelease);
if ThisOS.FServicePack <> 0 then
  Sess1.LogInteger( lvDebug, 'Service Pack (SP)', ThisOS.FServicePack);
Sess1.LogInteger( lvDebug, 'OS Build Number', ThisOS.FBuildNumber);
Sess1.LogBoolean( lvDebug, 'UCS-2 available', ThisOS.FUCS2);
end;
{$ENDIF}



//type
//THackApplication = class( TComponent)
//  private
//    FHandle: LongWord;
//    FBiDiMode: TBiDiMode;
//    FBiDiKeyboard: string;
//    FNonBiDiKeyboard: string;
//    FObjectInstance: Pointer;
//    FMainForm: TForm;
//  end;
//
//procedure SetMainForm( NewMainForm: TForm);
//begin
//THackApplication( Application).FMainForm := NewMainForm
//end;
//
//
//


{ TBinder }



const
  BootStrap_SiEnable = 'Si.Enable';
  BootStrap_SiLevel  = 'Si.Level';


constructor TBinder.Create( App: TMVC_App);
var
  Company, AppId, DocDesc: string;
  Enum: integer;
  SkinChoice: TSkinChoice;
  PlugInDir, Pattern: string;
  SkinLibs: TStrings;
  SkinLibFileName: string;
  j, i: integer;
  LibHandle: HMODULE;
  SkinGateWay: TSkinGateway;
  TheseSkins: IInterfaceList;
  AddendumSkin: ISkin;
  PackageName: string;
  MajorV, MinorV, ReleaseV, BuildV: integer;
  sLibVer: string;
  OpenedExternalSkinDatum: TOpenedExternalSkinDatum;

begin
FisShuttingDown := False;
hasClosedMainForm := False;
FApp := App;
FisIteratingViews := False;
FViewSubsystems := TInterfaceList.Create;
{$IFDEF SmartInspect}
FdoUseSI := osUseSmartInspect in FApp.Options;
{$ELSE}
FdoUseSI := False;
{$ENDIF}
FApp.GetAppStrings( Company, AppId, DocDesc);
FAppInfo := TAppInfo.Create( FApp.GetExeName, Company, AppId, DocDesc);
FApp.FisAdmin := uSBD_WindowsSecurity.IsAdmin;

{$IFDEF SmartInspect}
if FdoUseSI then
  begin
  BootStrapOptions := FApp.AcquireSystemOptions( FAppInfo, True, BootStrapObj);
  BootStrapOptions.DefineOption( BootStrap_SiEnable, BootStrap_SiEnable, otBoolean, nil, True);
  BootStrapOptions.DefineOption( BootStrap_SiLevel, BootStrap_SiLevel, otEnum, TypeInfo( TSiLevel), True);
  FApp.DefineBootStrapOptions( BootStrapOptions);
  BootStrapOptions.StartUp( False);
  FApp.AcquireSmartInspect( FSi, FSess);
  if assigned( FSi) and assigned( FSess) then
      begin
      FSi.Enabled := BootStrapOptions.GetBooleanOption( BootStrap_SiEnable);
      Enum := BootStrapOptions.GetIntegerOption( BootStrap_SiLevel);
      if (Enum >= 0) and (Enum <= Ord( High( TSiLevel))) then
        FSess.Level := TSiLevel( Enum);
      if FSi.Enabled then
        FApp.SmartInspectBootStrapReport( FSess, FAppInfo)
      end
    else
      FdoUseSI := False
  end;
{$ENDIF}


if opHasPersistenceStrata in FApp.Options then
  begin
  FPer := FApp.AcquirePersistenceStrata( FPrstncObj);
  if (not assigned( FPer)) and assigned( FPrstncObj) then
    FreeAndNil( FPrstncObj)
  end;

FModel := FApp.AcquireModelStrata( FModelObj);

Skins := TSkinChoiceList.Create( True);
SkinChoice := TSkinChoice.Create;
SkinChoice.FisInbuilt := True;
SkinChoice.FSkinProgId := '';
SkinChoice.FSkinDisplayName := 'In-built';
SkinChoice.FSkin := self;
SkinChoice.FPreview := nil;
SkinChoice.FRTLVersion := uSBD_AppComponents.LibRTLVersion;
SkinChoice.FFileName := '';
SkinChoice.FFileVersion := '';
SkinChoice.FModuleHandle := 0;
Skins.Add( SkinChoice);

OpenedExternalSkinData := TObjectList.Create( True);
if osCanUsePlugInSkins in FApp.Options then
    begin
    FApp.GetSkinLocation( FAppInfo, BootStrapOptions, PlugInDir, Pattern);
    SkinLibs := ScanDirectory( PlugInDir, Pattern)
    end
  else
    SkinLibs := TStringList.Create;
for j := 0 to SkinLibs.Count - 1 do
  begin
  SkinLibFileName := SkinLibs[ j];
  if OpenSkin( SkinLibFileName, LibHandle, SkinGateWay) then
    begin
    OpenedExternalSkinDatum := TOpenedExternalSkinDatum.Create;
    OpenedExternalSkinDatum.FFN := SkinLibFilename;
    OpenedExternalSkinDatum.FLibHandle := LibHandle;
    OpenedExternalSkinDatum.FSkinGateWay := SkinGateWay;
    OpenedExternalSkinData.Add( OpenedExternalSkinDatum);
    if uSBD_AppComponents.GetVersionInfo( SkinLibFileName, MajorV, MinorV, ReleaseV, BuildV) then
        sLibVer := Format( '%d.%d.%d.%d', [MajorV, MinorV, ReleaseV, BuildV])
      else
        sLibVer := '';
    PackageName := SkinGateway.PackageName;
    TheseSkins := SkinGateWay.AcquireSkins;
    for i := 0 to TheseSkins.Count - 1 do
      if Supports( TheseSkins[j], ISkin, AddendumSkin) then
        begin
        SkinChoice := TSkinChoice.Create;
        SkinChoice.FisInbuilt := False;
        SkinChoice.FSkin := AddendumSkin;
        SkinChoice.FSkinProgId := AddendumSkin.SkinProgId;
        SkinChoice.FSkinDisplayName := AddendumSkin.SkinDisplayName;
        if SkinChoice.FSkinDisplayName = '' then
            SkinChoice.FSkinDisplayName := Format( '%s[%d]', [PackageName, i+1])
          else if PackageName <> '' then
            SkinChoice.FSkinDisplayName := Format( '%s - %s',
              [PackageName, SkinChoice.FSkinDisplayName]);
        SkinChoice.FPreview := nil;
        SkinChoice.FRTLVersion := uSBD_AppComponents.LibRTLVersion;
        SkinChoice.FFileName := SkinLibFileName;
        SkinChoice.FFileVersion := sLibVer;
        SkinChoice.FModuleHandle := LibHandle;
        Skins.Add( SkinChoice)
        end
    end;
  end;
SkinLibs.Free
end;





function TBinder.ScanDirectory( const Dir, Pattern: string): TStrings;
var
  SR: TSearchRec;
  Dir1, Pattern1: string;
  Attr: integer;
begin
result := TStringList.Create;
try
Dir1 := RemoveFinalBackSlash( Dir);
if Dir1 = '' then
  Dir1 := FAppInfo.FAppExeDir;
Pattern1 := Pattern;
if Pattern1 = '' then
   Pattern1 := '*.*';
{$Warnings OFF}
Attr := faReadOnly or faHidden or faArchive;
{$Warnings ON}
if FindFirst( Dir1 + '\' + Pattern1, Attr, SR) = 0 then
  try
    repeat
      result.Add( Dir1 + '\' + SR.Name)
    until FindNext( SR) <> 0
  finally
    SysUtils.FindClose( SR)
    end
except
  // Failure to read the plug-in directory.
  // Silently swallow this error and revert to in-built presentation.
  {$IFDEF SmartInspect}
  if FdoUseSI then
    begin
    FSess.LogString( lvError, 'Failed to read skin directory.', Dir);
    end
  {$ENDIF}
  end
end;


procedure TBinder.Breathe;
begin
FApp.Breathe
end;

function TBinder.CanShutDown: boolean;
begin
result := ((not assigned( FPer)) or FPer.CanClose) and
           FModel.CanClose
end;



function TBinder.WillShutDownFromForm( OriginatingForm: TObject): boolean;
begin
result := FisShuttingDown or (
   CanShutDown and ((not assigned( OriginatingForm)) or
                    (OriginatingForm = FCtrl.ControllerMainForm)))
end;



procedure TBinder.ShutDown(
  ForceShutDown: boolean; OriginatingForm: TObject);
// ^ Will shut-down the application if either:
//    (1) ForceShutDown is True; or
//    (2) OriginatingForm is either nil or it is the Application MainForm.
// A View's main form will call the above when it closes, with itself as
//  the OriginatingForm parameter.
// UPSHOT: A main-view will shut-down the application.
//         A secondary view will just hide itself.
var
  doShutDown: boolean;
begin
if FisShuttingDown then exit;
if ForceShutDown then
    doShutDown := True
  else
    doShutDown := CanShutDown and ((not assigned( OriginatingForm)) or
                                        (OriginatingForm = FCtrl.ControllerMainForm));
if doShutDown then
  begin
  if assigned( OriginatingForm) and
    (OriginatingForm = FCtrl.ControllerMainForm) then
      hasClosedMainForm := True;
  Destroy
  end
end;



procedure TBinder.Subscribe( const Observer: IView);
begin
Observer.SetAppInfo( FAppInfo);
{$IFDEF SmartInspect}
if FdoUseSI then
  Observer.SetSmartInspect( FSi, FSess);
{$EndIf}
FViewSubsystems.Add( Observer);
Observer.SetModelStrata( FModel);
Observer.SetSubSystemViewManager( self);
end;



procedure TBinder.Unsubscribe( const Observer: IView);
begin
Observer.SetModelStrata( nil);
Observer.SetSubSystemViewManager( nil);
FViewSubsystems.Remove( Observer)
end;



function TBinder.OpenSkin(
  const FN:string; var LibHandle: HMODULE;
  var SkinGateWay: TSkinGateway): boolean;
var
  RTLVerFunc: TIntegerFunc;
  SkinEntryPoint: TSkinGatewayMaker;
begin
LibHandle      := 0;
SkinGateWay    := nil;
try
  LibHandle := SafeLoadLibrary( FN)
except
  LibHandle := 0
end;
if LibHandle > HINSTANCE_ERROR then
    begin
    SkinEntryPoint := GetProcAddress( LibHandle, SkinGatewayMakerName);
    if assigned( SkinEntryPoint) then
      SkinGateWay := SkinEntryPoint
    end
  else
    LibHandle := 0;
result := assigned( SkinGateWay);
if not result then exit;
RTLVerFunc := GetProcAddress( LibHandle, Library_RTLVersion_Name);
result := RTLVerFunc = uSBD_AppComponents.LibRTLVersion;
if not result then
  begin
  try FreeAndNil( SkinGateWay) except end;
  try FreeLibrary( LibHandle)  except end;
  LibHandle := 0
  end
end;




procedure TBinder.LateInitiation;
var
  SelectedSkinIdx: integer;
  View: IView;
  CtrlViewInfo: TViewInfo;
  PersistenceStrata_CallBack: IPersistenceStrata_CallBack;
  SkinChoice: TSkinChoice;
  j: integer;
  OpenedExternalSkinDatum: TOpenedExternalSkinDatum;
  OldForm, NewForm: TObject;
  Vw: TObject;
  PreviewObjs: TObjectList;
begin
// Late Init.
if (not (osCanUsePlugInSkins in FApp.Options)) or (Skins.Count <= 1) then
    SelectedSkinIdx := 0
  else
    begin
    PreviewObjs := nil;
    if osUsesPreview in FApp.Options then
      begin
      PreviewObjs := TObjectList.Create( True);
      for j := 0 to Skins.Count - 1 do
        begin
        Skins[j].FPreview := Skins[j].FSkin.CreatePreviewMain(
          Vw, FApp.PreviewConstructionContext);
        if assigned( Vw) then
          PreviewObjs.Add( Vw)
        end;
      end;
    SelectedSkinIdx := FApp.SelectSkin( FAppInfo, BootStrapOptions, Skins);
    if (SelectedSkinIdx < 0) or (SelectedSkinIdx >= Skins.Count) then
      SelectedSkinIdx := 0;
    if osUsesPreview in FApp.Options then
      begin
      for j := 0 to Skins.Count - 1 do
        Skins[j].FPreview := nil;
      PreviewObjs.Free
      end;
    end;
SkinChoice := Skins[ SelectedSkinIdx];
Skins.Extract( SkinChoice);
Skins.Free;
FSkinGateWay := nil;
FLibHandle   := 0;

BootStrapOptions := nil;
FreeAndNil( BootStrapObj);

for j := OpenedExternalSkinData.Count - 1 downto 0 do
  begin
  OpenedExternalSkinDatum := OpenedExternalSkinData[j] as TOpenedExternalSkinDatum;
  if SkinChoice.FisInbuilt or
     (OpenedExternalSkinDatum.FLibHandle <> SkinChoice.FModuleHandle) then
      begin
      try FreeAndNil( OpenedExternalSkinDatum.FSkinGateWay) except end;
      try FreeLibrary( OpenedExternalSkinDatum.FLibHandle)  except end;
      end
    else if (FLibHandle = 0) and (not SkinChoice.FisInbuilt) then
      begin
      FSkinGateWay := OpenedExternalSkinDatum.FSkinGateWay;
      FLibHandle   := OpenedExternalSkinDatum.FLibHandle
      end;
  end;
OpenedExternalSkinData.Free;
FCtrl := SkinChoice.FSkin.CreateController( FCtrlObj);
FViewInfos := SkinChoice.FSkin.CreateViews;
SkinChoice.Free;
FControllerIsView := FCtrl.UseControllerAsAView and Supports( FCtrl, IView, View);
if FControllerIsView then
  begin
  CtrlViewInfo := TViewInfo.Create;
  CtrlViewInfo.FView    := View;
  CtrlViewInfo.FViewObj := nil;
  CtrlViewInfo.FisController := True;
  FViewInfos.Insert( 0, CtrlViewInfo)
  end;

OldForm := FApp.Inbuilt_MainForm;       // Application.MainForm
NewForm := FCtrl.ControllerMainForm;
if (osAllowHijackMainForm in FApp.Options) and (
  (assigned( NewForm) and (NewForm <> OldForm)) or
  ((not assigned( NewForm)) and assigned( OldForm) and
    (OldForm.ClassType <> FCtrl.ControlMainFormClass) and
    (FCtrl.ControlMainFormClass <> nil))) then
    begin
    if assigned( NewForm) then
        FApp.SetMainForm( NewForm) // Application.MainForm := NewForm
      else
        begin
        FApp.SetMainForm( nil); // Application.MainForm := nil
        FApp.CreateMainForm( FCtrl.ControlMainFormClass)
          // Application.ShowMainForm := True;
          // Application.CreateForm( InstanceClass, Reference)
        end;
    if assigned( OldForm) then
      begin
      FApp.HijackMainForm( OldForm);
      // OldForm.OnClose := nil;
      // OldForm.OnCanClose := nil;
      // OldForm.FreeOnRelease
      end
    end;

if assigned( FPer) then
  FPer.SetAppInfo( FAppInfo);
FModel.SetAppInfo( FAppInfo);
FCtrl.SetAppInfo( FAppInfo);
for j := 0 to FViewInfos.Count - 1 do
  begin
  CtrlViewInfo := FViewInfos[j] as TViewInfo;
  if not CtrlViewInfo.FisController then
    CtrlViewInfo.FView.SetAppInfo( FAppInfo)
  end;

{$IFDEF SmartInspect}
if FdoUseSI then
  begin
  if assigned( FPer) then
    FPer.SetSmartInspect( FSi, FSess);
  FModel.SetSmartInspect( FSi, FSess);
  FCtrl.SetSmartInspect( FSi, FSess);
  for j := 0 to FViewInfos.Count - 1 do
    begin
    CtrlViewInfo := FViewInfos[j] as TViewInfo;
    if not CtrlViewInfo.FisController then
      CtrlViewInfo.FView.SetSmartInspect( FSi, FSess)
    end;
  end;
{$ENDIF}

FOptions := FApp.AcquireSystemOptions( FAppInfo, False, FOptionsObj);

if assigned( FPer) then
  FPer.DefineOptions( FOptions);
FModel.DefineOptions( FOptions);
FCtrl.DefineOptions( FOptions);
for j := 0 to FViewInfos.Count - 1 do
  begin
  CtrlViewInfo := FViewInfos[j] as TViewInfo;
  if not CtrlViewInfo.FisController then
    CtrlViewInfo.FView.DefineOptions( FOptions)
  end;

FApp.SetTheCryptoFunctionPointers;
FOptions.StartUp( osSetInitialOptionDefaultsWhereUndefined in FApp.Options);

if assigned( FPer) and (osPrstncHasAsychronousEvents in FApp.Options) and
   Supports( FModel, IPersistenceStrata_CallBack, PersistenceStrata_CallBack) then
  FPer.SetCallBack( PersistenceStrata_CallBack);

if assigned( FPer) then
  FModel.SetPersistenceStrata( FPer);

FModel.SetViews( self);

FCtrl.SetModelStrata( FModel);

FCtrl.SetControlServices( self);

for j := 0 to FViewInfos.Count - 1 do
  begin
  CtrlViewInfo := FViewInfos[j] as TViewInfo;
  if not CtrlViewInfo.FisController then
    CtrlViewInfo.FView.SetModelStrata( FModel);
  CtrlViewInfo.FView.SetSubSystemViewManager( self)
  end;

if assigned( FPer) then
  FPer.StartUp;
FModel.StartUp;
for j := 0 to FViewInfos.Count - 1 do
  begin
  CtrlViewInfo := FViewInfos[j] as TViewInfo;
  if not CtrlViewInfo.FisController then
    CtrlViewInfo.FView.StartUp;
  end;
FCtrl.StartUp;
end;




destructor TBinder.Destroy;
var
  j: integer;
  ViewInfo: TViewInfo;
  View: IView;
begin
if FisShuttingDown then exit;
FisShuttingDown := True;
if not hasClosedMainForm then
  FCtrl.FreeFormUponRelease( FCtrl.ControllerMainForm);

FCtrl.ShutDown;

for j := FViewSubsystems.Count - 1 downto 0 do
  if Supports( FViewSubsystems[j], IView, View) then
    View.ShutDown;
View := nil;
for j := FViewInfos.Count - 1 downto 0 do
  begin
  ViewInfo := FViewInfos[j] as TViewInfo;
  if not ViewInfo.FisController then
    ViewInfo.FView.ShutDown
  end;

FModel.ShutDown;

if assigned( FPer) then
  FPer.ShutDown;

FCtrl.SetModelStrata( nil);
FCtrl.SetControlServices( nil);

for j := FViewSubsystems.Count - 1 downto 0 do
  if Supports( FViewSubsystems[j], IView, View) then
    begin
    View.SetModelStrata( nil);
    View.SetSubSystemViewManager( nil)
    end;
View := nil;
FViewSubSystems := nil;
for j := FViewInfos.Count - 1 downto 0 do
  begin
  ViewInfo := FViewInfos[j] as TViewInfo;
  if not ViewInfo.FisController then
    begin
    ViewInfo.FView.SetModelStrata( nil);
    ViewInfo.FView.SetSubSystemViewManager( nil)
    end
  end;

FModel.SetViews( nil);
FModel.SetPersistenceStrata( nil);

if assigned( FPer) and (osPrstncHasAsychronousEvents in FApp.Options) then
  FPer.SetCallBack( nil);

FAppInfo.Free;
{$IFDEF SmartInspect}
if assigned( FSi) or assigned( FSess) then
  FApp.ReleaseSmartInspect( FSi, FSess);
{$ENDIF}

FOptions := nil;
FPer := nil;
FModel := nil;
FCtrl := nil;

FOptionsObj.Free;
FPrstncObj.Free;
FModelObj.Free;
FCtrlObj.Free;

if FLibHandle <> 0 then
  begin
  FSkinGateWay.Free;
  FreeLibrary( FLibHandle)
  end;

for j := FViewInfos.Count - 1 downto 0 do
  begin
  ViewInfo := FViewInfos[j] as TViewInfo;
  ViewInfo.FView := nil;
  ViewInfo.FViewObj.Free
  end;
FViewInfos.Free;
FApp.Free;
inherited
end;


function TBinder.InBuilt_CreateController( var Ctrl: TObject): IController;
begin
result := FApp.Acquire_InBuiltController( Ctrl)
end;



function TBinder.InBuilt_CreatePreviewMain(
  var Vw: TObject;
  const ConstructionContext: IInterface): IView;
begin
result := FApp.Acquire_InBuiltPreviewMain( Vw, ConstructionContext)
end;



function TBinder.InBuilt_CreateViews: TObjectList;
begin
result := FApp.Acquire_InBuiltViews
end;


function TBinder.InBuilt_SkinDisplayName: string;
begin
result := ''
end;


function TBinder.InBuilt_SkinProgId: string;
begin
result := ''
end;




procedure TBinder.IterateViews(
  Method: TExecuteViewProc; var Datum; Ascending: boolean);
var
  j: integer;
  View: IView;

  procedure DoIt( VwInf: TObject);
  var
    ViewInfo: TViewInfo;
  begin
  ViewInfo := VwInf as TViewInfo;
  Method( ViewInfo.FView, Datum)
  end;

begin
if FisIteratingViews then
  raise Exception.Create( 'Erroneous re-entry into View iteration.');
FisIteratingViews := True;
try
if not assigned( Method) then exit;
if Ascending then
    begin
    for j := 0 to FViewInfos.Count - 1 do
      DoIt( FViewInfos[j]);
    for j := 0 to FViewSubsystems.Count - 1 do
      if Supports( FViewSubsystems[j], IView, View) then
        Method( View, Datum)
    end
  else
    begin
    for j := FViewSubsystems.Count - 1 downto 0 do
      if Supports( FViewSubsystems[j], IView, View) then
        Method( View, Datum);
    for j := FViewInfos.Count - 1 downto 0 do
      DoIt( FViewInfos[j])
    end
finally
FisIteratingViews := False
end end;

end.
