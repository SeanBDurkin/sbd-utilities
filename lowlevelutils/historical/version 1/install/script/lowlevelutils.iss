[Setup]
OutputDir=V:\Projects\Samarkand\lowlevelutils\install\bin
SolidCompression=true
VersionInfoCompany=Samarkand and Sutherland Software Pty. Ltd.
VersionInfoDescription=SBD Delphi Low Level utility library
VersionInfoCopyright=Sean B. Durkin, 2003-2007
MinVersion=0,5.01.2600
AppCopyright=Sean B. Durkin, 2003-2007
AppName=SBD Low Level Utils
AppVerName=SBD Low Level Utils v1.0
DefaultDirName={pf}\Samarkand\lowlevelutils
AllowUNCPath=false
ShowComponentSizes=false
AlwaysShowDirOnReadyPage=true
ShowLanguageDialog=auto
AppPublisher=Samarkand and Sutherland Software
AppVersion=1.0
AppID={{7EC29CCE-D7A5-44B7-8019-765FB5873405}
UninstallDisplayName=SBD Low Level Utilities (Delphi library)


[CustomMessages]
cmDelphiInputDirPage_lblEdit0_Caption=directory:
cmDelphiInputDirPage_btnBrowse_Caption=Browse
cmDelphiInputDirPage_btnBrowse_DialogCaption=Select directory or use enviro. var. like $(DELPHI)

[Types]
Name: Normal; Description: Normal installation; Flags: iscustom

[Components]
Name: D7; Description: Install for Delphi 7
Name: D9; Description: Install for Delphi 2005
Name: D10; Description: Install for Delphi 2006

[Tasks]
Name: D7; Description: Compile package with Delphi 7; Components: D7
Name: D9; Description: Compile package with Delphi 2005; Components: D9
Name: D10; Description: Compile package with Delphi 2006; Components: D10

[Dirs]
Name: {code:GetIncDir}
Name: {code:GetBPL}; Tasks: D7 D9 D10; Check: IsNotEmpty( ExpandConstant( '{code:GetBPL}'))
Name: {code:GetD7_DCP}; Tasks: D7; Check: IsNotEmpty( ExpandConstant( '{code:GetD7_DCP}'))
Name: {code:GetD7_DCU}; Tasks: D7; Check: IsNotEmpty( ExpandConstant( '{code:GetD7_DCU}'))
Name: {code:GetD9_DCP}; Tasks: D9; Check: IsNotEmpty( ExpandConstant( '{code:GetD9_DCP}'))
Name: {code:GetD9_DCU}; Tasks: D9; Check: IsNotEmpty( ExpandConstant( '{code:GetD9_DCU}'))
Name: {code:GetD10_DCP}; Tasks: D10; Check: IsNotEmpty( ExpandConstant( '{code:GetD10_DCP}'))
Name: {code:GetD10_DCU}; Tasks: D10; Check: IsNotEmpty( ExpandConstant( '{code:GetD10_DCU}'))

[Files]
Source: V:\Projects\Samarkand\lowlevelutils\run\uSBD_FileUtils.pas; DestDir: {app}\run
Source: V:\Projects\Samarkand\lowlevelutils\run\uSBD_FlexiInterfaced.pas; DestDir: {app}\run
Source: V:\Projects\Samarkand\lowlevelutils\run\uSBD_FundamentTypeSupport.pas; DestDir: {app}\run
Source: V:\Projects\Samarkand\lowlevelutils\run\uSBD_IntegerList.pas; DestDir: {app}\run
Source: V:\Projects\Samarkand\lowlevelutils\run\uSBD_IntfRegistry.pas; DestDir: {app}\run
Source: V:\Projects\Samarkand\lowlevelutils\run\uSBD_ObjCache.pas; DestDir: {app}\run
Source: V:\Projects\Samarkand\lowlevelutils\run\uSBD_OSVersion.pas; DestDir: {app}\run
Source: V:\Projects\Samarkand\lowlevelutils\run\uSBD_PictureUtils.pas; DestDir: {app}\run
Source: V:\Projects\Samarkand\lowlevelutils\run\uSBD_RecallIntf.pas; DestDir: {app}\run
Source: V:\Projects\Samarkand\lowlevelutils\run\uSBD_Resources.pas; DestDir: {app}\run
Source: V:\Projects\Samarkand\lowlevelutils\run\uSBD_SortStrings.pas; DestDir: {app}\run
Source: V:\Projects\Samarkand\lowlevelutils\run\uSBD_Vers.pas; DestDir: {app}\run
Source: V:\Projects\Samarkand\lowlevelutils\run\uSBD_Visitor.pas; DestDir: {app}\run
Source: V:\Projects\Samarkand\lowlevelutils\run\uSBD_VisitorUtils.pas; DestDir: {app}\run
Source: V:\Projects\Samarkand\lowlevelutils\packages\d7\LowLevelUtils.cfg; DestDir: {app}\packages\d7
Source: V:\Projects\Samarkand\lowlevelutils\packages\d7\LowLevelUtils.dof; DestDir: {app}\packages\d7
Source: V:\Projects\Samarkand\lowlevelutils\packages\d7\LowLevelUtils.dpk; DestDir: {app}\packages\d7
Source: V:\Projects\Samarkand\lowlevelutils\packages\d7\LowLevelUtils.res; DestDir: {app}\packages\d7
Source: V:\projects\Samarkand\lowlevelutils\install\resources\Substitutions.ini; DestDir: {tmp}; Flags: deleteafterinstall
Source: V:\projects\Samarkand\lowlevelutils\install\resources\Substitutions7.ini; DestDir: {tmp}; Flags: deleteafterinstall; Components: D7
Source: V:\projects\Samarkand\lowlevelutils\install\resources\Substitutions9.ini; DestDir: {tmp}; Flags: deleteafterinstall; Components: D9
Source: V:\projects\Samarkand\lowlevelutils\install\resources\Substitutions10.ini; DestDir: {tmp}; Flags: deleteafterinstall; Components: D10
Source: V:\Projects\Common files\inc\SBDCompilers.inc; DestDir: {code:GetIncDir}
Source: V:\Projects\Common files\inc\SBDConfig.inc; DestDir: {code:GetIncDir}
Source: C:\Documents and Settings\Developer\My Documents\CodeGear Projects\Applications\Inno helper projects\string replacer\StringReplacer.exe; DestDir: {tmp}; Flags: deleteafterinstall
Source: C:\Documents and Settings\Developer\My Documents\CodeGear Projects\Applications\Inno helper projects\xml registry\xmlregistry.dll; DestDir: {tmp}; Flags: dontcopy
Source: C:\Documents and Settings\Developer\My Documents\CodeGear Projects\Applications\Inno helper projects\Command Console Pervert\CommandConsolePervert.exe; DestDir: {tmp}; Flags: deleteafterinstall
Source: V:\projects\Samarkand\lowlevelutils\install\resources\Run_Delphi7_CommandLine.xml; DestDir: {tmp}; Flags: deleteafterinstall; Tasks: D7
Source: V:\projects\Samarkand\lowlevelutils\install\resources\Run_Delphi9_CommandLine.xml; DestDir: {tmp}; Flags: deleteafterinstall; Tasks: D9
Source: V:\projects\Samarkand\lowlevelutils\install\resources\Run_Delphi10_CommandLine.xml; DestDir: {tmp}; Flags: deleteafterinstall; Tasks: D10

[INI]
FileName: {tmp}\Substitutions.ini; Section: Substitutions; Key: {{$I '..\..\..\Common files\inc\SBDCompilers.inc'}; String: {{$I '{code:GetIncDirRelativeWithSlash}SBDCompilers.inc'}
FileName: {tmp}\Substitutions.ini; Section: Substitutions; Key: {{$I '..\..\..\Common files\inc\SBDConfig.inc'}; String: {{$I '{code:GetIncDirRelativeWithSlash}SBDConfig.inc'}
FileName: {tmp}\Substitutions.ini; Section: Substitutions; Key: {{$define TntSupport}; String: {code:GetDefineTntSupport}
FileName: {tmp}\Substitutions.ini; Section: Substitutions; Key: {{$define USEJVCL}; String: {code:GetDefineUSEJVCL}
FileName: {tmp}\Substitutions7.ini; Section: Substitutions; Key: "-E""$(BPL)"""; String: "-E""{code:GetBPL}"""; Components: D7
FileName: {tmp}\Substitutions7.ini; Section: Substitutions; Key: "-N""$(DCU)"""; String: "-N""{code:GetD7_DCU}"""; Components: D7
FileName: {tmp}\Substitutions7.ini; Section: Substitutions; Key: "-LE""$(BPL)"""; String: "-LE""{code:GetBPL}"""; Components: D7
FileName: {tmp}\Substitutions7.ini; Section: Substitutions; Key: "-LN""$(DCP)"""; String: "-LN""{code:GetD7_DCP}"""; Components: D7
FileName: {tmp}\Substitutions7.ini; Section: Substitutions; Key: "-LU""rtl;vcl;vcljpg;TntUnicodeVcl_R70"""; String: "-LU""rtl;vcl;vcljpg{code:GetSemiTntUnicodeVcl_R70}"""; Components: D7
FileName: {tmp}\Substitutions7.ini; Section: Substitutions; Key: "-u"";"""; String: "-u"";{code:D7_Extra_DCP_WithSemi}{code:GetD7_DCU}"""; Components: D7
FileName: {tmp}\Substitutions7.ini; Section: Substitutions; Key: ",TntUnicodeVcl_R70;"; String: "{code:UseTntUnicodeVcl_R70}"; Components: D7
FileName: {app}\packages\d7\LowLevelUtils.dof; Section: Directories; Key: "OutputDir"; String: "{code:GetBPL}"; Components: D7
FileName: {app}\packages\d7\LowLevelUtils.dof; Section: Directories; Key: "UnitOutputDir"; String: "{code:GetD7_DCU}"; Components: D7
FileName: {app}\packages\d7\LowLevelUtils.dof; Section: Directories; Key: "PackageDLLOutputDir"; String: "{code:GetBPL}"; Components: D7
FileName: {app}\packages\d7\LowLevelUtils.dof; Section: Directories; Key: "PackageDCPOutputDir"; String: "{code:GetD7_DCP}"; Components: D7
FileName: {app}\packages\d7\LowLevelUtils.dof; Section: HistoryLists\hlUnitOutputDirectory; Key: "Count"; String: "0"; Components: D7
FileName: {app}\packages\d7\LowLevelUtils.dof; Section: HistoryLists\hlOutputDirectorry; Key: "Count"; String: "0"; Components: D7
FileName: {app}\packages\d7\LowLevelUtils.dof; Section: HistoryLists\hlBPLOutput; Key: "Count"; String: "0"; Components: D7
FileName: {app}\packages\d7\LowLevelUtils.dof; Section: HistoryLists\hlDCPOutput; Key: "Count"; String: "0"; Components: D7


;TO DO
;                                         need a page to ask for Tnt dcp path
;                                         need a page to ask for Tnt support & Jedi support
;                                         need to test for Tnt installed and Jedi installed for each compiler.
;                                         auto-detect if TntDCP is on target dcp dir.
;                                         auto-detect jedi, tnt installed
;                                         warn if not installed
;                                         warn if re-install to different app dir
[Run]
FileName: {tmp}\StringReplacer.exe; Parameters: """/s{tmp}\Substitutions.ini"" ""/t{app}\run\uSBD_FileUtils.pas"" /r+"; StatusMsg: "Localising unit locations"
FileName: {tmp}\StringReplacer.exe; Parameters: """/s{tmp}\Substitutions.ini"" ""/t{app}\run\uSBD_FlexiInterfaced.pas"" /r+"; StatusMsg: "Localising unit locations"
Filename: {tmp}\StringReplacer.exe; Parameters: """/s{tmp}\Substitutions.ini"" ""/t{app}\run\uSBD_PictureUtils.pas"" /r+"; StatusMsg: "Localising unit locations"
Filename: {tmp}\StringReplacer.exe; Parameters: """/s{tmp}\Substitutions.ini"" ""/t{app}\run\uSBD_SortStrings.pas"" /r+"; StatusMsg: "Localising unit locations"
Filename: {tmp}\StringReplacer.exe; Parameters: """/s{tmp}\Substitutions.ini"" ""/t{code:GetIncDir}\SBDConfig.inc"" /r+"; StatusMsg: "Configuring project files"
Filename: {tmp}\StringReplacer.exe; Parameters: """/s{tmp}\Substitutions7.ini"" ""/t{app}\packages\d7\LowLevelUtils.cfg"" /r+"; StatusMsg: "Configuring project files"; Components: D7
Filename: {tmp}\StringReplacer.exe; Parameters: """/s{tmp}\Substitutions7.ini"" ""/t{app}\packages\d7\LowLevelUtils.dpk"" /r+"; StatusMsg: "Configuring project files"; Components: D7
Filename: {tmp}\CommandConsolePervert.exe; Parameters: """{tmp}\Run_Delphi7_CommandLine.xml"""; WorkingDir: {code:Delphi7_RootDir}\bin; Tasks: D7
Filename: {tmp}\CommandConsolePervert.exe; Parameters: """{tmp}\Run_Delphi9_CommandLine.xml"""; WorkingDir: {code:Delphi9_RootDir}\bin; Tasks: D9
Filename: {tmp}\CommandConsolePervert.exe; Parameters: """{tmp}\Run_Delphi10_CommandLine.xml"""; WorkingDir: {code:Delphi10_RootDir}\bin; Tasks: D10

[UninstallDelete]
Type: files; Name: "{code:GetD7_DCU}\uSBD_FileUtils.dcu"; Components: D7
Type: files; Name: "{code:GetD7_DCU}\uSBD_FlexiInterfaced.dcu"; Components: D7
Type: files; Name: "{code:GetD7_DCU}\uSBD_FundamentTypeSupport.dcu"; Components: D7
Type: files; Name: "{code:GetD7_DCU}\uSBD_IntegerList.dcu"; Components: D7
Type: files; Name: "{code:GetD7_DCU}\uSBD_IntfRegistry.dcu"; Components: D7
Type: files; Name: "{code:GetD7_DCU}\uSBD_ObjCache.dcu"; Components: D7
Type: files; Name: "{code:GetD7_DCU}\uSBD_OSVersion.dcu"; Components: D7
Type: files; Name: "{code:GetD7_DCU}\uSBD_PictureUtils.dcu"; Components: D7
Type: files; Name: "{code:GetD7_DCU}\uSBD_RecallIntf.dcu"; Components: D7
Type: files; Name: "{code:GetD7_DCU}\uSBD_Resources.dcu"; Components: D7
Type: files; Name: "{code:GetD7_DCU}\uSBD_SortStrings.dcu"; Components: D7
Type: files; Name: "{code:GetD7_DCU}\uSBD_Vers.dcu"; Components: D7
Type: files; Name: "{code:GetD7_DCU}\uSBD_Visitor.dcu"; Components: D7
Type: files; Name: "{code:GetD7_DCU}\uSBD_VisitorUtils.dcu"; Components: D7
Type: files; Name: "{code:GetD7_DCU}\LowLevelUtils.dcp"; Components: D7

Type: files; Name: "{code:GetD9_DCU}\uSBD_FileUtils.dcu"; Components: D9
Type: files; Name: "{code:GetD9_DCU}\uSBD_FlexiInterfaced.dcu"; Components: D9
Type: files; Name: "{code:GetD9_DCU}\uSBD_FundamentTypeSupport.dcu"; Components: D9
Type: files; Name: "{code:GetD9_DCU}\uSBD_IntegerList.dcu"; Components: D9
Type: files; Name: "{code:GetD9_DCU}\uSBD_IntfRegistry.dcu"; Components: D9
Type: files; Name: "{code:GetD9_DCU}\uSBD_ObjCache.dcu"; Components: D9
Type: files; Name: "{code:GetD9_DCU}\uSBD_OSVersion.dcu"; Components: D9
Type: files; Name: "{code:GetD9_DCU}\uSBD_PictureUtils.dcu"; Components: D9
Type: files; Name: "{code:GetD9_DCU}\uSBD_RecallIntf.dcu"; Components: D9
Type: files; Name: "{code:GetD9_DCU}\uSBD_Resources.dcu"; Components: D9
Type: files; Name: "{code:GetD9_DCU}\uSBD_SortStrings.dcu"; Components: D9
Type: files; Name: "{code:GetD9_DCU}\uSBD_Vers.dcu"; Components: D9
Type: files; Name: "{code:GetD9_DCU}\uSBD_Visitor.dcu"; Components: D9
Type: files; Name: "{code:GetD9_DCU}\uSBD_VisitorUtils.dcu"; Components: D9
Type: files; Name: "{code:GetD9_DCU}\LowLevelUtils.dcp"; Components: D9

Type: files; Name: "{code:GetD10_DCU}\uSBD_FileUtils.dcu"; Components: D10
Type: files; Name: "{code:GetD10_DCU}\uSBD_FlexiInterfaced.dcu"; Components: D10
Type: files; Name: "{code:GetD10_DCU}\uSBD_FundamentTypeSupport.dcu"; Components: D10
Type: files; Name: "{code:GetD10_DCU}\uSBD_IntegerList.dcu"; Components: D10
Type: files; Name: "{code:GetD10_DCU}\uSBD_IntfRegistry.dcu"; Components: D10
Type: files; Name: "{code:GetD10_DCU}\uSBD_ObjCache.dcu"; Components: D10
Type: files; Name: "{code:GetD10_DCU}\uSBD_OSVersion.dcu"; Components: D10
Type: files; Name: "{code:GetD10_DCU}\uSBD_PictureUtils.dcu"; Components: D10
Type: files; Name: "{code:GetD10_DCU}\uSBD_RecallIntf.dcu"; Components: D10
Type: files; Name: "{code:GetD10_DCU}\uSBD_Resources.dcu"; Components: D10
Type: files; Name: "{code:GetD10_DCU}\uSBD_SortStrings.dcu"; Components: D10
Type: files; Name: "{code:GetD10_DCU}\uSBD_Vers.dcu"; Components: D10
Type: files; Name: "{code:GetD10_DCU}\uSBD_Visitor.dcu"; Components: D10
Type: files; Name: "{code:GetD10_DCU}\uSBD_VisitorUtils.dcu"; Components: D10
Type: files; Name: "{code:GetD10_DCU}\LowLevelUtils.dcp"; Components: D10

Type: files; Name: "{code:GetBPL}\LowLevelUtils.bpl"; Components: D7 D9 D10

[Code]
var
  CoLocateInc_Page: TInputOptionWizardPage;
  IncDir_Page: TWizardPage;
  D7Dirs_Page: TWizardPage;
  D9Dirs_Page: TWizardPage;
  D10Dirs_Page: TWizardPage;
  BPL_Page: TWizardPage;

  edtIncDir: TEdit;
  edtD7_DCU: TEdit;
  edtD7_DCP: TEdit;
  edtD9_DCU: TEdit;
  edtD9_DCP: TEdit;
  edtD10_DCU: TEdit;
  edtD10_DCP: TEdit;
  edt_BPL: TEdit;

  ObjectAssociator_From: TStrings;
  ObjectAssociator_To: TStrings;

  TntPath_Page: TWizardPage;
  edt_Tnt7, edt_Tnt9, edt_Tnt10: TEdit;

// Due to the limitations of the scripting language, it is very
//  difficult to efficiently and simply map one object to another.
// The next two procedures (Associate and FindAssociate), somewhat
//  inefficiently, associate or map one object to another.
// This could be used for example, to map a browse button to
// its associated TEdit box.

procedure Associate( FromObj, ToObj: TObject);
var
  Idx: integer;
begin
Idx := ObjectAssociator_From.Add('');
ObjectAssociator_From.Objects[ Idx] := FromObj;
ObjectAssociator_To.Add('');
ObjectAssociator_To.Objects[ Idx] := ToObj
end;


function FindAssociate( FromObj: TObject): TObject;
var
  j: integer;
begin
result := nil;
for j := 0 to ObjectAssociator_From.Count - 1 do
  begin
  if ObjectAssociator_From.Objects[j] <> FromObj then continue;
  result := ObjectAssociator_To.Objects[j];
  break
  end;
end;


function DLL_ReadXMLdoc( FileName, Key: PChar): PChar;
external 'ReadXMLdoc@files:xmlregistry.dll pascal';

function Inno_ReadXMLdoc( FileName, Key: string): string;
begin
result := DLL_ReadXMLdoc( FileName, Key)
end;




function isCoLocatingIni: boolean;
begin
result := CoLocateInc_Page.Values[0]
end;



function GetIncDir( Value: string): string;
begin
if isCoLocatingIni then
    result := ExpandConstant( '{app}\run')
  else
    result := edtIncDir.Text;
result := RemoveBackSlash( result)
end;



function GetIncDirRelativeWithSlash( Value: string): string;
begin
if isCoLocatingIni then
    result := ''
  else
    result := RemoveBackSlash( edtIncDir.Text);
if result <> '' then
  result := result + '\'
end;


function DelphiId( Delphi: integer): string;
begin
case Delphi of
  7:    result := 'Delphi\7.0';
  9:    result := 'BDS\3.0';
  10:   result := 'BDS\4.0';
  else  result := 'not supported'
  end
end;



function Delphi_RootDir( Delphi: integer): string;
begin
if RegQueryStringValue( HKEY_CURRENT_USER, 'Software\Borland\' + DelphiId( Delphi), 'RootDir', result) then
    result := RemoveBackSlash( result)
  else
    result := ''
end;




function hasDelphi( Delphi: integer): boolean;
begin
result := (Delphi_RootDir( Delphi) <> '') and FileExists( Delphi_RootDir( Delphi) + '\bin\dcc32.exe')
end;


function Delphi7_RootDir( Value: string): string;
begin
result := Delphi_RootDir( 7)
end;

function hasDelphi7: boolean;
begin
result := hasDelphi( 7)
end;


function Delphi9_RootDir( Value: string): string;
begin
result := Delphi_RootDir( 9)
end;




function hasDelphi9: boolean;
begin
result := hasDelphi( 9)
end;


function Delphi10_RootDir( Value: string): string;
begin
result := Delphi_RootDir( 10)
end;

function hasDelphi10: boolean;
begin
result := hasDelphi( 10)
end;


function LibraryRegKey( Delphi: integer): string;
begin
result := 'Software\Borland\' + DelphiId(Delphi) + '\Library'
end;


function GetDelphiReg( Delphi: integer; const Value: string; var Content: string): boolean;
begin
result := RegQueryStringValue( HKEY_CURRENT_USER, LibraryRegKey( Delphi), Value, Content) and
          (Content <> '');
if result then
  Content := RemoveBackSlash( Content)
end;



function DefBDS_Project( Delphi: integer): string;
begin
result := ExpandConstant('{localappdata}\Borland\' + DelphiId(Delphi) + '\DefProject.bdsproj')
end;


function EncodeProductKind( const Kind: string): string;
begin
if Kind = 'DCU' then
    result := 'UnitOutputDir';
if Kind = 'DCP' then
    result := 'PackageDCPOutputDir';
if Kind = 'BPL' then
    result := 'PackageDLLOutputDir'
end;


function DefBDSProj_Key( const Value: string): string;
begin
result := 'BorlandProject\Delphi.Personality\Directories\Directories\\Name="';
result := result + EncodeProductKind( Value) + '"'
end;


function DefProjXML( Delphi: integer; Kind: string): string;
begin
result := RemoveBackSlash( Inno_ReadXMLdoc( DefBDS_Project( Delphi), DefBDSProj_Key( Kind)))
end;


function D7_Dir( kind: string): string;
begin
result := RemoveBackSlash( GetIniString( 'Directories',
  EncodeProductKind( Kind), '', Delphi7_RootDir('') + '\bin\defproj.dof'))
end;


function D7_Default_DCU: string;
begin
result := D7_Dir( 'DCU')
end;


function GetNonRegDefaultDir( Delphi: integer; Kind: string): string;
begin
if Delphi = 7 then
    result := D7_Dir( Kind)
  else
    result := DefProjXML( Delphi, Kind)
end;


function Default_DCU( Delphi: integer): string;
begin
result := GetNonRegDefaultDir( Delphi, 'DCU')
end;

function D9_Default_DCU: string;
begin
result := Default_DCU( 9)
end;

function D10_Default_DCU: string;
begin
result := Default_DCU( 10)
end;


function Default_DCP( Delphi: integer): string;
begin
if not GetDelphiReg( Delphi, 'Package DCP Output', result) then
  result := GetNonRegDefaultDir( Delphi, 'DCP')
end;


function D7_Default_DCP: string;
begin
result := Default_DCP( 7)
end;

function D9_Default_DCP: string;
begin
result := Default_DCP( 9)
end;

function D10_Default_DCP: string;
begin
result := Default_DCP( 10)
end;


function Default_BPL( Delphi: integer): string;
begin
if not GetDelphiReg( Delphi, 'Package DPL Output', result) then
  result := GetNonRegDefaultDir( Delphi, 'BPL')
end;



function Universal_Default_BPL_ByInstallation: string;
begin
result := '';
if hasDelphi10 then
  result := Default_BPL( 10);
if (result = '') and hasDelphi9 then
  result := Default_BPL( 9);
if (result = '') and hasDelphi7 then
  result := Default_BPL( 7)
end;


function Universal_Default_BPL_BySelection: string;
begin
result := '';
if WizardForm.ComponentsList.Checked[2] then
  result := Default_BPL( 10);
if (result = '') and WizardForm.ComponentsList.Checked[1] then
  result := Default_BPL( 9);
if (result = '') and WizardForm.ComponentsList.Checked[0] then
  result := Default_BPL( 7);
if result = '' then
  result := Universal_Default_BPL_ByInstallation
end;

// OnClick event handler for Browse buttons.
procedure DelphiInputDir_BrowseClick( Sender: TObject);
var
  Edit1: TEdit;
  Dir: string;
  Obj: TObject;
begin
Obj := FindAssociate( Sender);
if not (Obj is TEdit) then exit;
Edit1 := TEdit( Obj);
Dir := Edit1.Text;
if BrowseForFolder( ExpandConstant('{cm:cmDelphiInputDirPage_btnBrowse_DialogCaption}'), Dir, True) then
  Edit1.Text := Dir
end;


// This function is very similiar in effect to the standard
//  function CreateInputDirPage. The difference is, that unlike
// TInputFileWizardPage, the user is allowed to enter in
// Delphi IDE environment variables, such as $(BPL) or $(DELPHI).
function CreateDelphiInputDirPage( PreviousPageId: Integer;
  Caption, Description, Intro: string; var ControlTop, TabOrder1: integer): TWizardPage;
var
  lblIntro: TNewStaticText;
begin
result := CreateCustomPage( PreviousPageId, Caption, Description);
lblIntro := TNewStaticText.Create(result);
with lblIntro do
  begin
  Parent := result.Surface;
  Caption := Intro;
  Left := ScaleX(0);
  Top := ScaleY(0);
  Width := ScaleX(413);
  Height := ScaleY(70);
  Align := alTop;
  AutoSize := False;
  TabOrder := 0;
  WordWrap := True
  end;
ControlTop := 80;
TabOrder1 := 1;
end;



function CreateDelphiDirEdit( Page: TWizardPage; Caption1: string; var ControlTop, TabOrder1: integer): TEdit;
var
  lblEdit0   : TLabel;
  btnBrowse0 : TButton;
begin
result := TEdit.Create( Page);
with result do
  begin
  Parent   := Page.Surface;
  Left     := ScaleX( 0);
  Top      := ScaleY( ControlTop + 16);
  Width    := ScaleX( 329);
  Height   := ScaleY( 21);
  TabOrder := TabOrder1;
  end;
TabOrder1 := TabOrder1 + 1;
lblEdit0 := TLabel.Create(result);
with lblEdit0 do
  begin
  Parent  := Page.Surface;
  if Caption1 = '' then
      Caption := ExpandConstant('{cm:cmDelphiInputDirPage_lblEdit0_Caption}')
    else
      Caption := Caption1;
  Left := ScaleX(0);
  Top  := ControlTop;
  Width := ScaleX(100);
  Height := ScaleY(13);
  AutoSize := True;
  FocusControl := result
  end;
btnBrowse0 := TButton.Create( Page);
with btnBrowse0 do
  begin
  Parent := Page.Surface;
  Caption := ExpandConstant('{cm:cmDelphiInputDirPage_btnBrowse_Caption}');
  Left := ScaleX(336);
  Top := ScaleY( ControlTop + 16);
  Width := ScaleX(75);
  Height := ScaleY(23);
  TabOrder := TabOrder1;
  Associate( btnBrowse0, result);
  OnClick := @DelphiInputDir_BrowseClick;
  end;
TabOrder1 := TabOrder1 + 1;
ControlTop := ControlTop + 38
end;




procedure CreateTheWizardPages;
var
  ControlTop: integer;
  TabOrder1: integer;
begin
CoLocateInc_Page := CreateInputOptionPage( wpSelectProgramGroup, 'Include files',
  'Do you want to co-locate include files (*.inc) with the source code? ',
  'If you want to co-locate the include files, please check the box below, then click Next. ' +
  'If you want to deploy them to another place, then uncheck the box below. ' +
  'Putting the include files from many different projects into one location helps the ' +
  'developer to share .inc files across projects and manage configurations more easily.',
  False, False);
CoLocateInc_Page.Add('Co-locate');
CoLocateInc_Page.Values[0] := False;


IncDir_Page := CreateDelphiInputDirPage( CoLocateInc_Page.Id, 'Include files',
  'Where should common include (*.inc) files be stored?',
  'Personal data files will be stored in the following folder.'#13#10#13#10 +
  'To continue, click Next. If you would like to select a different folder, click Browse.',
  ControlTop, TabOrder1);
edtIncDir := CreateDelphiDirEdit( IncDir_Page, 'Include directory', ControlTop, TabOrder1);
edtIncDir.Text := ExpandConstant('{cf}\Samarkand\inc');


D7Dirs_Page := CreateDelphiInputDirPage(IncDir_Page.Id, 'Compiler Directories',
  'Where should the compile products (*.dcu & *dcp) for Delphi 7 be stored?',
  'Specify the output folders. Can be be left blank. You can also use environment symbols, ' +
  'such as $(DCU).'#13#10#13#10 +
  'To continue, click Next. If you would like to select a different folder, click Browse.',
  ControlTop, TabOrder1);
edtD7_DCU := CreateDelphiDirEdit( D7Dirs_Page, 'dcu directory', ControlTop, TabOrder1);
edtD7_DCU.Text := D7_Default_DCU;
edtD7_DCP := CreateDelphiDirEdit( D7Dirs_Page, 'dcp directory', ControlTop, TabOrder1);
edtD7_DCP.Text := D7_Default_DCP;

D9Dirs_Page := CreateDelphiInputDirPage( D7Dirs_Page.Id, 'Compiler Directories',
  'Where should the compile products (*.dcu & *dcp) for Delphi 2005 be stored?',
  'Specify the output folders. Can be be left blank. You can also use environment symbols, ' +
  'such as $(DCU).'#13#10#13#10 +
  'To continue, click Next. If you would like to select a different folder, click Browse.',
  ControlTop, TabOrder1);
edtD9_DCU := CreateDelphiDirEdit( D9Dirs_Page, 'dcu directory', ControlTop, TabOrder1);
edtD9_DCU.Text := D9_Default_DCU;
edtD9_DCP := CreateDelphiDirEdit( D9Dirs_Page, 'dcp directory', ControlTop, TabOrder1);
edtD9_DCP.Text := D9_Default_DCP;

D10Dirs_Page := CreateDelphiInputDirPage( D9Dirs_Page.Id, 'Compiler Directories',
  'Where should the compile products (*.dcu & *dcp) for Delphi 2006 be stored?',
  'Specify the output folders. Can be be left blank. You can also use environment symbols, ' +
  'such as $(DCU).'#13#10#13#10 +
  'To continue, click Next. If you would like to select a different folder, click Browse.',
  ControlTop, TabOrder1);
edtD10_DCU := CreateDelphiDirEdit( D10Dirs_Page, 'dcu directory', ControlTop, TabOrder1);
edtD10_DCU.Text := D10_Default_DCU;
edtD10_DCP := CreateDelphiDirEdit( D10Dirs_Page, 'dcp directory', ControlTop, TabOrder1);
edtD10_DCP.Text := D10_Default_DCP;

BPL_Page := CreateDelphiInputDirPage( D10Dirs_Page.Id, 'Compiler Directories',
  'Where should the package binary (*.bpl) be stored?',
  'Specify the package output folder. This will be the same for all compilers. ' +
  'Can be be left blank. You can also use environment symbols, ' +
  'such as $(BPL). What-ever it is, it needs to be on the system PATH.'#13#10#13#10 +
  'To continue, click Next. If you would like to select a different folder, click Browse.',
  ControlTop, TabOrder1);
edt_BPL := CreateDelphiDirEdit( BPL_Page, 'BPL directory', ControlTop, TabOrder1);
edt_BPL.Text := Universal_Default_BPL_BySelection

TntPath_Page := CreateDelphiInputDirPage( BPL_Page.Id, 'Path to Tnt package',
  'Where is the Tnt Unicode Control run-time library compiled library located?',
  'For each of the comiplers, specify the location of the TntUnicodeVcl_RXX.dcp file. ' +
  'This can be be left blank if it is already in the same place as the dcp ' +
  'path that you specified in the previous pages. You can also use environment symbols, ' +
  'such as $(DCP).'#13#10#13#10 +
  'To continue, click Next. If you would like to select a different folder, click Browse.',
  ControlTop, TabOrder1);
edt_Tnt7 := CreateDelphiDirEdit( TntPath_Page, 'Path to TntUnicodeVcl_R70.dcp', ControlTop, TabOrder1);
edt_Tnt7.Text := ''
edt_Tnt9 := CreateDelphiDirEdit( TntPath_Page, 'Path to TntUnicodeVcl_R90.dcp', ControlTop, TabOrder1);
edt_Tnt9.Text := ''
edt_Tnt10 := CreateDelphiDirEdit( TntPath_Page, 'Path to TntUnicodeVcl_R100.dcp', ControlTop, TabOrder1);
edt_Tnt10.Text := ''
end;



procedure InitializeThePages;
begin
WizardForm.ComponentsList.Checked[0] := hasDelphi7;
WizardForm.ComponentsList.Checked[1] := hasDelphi9;
WizardForm.ComponentsList.Checked[2] := hasDelphi10;
WizardForm.ComponentsList.ItemEnabled[0] := hasDelphi7;
WizardForm.ComponentsList.ItemEnabled[1] := hasDelphi9;
WizardForm.ComponentsList.ItemEnabled[2] := hasDelphi10
end;


procedure InitializeWizard;
begin
ObjectAssociator_From := TStringList.Create;
ObjectAssociator_To   := TStringList.Create;
CreateTheWizardPages;
InitializeThePages;
end;


function ShouldSkip_IncDir_Page: boolean;
begin
result := isCoLocatingIni
end;


function ShouldSkip_D7_Page: boolean;
begin
result := not WizardForm.ComponentsList.Checked[0]
end;


function ShouldSkip_D9_Page: boolean;
begin
result := not WizardForm.ComponentsList.Checked[1]
end;


function ShouldSkip_D10_Page: boolean;
begin
result := not WizardForm.ComponentsList.Checked[2]
end;


function ShouldSkip_BPL_Page: boolean;
begin
result := not (
  WizardForm.ComponentsList.Checked[0] or
  WizardForm.ComponentsList.Checked[1] or
  WizardForm.ComponentsList.Checked[2])
end;



function ShouldSkipPage( PageID: Integer): Boolean;
begin
if        PageID = IncDir_Page.ID then
    result := ShouldSkip_IncDir_Page
  else if PageID = D7Dirs_Page.ID then
    result := ShouldSkip_D7_Page
  else if PageID = D9Dirs_Page.ID then
    result := ShouldSkip_D9_Page
  else if PageID = D10Dirs_Page.ID then
    result := ShouldSkip_D10_Page
  else if PageID = BPL_Page.ID then
    result := ShouldSkip_BPL_Page
end;



function GetBPL( Value: string): string;
begin
result := RemoveBackSlash( edt_BPL.Text)
end;

function GetD7_DCU( Value: string): string;
begin
result := RemoveBackSlash( edtD7_DCU.Text)
end;

function GetD7_DCP( Value: string): string;
begin
result := RemoveBackSlash( edtD7_DCP.Text)
end;

function D7_Extra_DCP_WithSemi( Value: string): string;
begin
result := '' // to be developed

if result <> '' then
  result := RemoveBackSlash( result) + ';'
end;


function GetD9_DCU( Value: string): string;
begin
result := RemoveBackSlash( edtD9_DCU.Text)
end;

function GetD9_DCP( Value: string): string;
begin
result := RemoveBackSlash( edtD9_DCP.Text)
end;

function D9_Extra_DCP_WithSemi( Value: string): string;
begin
result := '' // to be developed

if result <> '' then
  result := RemoveBackSlash( result) + ';'
end;

function GetD10_DCU( Value: string): string;
begin
result := RemoveBackSlash( edtD10_DCU.Text)
end;

function GetD10_DCP( Value: string): string;
begin
result := RemoveBackSlash( edtD10_DCP.Text)
end;

function D10_Extra_DCP_WithSemi( Value: string): string;
begin
result := '' // to be developed

if result <> '' then
  result := RemoveBackSlash( result) + ';'
end;


function TntSupport: boolean;
begin
result := True
end;


function JediSupport: boolean;
begin
result := True
end;


function UseTntUnicodeVcl_R70( Value: string): string;
begin
if TntSupport then
    result := ';'
  else
    result := ',TntUnicodeVcl_R70;'
end;

function GetSemiTntUnicodeVcl_R70( Value: string): string;
begin
if TntSupport then
    result := ''
  else
    result := ';TntUnicodeVcl_R70'
end;


function GetDefineTntSupport( Value: string): string;
begin
if TntSupport then
    result := '{$define TntSupport}'
  else
    result := '{.$define TntSupport}'
end;

function GetDefineUSEJVCL( Value: string): string;
begin
if JediSupport then
    result := '{$define USEJVCL}'
  else
    result := '{.$define USEJVCL}'
end;


function IsNotEmpty( Value: string): boolean;
begin
result := Value <> ''
end;
