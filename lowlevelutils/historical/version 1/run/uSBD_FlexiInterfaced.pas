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

unit uSBD_FlexiInterfaced;

{$I 'SBDCompilers.inc'}
{$I 'SBDConfig.inc'}

{$WARNINGS OFF}
interface
uses Classes;

type
  TInterfacingOption = (
    ioIsCounting,          // Object is reference counted.
    ioWeakOwnerReference,  // Object does not keep a counted reference to its owner.
    ioAggregating);        // Object is aggregated with its owner.
  TInterfacingOptionSet = set of TInterfacingOption;

  TFlexiInterfaced = class( TPersistent, IInterface)
  private
    FOwnerIntf_Weak  : pointer;
    FOptions: TInterfacingOptionSet;
    FDelayedIsCounting: boolean;
    FRefCount: Integer;

    function  GetIsCounting: boolean;

  protected

    function QueryInterface( const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

  public
    class function NewInstance: TObject; override;
    constructor Create( Options1: TInterfacingOptionSet);
    destructor  Destroy; override;

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    property RefCount: Integer    read FRefCount;
    property isCounting: boolean  read GetisCounting;
  end;


implementation







uses SysUtils, uSBD_Resources;

const  kernel = 'kernel32.dll';

function InterlockedIncrement(var Addend: Integer): Integer; stdcall;
  external kernel name 'InterlockedIncrement';

function InterlockedDecrement(var Addend: Integer): Integer; stdcall;
  external kernel name 'InterlockedDecrement';

{ TFlexiInterfaced }

procedure TFlexiInterfaced.AfterConstruction;
var
  j: integer;
  FOwnerIntf_Strong: IInterface;

begin
if GetOwner <> nil then
   GetOwner.GetInterface( IInterface, FOwnerIntf_Strong);
FOwnerIntf_Weak := pointer( FOwnerIntf_Strong);
if FDelayedIsCounting and assigned( FOwnerIntf_Strong) then
    for j := FRefCount downto 2 do
      begin
      FOwnerIntf_Strong._AddRef;
      InterlockedDecrement( FRefCount)
      end;
if not (ioWeakOwnerReference in FOptions) then
  FOwnerIntf_Strong._AddRef;
if ((not FDelayedIsCounting) or assigned( FOwnerIntf_Strong)) and
   (FRefCount <> 1) then
  Exception.Create( Format( sUnbalancedReferenceCount, [ClassName]));
if FDelayedIsCounting then
    Include( FOptions, ioIsCounting)
  else
    Exclude( FOptions, ioIsCounting);
InterlockedDecrement( FRefCount);
end;




procedure TFlexiInterfaced.BeforeDestruction;
begin
if (ioIsCounting in FOptions) and (RefCount <> 0) then
  Error( reInvalidPtr)
end;




constructor TFlexiInterfaced.Create( Options1: TInterfacingOptionSet);
begin
FOptions := Options1 * [ioWeakOwnerReference, ioAggregating]
end;



destructor TFlexiInterfaced.Destroy;
begin
if (not (ioWeakOwnerReference in FOptions)) and assigned( FOwnerIntf_Weak) then
  begin
  IInterface( FOwnerIntf_Weak)._Release;
  FOwnerIntf_Weak := nil
  end;
inherited
end;




function TFlexiInterfaced.GetIsCounting: boolean;
begin
result := ioIsCounting in FOptions
end;





class function TFlexiInterfaced.NewInstance: TObject;
begin
result := inherited NewInstance;
TFlexiInterfaced( result).FRefCount          := 1;
TFlexiInterfaced( result).FOptions           := [ioIsCounting];
TFlexiInterfaced( result).FDelayedIsCounting := False;
end;



function TFlexiInterfaced.QueryInterface( const IID: TGUID; out Obj): HResult;
begin
if GetInterface( IID, Obj) then
    result := 0
  else if (ioAggregating in FOptions) and assigned( FOwnerIntf_Weak) then
    result := IInterface( FOwnerIntf_Weak).QueryInterface( IID, Obj)
  else
    result := E_NOINTERFACE
end;




function TFlexiInterfaced._AddRef: Integer;
begin
if assigned( FOwnerIntf_Weak) then
    result := IInterface(FOwnerIntf_Weak)._AddRef
  else if ioWeakOwnerReference in FOptions then
    result := InterlockedIncrement( FRefCount)
  else
    result := -1
end;



function TFlexiInterfaced._Release: Integer;
begin
if assigned( FOwnerIntf_Weak) then
    result := IInterface(FOwnerIntf_Weak)._Release
  else if ioWeakOwnerReference in FOptions then
    begin
    result := InterlockedDecrement( FRefCount);
    if result = 0 then
      destroy
    end
  else
    result := -1
end;

end.

