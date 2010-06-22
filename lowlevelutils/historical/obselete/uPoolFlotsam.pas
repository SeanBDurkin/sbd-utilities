unit uPoolFlotsam;
// This is a utility unit belonging to the "SBD Libraries".
// Copyright (c) Sean B. Durkin, 2003 .  All rights reserved.
// sdurkin@siliconrose.com.au

interface
uses Classes;

type
  IWeakObject = interface
  ['{6BB9AB32-742B-4FCB-8414-98D6FBE9A811}']
    procedure Weaken;
    procedure Strengthen;
    end;

  TPoolFlotsam = class;
  IPoolKeeper = interface
  ['{3F2566E1-23FF-45B1-AF37-733E3A895518}']
    procedure FlotsamReturn( Returnee: TPoolFlotsam);
    end;

  TPoolFlotsam = class(TPersistent, IInterface, IWeakObject)
  private
    isCounting: boolean;
    FRefCount: Integer;
    FWeakRefs: Integer;

    function GetRefCount: Integer;
    procedure CheckReleaseable;
    function GetInside: boolean;

  protected
    PoolKeeper: IPoolKeeper;

    // IInterface
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    procedure ReleaseAction( KeeperAssigned: boolean); virtual;
    procedure ShutDown; virtual;

    // IWeakObject
    procedure Weaken;
    procedure Strengthen;

  public
    function   Acquire( PoolKeeper1: IPoolKeeper): TPoolFlotsam;
    procedure  Release;
    destructor Destroy; override;
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;

    property RefCount: Integer read GetRefCount; // -1 means not counted
    property Inside: boolean read GetInside;
  end;



  TReferenceFoil = class(TPersistent, IInterface)
  protected
    PoolFlotsam: TPoolFlotsam;

    { IInterface }
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

  public
    FoilDown: boolean;

    constructor Protect( Protectorate: TPoolFlotsam);
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
  end;


procedure WeakenIntf    ( const Intf: IInterface);
procedure StrengthenIntf( const Intf: IInterface);

implementation







uses SysUtils, Windows;
{ TPoolFlotsam }


const
  InvalidReleaseMessage   = 'RefCount <> 0';
  RedundantAcquireMessage = 'Redundant acquisition';

function TPoolFlotsam._AddRef: Integer;
begin
result := InterlockedIncrement( FRefCount);
if not isCounting then
  result := -1
end;

function TPoolFlotsam._Release: Integer;
begin
result := InterlockedDecrement( FRefCount);
if isCounting then
    begin
    if result <= FWeakRefs then
      Release
    end
  else
    result := -1
end;

function TPoolFlotsam.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
if GetInterface(IID, Obj) then
    result := 0
  else
    result := E_NOINTERFACE;
end;


procedure TPoolFlotsam.Release;
var
  KeeperAssigned: boolean;
begin
CheckReleaseable;
isCounting := False;
KeeperAssigned := assigned( PoolKeeper);
if KeeperAssigned then
  begin
  PoolKeeper.FlotsamReturn( self);
  PoolKeeper := nil
  end;
ReleaseAction( KeeperAssigned)
end;

function TPoolFlotsam.GetRefCount: Integer;
begin
if isCounting then
    result := FRefCount - FWeakRefs
  else
    result := -1
end;

procedure TPoolFlotsam.ReleaseAction( KeeperAssigned: boolean);
begin
if not KeeperAssigned then
  begin
  ShutDown;
  Destroy
  end
end;

destructor TPoolFlotsam.Destroy;
begin
ShutDown;
if FRefCount <> 0 then
  raise Exception.Create( InvalidReleaseMessage);
inherited
end;

procedure TPoolFlotsam.CheckReleaseable;
begin
if FRefCount <> FWeakRefs then
  raise Exception.Create( InvalidReleaseMessage)
end;

function TPoolFlotsam.Acquire( PoolKeeper1: IPoolKeeper): TPoolFlotsam;
begin
if isCounting then
  raise Exception.Create( RedundantAcquireMessage);
PoolKeeper := PoolKeeper1;
isCounting := True;
result := self
end;

function TPoolFlotsam.GetInside: boolean;
begin
result := not isCounting
end;

procedure TPoolFlotsam.Strengthen;
begin
InterlockedDecrement( FWeakRefs)
end;

procedure TPoolFlotsam.Weaken;
begin
InterlockedIncrement( FWeakRefs);
if isCounting and (FWeakRefs <= FWeakRefs) then
  Release
end;

procedure TPoolFlotsam.ShutDown;
begin
// Override to get rid of all the weak references
end;

{ TReferenceFoil }

function TReferenceFoil._AddRef: Integer;
begin
if FoilDown then
    result := PoolFlotsam._AddRef
  else
    result := -1
end;

function TReferenceFoil._Release: Integer;
begin
if FoilDown then
    result := PoolFlotsam._Release
  else
    result := -1
end;

constructor TReferenceFoil.Protect( Protectorate: TPoolFlotsam);
begin
PoolFlotsam := Protectorate;
FoilDown := False
end;

function TReferenceFoil.QueryInterface( const IID: TGUID; out Obj): HResult;
begin
result := PoolFlotsam.QueryInterface( IID, Obj)
end;


procedure WeakenIntf    ( const Intf: IInterface);
var
  Weak: IWeakObject;
begin
if supports( Intf, IWeakObject, Weak) then
  Weak.Weaken
end;

procedure StrengthenIntf( const Intf: IInterface);
var
  Weak: IWeakObject;
begin
if supports( Intf, IWeakObject, Weak) then
  Weak.Strengthen
end;


end.
