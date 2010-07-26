{*****************************************************************************}
{                                                                             }
{    SBD Low Level Utility library                                            }
{        Version: 2.0.0.X                                                     }
{                                                                             }
{    Copyright (c) 2010, Sean B. Durkin (sean@seanbdurkin.id.au)              }
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

unit uSBD_Busy;
interface
uses Classes, ActnList, Contnrs, SyncObjs;

type
TTransitBusyEvent = procedure (Sender: TObject; isEntering: boolean) of object;

TBusyTransitionOption = (boCursor);
TBusyTransitionOptionSet = set of TBusyTransitionOption;

TBusy = class( TInterfacedPersistent)
  private
    FBusyCounter: TObject;
    FTransitEvent: TTransitBusyEvent;
    FOptions: TBusyTransitionOptionSet;
    FBusyAListState: TActionListState;
    FActionList: TObjectList; // of TCustomActionList;
    FSynchroList: TObjectList; // of TSynchroObject;

  protected
    procedure EnterBusy( const Recalls: IInterfaceList); virtual;
    procedure ExitBusy;  virtual;

  public
    constructor Create( TransitEventHandler: TTransitBusyEvent;
                        Options1: TBusyTransitionOptionSet;
                        BusyAListState1: TActionListState);
    destructor Destroy; override;

    procedure AddActionList   ( AList: TCustomActionList);
    procedure RemoveActionList( AList: TCustomActionList);
    procedure AddSynchro      ( ASynch: TSynchroObject  );
    procedure RemoveSynchro   ( ASynch: TSynchroObject  );

    function  Busy: IInterface;
    function  isBusy: boolean;
  end;




implementation









uses SysUtils, uSBD_RecallIntf;

type
TCounterObject = class( TInterfacedObject)
  private
    FRecalls: IInterfaceList;

  public
    FOwner: TBusy;

    constructor Create( Owner1: TBusy);
    destructor  Destroy; override;
  end;




{ TBusy }

procedure TBusy.AddActionList( AList: TCustomActionList);
begin
FActionList.Add( AList)
end;

procedure TBusy.AddSynchro(ASynch: TSynchroObject);
begin
FSynchroList.Add( ASynch)
end;

function TBusy.Busy: IInterface;
begin
result := nil;
if not Supports( FBusyCounter, IInterface, result) then
  begin
  TCounterObject.Create( self);
  Supports( FBusyCounter, IInterface, result)
  end
end;



constructor TBusy.Create(
  TransitEventHandler: TTransitBusyEvent;
  Options1: TBusyTransitionOptionSet;
  BusyAListState1: TActionListState);
begin
FBusyCounter  := nil;
FTransitEvent := TransitEventHandler;
FOptions      := Options1;
FBusyAListState := BusyAListState1;
FActionList := TObjectList.Create( False); // of TCustomActionList;
FSynchroList := TObjectList.Create( False)// of TSynchroObject;
end;



destructor TBusy.Destroy;
begin
FActionList.Free;
FSynchroList.Free;
inherited
end;

function TBusy.isBusy: boolean;
begin
result := assigned( FBusyCounter)
end;

procedure TBusy.RemoveActionList( AList: TCustomActionList);
begin
FActionList.Remove( AList)
end;

procedure TBusy.RemoveSynchro( ASynch: TSynchroObject);
begin
FSynchroList.Remove( ASynch)
end;

{ TCounterObject }

constructor TCounterObject.Create( Owner1: TBusy);
begin
FOwner := Owner1;
FOwner.FBusyCounter := self;
FRecalls := TInterfaceList.Create;
FOwner.EnterBusy( FRecalls)
end;


destructor TCounterObject.Destroy;
begin
FOwner.FBusyCounter := nil;
FRecalls := nil;
FOwner.ExitBusy;
inherited
end;



procedure TBusy.EnterBusy( const Recalls: IInterfaceList);
var
  j: integer;
begin
if boCursor in FOptions then
  Recalls.Add( TCursorRecall.Store( nil));
for j := 0 to FSynchroList.Count - 1 do
  Recalls.Add( TSynchroRecall.Store( FSynchroList[j]));
for j := 0 to FActionList.Count - 1 do
  (FActionList[j] as TCustomActionList).State := FBusyAListState;
if assigned( FTransitEvent) then
  FTransitEvent( self, True)
end;


procedure TBusy.ExitBusy;
var
  j: integer;
begin
for j := 0 to FActionList.Count - 1 do
  (FActionList[j] as TCustomActionList).State := asNormal;
if assigned( FTransitEvent) then
  FTransitEvent( self, False)
end;


end.
