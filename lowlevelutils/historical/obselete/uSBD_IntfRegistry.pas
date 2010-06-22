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

unit uSBD_IntfRegistry;

interface
uses Classes, uSBD_Visitor, Contnrs, uSBD_SortStrings, uSBD_FlexiInterfaced;


type
  IIdentified = interface
    ['{4C987A36-5187-4B55-9F26-34C20437DE8A}']
      function ProgId: string;
      function DisplayName: widestring;
    end;

  TIdentifiedObjWrapper = class // Created by visiting a TIntfProvider
    public
      FIdent: IIdentified;
    end;

  IIntfToObj = interface
    ['{392E3741-0304-4B28-892F-A2029120A14F}']
      function ObjectSelf: TObject;
    end;

  IIntfRegistrant = interface;
  
  TUnitRegisterProc = procedure (Datum: TObject; const Registrar: IIntfRegistrant);

  IIntfRegistrant = interface
    ['{97B908B7-906A-452F-A35D-9F0ADEF17A2C}']
      procedure RegisterIntf( const Slctn: IIdentified; Enabled1: boolean; DestroyUponDereg: boolean);
      procedure DeregisterIntf( var Slctn: IIdentified);
      procedure RegisterASet( UnitProcData: TObject;
        const UnitProcs: array of TUnitRegisterProc);
    end;

  TDeregistrationReason = ( // The reason why the selection is being de-registered.
    drRegistrarClientDemand,  // Because the client said so.
    drOverload,               // Because another is being registered with the same ProgId.
    drProviderDestruction);   // Because the Provider is undergoing destruction.

  IIntfProvider = interface;

  IDeregObserver = interface
    ['{7EEC765C-A770-47CD-A528-BA0A53525D9B}']
      procedure DereferenceSlctn(
        const Provider: IIntfProvider; const Slctn: IInterface;
        Reason: TDeregistrationReason);
    end;

  TSortOption = (
    soSortByDisplayName,
    soSortByProgId,
    soUnsorted);

  IIntfProvider = interface
    ['{0C82734E-3656-4F30-BFE1-D98EAEE6F435}']
      function  GetEnabled( const Slctn: IInterface): boolean;
      procedure SetEnabled( const Slctn: IInterface; Value: boolean);
      function  GetSelection( const ProgId: string): IInterface;
      function  isASelection( const ProgId: string; out Slctn: IInterface): boolean;
      procedure GetMenu( Selections: TStrings; SortBy: TSortOption; IncludeDisabled: boolean);
      procedure Subscribe  ( const Observer: IDeregObserver; const SubjectSlctn: IInterface);
      procedure Unsubscribe( const Observer: IDeregObserver; const SubjectSlctn: IInterface);
    end;



  TIntfProvider = class( TFlexiInterfaced,
      IVisitationNode, IIntfToObj, IIntfRegistrant, IIntfProvider)
    private
      FGUID: TGUID;
      FUniversalSubscribers: IInterfaceList; // of IDeregObserver
      FItems: TObjectList; // of TItemPacket
      FRequireUniqueDisplayName: boolean;
      FDeregItem: IInterface;

      function  ParseOverChildren( Visitor: TVisitor): boolean;
      function  ObjectSelf: TObject;
      procedure RegisterIntf( const Slctn: IIdentified; Enabled1: boolean; DestroyUponDereg: boolean);
      procedure DeregisterIntf( var Slctn: IIdentified);
      function  GetEnabled( const Slctn: IInterface): boolean;
      procedure SetEnabled( const Slctn: IInterface; Value: boolean);
      function  GetSelection( const ProgId: string): IInterface;
      function  isASelection( const ProgId: string; out Slctn: IInterface): boolean;
      procedure GetMenu( Selections: TStrings; SortBy: TSortOption; IncludeDisabled: boolean);
      procedure Subscribe  ( const Observer: IDeregObserver; const SubjectSlctn: IInterface);
      procedure Unsubscribe( const Observer: IDeregObserver; const SubjectSlctn: IInterface);

      procedure DeregForDestruction( ObjectsToFree: TObjectList);
      procedure Dereg( const SubjectSlctn: IInterface;
        Reason: TDeregistrationReason; var ObjToFree: TObject);
      procedure RegisterASet( UnitProcData: TObject; const UnitProcs: array of TUnitRegisterProc);

    public
      constructor Create( const ItemGUID: TGUID; RequireUniqueDisplayName: boolean; isCountedObject: boolean);
      destructor  Destroy; override;
    end;




implementation














uses SysUtils, uSBD_FundamentTypeSupport, uSBD_Resources;


type
  TItemPacket = class( TObject)
    public
      FItem: IInterface;
      FEnabled: boolean;
      FAutoDestroy: boolean;
      FSpecificObservers: IInterfaceList; // of IDeregObserver

      constructor Create( const Item1: IInterface;
                           Enabled1, AutoDestroy1: boolean);
      destructor Destroy; override;
    end;





constructor TIntfProvider.Create(
  const ItemGUID: TGUID; RequireUniqueDisplayName: boolean; isCountedObject: boolean);
var
  Options: TInterfacingOptionSet;
begin
Options := [];
if isCountedObject then
  Include( Options, ioIsCounting);
inherited Create( Options);
FGUID := ItemGUID;
FUniversalSubscribers := TInterfaceList.Create; // of IDeregObserver
FItems := TObjectList.Create( True); // of TItemPacket
FRequireUniqueDisplayName := RequireUniqueDisplayName
end;






destructor TIntfProvider.Destroy;
var
  ObjectsToFree: TObjectList;
begin
if assigned( FDeregItem) then
  raise Exception.Create( sCannotDestroy);
ObjectsToFree := TObjectList.Create( True);
try
  DeregForDestruction( ObjectsToFree)
  finally
  ObjectsToFree.Free
end;
inherited
end;


procedure TIntfProvider.DeregForDestruction( ObjectsToFree: TObjectList);
var
  i: integer;
  Packet: TItemPacket;
  Slnctn: IInterface;
  ObjToFree: TObject;
begin
for i := FItems.Count - 1 downto 0 do
  begin
  Packet := FItems[i] as TItemPacket;
  Slnctn := Packet.FItem;
  Dereg( Slnctn, drProviderDestruction, ObjToFree);
  if assigned( ObjToFree) and (ObjectsToFree.IndexOf( ObjToFree) = -1) then
    ObjectsToFree.Add( ObjToFree)
  end;
FUniversalSubscribers := nil;
FreeAndNil( FItems)
end;



function FindPacket( This: TIntfProvider; const Slnctn: IInterface;
  var Packet: TItemPacket; var Idx: integer): boolean;
var
  j: integer;
begin
result := False;
Packet := nil;
Idx    := -1;
for j := 0 to This.FItems.Count - 1 do
  begin
  result := (This.FItems[j] as TItemPacket).FItem = Slnctn;
  if not result then continue;
  Packet := TItemPacket( This.FItems[j]);
  Idx := j;
  break
  end;
end;




procedure TIntfProvider.Dereg(
  const SubjectSlctn: IInterface;
  Reason: TDeregistrationReason; var ObjToFree: TObject);
var
  Packet: TItemPacket;
  Idx, j: integer;
  Obs: IDeregObserver;
  IntfToObj: IIntfToObj;
begin
ObjToFree := nil;
if not FindPacket( self, SubjectSlctn, Packet, Idx) then exit;
Assert( FDeregItem = nil, 'TIntfProvider.Dereg precondition not met.');
FDeregItem := SubjectSlctn;
try
  for j := Packet.FSpecificObservers.Count - 1 downto 0 do
    if supports( Packet.FSpecificObservers[j], IDeregObserver, Obs) then
      Obs.DereferenceSlctn( self, SubjectSlctn, Reason);
  for j := FUniversalSubscribers.Count - 1 downto 0 do
    if supports( FUniversalSubscribers[j], IDeregObserver, Obs) then
      Obs.DereferenceSlctn( self, SubjectSlctn, Reason);
  if Packet.FAutoDestroy and supports( SubjectSlctn, IIntfToObj, IntfToObj) then
    ObjToFree := IntfToObj.ObjectSelf
finally
  FDeregItem := nil
end end;




procedure TIntfProvider.DeregisterIntf( var Slctn: IIdentified);
var
  Packet: TItemPacket;
  Idx: integer;
  ObjToFree: TObject;
begin
if not FindPacket( self, Slctn, Packet, Idx) then exit;
if assigned( FDeregItem) and (FDeregItem <> Slctn) then
  raise Exception.Create( sCannotDereg);
Dereg( Slctn, drRegistrarClientDemand, ObjToFree);
Slctn := nil;
ObjToFree.Free
end;





function TIntfProvider.GetEnabled( const Slctn: IInterface): boolean;
var
  Packet: TItemPacket;
  Idx: integer;
begin
result := FindPacket( self, Slctn, Packet, Idx) and
          Packet.FEnabled
end;




function DisplayNameSort(
  Datum: TObject; const List: IInterfaceList;
  const Item1, Item2: IInterface): TComparisonResult;
var
  Ident1, Ident2: IIdentified;
begin
if supports( Item1, IIdentified, Ident1) and
   supports( Item2, IIdentified, Ident2) then
     begin
     result := WCompare_PhoneBook( Ident1.DisplayName, Ident2.DisplayName);
     if result = crEquivalent then
       result := Compare_Alphabetical( Ident1.ProgId, Ident2.ProgId)
     end
  else
    result := crEquivalent
end;





procedure TIntfProvider.GetMenu(
  Selections: TStrings; SortBy: TSortOption;
  IncludeDisabled: boolean);
var
  j: integer;
  Packet: TItemPacket;
  Ident: IIdentified;
  Items: IInterfaceList;
begin
if SortBy = soSortByDisplayName then
  Items := TInterfaceList.Create;
Selections.BeginUpdate;
try
Selections.Clear;
for j := 0 to FItems.Count - 1 do
  begin
  Packet := FItems[j] as TItemPacket;
  if (IncludeDisabled or Packet.FEnabled) and
    supports( Packet.FItem, IIdentified, Ident) then
      begin
      if SortBy = soSortByDisplayName then
          Items.Add( Ident)
        else
          Selections.Add( Ident.ProgId)
      end;
  end;
case SortBy of
  soSortByDisplayName:
    begin
    InterfaceList_QuickSort( Items, nil, True, DisplayNameSort);
    for j := 0 to Items.Count - 1 do
      if supports( Items[j], IIdentified, Ident) then
        Selections.Add( Ident.ProgId)
    end;

  soSortByProgId:
    Strings_QuickSort( Selections, nil, True, Alphabetical_StringsSortCompare);

  soUnsorted: begin end
  end
finally
  Selections.EndUpdate
end end;





function TIntfProvider.GetSelection( const ProgId: string): IInterface;
var
  j: integer;
  Ident: IIdentified;
begin
result := nil;
for j := 0 to FItems.Count - 1 do
  begin
  if (not supports( (FItems[j] as TItemPacket).FItem, IIdentified, Ident)) or
     (Ident.ProgId <> ProgId) then continue;
  result := Ident;
  break
  end;
end;





function TIntfProvider.isASelection(
  const ProgId: string; out Slctn: IInterface): boolean;
var
  j: integer;
  Ident: IIdentified;
begin
result := False;
for j := 0 to FItems.Count - 1 do
  begin
  result := supports( (FItems[j] as TItemPacket).FItem, IIdentified, Ident) and
           (Ident.ProgId = ProgId);
  if not result then continue;
  Slctn := Ident;
  break
  end;
end;




function TIntfProvider.ObjectSelf: TObject;
begin
result := self
end;





function TIntfProvider.ParseOverChildren( Visitor: TVisitor): boolean;
// Visitor.Applies or Visitor.Visit must clear FNode each visit!
var
  j: integer;
  Wrapper: TIdentifiedObjWrapper;
  Ident: IIdentified;

  function VisitJth: boolean;
  begin
  result := False;
  if not supports( FItems[j], IIdentified, Ident) then exit;
  Wrapper := TIdentifiedObjWrapper.Create;
  Wrapper.FIdent := Ident;
  result := Visitor.Parse( Wrapper)
  end;

begin
result := False;
if Visitor.goForward then
    begin
    for j := 0 to FItems.Count - 1 do
      begin
      result := VisitJth;
      if result then break
      end
    end
  else
    begin
    for j := FItems.Count - 1 downto 0 do
      begin
      result := VisitJth;
      if result then break
      end
    end
end;




procedure TIntfProvider.RegisterASet(
  UnitProcData: TObject;
  const UnitProcs: array of TUnitRegisterProc);
var
  j: integer;
  Obj: TObject;
  isObjectList: boolean;
begin
isObjectList := False;
Obj := nil;
if not assigned( UnitProcData)  then
    begin end
  else if UnitProcData is  TObjectList then
    isObjectList := True
  else
    Obj := UnitProcData;
for j := Low( UnitProcs) to High( UnitProcs) do
  begin
  if isObjectList then
    begin
    if j < TObjectList( UnitProcData).Count then
        Obj := TObjectList( UnitProcData)[j]
      else
        Obj := nil  
    end;
  UnitProcs[j]( Obj, self)
  end
end;




procedure TIntfProvider.RegisterIntf(
  const Slctn: IIdentified; Enabled1, DestroyUponDereg: boolean);
var
  j: integer;
  Ok: boolean;
  Ident: IIdentified;
  Packet: TItemPacket;
  Idx: integer;
  ObjToFree: TObject;
  OldSlctn: IInterface;
begin
if FindPacket( self, Slctn, Packet, Idx) then exit;
if assigned( FDeregItem) then
  raise Exception.Create( sCannotReg);
if (Slctn.ProgId = '') or (Slctn.DisplayName = '') or
   (Trim( Slctn.DisplayName) <> Slctn.DisplayName) then
  raise Exception.Create( sBadName);
if FRequireUniqueDisplayName then
  begin
  Ok := True;
  for j := 0 to FItems.Count - 1 do
    begin
    Packet := FItems[j] as TItemPacket;
    if not supports( Packet.FItem, IIdentified, Ident) then continue;
    Ok := (Ident.ProgId = Slctn.ProgId) or
          (Ident.DisplayName <> Slctn.DisplayName);
    if not Ok then  break
    end;
  if not Ok then
    raise Exception.Create( sOverloadDisplayName);
  end;
if isASelection( Slctn.ProgId, OldSlctn) then
  begin
  Dereg( Slctn, drOverload, ObjToFree);
  Ident := nil;
  ObjToFree.Free
  end;
Packet := TItemPacket.Create( Slctn, Enabled1, DestroyUponDereg);
FItems.Add( Packet)
end;





procedure TIntfProvider.SetEnabled( const Slctn: IInterface; Value: boolean);
var
  Packet: TItemPacket;
  Idx: integer;
begin
if FindPacket( self, Slctn, Packet, Idx) then
  Packet.FEnabled := Value
end;





procedure TIntfProvider.Subscribe(
  const Observer: IDeregObserver; const SubjectSlctn: IInterface);
var
  Packet: TItemPacket;
  Idx: integer;
begin
if not assigned( Observer) then exit;
if assigned( SubjectSlctn) then
    begin
    if FindPacket( self, SubjectSlctn, Packet, Idx) then
      Packet.FSpecificObservers.Add( Observer)
    end
  else
    FUniversalSubscribers.Add( Observer)
end;






procedure TIntfProvider.Unsubscribe(
  const Observer: IDeregObserver; const SubjectSlctn: IInterface);
var
  Packet: TItemPacket;
  Idx: integer;
begin
if not assigned( Observer) then exit;
if assigned( SubjectSlctn) then
    begin
    if FindPacket( self, SubjectSlctn, Packet, Idx) then
      Packet.FSpecificObservers.Remove( Observer)
    end
  else
    FUniversalSubscribers.Remove( Observer)
end;



{ TItemPacket }

constructor TItemPacket.Create(
  const Item1: IInterface; Enabled1, AutoDestroy1: boolean);
begin
FItem    := Item1;
FEnabled := Enabled1;
FAutoDestroy := AutoDestroy1;
FSpecificObservers := TInterfaceList.Create
end;



destructor TItemPacket.Destroy;
begin
FSpecificObservers := nil;
FItem := nil;
inherited
end;

end.
