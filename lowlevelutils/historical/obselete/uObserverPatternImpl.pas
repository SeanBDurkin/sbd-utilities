unit uObserverPatternImpl;
// This is a utility unit belonging to the "SBD Libraries".
// Copyright (c) Sean B. Durkin, 2003 .  All rights reserved.
// sdurkin@siliconrose.com.au

interface
uses uObserverPattern, Classes, Contnrs;
type
  TObserver = class( TInterfacedObject)
    private
      FObs: IObserver;
      FSensitiveTo, FBlindTo: Cardinal;

      constructor Create( const Obs1: IObserver; Sensitive1: Cardinal);

      property Obs: IObserver read FObs;
    end;

  TSubject = class( TInterfacedObject, ISubject)
    private
      FDatumQueue: IInterfaceList;
      FEventQueue: TList;
      FObservers: TObjectList;
      FUpdateCount: integer;
      FLink: IInterface;

      function FindObserver( const Observer: IObserver): TObserver;
      function GetObs( Idx: integer): TObserver;
      procedure ReleaseObserver( Obs: TObserver);
      property Observers[ Idx: integer]: TObserver read GetObs; default;
      function  GetLink: IInterface;
      procedure SetLink( const NewLink: IInterface);

    protected
      FDoQueueEvents: boolean;

      procedure Update; virtual;

    public
      constructor Create( const GenericLink1: IInterface);
      destructor Destroy; override;

      // ISubject
      procedure AttachObserver( const Observer: IObserver; SensitiveEvents: Cardinal);
      procedure DetachObserver( const Observer: IObserver; SensitiveEvents: Cardinal);
      procedure NotifyObservers( Events: Cardinal; const Datum: IInterface);
      function  BlindObserver( const Observer: IObserver; BlindTo: Cardinal): IInterface;
      function  BeginUpdate: IInterface;
    end;

implementation









type
  TRestoreState = class( TInterfacedObject)
    private
      FState: Cardinal;
      FObs: TObserver;

      constructor Create( Obs1: TObserver);

    public
      destructor Destroy; override;
    end;


  TSubjectUpdater = class( TInterfacedObject)
    private
      FSubject: TSubject;

      constructor Create( Subj1: TSubject);
    public
      destructor Destroy; override;
    end;

{ TObserver }

constructor TObserver.Create( const Obs1: IObserver; Sensitive1: Cardinal);
begin
FObs := Obs1;
FSensitiveTo := Sensitive1;
// FBlindTo = 0
end;

{ TSubject }

procedure TSubject.AttachObserver( const Observer: IObserver;
  SensitiveEvents: Cardinal);
var
  Obs: TObserver;
begin
if SensitiveEvents = 0 then exit;
Obs := FindObserver( Observer);
if assigned( Obs) then
    Obs.FSensitiveTo := Obs.FSensitiveTo or SensitiveEvents
  else
    begin
    Obs := TObserver.Create( Observer, SensitiveEvents);
    FObservers.Add( Obs);
    Obs._AddRef
    end
end;

function TSubject.BeginUpdate: IInterface;
begin
result := TSubjectUpdater.Create( self)
end;

function TSubject.BlindObserver( const Observer: IObserver;
  BlindTo: Cardinal): IInterface;
var
  Obs: TObserver;
begin
if BlindTo = 0 then exit;
Obs := FindObserver( Observer);
if not assigned( Obs) then exit;
result := TRestoreState.Create( Obs);
Obs.FBlindTo := Obs.FBlindTo or BlindTo
end;

constructor TSubject.Create( const GenericLink1: IInterface);
begin
FDoQueueEvents := True;
FDatumQueue  := TInterfaceList.Create;
FEventQueue  := TList.Create;
FLink := GenericLink1;
FObservers := TObjectList.Create( False)
end;

destructor TSubject.Destroy;
var
  j: Integer;
begin
for j := FObservers.Count - 1 downto 0 do
  ReleaseObserver( Observers[j]);
FObservers.Free;
FEventQueue.Free;
inherited
end;

procedure TSubject.DetachObserver( const Observer: IObserver;
  SensitiveEvents: Cardinal);
var
  Obs: TObserver;
begin
if SensitiveEvents = 0 then exit;
Obs := FindObserver( Observer);
if not assigned( Obs) then exit;
Obs.FSensitiveTo := Obs.FSensitiveTo and not SensitiveEvents;
if Obs.FSensitiveTo = 0 then
  ReleaseObserver( Obs)
end;

function TSubject.FindObserver( const Observer: IObserver): TObserver;
var
  j: Integer;
  Obs: TObserver;
begin
result := nil;
for j := 0 to FObservers.Count - 1 do
  begin
  Obs := Observers[j];
  if Obs.Obs <> Observer then continue;
  result := Obs;
  break
  end
end;

function TSubject.GetLink: IInterface;
begin
result := FLink
end;

function TSubject.GetObs( Idx: integer): TObserver;
begin
result := FObservers.Items[ Idx] as TObserver
end;

procedure TSubject.NotifyObservers(
  Events: Cardinal; const Datum: IInterface);
var
  j: Integer;
  ISelf: ISubject;
  Obs: TObserver;
  EventMask: Cardinal;
  LifePreserver: IInterface;

begin
if Events = 0 then exit;
if FUpdateCount > 0 then
    begin
    if FDoQueueEvents then // Otherwise ignore them.
      begin
      FDatumQueue.Add( Datum);
      {$WARNINGS OFF}
      FEventQueue.Add( Pointer( Events))
      {$WARNINGS ON}
      end
    end
  else
    begin
    LifePreserver := Datum;
    ISelf := self;
    for j := 0 to FObservers.Count - 1 do
      begin
      Obs := Observers[ j];
      EventMask := Obs.FSensitiveTo and Events and not Obs.FBlindTo;
      if EventMask <> 0 then
        Obs.Obs.NotifyObserver( ISelf, EventMask, Datum)
      end
    end
end;

procedure TSubject.ReleaseObserver( Obs: TObserver);
begin
FObservers.Remove( Obs);
Obs.Obs.DereferenceSubject( self);
Obs._Release
end;

procedure TSubject.SetLink( const NewLink: IInterface);
begin
FLink := NewLink
end;

procedure TSubject.Update;
var
  Ev: Cardinal;
  Datum: IInterface;
begin
while (FEventQueue.Count > 0) and (FUpdateCount = 0) do
  begin
  Ev := Cardinal( FEventQueue[0]);
  Datum := FDatumQueue[0];
  FEventQueue.Delete( 0);
  FDatumQueue.Delete( 0);
  NotifyObservers( Ev, Datum)
  end
end;

{ TRestoreState }

constructor TRestoreState.Create( Obs1: TObserver);
begin
FObs := Obs1;
FObs._AddRef;
FState := FObs.FBlindTo;
end;

destructor TRestoreState.Destroy;
begin
FObs.FBlindTo := FState;
FObs._Release;
inherited
end;

{ TSubjectUpdater }

constructor TSubjectUpdater.Create( Subj1: TSubject);
begin
FSubject := Subj1;
FSubject._AddRef;
Inc( FSubject.FUpdateCount)
end;

destructor TSubjectUpdater.Destroy;
begin
Dec( FSubject.FUpdateCount);
if FSubject.FUpdateCount = 0 then
  FSubject.Update;
FSubject._Release;
inherited
end;

end.
