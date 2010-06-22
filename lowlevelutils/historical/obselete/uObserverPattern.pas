unit uObserverPattern;
// This is a utility unit belonging to the "SBD Libraries".
// Copyright (c) Sean B. Durkin, 2003 .  All rights reserved.
// sdurkin@siliconrose.com.au

interface
const
  AllEvents: Cardinal = $FFFFFFFF; // 32 bits

type
  ISubject  = interface;
  IObserver = interface
  ['{F7179961-B4C9-47AC-8200-6F91EB4DFCBF}']
    procedure NotifyObserver( const Subject: ISubject; Events: Cardinal; const Datum: IInterface);
    procedure DereferenceSubject( const Subject: ISubject);
    end;

  ISubject = interface
  ['{7E7C7B0D-2118-43D6-A46A-03ABF8DC242E}']
    procedure AttachObserver( const Observer: IObserver; SensitiveEvents: Cardinal);
    procedure DetachObserver( const Observer: IObserver; SensitiveEvents: Cardinal);
    procedure NotifyObservers( Events: Cardinal; const Datum: IInterface);
    function  BlindObserver( const Observer: IObserver; BlindTo: Cardinal): IInterface;
    function  BeginUpdate: IInterface;
    function  GetLink: IInterface;
    procedure SetLink( const NewLink: IInterface);

    property  GenericLink: IInterface read GetLink write SetLink;
    end;


  function MakeSimpleSubject( const GenericLink: IInterface): ISubject;

implementation






uses uObserverPatternImpl;

function MakeSimpleSubject( const GenericLink: IInterface): ISubject;
begin
result := TSubject.Create( GenericLink)
end;

end.
