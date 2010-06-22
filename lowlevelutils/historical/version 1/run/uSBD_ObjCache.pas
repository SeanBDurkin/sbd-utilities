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

unit uSBD_ObjCache;

interface
uses Classes, uSBD_FlexiInterfaced;
type
TObjCache = class( TFlexiInterfaced)
  private
    procedure SetIsCached( NewIsCached: boolean);
    function  GetObj: TObject;
    procedure SetObj( NewObj: TObject);

  protected
    FisCached: boolean;
    FObject: TObject;

    function  Acquire: TObject; virtual; abstract;
    procedure Release( Acquisition: TObject); virtual;
    procedure WriteThrough; virtual;
    function  isEquivalent( Acquisition_A, Acquisition_B: TObject): boolean; virtual;

  public
    destructor  Destroy; override;

    property isCached: boolean read FisCached write SetIsCached;
    property Obj: TObject   read GetObj write SetObj;
  end;

implementation















{ TObjCache }

destructor TObjCache.Destroy;
begin
IsCached := False;
inherited
end;




function TObjCache.GetObj: TObject;
begin
IsCached := True;
result := FObject
end;



function TObjCache.isEquivalent(
  Acquisition_A, Acquisition_B: TObject): boolean;
begin
result := False
end;




procedure TObjCache.Release( Acquisition: TObject);
begin
Acquisition.Free
end;



procedure TObjCache.SetIsCached( NewIsCached: boolean);
begin
if FisCached = NewIsCached then exit;
if NewIsCached then
    FObject := Acquire
  else
    begin
    Release( FObject);
    FObject := nil
    end;
FisCached := NewIsCached
end;



procedure TObjCache.SetObj( NewObj: TObject);
begin
if FisCached and ((FObject = NewObj) or isEquivalent( FObject, NewObj)) then
  exit;
if FisCached then
    Release( FObject)
  else
    FisCached := True;
FObject := NewObj;
WriteThrough
end;



procedure TObjCache.WriteThrough;
begin
end;

end.
