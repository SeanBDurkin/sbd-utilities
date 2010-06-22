{*****************************************************************************}
{                                                                             }
{    SBD Low Level Utility library                                            }
{        Version: 2.0.0.X                                                     }
{                                                                             }
{    Copyright (c) 2003-2010, Sean B. Durkin (sean@seanbdurkin.id.au)         }
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

unit uSBD_ObjCache;

interface
uses Classes;
type
TObjCache = class( TInterfacedPersistent)
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
