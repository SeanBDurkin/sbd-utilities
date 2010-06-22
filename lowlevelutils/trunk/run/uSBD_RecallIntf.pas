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

unit uSBD_RecallIntf;

interface
uses Classes, Controls, Registry, Windows;

type
IRecallIntf = interface
  ['{893946B1-840F-4001-8376-CAB340166965}']
    procedure Forget;
  end;

TRecallIntf = class( TInterfacedObject, IRecallIntf)
  protected
    FRef: TObject;
    doRestore: boolean;

    procedure StoreIt; virtual;
    procedure ForgetIt; virtual;
    procedure RestoreIt; virtual;
    procedure ReleaseIt; virtual;

  public
    class function Store( Ref: TObject): IInterface;
    procedure Forget;
    destructor Destroy; override;
  end;


TCursorRecall = class( TRecallIntf)
  private
    SaveCursor: TCursor;

  protected
    procedure StoreIt  ; override;
    procedure ForgetIt ; override;
    procedure RestoreIt; override;
    procedure ReleaseIt; override;
  end;


TLifeRecall = class( TRecallIntf)
  protected
    procedure ForgetIt ; override;
    procedure RestoreIt; override;
    procedure ReleaseIt; override;
  end;


TSynchroRecall = class( TRecallIntf)
  protected
    procedure StoreIt  ; override;
    procedure ForgetIt ; override;
    procedure RestoreIt; override;
    procedure ReleaseIt; override;
  end;


TTreeViewUpdateRecall = class( TRecallIntf)
  protected
    procedure StoreIt  ; override;
    procedure ForgetIt ; override;
    procedure RestoreIt; override;
    procedure ReleaseIt; override;
  end;


{TIBTransactionRecall = class( TRecallIntf)
  protected
    procedure StoreIt  ; override;
    procedure ForgetIt ; override;
    procedure RestoreIt; override;
    procedure ReleaseIt; override;
  end;       }

TRegRecall = class( TRecallIntf)
  protected
    FLazyWrite: boolean;
    FRootKey  : HKEY;
    FAccess   : Longword;
    FPath     : string;
    procedure StoreIt  ; override;
    procedure ForgetIt ; override;
    procedure RestoreIt; override;
    procedure ReleaseIt; override;
  end;


function Assign( var Destination; Source: TObject): TObject;

function RecallableStringList( var Strings): IRecallIntf;

implementation













uses Forms, Syncobjs, ComCtrls, SysUtils;
{ TRecallIntf }

{$WARNINGS OFF}
function Assign( var Destination; Source: TObject): TObject;
begin
TObject( Destination) := Source;
result := TObject( Destination)
end;
{$WARNINGS ON}

function RecallableStringList( var Strings): IRecallIntf;
begin
supports( TLifeRecall.Store( Assign( Strings, TStringList.Create)),
          IRecallIntf, result)
end;


destructor TRecallIntf.Destroy;
begin
if doRestore then
    RestoreIt
  else
    ForgetIt;
ReleaseIt;
inherited
end;

procedure TRecallIntf.Forget;
begin
doRestore := False
end;

procedure TRecallIntf.ForgetIt;
begin
(FRef as TRecall).Forget
end;

procedure TRecallIntf.ReleaseIt;
begin
FRef.Free
end;

procedure TRecallIntf.RestoreIt;
begin
end;

class function TRecallIntf.Store( Ref: TObject): IInterface;
var
  Recall: TRecallIntf;
begin
Recall := self.Create;
result := Recall;
Recall.FRef := Ref;
Recall.doRestore := True;
Recall.StoreIt
end;

procedure TRecallIntf.StoreIt;
begin
end;






{ TCursorRecall }

procedure TCursorRecall.ForgetIt;
begin
end;

procedure TCursorRecall.ReleaseIt;
begin
end;

procedure TCursorRecall.RestoreIt;
begin
Screen.Cursor := SaveCursor
end;

procedure TCursorRecall.StoreIt;
begin
SaveCursor := Screen.Cursor;
Screen.Cursor := crHourGlass
end;

{ TLifeRecall }

procedure TLifeRecall.ForgetIt;
begin
end;

procedure TLifeRecall.ReleaseIt;
begin
end;

procedure TLifeRecall.RestoreIt;
begin
FRef.Free
end;


{ TSynchroObject }

procedure TSynchroRecall.ForgetIt;
begin
end;

procedure TSynchroRecall.ReleaseIt;
begin
end;

procedure TSynchroRecall.RestoreIt;
begin
(FRef as TSynchroObject).Release
end;

procedure TSynchroRecall.StoreIt;
begin
(FRef as TSynchroObject).Acquire
end;

{ TTreeViewUpdateRecall }

procedure TTreeViewUpdateRecall.ForgetIt;
begin
end;

procedure TTreeViewUpdateRecall.ReleaseIt;
begin
end;

procedure TTreeViewUpdateRecall.RestoreIt;
begin
(FRef as TTreeNodes).EndUpdate
end;

procedure TTreeViewUpdateRecall.StoreIt;
begin
(FRef as TTreeNodes).BeginUpdate
end;

{ TRegRecall }

procedure TRegRecall.ForgetIt;
begin
end;

procedure TRegRecall.ReleaseIt;
begin
end;

procedure TRegRecall.RestoreIt;
begin
with TRegistry( FRef) do
  begin
  CloseKey;
  RootKey := FRootKey;
  FAccess := FAccess;
  LazyWrite := FLazyWrite;
  OpenKey( FPath, False)
  end
end;

procedure TRegRecall.StoreIt;
begin
FLazyWrite := TRegistry( FRef).LazyWrite;
FRootKey   := TRegistry( FRef).RootKey;
FAccess    := TRegistry( FRef).Access;
FPath      := TRegistry( FRef).CurrentPath
end;

end.

