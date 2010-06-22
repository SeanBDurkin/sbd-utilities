{*****************************************************************************}
{                                                                             }
{    SBD Model-View-Controller Framework                                      }
{        Version: 1.0.0.X                                                     }
{                                                                             }
{    Copyright (c) 2010, Sean B. Durkin (sean@seanbdurkin.id.au)         }
{                                                                             }
{*****************************************************************************}

{* ***** BEGIN LICENSE BLOCK *****
This file is part of SBD Low Level Utils.
SBD Model-View-Controller is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

SBD Model-View-Controller is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.

You should have received a copy of the Lesser GNU General Public License
along with SBD Model-View-Controller.  If not, see <http://www.gnu.org/licenses/>.

 * ***** END LICENSE BLOCK ***** *}
unit uSBD_Desktop_MVC;
interface
uses uSBD_MVC_App;

type

TDesktop_MVC_App = class( TMVC_App)
  protected
    function  ApplicationDisplayName: string;        override;
    function  Options: TMVC_OptionSet;               override;
    function  GetExeName: string;                    override;
    function  Inbuilt_MainForm: TObject;             override;
    procedure SetMainForm( NewForm: TObject); override;
    procedure CreateMainForm( MainFormClass: TClass); override;
    procedure HijackMainForm( OldForm: TObject); override;
    procedure ClearApplicationEvents( ApplicationEventsSender: TObject); override;
    procedure Breathe; override;
  end;

implementation

















uses Forms, AppEvnts, SysUtils, Classes;
{ TDesktop_MVC_App }

function TDesktop_MVC_App.ApplicationDisplayName: string;
begin
result := ChangeFileExt( ExtractFileName( GetExeName), '.');
if (result <> '') and (result[Length(result)] = '.') then
  SetLength( result, Length( result) - 1)
end;



procedure TDesktop_MVC_App.Breathe;
begin
Application.ProcessMessages
end;



procedure TDesktop_MVC_App.ClearApplicationEvents(
  ApplicationEventsSender: TObject);
begin
if ApplicationEventsSender is TApplicationEvents then
  TApplicationEvents( ApplicationEventsSender).OnIdle := nil
end;



procedure TDesktop_MVC_App.CreateMainForm( MainFormClass: TClass);
var
  Reference: TObject;
begin
if (not assigned( MainFormClass)) or
   (not MainFormClass.InheritsFrom( TComponent)) then exit;
Application.ShowMainForm := True;
Application.CreateForm( TComponentClass( MainFormClass), Reference)
end;



function TDesktop_MVC_App.GetExeName: string;
begin
result := Application.ExeName
end;



procedure TDesktop_MVC_App.HijackMainForm( OldForm: TObject);
begin
if not (OldForm is TForm) then exit;
TForm( OldForm).OnClose := nil;
TForm( OldForm).OnCloseQuery := nil
end;



function TDesktop_MVC_App.Inbuilt_MainForm: TObject;
begin
result := Application.MainForm
end;



function TDesktop_MVC_App.Options: TMVC_OptionSet;
begin
result := [ osAllowHijackMainForm ]
end;


{$WARNINGS OFF}
{$HINTS OFF}
{$ifdef VER210}
type
  THackApplication = class(TComponent)
  private
    FAppIconic: Boolean;
    FBiDiMode: TBiDiMode;
    FBiDiKeyboard: string;
    FDefaultFont: TObject;
    FNonBiDiKeyboard: string;
    FMainForm: TForm;
   end;
{$ENDIF}

procedure TDesktop_MVC_App.SetMainForm( NewForm: TObject);
begin
if not (NewForm is TForm) then exit;
{$ifdef VER210}
THackApplication( Application).FMainForm := TForm( NewForm)
{$ELSE}
Application).MainForm := TForm( NewForm)
{$ENDIF}
end;
{$WARNINGS ON}
{$HINTS ON}

end.
