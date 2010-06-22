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

unit uSBD_PresentationStrata_Framework;
interface
uses uSBD_AppComponents, uSBD_ModelStrata_Framework;


type
IControlServices = interface
  ['{3BBDB0BC-0848-48D4-885E-01AB6BEE8F4C}']
    procedure ShutDown( ForceShutDown: boolean; OriginatingForm: TObject);
      // ^ Will shut-down the application if either:
      //    (1) ForceShutDown is True; or
      //    (2) OriginatingForm is either nil or it is the Application MainForm.
      // A View's main form will call the above when it closes, with itself as
      //  the OriginatingForm parameter.
      // UPSHOT: A main-view will shut-down the application.
      //         A secondary view will just hide itself.

    function  WillShutDownFromForm( OriginatingForm: TObject): boolean;
      // ^ Will return True iff:
      //    (1) Model & Persistence strata agree to it; and
      //    (2) Either OriginatingForm is nil or it is the Application MainForm.
      // A View's main form CanClose event will call the above, with itself as
      //  the OriginatingForm parameter.

    function  CanShutDown: boolean;
      // ^ Will return True iff Model & Persistence strata agree to it.

    procedure Breathe;
      // ^ Application level idle processing.
  end;


IController = interface( IApplicationComponent)
  ['{CF31D930-50F9-46E0-AACD-77D4650013D4}']
    procedure SetModelStrata ( const Model1: IModel);
    procedure SetControlServices( const CntrlSvcs1: IControlServices);
    function  ControllerMainForm: TObject;
    function  ControlMainFormClass: TClass;
    procedure FreeFormUponRelease( MainForm: TObject);
    function  UseControllerAsAView: boolean;
  end;


implementation

end.
