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

unit uSBD_ModelStrata_Framework;
interface
uses uSBD_PersistenceStrata_Framework, uSBD_AppComponents, Classes, Contnrs;

type


IView = interface;
TExecuteViewProc = procedure ( const View: IView; var Datum) of object;
IViewIterator = interface
  ['{76F0E441-A575-466C-8971-830DB6AFCF29}']
  procedure IterateViews( Method: TExecuteViewProc; var Datum; Ascending: boolean);
  end;

IModel = interface( IApplicationComponent)
// This interface is the view into an MVC pattern application's model strata or 'engine'.
// The Application developer needs to descend from this interface.
//  The model strata is responsible for:
//    (1) the logical data model; and
//    (2) business rules.
  ['{F1DDFAB9-B197-4F94-AAB2-31FFA4112A55}']
    // Inherits from IApplicationComponent the following ...
//    procedure SetSmartInspect(
//      Si1: TSmartInspect; Sess1: TSiSession);
//    procedure SetAppInfo( Info: TAppInfo);
//    procedure DefineOptions( const Options: ISystemOptions);
//    procedure StartUp;
//    procedure ShutDown;

    procedure SetPersistenceStrata  ( const Prstnc1: IPersistenceStrata);
    procedure SetViews( const View1: IViewIterator);
    function  CanClose: boolean;
  end;
// Your application will declare a descendant of IModel to
//  expose the application-specific methods of the application's model.


ISubSystemViewManager = interface
  procedure Subscribe  ( const Observer: IView); // Must be a reference
    // counted Subsystem of the presentation layer, such as a page.
  procedure Unsubscribe( const Observer: IView);
  end;



TMVC_MessageType =
  (mmtConfirmation, mmYesNo, mmtDiagnostic, mmtInformation,
   mmtSuccess, mmtWarning, mmtError);

IView = interface( IApplicationComponent)
// This interface is the vista into an MVC pattern application's view strata.
// The view strata may also be known as the presentation layer.
// The Application developer needs to descend from this interface.
//  There may be multiple views. Each view is responsible for:
//    (1) presentation to the user; and
//    (2) result of asynchronous commands given by the controller.
  ['{43EC9CE8-BAF4-4AFC-9DE1-3E2EE593B355}']
    procedure SetModelStrata ( const Model1: IModel);
    procedure SetSubSystemViewManager( const Manager: ISubSystemViewManager);
  end;

IViewEx_Reporting = interface
// Optional extension to the IView interface.
  ['{7DB81D7B-DB00-4948-AC30-16280F316BDF}']
    procedure ReportMessage( MsgType: TMVC_MessageType; const Msg: string);
      // Type MUST EXCLUDE mmtConfirmation, mmYesNo
  end;

IControllerEx_Reporting = interface
// Optional extension to the IController interface.
  ['{FA68858A-4475-49AE-AED1-9EDCC0C8B2F9}']
    function  QueryUser( MsgType: TMVC_MessageType; const Msg: string;
      Default: boolean): boolean; // Type can ONLY BE mmtConfirmation, mmYesNo
  end;


implementation



end.
