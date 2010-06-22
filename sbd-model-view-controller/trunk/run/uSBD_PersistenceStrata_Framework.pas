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

unit uSBD_PersistenceStrata_Framework;
interface
uses uSBD_AppComponents;

type


IPersistenceStrata_CallBack = interface;

IPersistenceStrata = interface( IApplicationComponent)
// This interface is the view into an MVC pattern application's persistence strata.
// The Application developer needs to descend from this interface.
//  The persistence strata is responsible for:
//    (1) the persistence of data; and
//    (2) device communication.
  ['{F5313A44-0E8C-432B-9F14-BF9E77007FB8}']
    // Inherits from IApplicationComponent the following ...
//    procedure SetSmartInspect(
//      Si1: TSmartInspect; Sess1: TSiSession);
//    procedure SetAppInfo( Info: TAppInfo);
//    procedure DefineOptions( const Options: ISystemOptions);
//    procedure StartUp;
//    procedure ShutDown;

    procedure SetCallBack( const Client1: IPersistenceStrata_CallBack);
    function  CanClose: boolean;
  end;
// Your application will declare a descendant of IPersistenceStrata to
//  expose the application-specific methods of the application's persistence strata.



IPersistenceStrata_CallBack = interface
// This interface is a call-back to the invoker of the
//  IPersistenceStrata's descendant's asynchonous methods.
// The Application developer needs to descend from this interface.
// Some persistence or communications methods may have asynchronous (delayed)
//  responses or results.
// When the asynchronous response is available, the results will be returned
//  by the persistence strata, via this interface.
  ['{49CE21BF-CA74-4A12-8F0C-80D3C0FA15E8}']
  end;


// EXAMPLE
// ========

// The following example for an MVC pattern application, uses an application
//   identifer "Prog123"
//IProg123_PersistenceStrata = interface( IPersistenceStrata)
//  ['{some guid goes here}']
//  function FetchBirthDay( const PersonName: string): TDateTime;
//     // ^ Get PersonName's birthday from the database.
//  procedure SendMessage( const Message: string); // Method with asynchronous result.
//  end;

//IProg123_PersistenceStrata_CallBack = interface( IPersistenceStrata_CallBack)
//  ['{some other guid goes here}']
//  function FetchBirthDay( const PersonName: string): TDateTime;
//     // ^ Get PersonName's birthday from the database.
//  function SentMessageOk: boolean; // Asynchronous result for
//     // IProg123_PersistenceStrata.SendMessage
//  end;



// ORDER OF INVOCATIONS
// ======================

// For the persistence layer, methods will be invoked, or events occur
// in the following order.

//  1.  Application starts.
//  2.  Some auto-created forms are created by the VCL.
//  3.  The persistence strata is created/acquired
//  4.  The remaining auto-created forms are created by the VCL.
//  5.  The first idle event is captured.
//  6.  Application information is captured and supplied (procedure SetAppInfo)
//  7.  procedure DefineOptions is called. The persistence strata has a
//       chance to define its system options.
//  8.  The SmartInspect session is initialized according to the following options.
// option-type ProgId                 Kind   What-it-controls
// -----------------------------------------------------------------
// boolean     'SmartInspect.Enabled' user   TSmartInspect.Enabled
// enum        'SmartInspect.Level'   user   TSiSession.Level

//  9.  The SmartInspect session is supplied via IPersistenceStrata.SetSmartInspect
//  10. Model and Persistence layer two-way interface connections are made.
//  11. Start-up is called.
//  12. The model invokes IProg123_PersistenceStrata method calls as required.
//      Synchronous results are retuned with the calling method;
//      Asynchronous results are returned, like events, through the call-back.
//  13. User or model requests shut-down.
//  14. IPersistenceStrata.CanClose returns True
//  15. Model and Persistence stratas are disconnected.
//  16. IPersistenceStrata.ShutDown
//  17. Persistence Strata is destroyed or released.


implementation

end.
