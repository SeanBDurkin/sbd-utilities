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

unit uSBD_PresentationStrata_Skins;
interface
uses Classes, Contnrs, uSBD_PresentationStrata_Framework, uSBD_ModelStrata_Framework;

type
// Skin Modules
TSkinGateway = class( TObject)
  public
    function PackageName: string; virtual; abstract;
    function AcquireSkins: IInterfaceList;   virtual; abstract;
      // ^^ All elements MUST support ISkin. MUST return at least one.
      // Members must be reference counted.
  end;
TSkinGatewayMaker = function: TSkinGateway;
const SkinGatewayMakerName = 'SkinEntry';
type TIntegerFunc = function: double;

//function SkinGateway: TSkinGateway; exports SkinGateway name SkinGatewayMakerName;
//function LibRTLVersion: double; exports LibRTLVersion name 'RTLVersion';

type
TViewInfo = class
  public
    FView: IView;
    FViewObj: TObject;
    FisController: boolean; // True iff this view is also the controller.
    // If FView is counted, then FViewObj will be nil
    //  Otherwise it will be the object to be destroyed upon release.
  end;

ISkin = interface
  // Must be reference counted objects.
  ['{B4CD62A6-3248-4F74-BDFC-9BCE539A2A5D}']
    function SkinProgId: string;
    function SkinDisplayName: string;
    function CreateController( var Ctrl: TObject): IController;
    // ^ If the controller is reference counted, the Ctrl shall return nil.
    // ^ If the controller is non-counted, then Ctrl should be the object to
    //     be destroyed upon shut-down or library release.
    function CreateViews: TObjectList; // Owning ObjectList of TViewInfo
      // Ownership of the list is also transferred.
    function CreatePreviewMain(
      var Vw: TObject; const ConstructionContext: IInterface): IView;
  end;

implementation






end.
