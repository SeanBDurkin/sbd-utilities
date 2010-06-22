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

unit uSBD_FundamentTypeSupport;

interface uses Classes, Graphics, Types;
type

TComparisonResult = (
  crGreater,       // Item 1 of 2 is first.
  crEquivalent,
  crLess);         // Item 2 of 2 is first.

IStringLike = interface
  ['{7E59445F-04EE-40B2-B98F-D540DA6761AA}']
  function  GetStr: string;
  procedure SetStr( const NewVal: string);

  property AsString: string read GetStr write SetStr;
  end;

IUTF8Like = interface
  ['{428434A9-C0C8-4DB8-A044-4A3EE2D4AF50}']
  function  GetUTF8Str: UTF8String;
  procedure SetUTF8Str( const NewVal: UTF8String);

  property AsString: UTF8string read GetUTF8Str write SetUTF8Str;
  end;

IIntegerLike = interface
  ['{202258CF-9824-41F3-AA78-3AB06CC9231F}']
  function GetInt: Integer;
  procedure SetInt( NewValue: Integer);

  property AsInteger: Integer read GetInt write SetInt;
  end;

IsInt64Like = interface
  ['{C3074EF3-0297-4E81-B06F-96FA17CD3E07}']
  function  GetsInt64: Int64;
  procedure SetsInt64( NewValue: Int64);

  property AsInteger: int64 read GetsInt64 write SetsInt64;
  end;

IuInt64Like = interface
  ['{9DEFE2E6-56C7-4DCC-88FD-B3B005820B27}']
  function  GetuInt64: uint64;
  procedure SetuInt64( NewValue: uint64);

  property AsInteger: uint64 read GetuInt64 write SetuInt64;
  end;

IFloatLike = interface
  ['{3E1F8FFF-EBE9-4BD5-A10E-4E2B411C9BD9}']
  function GetDouble: Double;
  procedure SetDouble( NewValue: Double);

  property AsDouble: Double read GetDouble write SetDouble;
  end;

IDateTimeLike = interface
  ['{F3CB6D24-78A0-4093-9B7A-9DB727081BF0}']
  function GetDateTime: TDateTime;
  procedure SetDateTime( NewValue: TDateTime);

  property AsDateTime: TDateTime read GetDateTime write SetDateTime;
  end;


IBooleanLike = interface
  ['{C319006D-DB7A-491D-A6B8-9979369392E9}']
  function GetBoolean: Boolean;
  procedure SetBoolean( NewValue: Boolean);

  property AsBoolean: Boolean read GetBoolean write SetBoolean;
  end;

IMemoLike = interface
  ['{51ED9591-33B2-4456-9BDC-8461CF03D708}']
  function  GetMemo: TStrings;
  procedure SetMemo( NewValue: TStrings);

  property AsMemo: TStrings read GetMemo write SetMemo;
  end;

ICurrencyLike = interface
  ['{241E4849-36E1-4AAD-8DF4-803AEC60F9A9}']
  function GetCurrency: Currency;
  procedure SetCurrency( NewValue: Currency);

  property AsCurrency: Currency read GetCurrency write SetCurrency;
  end;

IGraphicLike = interface
  ['{A090FDBA-4F42-4C38-8747-04208596EF57}']
  function GetGraphic: TGraphic;
  procedure SetGraphic( NewValue: TGraphic);

  property AsGraphic: TGraphic read GetGraphic write SetGraphic;
  end;

IPaintLike = interface
  ['{E32A4759-5C02-4521-A3B3-FE802918FB0B}']
   procedure Paint( Canvas: TCanvas; Origin: TPoint);
  end;

TBlobStreamMode = (bmRead, bmWrite, bmReadWrite);
IBlobLike = interface
  ['{E0963F14-1C37-43D1-80C7-A1F64205D120}']
  function CreateBlobStream( Mode: TBlobStreamMode): TStream;
  end;

implementation




end.
