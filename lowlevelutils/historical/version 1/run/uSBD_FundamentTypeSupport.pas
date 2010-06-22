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

IIntegerLike = interface
  ['{202258CF-9824-41F3-AA78-3AB06CC9231F}']
  function GetInt: Integer;
  procedure SetInt( NewValue: Integer);

  property AsInteger: Integer read GetInt write SetInt;
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
