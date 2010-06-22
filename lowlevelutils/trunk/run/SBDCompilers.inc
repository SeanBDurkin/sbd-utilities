// This include file defines the following conditional symbols.

// Symbol          ... is defined if this compiler/IDE is ...
// DELPHI          any version of DELPHI.
// DELPHI_<x>      Delphi <x>, where <x> is a place-marker for 1..8 or 2005..2009 .
// DELPHI_Future   Some version of Delphi beyond Delphi 2010.
// DELPHI_2005or6  Either Delphi 2005 or Delphi 2006. These have the same
//                  compiler and compatible DCU's and DCP's.
// DELPHI_<x>_UP   Delphi <x> or superior.
// DELPHI_6_ORIGINAL       Delphi 6 without updates/patches.
// DELPHI6_UPDATE_<x>      Delphi 6 with update <x> applied. x is 1, 2 or 3.
// DELPHI6_UPDATE_<x>_UP   Delphi 6 with at least update <x> applied
// DOTNET          Delphi 2007 for .NET

// Nomenclature, Delphi versions and code-names
// ============================================
// Nota bene, the following versions of Delphi have equivalent names and code-names
//  as indicated below. At this date (Sep-2008), the Commodore version numbers/
//  names are speculative.
// Delphi 2005  == SideWinder
// Delphi 2006  == DeXter
// Delphi 2007  == Spacely
// Delphi 2009  == Tiburón
// Delphi 2010 == Commodore

{$ifdef win32}

 {$ifdef conditionalexpressions}
   {$if Declared( RTLVersion) and (RTLVersion >= 22)}
     {$define DELPHI_Future}
   {$ifend}
 {$endif}

 {$ifdef VER210}
  // Commodore. Delphi 2010? RTLVersion = 21?
  {$define DELPHI}
  {$define DELPHI_2010}
 {$endif}

 {$ifdef VER200}
  // Tiburón. Delphi 2009. RTLVersion = 20
  {$define DELPHI}
  {$define DELPHI_2009}
 {$endif}

 {$ifdef VER190}
  // Spacely. Delphi 2007 for .NET. RTLVersion = 19
  {$define DELPHI}
  {$define DELPHI_2006or7}
  {$define DELPHI_2007}
  {$define DOTNET}
 {$endif}

 {$ifdef VER180}
  {$define DELPHI}
  {$define DELPHI_2006or7}
  {$ifdef VER185}
    // Spacely. Delphi 2007 for win32. RTLVersion = 18.5
    {$define DELPHI_2007}
   {$else}
    // DeXter. Delphi 2006. RTLVersion = 18
    {$define DELPHI_2006}
  {$endif}
 {$endif}

 {$ifdef VER170}
  {$define DELPHI}
  {$define DELPHI_2005}
 {$endif}

 {$ifdef VER160}
  {$define DELPHI}
  {$define DELPHI_8}
 {$endif}

 {$ifdef VER150}
  {$define DELPHI}
  {$define DELPHI_7}
 {$endif}

 {$ifdef VER140}
  {$ifdef BCB}
    {$define CPPB}
    {$define CPPB_6}
   {$else}
    {$define DELPHI}
    {$define DELPHI_6}
  {$endif}
 {$endif}

 {$ifdef VER130}
  {$ifdef BCB}
    {$define CPPB}
    {$define CPPB_5}
   {$else}
    {$define DELPHI}
    {$define DELPHI_5}
  {$endif}
 {$endif}

 {$ifdef VER125}
  {$define CPPB}
  {$define CPPB_4}
 {$endif}

 {$ifdef VER120}
  {$define DELPHI}
  {$define DELPHI_4}
 {$endif}

 {$ifdef VER110}
  {$define CPPB}
  {$define CPPB_3}
 {$endif}

 {$ifdef VER100}
  {$define DELPHI}
  {$define DELPHI_3}
 {$endif}

 {$ifdef VER93}
  {$define CPPB}
  {$define CPPB_2}
 {$endif}

 {$ifdef VER90}
  {$define DELPHI}
  {$define DELPHI_2}
 {$endif}

 {$ifdef VER80}
  {$define DELPHI}
  {$define DELPHI_1}
 {$endif}

{$endif}

{$ifdef LINUX}
  {$define KYLIX}
  {$ifdef conditionalexpressions}
    {$if Declared(RTLVersion) and (RTLVersion = 14.00)}
      {$define KYLIX_1}
    {$ifend}

    {$if Declared(RTLVersion) and (RTLVersion = 14.20)}
      {$define KYLIX_2}
    {$ifend}

    {$if Declared(RTLVersion) and (RTLVersion = 14.50)}
      {$define KYLIX_3}
    {$ifend}

    {$if Declared(RTLVersion) and (RTLVersion > 14.50)}
      {$define KYLIX_Future}
    {$ifend}
  {$endif}
{$endif}



{$ifdef DELPHI_Future}
  {$define DELPHI_2010_UP}
  {$define DELPHI_2009_UP}
  {$define DELPHI_2007_UP}
  {$define DELPHI_2006_UP}
  {$define DELPHI_2005_UP}
  {$define DELPHI_8_UP}
  {$define DELPHI_7_UP}
  {$define DELPHI_6_UP}
  {$define DELPHI_5_UP}
  {$define DELPHI_4_UP}
  {$define DELPHI_3_UP}
  {$define DELPHI_2_UP}
{$endif}

{$ifdef DELPHI_2010}
  {$define DELPHI_2010_UP}
  {$define DELPHI_2009_UP}
  {$define DELPHI_2007_UP}
  {$define DELPHI_2006_UP}
  {$define DELPHI_2005_UP}
  {$define DELPHI_8_UP}
  {$define DELPHI_7_UP}
  {$define DELPHI_6_UP}
  {$define DELPHI_5_UP}
  {$define DELPHI_4_UP}
  {$define DELPHI_3_UP}
  {$define DELPHI_2_UP}
{$endif}

{$ifdef DELPHI_2009}
  {$define DELPHI_2009_UP}
  {$define DELPHI_2007_UP}
  {$define DELPHI_2006_UP}
  {$define DELPHI_2005_UP}
  {$define DELPHI_8_UP}
  {$define DELPHI_7_UP}
  {$define DELPHI_6_UP}
  {$define DELPHI_5_UP}
  {$define DELPHI_4_UP}
  {$define DELPHI_3_UP}
  {$define DELPHI_2_UP}
{$endif}

{$ifdef DELPHI_2007}
  {$define DELPHI_2007_UP}
  {$define DELPHI_2006_UP}
  {$define DELPHI_2005_UP}
  {$define DELPHI_8_UP}
  {$define DELPHI_7_UP}
  {$define DELPHI_6_UP}
  {$define DELPHI_5_UP}
  {$define DELPHI_4_UP}
  {$define DELPHI_3_UP}
  {$define DELPHI_2_UP}
{$endif}

{$ifdef DELPHI_2006}
  {$define DELPHI_2006_UP}
  {$define DELPHI_2005_UP}
  {$define DELPHI_8_UP}
  {$define DELPHI_7_UP}
  {$define DELPHI_6_UP}
  {$define DELPHI_5_UP}
  {$define DELPHI_4_UP}
  {$define DELPHI_3_UP}
  {$define DELPHI_2_UP}
{$endif}

{$ifdef DELPHI_2005}
  {$define DELPHI_2005_UP}
  {$define DELPHI_8_UP}
  {$define DELPHI_7_UP}
  {$define DELPHI_6_UP}
  {$define DELPHI_5_UP}
  {$define DELPHI_4_UP}
  {$define DELPHI_3_UP}
  {$define DELPHI_2_UP}
{$endif}

{$ifdef DELPHI_8}
  {$define DELPHI_8_UP}
  {$define DELPHI_7_UP}
  {$define DELPHI_6_UP}
  {$define DELPHI_5_UP}
  {$define DELPHI_4_UP}
  {$define DELPHI_3_UP}
  {$define DELPHI_2_UP}
{$endif}

{$ifdef DELPHI_7}
  {$define DELPHI_7_UP}
  {$define DELPHI_6_UP}
  {$define DELPHI_5_UP}
  {$define DELPHI_4_UP}
  {$define DELPHI_3_UP}
  {$define DELPHI_2_UP}
{$endif}

{$ifdef DELPHI_6}
  {$define DELPHI_6_UP}
  {$define DELPHI_5_UP}
  {$define DELPHI_4_UP}
  {$define DELPHI_3_UP}
  {$define DELPHI_2_UP}
{$endif}

{$ifdef DELPHI_5}
  {$define DELPHI_5_UP}
  {$define DELPHI_4_UP}
  {$define DELPHI_3_UP}
  {$define DELPHI_2_UP}
{$endif}

{$ifdef DELPHI_4}
  {$define DELPHI_4_UP}
  {$define DELPHI_3_UP}
  {$define DELPHI_2_UP}
{$endif}

{$ifdef DELPHI_3}
  {$define DELPHI_3_UP}
  {$define DELPHI_2_UP}
{$endif}

{$ifdef DELPHI_2}
  {$define DELPHI_2_UP}
{$endif}



{$ifdef KYLIX_Future}
  {$define KYLIX_3_UP}
  {$define KYLIX_2_UP}
{$endif}

{$ifdef KYLIX_3}
  {$define KYLIX_3_UP}
  {$define KYLIX_2_UP}
{$endif}

{$ifdef KYLIX_2}
  {$define KYLIX_2_UP}
{$endif}


{$ifdef CPPB_6}
  {$define CPPB_6_UP}
  {$define CPPB_5_UP}
  {$define CPPB_4_UP}
  {$define CPPB_3_UP}
{$endif}

{$ifdef CPPB_5}
  {$define CPPB_5_UP}
  {$define CPPB_4_UP}
  {$define CPPB_3_UP}
{$endif}

{$ifdef CPPB_4}
  {$define CPPB_4_UP}
  {$define CPPB_3_UP}
{$endif}

{$ifdef CPPB}
  {$ObjExportAll On}
{$endif}

{$ifdef DELPHI_6}
  {$ifdef conditionalexpressions}
    {$if Declared( RTLVersion)}
      {$if (RTLVersion = 14.10)}
        {$define DELPHI_6_ORIGINAL}
      {$ifend}
      {$if (RTLVersion = 14.11)}
        {$define DELPHI_6_UPDATE_1}
      {$ifend}
      {$if (RTLVersion = 14.20)}
        {$define DELPHI_6_UPDATE_2}
      {$ifend}
    {$ifend}
  {$endif}
{$endif}

{$ifdef DELPHI_6_UPDATE_2}
  {$define DELPHI6_UPDATE_2_UP}
  {$define DELPHI6_UPDATE_1_UP}
{$endif}

{$ifdef DELPHI_6_UPDATE_1}
  {$define DELPHI6_UPDATE_1_UP}
{$endif}

// TO DO: Detect updates for D7 and up
