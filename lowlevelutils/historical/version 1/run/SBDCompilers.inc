// This include file defines the following conditional symbols.

// Symbol          ... is defined if this compiler/IDE is ...
// DELPHI          any version of DELPHI.
// DELPHI_<x>      Delphi <x>, where <x> is a place-marker for 1..9 or 2005..2007 .
//                  Note: DELPHI_9 is an synonym for DELPHI_2005
// DELPHI_Future   Some version of Delphi beyond Delphi 2007.
// DELPHI_2005or6  Either Delphi 2005 or Delphi 2006. These have the same
//                  compiler and compatible DCU's and DCP's.
// DELPHI_<x>_UP   Delphi <x> or superior.
// COMPILER_<x>    Either Kylix, Delphi or BCB where <x> is the compiler version.
// COMPILER_<x>_UP Compiler <x> or superior.
// CPPB            C++ Builder
// CPPB_<x>        C++ Builder <x> where <x> is in 3..6
// CPPB_<x>_UP     C++ Builder <x> or superior
// KYLIX           Kylix
// KYLIX_<x>       Kylix 1, 2 or 3
// KYLIX_Future    Some version Kylix beyond 3
// KYLIX_<x>_UP    Kylix <x> or superior
// DELPHI_6_ORIGINAL       Delphi 6 without updates/patches.
// DELPHI6_UPDATE_<x>      Delphi 6 with update <x> applied. x is 1, 2 or 3.
// DELPHI6_UPDATE_<x>_UP   Delphi 6 with at least update <x> applied



{$ifdef win32}

 {$ifdef conditionalexpressions}
   {$if Declared( RTLVersion) and (RTLVersion >= 19)}
     {$define DELPHI_Future}
   {$ifend}
 {$endif}

 {$ifdef VER180}
  {$define COMPILER_10}
  {$define DELPHI}
  {$define DELPHI_2006or7}
  {$ifdef VER185}
    {$define DELPHI_2007}
   {$else}
    {$define DELPHI_2006}
  {$endif}
 {$endif}

 {$ifdef VER170}
  {$define COMPILER_9}
  {$define DELPHI}
  {$define DELPHI_2005}
  {$define DELPHI_9}
 {$endif}

 {$ifdef VER160}
  {$define COMPILER_8}
  {$define DELPHI}
  {$define DELPHI_8}
 {$endif}

 {$ifdef VER150}
  {$define COMPILER_7}
  {$define DELPHI}
  {$define DELPHI_7}
 {$endif}

 {$ifdef VER140}
  {$define COMPILER_6}
  {$ifdef BCB}
    {$define CPPB}
    {$define CPPB_6}
   {$else}
    {$define DELPHI}
    {$define DELPHI_6}
  {$endif}
 {$endif}

 {$ifdef VER130}
  {$define COMPILER_5}
  {$ifdef BCB}
    {$define CPPB}
    {$define CPPB_5}
   {$else}
    {$define DELPHI}
    {$define DELPHI_5}
  {$endif}
 {$endif}

 {$ifdef VER125}
  {$define COMPILER_4}
  {$define CPPB}
  {$define CPPB_4}
 {$endif}

 {$ifdef VER120}
  {$define COMPILER_4}
  {$define DELPHI}
  {$define DELPHI_4}
 {$endif}

 {$ifdef VER110}
  {$define COMPILER_3}
  {$define CPPB}
  {$define CPPB_3}
 {$endif}

 {$ifdef VER100}
  {$define COMPILER_3}
  {$define DELPHI}
  {$define DELPHI_3}
 {$endif}

 {$ifdef VER93}
  {$define COMPILER_2}
  {$define CPPB}
  {$define CPPB_2}
 {$endif}

 {$ifdef VER90}
  {$define COMPILER_2}
  {$define DELPHI}
  {$define DELPHI_2}
 {$endif}

 {$ifdef VER80}
  {$define COMPILER_1}
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

 {$ifdef VER140}
  {$define COMPILER_6}
 {$endif}

 {$ifdef VER150}
  {$define COMPILER_7}
 {$endif}
{$endif}



{$ifdef DELPHI_Future}
  {$define DELPHI_2007_UP}
  {$define DELPHI_2006_UP}
  {$define DELPHI_2005_UP}
  {$define DELPHI_9_UP}
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
  {$define DELPHI_9_UP}
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
  {$define DELPHI_9_UP}
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
  {$define DELPHI_9_UP}
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



{$ifdef Win32_Future}
  {$define COMPILER_10_UP}
  {$define COMPILER_9_UP}
  {$define COMPILER_8_UP}
  {$define COMPILER_7_UP}
  {$define COMPILER_6_UP}
  {$define COMPILER_5_UP}
  {$define COMPILER_4_UP}
  {$define COMPILER_3_UP}
  {$define COMPILER_2_UP}
{$endif}

{$ifdef COMPILER_10}
  {$define COMPILER_10_UP}
  {$define COMPILER_9_UP}
  {$define COMPILER_8_UP}
  {$define COMPILER_7_UP}
  {$define COMPILER_6_UP}
  {$define COMPILER_5_UP}
  {$define COMPILER_4_UP}
  {$define COMPILER_3_UP}
  {$define COMPILER_2_UP}
{$endif}

{$ifdef COMPILER_9}
  {$define COMPILER_9_UP}
  {$define COMPILER_8_UP}
  {$define COMPILER_7_UP}
  {$define COMPILER_6_UP}
  {$define COMPILER_5_UP}
  {$define COMPILER_4_UP}
  {$define COMPILER_3_UP}
  {$define COMPILER_2_UP}
{$endif}

{$ifdef COMPILER_8}
  {$define COMPILER_8_UP}
  {$define COMPILER_7_UP}
  {$define COMPILER_6_UP}
  {$define COMPILER_5_UP}
  {$define COMPILER_4_UP}
  {$define COMPILER_3_UP}
  {$define COMPILER_2_UP}
{$endif}

{$ifdef COMPILER_7}
  {$define COMPILER_7_UP}
  {$define COMPILER_6_UP}
  {$define COMPILER_5_UP}
  {$define COMPILER_4_UP}
  {$define COMPILER_3_UP}
  {$define COMPILER_2_UP}
{$endif}

{$ifdef COMPILER_6}
  {$define COMPILER_6_UP}
  {$define COMPILER_5_UP}
  {$define COMPILER_4_UP}
  {$define COMPILER_3_UP}
  {$define COMPILER_2_UP}
{$endif}

{$ifdef COMPILER_5}
  {$define COMPILER_5_UP}
  {$define COMPILER_4_UP}
  {$define COMPILER_3_UP}
  {$define COMPILER_2_UP}
{$endif}

{$ifdef COMPILER_4}
  {$define COMPILER_4_UP}
  {$define COMPILER_3_UP}
  {$define COMPILER_2_UP}
{$endif}

{$ifdef COMPILER_3}
  {$define COMPILER_3_UP}
  {$define COMPILER_2_UP}
{$endif}

{$ifdef COMPILER_2}
  {$define COMPILER_2_UP}
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

// TO DO: Detect updates for D7, D2005, D2006 and D2007
