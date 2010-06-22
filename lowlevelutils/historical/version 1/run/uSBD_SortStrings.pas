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

unit uSBD_SortStrings;
{$I 'SBDCompilers.inc'}

interface
  uses Classes, uSBD_FundamentTypeSupport,
{$IFDEF COMPILER_10_UP}
  WideStrings,
{$ENDIF}
// TO DO: Find a replacment WideStrings for NDEF COMPILER_10_UP.

  Contnrs;

type
  TStringsSortCompare = function(
    Datum: TObject; const s1, s2: string;
    Obj1, Obj2: TObject): TComparisonResult;

  TWStringsSortCompare = function(
    Datum: TObject; const s1, s2: widestring;
    Obj1, Obj2: TObject): TComparisonResult;

  TObjectSortCompare = function(
    Datum: TObject; List: TObjectList;
    Obj1, Obj2: TObject): TComparisonResult;

  TIntfSortCompare = function(
    Datum: TObject; const List: IInterfaceList;
    const Item1, Item2: IInterface): TComparisonResult;

procedure Strings_QuickSort(
  This: TStrings; Datum: TObject; isAscending: boolean;
  SCompare: TStringsSortCompare);

{$IFDEF COMPILER_10_UP}
procedure WStrings_QuickSort(
  This: TWideStrings; Datum: TObject; isAscending: boolean;
  SCompare: TWStringsSortCompare);
{$ENDIF}

procedure ObjectList_QuickSort(
  This: TObjectList; Datum: TObject; isAscending: boolean;
  SCompare: TObjectSortCompare);

procedure InterfaceList_QuickSort(
  const This: IInterfaceList; Datum: TObject; isAscending: boolean;
  SCompare: TIntfSortCompare);

function Alphabetical_StringsSortCompare(
    Datum: TObject; const s1, s2: string;
    Obj1, Obj2: TObject): TComparisonResult;

function Alphabetical_WStringsSortCompare(
    Datum: TObject; const s1, s2: widestring;
    Obj1, Obj2: TObject): TComparisonResult;

function PhoneBook_StringsSortCompare(
    Datum: TObject; const s1, s2: string;
    Obj1, Obj2: TObject): TComparisonResult;

function PhoneBook_WStringsSortCompare(
    Datum: TObject; const s1, s2: widestring;
    Obj1, Obj2: TObject): TComparisonResult;

function  Compare_Alphabetical( const s1, s2: string): TComparisonResult;
function WCompare_Alphabetical( const s1, s2: widestring): TComparisonResult;

function  Compare_PhoneBook( const s1, s2: string): TComparisonResult;
function WCompare_PhoneBook( const s1, s2: widestring): TComparisonResult;



implementation







uses SysUtils;

procedure Strings_QuickSort_Inner(
  This: TStrings; Datum: TObject; isAscending: boolean;
  L, R: Integer; SCompare: TStringsSortCompare);
var
  i, j, Pivot: Integer;
  sPivot: string;
  objPivot: TObject;
  LeftBreakingResult, RightBreakingResult: TComparisonResult;
begin
if isAscending then
    begin
    LeftBreakingResult := crLess;
    RightBreakingResult:= crGreater
    end
  else
    begin
    LeftBreakingResult := crGreater;
    RightBreakingResult:= crLess
    end;
while L < R do
  begin
  i := L;
  j := R;
  Pivot := (L + R) shr 1;
  sPivot := This[ Pivot];
  objPivot := This.Objects[ Pivot];
  repeat
    // L..i-1 is <= pivot
    // j+1..R is >= pivot
    // i..j is as yet unknown
    while (i < j) and
      ((i = Pivot) or
       (SCompare( Datum, This[i], sPivot, This.Objects[i], objPivot) <>
         LeftBreakingResult)) do
           Inc( i);
    // either i=j or This[i] > pivot
    // L..i-1 is <= pivot
    while (j > i) and
      ((j = Pivot) or
       (SCompare( Datum, This[j], sPivot, This.Objects[j], objPivot) <>
         RightBreakingResult)) do
           Dec( j);
    // either i=j or (This[i] > sPivot and sPivot < This[j])
    if i < j then
      begin
      // This[i] > sPivot and sPivot < This[j], so swap them.
      This.Exchange( i, j);
      if j = Pivot then
          Pivot := i
        else if i = Pivot then
          Pivot := j;
      end;
    // L..i is <= pivot
    // j..R is >= pivot
    Inc( i);
    Dec( j)
  until i >= j;
  Inc( j);
  Dec( i);
    // L..i is <= pivot
    // j..R is >= pivot
  if i = Pivot then
    Dec( i);
  if j = Pivot then
    Inc( i);
  Strings_QuickSort_Inner( This, Datum, isAscending, j, R, SCompare);
  R := i
  end
end;


{$IFDEF COMPILER_10_UP}
procedure WStrings_QuickSort_Inner(
  This: TWideStrings; Datum: TObject; isAscending: boolean;
  L, R: Integer; SCompare: TWStringsSortCompare);
var
  i, j, Pivot: Integer;
  sPivot: widestring;
  objPivot: TObject;
  LeftBreakingResult, RightBreakingResult: TComparisonResult;
begin
if isAscending then
    begin
    LeftBreakingResult := crItem2isFirst;
    RightBreakingResult:= crItem1isFirst
    end
  else
    begin
    LeftBreakingResult := crItem1isFirst;
    RightBreakingResult:= crItem2isFirst
    end;
while L < R do
  begin
  i := L;
  j := R;
  Pivot := (L + R) shr 1;
  sPivot := This[ Pivot];
  objPivot := This.Objects[ Pivot];
  repeat
    // L..i-1 is <= pivot
    // j+1..R is >= pivot
    // i..j is as yet unknown
    while (i < j) and
      ((i = Pivot) or
       (SCompare( Datum, This[i], sPivot, This.Objects[i], objPivot) <>
         LeftBreakingResult)) do
           Inc( i);
    // either i=j or This[i] > pivot
    // L..i-1 is <= pivot
    while (j > i) and
      ((j = Pivot) or
       (SCompare( Datum, This[j], sPivot, This.Objects[j], objPivot) <>
         RightBreakingResult)) do
           Dec( j);
    // either i=j or (This[i] > sPivot and sPivot < This[j])
    if i < j then
      begin
      // This[i] > sPivot and sPivot < This[j], so swap them.
      This.Exchange( i, j);
      if j = Pivot then
          Pivot := i
        else if i = Pivot then
          Pivot := j;
      end;
    // L..i is <= pivot
    // j..R is >= pivot
    Inc( i);
    Dec( j)
  until i >= j;
  Inc( j);
  Dec( i);
    // L..i is <= pivot
    // j..R is >= pivot
  if i = Pivot then
    Dec( i);
  if j = Pivot then
    Inc( i);
  WStrings_QuickSort_Inner( This, Datum, isAscending, j, R, SCompare);
  R := i
  end
end;
{$ENDIF}

function Alphabetical_StringsSortCompare(
    Datum: TObject; const s1, s2: string;
    Obj1, Obj2: TObject): TComparisonResult;
begin
result := Compare_Alphabetical( s1, s2)
end;



function Alphabetical_WStringsSortCompare(
    Datum: TObject; const s1, s2: widestring;
    Obj1, Obj2: TObject): TComparisonResult;
begin
result := WCompare_Alphabetical( s1, s2)
end;



function Compare_Alphabetical( const s1, s2: string): TComparisonResult;
begin
if s1 > s2 then
    result := crGreater
  else if s1 < s2 then
    result := crLess
  else
    result := crEquivalent
end;


function WCompare_Alphabetical( const s1, s2: widestring): TComparisonResult;
begin
if s1 > s2 then
    result := crGreater
  else if s1 < s2 then
    result := crLess
  else
    result := crEquivalent
end;


procedure Strings_QuickSort(
  This: TStrings; Datum: TObject; isAscending: boolean;
  SCompare: TStringsSortCompare);
begin
if assigned( This) and (This.Count >= 2) then
  begin
  if not assigned( SCompare) then
    SCompare := Alphabetical_StringsSortCompare;
  This.BeginUpdate;
  try
    Strings_QuickSort_Inner(
      This, Datum, isAscending, 0, This.Count-1, SCompare)
  finally
    This.EndUpdate
    end
  end
end;



{$IFDEF COMPILER_10_UP}
procedure WStrings_QuickSort(
  This: TWideStrings; Datum: TObject; isAscending: boolean;
  SCompare: TWStringsSortCompare);
begin
if assigned( This) and (This.Count >= 2) then
  begin
  if not assigned( SCompare) then
    SCompare := Alphabetical_WStringsSortCompare;
  This.BeginUpdate;
  try
    WStrings_QuickSort_Inner(
      This, Datum, isAscending, 0, This.Count-1, SCompare)
  finally
    This.EndUpdate
    end
  end
end;
{$ENDIF}

function  Compare_PhoneBook( const s1, s2: string): TComparisonResult;
var
  Cannon1, Cannon2: string;
begin
if s1 = s2 then
    result := crEquivalent
  else
    begin
    Cannon1 := Uppercase( Trim( s1));
    Cannon2 := Uppercase( Trim( s2));
    result := Compare_Alphabetical( Cannon1, Cannon2);
    if result = crEquivalent then
      result := Compare_Alphabetical( s1, s2)
    end
end;


function WCompare_PhoneBook( const s1, s2: widestring): TComparisonResult;
var
  Cannon1, Cannon2: widestring;
begin
if s1 = s2 then
    result := crEquivalent
  else
    begin
    Cannon1 := WideUppercase( Trim( s1)); // wide version of trim.
    Cannon2 := WideUppercase( Trim( s2));
    result := WCompare_Alphabetical( Cannon1, Cannon2);
    if result = crEquivalent then
      result := Compare_Alphabetical( s1, s2)
    end
end;


function PhoneBook_StringsSortCompare(
    Datum: TObject; const s1, s2: string;
    Obj1, Obj2: TObject): TComparisonResult;
begin
result := Compare_PhoneBook( s1, s2)
end;


function PhoneBook_WStringsSortCompare(
    Datum: TObject; const s1, s2: widestring;
    Obj1, Obj2: TObject): TComparisonResult;
begin
result := WCompare_PhoneBook( s1, s2)
end;


procedure ObjectList_QuickSort_Inner(
  This: TObjectList; Datum: TObject; isAscending: boolean;
  L, R: Integer; SCompare: TObjectSortCompare);
var
  i, j, Pivot: Integer;
  objPivot: TObject;
  LeftBreakingResult, RightBreakingResult: TComparisonResult;
begin
if isAscending then
    begin
    LeftBreakingResult := crLess;
    RightBreakingResult:= crGreater
    end
  else
    begin
    LeftBreakingResult := crGreater;
    RightBreakingResult:= crLess
    end;
while L < R do
  begin
  i := L;
  j := R;
  Pivot := (L + R) shr 1;
  objPivot := This[ Pivot];
  repeat
    // L..i-1 is <= pivot
    // j+1..R is >= pivot
    // i..j is as yet unknown
    while (i < j) and
      ((i = Pivot) or
       (SCompare( Datum, This, This[i], objPivot) <>
         LeftBreakingResult)) do
           Inc( i);
    // either i=j or This[i] > pivot
    // L..i-1 is <= pivot
    while (j > i) and
      ((j = Pivot) or
       (SCompare( Datum, This, This[j], objPivot) <>
         RightBreakingResult)) do
           Dec( j);
    // either i=j or (This[i] > sPivot and sPivot < This[j])
    if i < j then
      begin
      // This[i] > sPivot and sPivot < This[j], so swap them.
      This.Exchange( i, j);
      if j = Pivot then
          Pivot := i
        else if i = Pivot then
          Pivot := j;
      end;
    // L..i is <= pivot
    // j..R is >= pivot
    Inc( i);
    Dec( j)
  until i >= j;
  Inc( j);
  Dec( i);
    // L..i is <= pivot
    // j..R is >= pivot
  if i = Pivot then
    Dec( i);
  if j = Pivot then
    Inc( i);
  ObjectList_QuickSort_Inner( This, Datum, isAscending, j, R, SCompare);
  R := i
  end
end;

procedure ObjectList_QuickSort(
  This: TObjectList; Datum: TObject; isAscending: boolean;
  SCompare: TObjectSortCompare);
begin
if assigned( This) and (This.Count >= 2) then
  ObjectList_QuickSort_Inner(
      This, Datum, isAscending, 0, This.Count-1, SCompare)
end;




procedure InterfaceList_QuickSort_Inner(
  const This: IInterfaceList; Datum: TObject; isAscending: boolean;
  L, R: Integer; SCompare: TIntfSortCompare);
var
  i, j, Pivot: Integer;
  intfPivot: IInterface;
  LeftBreakingResult, RightBreakingResult: TComparisonResult;
begin
if isAscending then
    begin
    LeftBreakingResult := crLess;
    RightBreakingResult:= crGreater
    end
  else
    begin
    LeftBreakingResult := crGreater;
    RightBreakingResult:= crLess
    end;
while L < R do
  begin
  i := L;
  j := R;
  Pivot := (L + R) shr 1;
  intfPivot := This[ Pivot];
  repeat
    // L..i-1 is <= pivot
    // j+1..R is >= pivot
    // i..j is as yet unknown
    while (i < j) and
      ((i = Pivot) or
       (SCompare( Datum, This, This[i], intfPivot) <>
         LeftBreakingResult)) do
           Inc( i);
    // either i=j or This[i] > pivot
    // L..i-1 is <= pivot
    while (j > i) and
      ((j = Pivot) or
       (SCompare( Datum, This, This[j], intfPivot) <>
         RightBreakingResult)) do
           Dec( j);
    // either i=j or (This[i] > sPivot and sPivot < This[j])
    if i < j then
      begin
      // This[i] > sPivot and sPivot < This[j], so swap them.
      This.Exchange( i, j);
      if j = Pivot then
          Pivot := i
        else if i = Pivot then
          Pivot := j;
      end;
    // L..i is <= pivot
    // j..R is >= pivot
    Inc( i);
    Dec( j)
  until i >= j;
  Inc( j);
  Dec( i);
    // L..i is <= pivot
    // j..R is >= pivot
  if i = Pivot then
    Dec( i);
  if j = Pivot then
    Inc( i);
  InterfaceList_QuickSort_Inner( This, Datum, isAscending, j, R, SCompare);
  R := i
  end
end;


procedure InterfaceList_QuickSort(
  const This: IInterfaceList; Datum: TObject; isAscending: boolean;
  SCompare: TIntfSortCompare);
begin
if assigned( This) and (This.Count >= 2) then
  InterfaceList_QuickSort_Inner(
      This, Datum, isAscending, 0, This.Count-1, SCompare)
end;


end.
