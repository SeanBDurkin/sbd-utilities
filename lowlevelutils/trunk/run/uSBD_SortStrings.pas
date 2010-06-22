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

unit uSBD_SortStrings;
{$I 'SBDCompilers.inc'}

interface
  uses Classes, uSBD_FundamentTypeSupport, Contnrs;

type
  TStringsSortCompare = function(
    Datum: TObject; const s1, s2: string;
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

procedure ObjectList_QuickSort(
  This: TObjectList; Datum: TObject; isAscending: boolean;
  SCompare: TObjectSortCompare);

procedure InterfaceList_QuickSort(
  const This: IInterfaceList; Datum: TObject; isAscending: boolean;
  SCompare: TIntfSortCompare);

function Alphabetical_StringsSortCompare(
    Datum: TObject; const s1, s2: string;
    Obj1, Obj2: TObject): TComparisonResult;

function PhoneBook_StringsSortCompare(
    Datum: TObject; const s1, s2: string;
    Obj1, Obj2: TObject): TComparisonResult;

function  Compare_Alphabetical( const s1, s2: string): TComparisonResult;

function  Compare_PhoneBook( const s1, s2: string): TComparisonResult;



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



function Alphabetical_StringsSortCompare(
    Datum: TObject; const s1, s2: string;
    Obj1, Obj2: TObject): TComparisonResult;
begin
result := Compare_Alphabetical( s1, s2)
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


function PhoneBook_StringsSortCompare(
    Datum: TObject; const s1, s2: string;
    Obj1, Obj2: TObject): TComparisonResult;
begin
result := Compare_PhoneBook( s1, s2)
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
