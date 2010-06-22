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

unit uSBD_IntegerList;

interface
uses Classes;

type
  TInt64List = class;
  TIntegerList = class( TList)
  protected
    function  Get(Index: Integer): Integer; overload;
    procedure Put(Index, Item: Integer);  overload;

  public
    function  Add(Item: Integer): Integer;  overload; virtual;
    function  Extract(Item: Integer): Integer;  overload;
    function  First: Integer;  overload;
    function  IndexOf(Item: Integer): Integer; overload; virtual;
    procedure Insert(Index, Item: Integer);  overload;
    function  Last: Integer;  overload;
    function  Remove(Item:  Integer): Integer;  overload;

    function  Clone: TIntegerList; virtual;
    function  CloneAsInt64: TInt64List; virtual;
    procedure AddList (Source: TIntegerList); virtual;
    procedure SortAscending; virtual;

    property  Items[Index: Integer]: Integer read Get write Put; default;
  end;
  TIntegerListClass = class of TIntegerList;

  TSortedIntegerList = class(TIntegerList)
  public
    function Add(Item: Integer): Integer; override;
    function IndexOf( Item: Integer): Integer; override;
    function IndexOfNearest( Item: Integer): Integer; virtual;
    function IndexMatches (Item, NearestIndex: Integer): boolean;
  end;

  TInt64List = class( TList)
  protected
    function  Get( Index: Integer): int64; overload;
    procedure Put( Index: Integer; Item: int64);  overload;

{$IFOPT W+}
  {$DEFINE SDB_Warnings}
  {$WARNINGS OFF}
  {$ENDIF}
    procedure Notify( Ptr: Pointer; Action: TListNotification); override;
{$IFDEF SBD_Warnings}
  {$WARNINGS ON}
  {$UNDEF SBD_Warnings}
  {$ENDIF}

  public
    function  Add( Item: int64): Integer;  overload; virtual;
    function  Extract( Item: int64): int64;  overload;
    function  First: int64;  overload;
    function  IndexOf( Item: int64): Integer; overload; virtual;
    procedure Insert( Index: Integer; Item: int64);  overload;
    function  Last: int64;  overload;
    function  Remove( Item:  int64): Integer;  overload;

    function  Clone: TInt64List; virtual;
    function  CloneAsInteger: TIntegerList; virtual;
    procedure AddList (Source: TInt64List); virtual;
    procedure SortAscending; virtual;

    property  Items[ Index: Integer]: Int64 read Get write Put; default;
  end;
  TInt64ListClass = class of TInt64List;

{$IFOPT W+}
  {$DEFINE SDB_Warnings}
  {$WARNINGS OFF}
  {$ENDIF}
function IntegerListSortCompare (Item1, Item2: Pointer): Integer;
function Int64ListSortCompare   (Item1, Item2: Pointer): Integer;
{$IFDEF SBD_Warnings}
  {$WARNINGS ON}
  {$UNDEF SBD_Warnings}
  {$ENDIF}

type
  TBooleanList = class;
  TBooleanListIterativeTask = function (
    Sender: TBooleanList; Datum: TObject;
    Value: boolean; Idx: integer): boolean of object;

  TBooleanList = class( TBits, IInterface)
    private
      FOwnerInterface: IInterface;

      function CloseBit_Helper(
        Sender: TBooleanList; Datum: TObject;
        Value: boolean; Idx: integer): boolean;
      function Insert_Helper(
        Sender: TBooleanList; Datum: TObject;
        Value: boolean; Idx: integer): boolean;
      function SetAllBits_Helper(
        Sender: TBooleanList; Datum: TObject;
        Value: boolean; Idx: integer): boolean;

    protected
      function  _AddRef : Integer; virtual; stdcall;
      function  _Release: Integer; virtual; stdcall;
      function  GetOwner: TPersistent; virtual;
      class procedure Error( Msg: PResStringRec; Data: Integer); virtual;

    public
      FIterativeLoopCount: integer;

      function  QueryInterface( const IID: TGUID; out Obj): HResult; virtual; stdcall;
      procedure AfterConstruction; override;
      function  ClosedBit: Integer;
      function  First: boolean;
      function  Last : boolean;
      procedure Clear;
      procedure SetAllBits( Value: boolean);
      procedure Insert( Index: integer; Value: boolean);
      procedure Delete( Index: integer);
      function  Add( Value: boolean): integer;
      function  Iterate( SetTask, ClearTask: TBooleanListIterativeTask;
          Datum: TObject; goForward: boolean; var Idx: integer): boolean;
    end;

implementation








uses RTLConsts, SysUtils, uSBD_Resources;

const
  BitsPerInt = SizeOf(Integer) * 8;

type
  TBitEnum = 0..BitsPerInt - 1;
  TBitSet = set of TBitEnum;
  PBitArray = ^TBitArray;
  TBitArray = array[0..4096] of TBitSet;

  TAbstractBits = class
    protected
      function  _AddRef : Integer; virtual; stdcall; abstract;

    public
      FSize: Integer;
      FBits: Pointer;
    end;

  TInsertDeleteParam = class( TInterfacedObject)
    private
      FSize : integer;
      FIndex: integer;

    public
      class function MakeParam( Size, Index: integer; var Obj: TObject): IInterface;
    end;

function TIntegerList.Get(Index: Integer): Integer;
begin
result := Integer(inherited Get(Index))
end;


procedure TIntegerList.SortAscending;
begin
Sort( IntegerListSortCompare)
end;


{$IFOPT W+}
  {$DEFINE SDB_Warnings}
  {$WARNINGS OFF}
  {$ENDIF}
procedure TIntegerList.Put(Index, Item: Integer);
begin
inherited Put(Index,Pointer(Item))
end;



function TIntegerList.Add(Item: Integer): Integer;
begin
result := inherited Add(Pointer(Item))
end;



function TIntegerList.Extract(Item: Integer): Integer;
begin
result := Integer(inherited Extract(Pointer(Item)))
end;



function TIntegerList.First: Integer;
begin
result := Integer(inherited First)
end;



function TIntegerList.IndexOf(Item: Integer): Integer;
begin
result := inherited IndexOf(Pointer(Item))
end;



procedure TIntegerList.Insert(Index, Item: Integer);
begin
inherited Insert(Index,Pointer(Item))
end;



function TIntegerList.Last: Integer;
begin
result := Integer(inherited Last)
end;



function TIntegerList.Remove(Item:  Integer): Integer;
begin
result := inherited Remove( Pointer(Item))
end;
{$IFDEF SBD_Warnings}
  {$WARNINGS ON}
  {$UNDEF SBD_Warnings}
  {$ENDIF}




{ TSortedIntegerList }

function TSortedIntegerList.Add(Item: Integer): Integer;
begin
result := IndexOfNearest(Item);
Insert( result, Item)
end;


function TSortedIntegerList.IndexMatches (Item, NearestIndex: Integer): boolean;
begin
result := (NearestIndex >= 0) and (NearestIndex < Count)
      and (Item = Items[NearestIndex])
end;

function TSortedIntegerList.IndexOf(Item: Integer): Integer;
begin
if count < 128 then
    result := inherited IndexOf(Item)
  else
    begin
    result := IndexOfNearest(Item);
    if not IndexMatches( Item, result) then
      result := -1
    end
end;

function TSortedIntegerList.IndexOfNearest(Item: Integer): Integer;
var
  c, Low, High, Value: Integer;
begin
c := Count;
result := 0;
if (c = 0) or (Item = Items[0]) then exit;
High := c-1;
Value := Items[High];
if Item >= Value then
  begin
  result := High;
  if Item > Value then
    inc(result);
  exit
  end;
Low := 0;
repeat
  result := (Low + High) div 2;
  if result = Low then
    begin
    result := High;
    break
    end;
  Value := Items[result];
  if Item=Value then break;
  if Item < Value then
      High := result
    else
      Low := result
until False
end;


{$IFOPT W+}
  {$DEFINE SDB_Warnings}
  {$WARNINGS OFF}
  {$ENDIF}
function IntegerListSortCompare( Item1, Item2: Pointer): Integer;
begin
if Integer(Item1) < Integer(Item2) then
    result := -1
  else if Integer(Item1) = Integer(Item2) then
    result := 0
  else
    result := 1
end;


function Int64ListSortCompare( Item1, Item2: Pointer): Integer;
begin
if PInt64( Item1)^ < PInt64( Item2)^ then
    result := -1
  else if PInt64( Item1)^ = PInt64( Item2)^ then
    result := 0
  else
    result := 1
end;
{$IFDEF SBD_Warnings}
  {$WARNINGS ON}
  {$UNDEF SBD_Warnings}
  {$ENDIF}

function TIntegerList.Clone: TIntegerList;
begin
result := TIntegerListClass(ClassType).Create;
result.AddList(self)
end;

function TIntegerList.CloneAsInt64: TInt64List;
var
  j: Integer;
begin
result := TInt64List.Create;
for j := 0 to Count - 1 do
  result.Add( Get( j))
end;



procedure TIntegerList.AddList (Source: TIntegerList);
var
  j: Integer;
begin
for j := 0 to Source.Count-1 do
  Insert( Count, Source[j]) // do not Add() ! This preserves order.
end;


{ TInt64List }
type PInt64 = ^int64;

function TInt64List.Add( Item: int64): Integer;
begin
result := Count;
Insert( result, Item)
end;

procedure TInt64List.AddList( Source: TInt64List);
var
 j: Integer;
begin
for j := 0 to Source.Count - 1 do
  Add( Source[j])
end;

function TInt64List.Clone: TInt64List;
var
  j: Integer;
begin
result := TInt64ListClass(ClassType).Create;
for j := 0 to Count - 1 do
  result.Add( Get( j))
end;

function TInt64List.CloneAsInteger: TIntegerList;
var
  j: Integer;
  Value: int64;
  doError: boolean;
begin
result := TIntegerList.Create;
for j := 0 to Count - 1 do
  begin
  Value := Get( j);
  doError := (Value > MaxInt) or (Value < (-MaxInt - 1));
  if doError then
    Value := Integer( Int64Rec( Value).Lo);
  result.Add( Value);
  if doError then
    Error( @SInt64ToIntegerConv, j)
  end
end;

function TInt64List.Extract( Item: int64): int64;
var
  Idx: integer;
begin
result := Item;
Idx := IndexOf( Item);
if Idx <> -1 then
  Delete( Idx)
end;

function TInt64List.First: int64;
begin
if Count = 0 then
    result := 0
  else
    result := PInt64( inherited First)^
end;

function TInt64List.Get( Index: Integer): int64;
begin
result := PInt64( inherited Get( Index))^
end;

function TInt64List.IndexOf( Item: int64): Integer;
var
  j: Integer;
begin
result := -1;
for j := 0 to Count - 1 do
  begin
  if Get( j) <> Item then continue;
  result := j;
  break
  end
end;

procedure TInt64List.Insert( Index: Integer; Item: int64);
var
  P: PInt64;
begin
if (Index < 0) or (Index > Count) then
  Error(@SListIndexError, Index);
GetMem( P, SizeOf( int64));
P^ := Item;
inherited Insert( Index, P)
end;

function TInt64List.Last: int64;
begin
if Count = 0 then
    result := 0
  else
    result := Get( Count - 1)
end;

procedure TInt64List.Notify( Ptr: Pointer; Action: TListNotification);
begin
if Action = lnDeleted then
  FreeMem( Ptr, SizeOf( int64))
end;

procedure TInt64List.Put( Index: Integer; Item: int64);
var
  P: PInt64;
begin
if (Index < 0) or (Index >= Count) then
    Error(@SListIndexError, Index);
P  := inherited Get( Index);
P^ := Item
end;

function TInt64List.Remove( Item: int64): Integer;
begin
result := IndexOf( Item);
if result <> -1 then
  Delete( result)
end;

procedure TInt64List.SortAscending;
begin
Sort( Int64ListSortCompare)
end;

{ TBooleanList }

function TBooleanList._AddRef: Integer;
begin
if assigned( FOwnerInterface) then
    result := FOwnerInterface._AddRef
  else
    result := -1
end;

function TBooleanList._Release: Integer;
begin
if assigned( FOwnerInterface) then
    result := FOwnerInterface._Release
  else
    result := -1
end;

function TBooleanList.Add( Value: boolean): integer;
begin
result := Size;
Insert( result, Value)
end;

procedure TBooleanList.AfterConstruction;
begin
inherited;
supports( GetOwner, IInterface, FOwnerInterface)
end;

function TBooleanList.ClosedBit: Integer;
begin
Iterate( CloseBit_Helper, nil, nil, True, result)
end;

function TBooleanList.CloseBit_Helper(
  Sender: TBooleanList; Datum: TObject;
  Value: boolean; Idx: integer): boolean;
begin
result := True
end;

procedure TBooleanList.Delete( Index: integer);
var
  Size1: integer;
  j: Integer;
begin
Size1 := Size;
if (Index < 0) or (Index >= Size1) then
  Error( @SListIndexError, Index);
for j := Index to Size1 - 2 do
  Bits[ j] := Bits[ j + 1];
Size := Size1 - 1
end;

function TBooleanList.First: boolean;
begin
result := (Size > 0) and Bits[ 0]
end;

procedure TBooleanList.Insert( Index: integer; Value: boolean);
var
  Size1: integer;
  j    : Integer;
  Obj  : TObject;
begin
Size1 := Size;
if (Index < 0) or (Index > Size1) then
  Error(  @SListIndexError, Index);
Size  := Size1 + 1;
if Index <> Size1 then
  begin
  TInsertDeleteParam.MakeParam( Size, Index, Obj);
  Iterate( Insert_helper, Insert_Helper, Obj, False, j)
  end;
{ Equivalently, (but with less efficiency) ...
    for j := Size1 - 1 downto Index do
      Bits[ j + 1] := Bits[ j];        }
Bits[ Index] := Value
end;

function TBooleanList.Insert_Helper(
  Sender: TBooleanList; Datum: TObject;
  Value: boolean; Idx: integer): boolean;
begin
result := Idx <= TInsertDeleteParam( Datum).FIndex;
if Idx < (TInsertDeleteParam( Datum).FSize - 2) then
  Bits[ Idx + 1] := Value
end;


function TBooleanList.Iterate( SetTask, ClearTask: TBooleanListIterativeTask;
  Datum: TObject; goForward: boolean; var Idx: integer): boolean;
const
  FullSet  = [0..BitsPerInt - 1];
  EmptySet = [];
var
  I: Integer;
  E: Integer;
  FBits: PBitArray;
  FSize: integer;
  IncPerInt: integer;

  function IterateOverBits( isSet: boolean): boolean;
  begin
  result := (assigned( SetTask  ) and      isSet  and SetTask  ( self, Datum, True , Idx)) or
            (assigned( ClearTask) and (not isSet) and ClearTask( self, Datum, False, Idx))
  end;

  function IterateOverInteger( B: TBitSet): boolean;
  var
    J: TBitEnum;
  begin
  result := False;
  if ((not assigned( SetTask  )) and (B = FullSet )) or
     ((not assigned( ClearTask)) and (B = EmptySet)) then
         begin
         Inc( Idx, IncPerInt);
         if Idx  > FSize then
            Idx := FSize
         end

    else if goForward then
      for J := Low( J) to High( J) do
         begin
         result := (Idx >= FSize) or IterateOverBits( J in B);
         if result then break;
         Inc( Idx);
         end

    else
      for J := High( J) downto Low( J) do
         begin
         result := (Idx <= -1) or IterateOverBits( J in B);
         if result then break;
         Dec( Idx)
         end
  end;

begin
result := False;
FSize  := TAbstractBits( self).FSize;
FBits  := TAbstractBits( self).FBits;
E      := (FSize + BitsPerInt - 1) div BitsPerInt - 1;
if (not assigned( SetTask)) and not assigned( ClearTask) then
  begin
  if goForward then
      Idx := FSize
    else
      Idx := -1;
  exit
  end;
if goForward then
    begin
    Idx       := 0;
    IncPerInt := BitsPerInt
    end
  else
    begin
    Idx       := FSize - 1;
    IncPerInt := -BitsPerInt
    end;
Inc( FIterativeLoopCount);
try
if goForward then
    for I := 0 to E do
      begin
      result := IterateOverInteger( FBits^[I]);
      if result then break
      end
  else
    for I := E downto 0 do
      begin
      result := IterateOverInteger( FBits^[I]);
      if result then break
      end
finally
Dec( FIterativeLoopCount)
end end;




function TBooleanList.Last: boolean;
begin
result := (Size > 0) and Bits[ Size - 1]
end;

function TBooleanList.QueryInterface( const IID: TGUID; out Obj): HResult;
const
  E_NOINTERFACE = HResult($80004002);
begin
if GetInterface( IID, Obj) then
    result := 0
  else
    result := E_NOINTERFACE
end;

function TBooleanList.GetOwner: TPersistent;
begin
result := nil
end;


class procedure TBooleanList.Error( Msg: PResStringRec; Data: Integer);
  function ReturnAddr: Pointer;
  asm
  MOV EAX,[EBP+4]
  end;
begin
raise EBitsError.CreateFmt( LoadResString(Msg), [Data]) at ReturnAddr
end;

procedure TBooleanList.Clear;
begin
Size := 0
end;

procedure TBooleanList.SetAllBits( Value: boolean);
var
  Obj: TObject;
  Idx: integer;
  SetAction, ClearAction: TBooleanListIterativeTask;

begin
SetAction   := nil;
ClearAction := nil;
if Value then
    SetAction   := SetAllBits_Helper
  else
    ClearAction := SetAllBits_Helper;
TInsertDeleteParam.MakeParam( Size, 0, Obj);
Iterate( ClearAction, SetAction, Obj, True, Idx)
end;


function TBooleanList.SetAllBits_Helper(
  Sender: TBooleanList; Datum: TObject; Value: boolean; Idx: integer): boolean;
begin
result := False;
Bits[ Idx] := not Value
end;

{ TInsertDeleteParam }

class function TInsertDeleteParam.MakeParam(
  Size, Index: integer; var Obj: TObject): IInterface;
begin
Obj := TInsertDeleteParam.Create;
with TInsertDeleteParam( Obj) do
  begin
  FSize  := Size;
  FIndex := Index
  end;
Obj.GetInterface( IInterface, result)
end;

end.
