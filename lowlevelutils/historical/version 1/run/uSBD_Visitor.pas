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

unit uSBD_Visitor;
interface
uses Classes, Contnrs, uSBD_FlexiInterfaced;

type
TVisitationTranslator = class;
TVisitor = class( TFlexiInterfaced)
  private
    Parsed: TObjectList;
    Translator: TVisitationTranslator;
    FOwnsTranslator: boolean;
    FAllowColleagueToParse: boolean; // Set to True to allow another
      // visitor to parse our parsed list.

  protected
    FTargetClass: TClass;

    function MatchesTargetClass: boolean;
    function NativeParseChildren: boolean;

    function Applies: boolean; virtual;
    function Visit: boolean; virtual;
    function AppliesChildren: boolean; virtual;
    function ParseChildren: boolean; virtual;

  public
    FRootNode: TObject;
    FNode: TObject;
    goForward: boolean;

    constructor Create( TargetClass: TClass; Trans: TVisitationTranslator;
        isCounted: boolean = False); virtual;
    constructor CreateStandard( TargetClass: TClass);
    destructor  Destroy; override;
    function    Parse( Node: TObject): boolean;
    procedure   Clear; virtual; // Prepares it for another Parse.
  end;

IVisitationNode = interface
['{8ACFF22B-8D04-489F-8613-5520BC632891}']
  function ParseOverChildren( Visitor: TVisitor): boolean;
  // Initialise result := False;
  // For each "child" object, set result := Visitor.Parse( Child);
  //   but break if result is True.
  end;

TVisitationNodeAdapter = class( TObject)
  protected
    function GetBaseClass: TClass; virtual; abstract;

  public
    function AdaptsNode( Node: TObject): boolean; virtual;

    property BaseClass: TClass // If an object descends from this class,
      read GetBaseClass;       //  but does not implement IVisitationNode,
                               //  then use this adapter.
    function ParseChildrenOf( Visitor: TVisitor; Node: TObject): boolean;
      virtual; abstract;
  end;

TVisitationTranslator = class( TObject)
  private
    FAdapters: TObjectList;

  public
    constructor CreateEmpty( dummy : integer = 0);
    constructor Create; // Create with default adapters.
    destructor  Destroy; override;
    procedure   RegisterAdapter( Adapter: TVisitationNodeAdapter);
    procedure   DeregisterAdapter( Adapter: TVisitationNodeAdapter);
    function    ParseAdapters( Visitor: TVisitor; Node: TObject;
                               var FoundAdapter: boolean): boolean;
  end;
{ HINT:
In each application, create a globally accessible singleton like so:
  VTrans := TVisitationTranslator.Create;
and then for every visitor construction, pass this.}

TFinder = class;

TMatchTest = function ( RootNode, Subject: TObject;
                        Visitor: TFinder; Datum: longint): boolean of object;

TFinder = class( TVisitor)
  protected
    FMatchTest: TMatchTest;

    function Applies: boolean; override;
    function Visit: boolean; override;

  public
    FDatum    : longint;
    FoundObj: TObject;

    constructor CreateFinder( TargetClass: TClass;
      MatchTest: TMatchTest; Trans: TVisitationTranslator); virtual;
    procedure Clear; override;

    function Find( Node1: TObject; Datum: longint; var Finding: TObject): boolean;
  end;



TListMaker = class( TFinder)
  protected
    function Visit: boolean; override;

  public
    FList: TObjectList;

    constructor CreateFinder( TargetClass: TClass;
      MatchTest: TMatchTest; Trans: TVisitationTranslator); override;
    destructor Destroy; override;
    procedure  Clear; override;
  end;

TStringsMaker = class;
TStringValue = function ( ParseNode, Subject: TObject;
                          Visitor: TStringsMaker): string of object;
TStringsMaker = class( TFinder)
  protected
    FList: TStrings;
    FStringValue: TStringValue;

    function Visit: boolean; override;

  public
    constructor CreateStringsMaker( TargetClass: TClass;
      MatchTest: TMatchTest; StringValue: TStringValue;
      OutList: TStrings; Trans: TVisitationTranslator);
  end;


TIterativeAction = function ( PropertyValue, Datum: TObject): boolean;
// return True to break iterations.
function IterateOverObjectProperties(
  Subject: TObject; Proc: TIterativeAction; Datum: TObject;
  goForward: boolean=True): boolean;

implementation













uses SysUtils, TypInfo, ComCtrls;
{ TVisitor }

type
TListAdapter = class( TVisitationNodeAdapter)
  protected
    function GetBaseClass: TClass; override;
  public
    function ParseChildrenOf( Visitor: TVisitor; Node: TObject): boolean;
      override;
  end;
TComponentAdapter = class( TVisitationNodeAdapter)
  protected
    function GetBaseClass: TClass; override;
  public
    function ParseChildrenOf( Visitor: TVisitor; Node: TObject): boolean;
      override;
  end;
TCollectionAdapter = class( TVisitationNodeAdapter)
  protected
    function GetBaseClass: TClass; override;
  public
    function ParseChildrenOf( Visitor: TVisitor; Node: TObject): boolean;
      override;
  end;
TPageControlAdapter = class( TVisitationNodeAdapter)
  protected
    function GetBaseClass: TClass; override;
  public
    function ParseChildrenOf( Visitor: TVisitor; Node: TObject): boolean;
      override;
  end;


function TVisitor.Applies: boolean;
begin
result := MatchesTargetClass
end;

function TVisitor.AppliesChildren: boolean;
begin
result := not MatchesTargetClass
end;

procedure TVisitor.Clear;
begin
Parsed.Clear;
FRootNode := nil;
FNode := nil
end;



constructor TVisitor.Create( TargetClass: TClass;
                             Trans: TVisitationTranslator;
                             isCounted: boolean = False);
var
  ConstructionOptions: TInterfacingOptionSet;
begin
ConstructionOptions := [];
if isCounted then
  Include( ConstructionOptions, ioIsCounting);
inherited Create( ConstructionOptions);
goForward    := True;
FRootNode    := nil;
Parsed       := TObjectList.Create( False);
FNode        := nil;
FTargetClass := TargetClass;
Translator   := Trans;
FAllowColleagueToParse := False;
FOwnsTranslator := False
end;



constructor TVisitor.CreateStandard( TargetClass: TClass);
begin
Create( TargetClass, nil);
FOwnsTranslator := True;
Translator      := TVisitationTranslator.Create
end;





destructor TVisitor.Destroy;
begin
Clear;
Parsed.Free;
if FOwnsTranslator then
  Translator.Free;
inherited
end;

function TVisitor.MatchesTargetClass: boolean;
// If FNode = nil then no match.
// Otherwise if FTargetClass = nil then match anything.
// Otherwise match iff FNode is the target class.
begin
result := assigned( FNode) and
 ((not assigned(FTargetClass)) or FNode.InheritsFrom( FTargetClass))
end;

{$WARNINGS OFF}
function IterateOverObjectProperties(
  Subject: TObject; Proc: TIterativeAction; Datum: TObject;
  goForward: boolean): boolean;
var
  j: Integer;
  Props: PPropList;
  TypeData: PTypeData;
  LastIdx: integer;

  function IterateOverJth: boolean;
  var
    Child: TObject;
  begin
  result := False;
  with Props^[j]^ do
    if (PropType^^.Kind = tkClass) and (GetProc <> nil) then
      begin
      Child := TObject( GetOrdProp( Subject, Props^[j]));
      result := assigned( Child) and Proc( Child, Datum);
      end
  end;

begin
result := False;
if Subject.ClassInfo = nil then exit;
TypeData := GetTypeData( Subject.ClassInfo);
if (not assigned( TypeData)) or (TypeData^.PropCount = 0) then exit;
GetMem( Props, TypeData^.PropCount * sizeof(Pointer));
// GetMem normally generates a compiler warning.
// Don't sweat it. The compiler is just being paranoid.
try
  GetPropInfos( Subject.ClassInfo, Props);
  LastIdx := TypeData^.PropCount - 1;
  if goForward then
      for j := 0 to LastIdx do
        begin
        result := IterateOverJth;
        if result then break
        end
    else
      for j := LastIdx downto 0 do
        begin
        result := IterateOverJth;
        if result then break
        end
  finally
    Freemem(Props)
end end;
{$WARNINGS ON}



function TVisitor.NativeParseChildren: boolean;
var
  j, CountMinusOne: integer;
  FoundAdapter: boolean;

 function ParseObject( PropertyValue, Datum: TObject): boolean;
 begin
 result := TVisitor( Datum).Parse( PropertyValue)
 end;

 function ParseJthParsee: boolean;
 begin
 result := Parse( TVisitor(FNode).Parsed[j])
 end;

begin
FoundAdapter := False;
{$WARNINGS OFF}
result := (assigned( Translator) and Translator.ParseAdapters( self, FNode, FoundAdapter)) or
         ((not FoundAdapter) and IterateOverObjectProperties( FNode, @ParseObject, self, goForward));
{$WARNINGS ON}
if (not result) and (FNode is TVisitor) and
   (FNode <> self) and TVisitor( FNode).FAllowColleagueToParse then
  begin
  CountMinusOne := TVisitor( FNode).Parsed.Count - 1;
  if goForward then
      for j := 0 to CountMinusOne do
        begin
        result := ParseJthParsee;
        if result then break
        end
    else
      for j := CountMinusOne downto 0 do
        begin
        result := ParseJthParsee;
        if result then break
end end end;




function TVisitor.Parse( Node: TObject): boolean;
var
  OldNode: TObject;
  Idx    : integer;
begin
result := False;
if Parsed.IndexOf( Node) <> -1 then exit;
if not assigned( FRootNode) then
  FRootNode := Node;
OldNode := FNode;
FNode := Node;
try
Idx := Parsed.Add( FNode);
result := (                     Applies         and Visit        )
       or (assigned( FNode) and AppliesChildren and ParseChildren);
if not assigned( FNode) then
  // Visit must have destroyed the node.
  Parsed.Delete( Idx); // Remove reference to destroyed object.
finally
FNode := OldNode
end end;


function TVisitor.ParseChildren: boolean;
var
  VisitationNode: IVisitationNode;

begin
if supports( FNode, IVisitationNode, VisitationNode) then
    result := VisitationNode.ParseOverChildren( self)
  else
    result := NativeParseChildren
end;

function TVisitor.Visit: boolean;
begin
result := False
end;




{ TFinder }

function TFinder.Applies: boolean;
begin
result := (inherited Applies) // MatchesTargetClass
  and ((not assigned( FMatchTest))
        or FMatchTest( FRootNode, FNode, self, FDatum))
end;




procedure TFinder.Clear;
begin
inherited;
FoundObj := nil;
FDatum   := 0
end;




constructor TFinder.CreateFinder( TargetClass: TClass;
  MatchTest: TMatchTest; Trans: TVisitationTranslator);
begin
inherited Create( TargetClass, Trans);
FMatchTest := MatchTest;
FoundObj   := nil;
FDatum     := 0
end;





function TFinder.Find(
  Node1: TObject; Datum: Integer; var Finding: TObject): boolean;
begin
Clear;
FDatum := Datum;
result := Parse( Node1);
if result then
    Finding := FoundObj
  else
    Finding := nil;
Clear
end;





function TFinder.Visit: boolean;
begin
result   := True;
FoundObj := FNode
end;



{ TListMaker }

procedure TListMaker.Clear;
begin
inherited;
if assigned( FList) then
  FList.Clear
end;



constructor TListMaker.CreateFinder( TargetClass: TClass;
  MatchTest: TMatchTest; Trans: TVisitationTranslator);
begin
inherited CreateFinder( TargetClass, MatchTest, Trans);
FList := TObjectList.Create( False)
end;



destructor TListMaker.Destroy;
begin
FreeAndNil( FList);
inherited
end;



function TListMaker.Visit: boolean;
begin
result := False;
FList.Add( FNode)
end;



{ TStringsMaker }

constructor TStringsMaker.CreateStringsMaker( TargetClass: TClass;
  MatchTest: TMatchTest; StringValue: TStringValue; OutList: TStrings;
  Trans: TVisitationTranslator);
begin
inherited CreateFinder( TargetClass, MatchTest, Trans);
FList := OutList;
FStringValue := StringValue
end;



function TStringsMaker.Visit: boolean;
begin
result := False;
if assigned( FStringValue) then
    FList.Add( FStringValue( FRootNode, FNode, self))
  else
    FList.Add( FNode.ClassName)
    // An alternative here would be to check for 'Name'
    //   or 'DisplayName' published string properties.
end;


{ TVisitationTranslator }

constructor TVisitationTranslator.Create;
begin
CreateEmpty;
FAdapters.Add( TComponentAdapter.Create);
FAdapters.Add( TPageControlAdapter.Create);
FAdapters.Add( TCollectionAdapter.Create);
FAdapters.Add( TListAdapter.Create)
end;



constructor TVisitationTranslator.CreateEmpty;
begin
FAdapters := TObjectList.Create
end;



procedure TVisitationTranslator.DeregisterAdapter(
  Adapter: TVisitationNodeAdapter);
begin
FAdapters.Remove( Adapter)
end;



destructor TVisitationTranslator.Destroy;
begin
FAdapters.Free;
inherited
end;



function TVisitationTranslator.ParseAdapters(
  Visitor: TVisitor; Node: TObject; var FoundAdapter: boolean): boolean;
var
  j: Integer;
  Adapter: TVisitationNodeAdapter;

begin
result       := False;
FoundAdapter := False;
for j := FAdapters.Count - 1 downto 0 do
  begin
  Adapter      := TVisitationNodeAdapter( FAdapters[j]);
  FoundAdapter := Adapter.AdaptsNode( Node);
  if not FoundAdapter then continue;
  result := Adapter.ParseChildrenOf( Visitor, Node);
  break
  end
end;



procedure TVisitationTranslator.RegisterAdapter(
  Adapter: TVisitationNodeAdapter);
begin
FAdapters.Add( Adapter)
end;





function TListAdapter.GetBaseClass: TClass;
begin
result := TObjectList
end;

function TListAdapter.ParseChildrenOf( Visitor: TVisitor; Node: TObject): boolean;
var
  j, Lst: Integer;

  function DoJth: boolean;
  var
    Child: TObject;
  begin
  Child := TObjectList(Node)[j];
  result := (not assigned( Child)) or Visitor.Parse( Child);
  end;

begin
result := False;
Lst := TObjectList(Node).Count - 1;
if Visitor.goForward then
    for j := 0 to Lst do
      begin
      result := DoJth;
      if result then break
      end
  else
    for j := Lst downto 0 do
      begin
      result := DoJth;
      if result then break
      end
end;

{ TComponentAdapter }

function TComponentAdapter.GetBaseClass: TClass;
begin
result := TComponent
end;

function TComponentAdapter.ParseChildrenOf( Visitor: TVisitor;
  Node: TObject): boolean;
var
  j, Lst: Integer;

  function DoJth: boolean;
  var
    Child: TObject;
  begin
  Child := TComponent(Node).Components[j];
  result := Visitor.Parse( Child);
  end;

begin
result := False;
Lst := TComponent(Node).ComponentCount - 1;
if Visitor.goForward then
    for j := 0 to Lst do
      begin
      result := DoJth;
      if result then break
      end
  else
    for j := Lst downto 0 do
      begin
      result := DoJth;
      if result then break
      end
end;

{ TCollectionAdapter }

function TCollectionAdapter.GetBaseClass: TClass;
begin
result := TCollection
end;

function TCollectionAdapter.ParseChildrenOf( Visitor: TVisitor;
  Node: TObject): boolean;
var
  j, Lst: Integer;

  function DoJth: boolean;
  var
    Child: TObject;
  begin
  Child := TCollection(Node).Items[j];
  result := Visitor.Parse( Child);
  end;

begin
result := False;
Lst := TCollection(Node).Count - 1;
if Visitor.goForward then
    for j := 0 to Lst do
      begin
      result := DoJth;
      if result then break
      end
  else
    for j := Lst downto 0 do
      begin
      result := DoJth;
      if result then break
      end
end;

{ TPageControlAdapter }

function TPageControlAdapter.GetBaseClass: TClass;
begin
result := TPageControl
end;

function TPageControlAdapter.ParseChildrenOf( Visitor: TVisitor;
  Node: TObject): boolean;
var
  j, Lst: Integer;

  function DoJth: boolean;
  var
    Child: TObject;
  begin
  Child := TPageControl(Node).Pages[j];
  result := Visitor.Parse( Child);
  end;

begin
result := False;
Lst := TPageControl(Node).PageCount - 1;
if Visitor.goForward then
    for j := 0 to Lst do
      begin
      result := DoJth;
      if result then break
      end
  else
    for j := Lst downto 0 do
      begin
      result := DoJth;
      if result then break
      end
end;

{ TVisitationNodeAdapter }

function TVisitationNodeAdapter.AdaptsNode( Node: TObject): boolean;
begin
result := Node.InheritsFrom( BaseClass)
end;

end.
