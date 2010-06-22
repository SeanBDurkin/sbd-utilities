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

unit uSBD_VisitorUtils;
interface
uses Classes, uSBD_Visitor, Contnrs;

type

  TPopper = class( TVisitor)
    private
      FPoplist: TObjectlist;

    protected
      function Visit: boolean; override;

    public
      constructor Create( TargetClass: TClass; Trans: TVisitationTranslator;
        isCounted: boolean = False); override;
      destructor  Destroy; override;
      procedure   Clear; override;
      function    Pop: integer;  virtual;
      function    ParseAndPop( Node: TObject): integer;
      procedure   NoticeNodeRelease( Releasee: TObject);
  end;

implementation











{ TPopper }

constructor TPopper.Create(
  TargetClass: TClass; Trans: TVisitationTranslator; isCounted: boolean = False);
begin
inherited;
FPoplist := TObjectlist.Create( True)
end;





destructor TPopper.Destroy;
begin
FPoplist.OwnsObjects := False;
inherited;
FPoplist.Free
end;





procedure TPopper.Clear;
begin
inherited;
FPoplist.OwnsObjects := False;
try
  FPoplist.Clear
finally
  FPoplist.OwnsObjects := True
end end;





function TPopper.ParseAndPop( Node: TObject): integer;
begin
try
  Parse( Node);
  result := Pop;
  finally
  Clear
end end;





function TPopper.Pop: integer;
begin
result := 0;
while FPoplist.Count > 0 do
  begin
  Inc( result);
  FPopList.Delete( 0)
  end
end;





function TPopper.Visit: boolean;
begin
result := False;
FPoplist.Add( FNode)
end;




procedure TPopper.NoticeNodeRelease( Releasee: TObject);
begin
FPoplist.Extract( Releasee)
end;

end.
