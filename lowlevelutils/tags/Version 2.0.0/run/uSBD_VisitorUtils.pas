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
      constructor Create( TargetClass: TClass; Trans: TVisitationTranslator); override;
      destructor  Destroy; override;
      procedure   Clear; override;
      function    Pop: integer;  virtual;
      function    ParseAndPop( Node: TObject): integer;
      procedure   NoticeNodeRelease( Releasee: TObject);
  end;

implementation











{ TPopper }

constructor TPopper.Create(
  TargetClass: TClass; Trans: TVisitationTranslator);
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
