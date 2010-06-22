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

unit uSBD_PictureUtils;

interface uses Graphics, Classes;

procedure LoadPictureFromFile        (     Pic: TPicture; const FileName: string);
procedure LoadGraphicFromFile        ( var Pic: TGraphic; const FileName: string);
  // The above uses Pic.LoadFromFile not LoadGraphicFromStream.
procedure SavePictureToFile          (     Pic: TPicture; const FileName: string);
procedure SaveGraphicToFile          (     Pic: TGraphic; const FileName: string);

// Robust versions check for file existance and use Load/SaveGraphicFromStream
procedure RobustLoadGraphicFromFile  ( var Pic: TGraphic; const FileName: string; isPolymorphic: boolean);
procedure RobustSaveGraphicToFile    (     Pic: TGraphic; const FileName: string; isPolymorphic: boolean);

procedure LoadPictureFromStream(     Pic: TPicture; Source: TStream; isPolymorphic: boolean);
  // if not isPolymorphic then assume picture is JPeg.
  // if isPolymorphic then only standard types or JPeg may be loaded.
procedure LoadGraphicFromStream( var Pic: TGraphic; Source: TStream; isPolymorphic: boolean);
procedure SavePictureToStream  (     Pic: TPicture; Destin: TStream; isPolymorphic: boolean);
procedure SaveGraphicToStream  (     Pic: TGraphic; Destin: TStream; isPolymorphic: boolean);

procedure ClearPicture  (     Pic: TPicture);
procedure ClearGraphic  ( var Pic: TGraphic);
function  isEmptyPicture(     Pic: TPicture): boolean;
function  isEmptyGraphic(     Pic: TGraphic): boolean;

function  ComparePicture( Subject, Reference: TPicture): integer;
function  CompareGraphic( Subject, Reference: TGraphic): integer;
function ReadShortStr( Source: TStream): string;

implementation














uses JPeg, SysUtils, uSBD_RecallIntf;




procedure WideSavePictureToFile( Pic: TPicture; const Filename: string);
begin
Pic.SaveToFile( FileName)
end;


procedure LoadPictureFromFile  ( Pic: TPicture; const FileName: string);
begin
if FileName = '' then
    ClearPicture( Pic)

  else if assigned( Pic) then
    Pic.LoadFromFile( FileName)
end;



procedure LoadGraphicFromFile  ( var Pic: TGraphic; const FileName: string);
var
  Stream: TStream;
begin
if FileName = '' then
    ClearGraphic( Pic)

  else if assigned( Pic) then
    Pic.LoadFromFile( FileName)
end;




procedure SavePictureToFile    ( Pic: TPicture; const FileName: string);
begin
if assigned( Pic) and (FileName <> '') then
  WideSavePictureToFile( Pic, FileName)
end;




procedure SaveGraphicToFile( Pic: TGraphic; const FileName: string);
begin
if assigned( Pic) and (FileName <> '') then
    Pic.SaveToFile( FileName)
end;














procedure RobustSaveGraphicToFile( Pic: TGraphic; const FileName: string; isPolymorphic: boolean);
// Robust version uses SaveGraphicToStream
var
  Stream: TStream;
begin
if isEmptyGraphic( Pic) or (FileName = '') then exit;
  Stream := TFileStream.Create( Filename, fmCreate);
try
  SaveGraphicToStream  ( Pic, Stream, isPolymorphic)
finally
  Stream.Free;
end end;


function ReadShortStr( Source: TStream): string;
var
  L: byte;
begin
Source.ReadBuffer( L, 1);
SetLength( result, L);
if L > 0 then
  Source.ReadBuffer( result[1], L * SizeOf( Char))
end;



function ReadGraphicClass( Source: TStream; isPolymorphic: boolean): TGraphicClass;
var
  CName: string;
begin
result := nil;
if isPolymorphic then
  begin
  CName := ReadShortStr( Source);
  // GClass := FileFormats.FindClassName( CName);  Cannot access FileFormats
  if CName = 'TMetafile'  then result := TMetafile;
  if CName = 'TIcon'      then result := TIcon;
  if CName = 'TBitmap'    then result := TBitmap
  end;
if not assigned( result) then
  result := TJPEGImage; // 'TJPEGImage'
end;


procedure LoadPictureFromStream( Pic: TPicture; Source: TStream; isPolymorphic: boolean);
var
  GClass: TGraphicClass;

begin
if not assigned( Pic) then exit;
if not assigned( Source) then
  begin
  ClearPicture( Pic);
  exit
  end;
GClass := ReadGraphicClass( Source, isPolymorphic);
if assigned( GClass) then
    begin
    Pic.Graphic := GClass.Create;
    Pic.Graphic.LoadFromStream( Source)
    end
  else
    ClearPicture( Pic)
end;


procedure LoadGraphicFromStream( var Pic: TGraphic; Source: TStream; isPolymorphic: boolean);
var
  GClass: TGraphicClass;

begin
if not assigned( Source) then
  begin
  ClearGraphic( Pic);
  exit
  end;
GClass := ReadGraphicClass( Source, isPolymorphic);
if assigned( GClass) then
    begin
    if assigned( Pic) and (Pic.ClassType <> GClass) then
      begin
      Pic.Free;
      Pic := GClass.Create
      end;
    Pic.LoadFromStream( Source)
    end
  else
    ClearGraphic( Pic)
end;




// Robust versions check for file existance and use Load/SaveGraphicFromStream
procedure RobustLoadGraphicFromFile  ( var Pic: TGraphic; const FileName: string; isPolymorphic: boolean);
var
  Stream: TStream;
begin
if not FileExists( FileName) then
    begin
    ClearGraphic( Pic);
    exit
    end;
Stream := TFileStream.Create( Filename, fmOpenRead or fmShareDenyWrite);
try
  LoadGraphicFromStream( Pic, Stream, isPolymorphic)
finally
  Stream.Free;
end end;



Procedure WriteClass( Destin: TStream; Graphic: TGraphic);
var
  CName: string;
  L: byte;
begin
if assigned( Graphic) then
    CName := Graphic.ClassName
  else
    CName := '';
L := Length( CName);
if L > 255 then
  L := 255;
Destin.WriteBuffer( L, 1);
Destin.WriteBuffer( CName[1], L * SizeOf( Char))
end;



procedure SavePictureToStream  ( Pic: TPicture; Destin: TStream; isPolymorphic: boolean);
begin
if isEmptyPicture( Pic) or not assigned( Destin) then exit;
if isPolymorphic then
  WriteClass( Destin, Pic.Graphic);
Pic.Graphic.SaveToStream( Destin)
end;




procedure SaveGraphicToStream  ( Pic: TGraphic; Destin: TStream; isPolymorphic: boolean);
begin
if isEmptyGraphic( Pic) or not assigned( Destin) then exit;
if isPolymorphic then
  WriteClass( Destin, Pic);
Pic.SaveToStream( Destin)
end;




procedure ClearPicture  ( Pic: TPicture);
begin
if assigned( Pic) then
  Pic.Graphic := nil
end;



procedure ClearGraphic  ( var Pic: TGraphic);
begin
FreeAndNil( Pic)
end;



function  isEmptyPicture( Pic: TPicture): boolean;
begin
result := (not assigned( Pic)) or isEmptyGraphic( Pic.Graphic)
end;

function  isEmptyGraphic( Pic: TGraphic): boolean;
begin
result := (not assigned( Pic)) or Pic.Empty or (Pic.Width = 0)
end;


function  CompareGraphic( Subject, Reference: TGraphic): integer;
var
  SubjStrm, RefStrm: TMemoryStream;
  Buff1, Buff2: integer;
  MiniBuff1, MiniBuff2: byte;
  j: integer;

begin
result := Ord( assigned( Subject)) - Ord( assigned( Reference));
if not ( assigned( Subject) and assigned( Reference)) then exit;
if Subject.ClassName = Reference.ClassName then
    result :=  0
  else if Subject.ClassName < Reference.ClassName then
    result := -1
  else
    result :=  1;
if result <> 0 then exit;
SubjStrm := TMemoryStream.Create;
TLifeRecall.Store( SubjStrm);
RefStrm := TMemoryStream.Create;
TLifeRecall.Store( RefStrm);
SaveGraphicToStream( Subject  , SubjStrm, False);
SaveGraphicToStream( Reference, RefStrm , False);
result := SubjStrm.Size - RefStrm.Size;
if (result <> 0) or (SubjStrm.Size = 0) or
    CompareMem( SubjStrm.Memory, RefStrm.Memory, SubjStrm.Size) then exit;
SubjStrm.Position := 0;
RefStrm .Position := 0;
for j := 0 to (SubjStrm.Size div 4) - 1 do
  begin
  SubjStrm.ReadBuffer( Buff1, 4);
  RefStrm .ReadBuffer( Buff2, 4);
  result := Buff1 - Buff2;
  if result <> 0 then exit
  end;
for j := 0 to (SubjStrm.Size mod 4) - 1 do
  begin
  SubjStrm.ReadBuffer( MiniBuff1, 1);
  RefStrm .ReadBuffer( MiniBuff2, 1);
  result := MiniBuff1 - MiniBuff2;
  if result <> 0 then break
  end
end;


function ComparePicture( Subject, Reference: TPicture): integer;
begin
result := CompareGraphic( Subject.Graphic, Reference.Graphic)
end;

end.
