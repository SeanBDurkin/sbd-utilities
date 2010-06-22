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

unit uSBD_PictureUtils;

interface uses Graphics, Classes;

procedure LoadPictureFromFile        (     Pic: TPicture; const FileName: widestring);
procedure LoadGraphicFromFile        ( var Pic: TGraphic; const FileName: widestring);
  // The above uses Pic.LoadFromFile not LoadGraphicFromStream.
procedure SavePictureToFile          (     Pic: TPicture; const FileName: widestring);
procedure SaveGraphicToFile          (     Pic: TGraphic; const FileName: widestring);

// Robust versions check for file existance and use Load/SaveGraphicFromStream
procedure RobustLoadGraphicFromFile  ( var Pic: TGraphic; const FileName: widestring; isPolymorphic: boolean);
procedure RobustSaveGraphicToFile    (     Pic: TGraphic; const FileName: widestring; isPolymorphic: boolean);

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












{$I 'SBDCompilers.inc'}
{$I 'SBDConfig.inc'}


uses JPeg, SysUtils, uSBD_RecallIntf
{$ifdef TntSupport}     
  , TntGraphics, TntClasses, TntSysUtils
{$endif}
;

{$WARNINGS OFF}



procedure WideLoadPictureFromFile( Pic: TPicture; const Filename: widestring);
begin
{$ifdef TntSupport}
if Pic is TTntPicture then
    TTntPicture( Pic).LoadFromFile( Filename)
  else
{$endif}
    Pic.LoadFromFile( FileName)
end;


procedure WideSavePictureToFile( Pic: TPicture; const Filename: widestring);
begin
{$ifdef TntSupport}
if Pic is TTntPicture then
    TTntPicture( Pic).SaveToFile( Filename)
  else
{$endif}
    Pic.SaveToFile( FileName)
end;


procedure LoadPictureFromFile  ( Pic: TPicture; const FileName: widestring);
begin
if FileName = '' then
    ClearPicture( Pic)

  else if assigned( Pic) then
    WideLoadPictureFromFile( Pic, FileName)
end;



procedure LoadGraphicFromFile  ( var Pic: TGraphic; const FileName: widestring);
var
  Stream: TStream;
begin
if FileName = '' then
    ClearGraphic( Pic)

  else if assigned( Pic) then
    begin
{$ifdef TntSupport}
    Stream := TTnTFileStream.Create( Filename, fmOpenRead or fmShareDenyWrite);
    try
      Pic.LoadFromStream( Stream);
    finally
      Stream.Free
      end
{$else}
    Pic.LoadFromFile( FileName)
{$endif}
    end
end;




procedure SavePictureToFile    ( Pic: TPicture; const FileName: widestring);
begin
if assigned( Pic) and (FileName <> '') then
  WideSavePictureToFile( Pic, FileName)
end;




procedure SaveGraphicToFile( Pic: TGraphic; const FileName: widestring);
{$ifdef TntSupport}
const
  Modes: array[ boolean] of word = ( fmCreate, fmOpenWrite);
var
  FileMode: word;
  Stream: TStream;
{$endif}
begin
if assigned( Pic) and (FileName <> '') then
    begin
{$ifdef TntSupport}
    FileMode := Modes[ WideFileExists( FileName)];
    Stream := TTnTFileStream.Create( Filename, FileMode);
    try
      if FileMode = fmOpenWrite then
        Stream.Size := 0;
      Pic.SaveToStream( Stream);
    finally
      Stream.Free
      end
{$else}
    Pic.SaveToFile( FileName)
{$endif}
    end
end;














procedure RobustSaveGraphicToFile( Pic: TGraphic; const FileName: widestring; isPolymorphic: boolean);
// Robust version uses SaveGraphicToStream
var
  Stream: TStream;
begin
if isEmptyGraphic( Pic) or (FileName = '') then exit;
{$ifdef TntSupport}
  Stream := TTntFileStream.Create( Filename, fmCreate);
{$else}
  Stream := TFileStream.Create( Filename, fmCreate);
{$endif}
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
  Source.ReadBuffer( result[1], L)
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
procedure RobustLoadGraphicFromFile  ( var Pic: TGraphic; const FileName: widestring; isPolymorphic: boolean);
var
  Stream: TStream;
begin
{$ifdef TntSupport}
  if not WideFileExists( FileName) then
    begin
    ClearGraphic( Pic);
    exit
    end;
  Stream := TTntFileStream.Create( Filename, fmOpenRead or fmShareDenyWrite);
{$else}
  if not FileExists( FileName) then
    begin
    ClearGraphic( Pic);
    exit
    end;
  Stream := TFileStream.Create( Filename, fmOpenRead or fmShareDenyWrite);
{$endif}
try
  LoadGraphicFromStream( Pic, Stream, isPolymorphic)
finally
  Stream.Free;
end end;



Procedure WriteClass( Destin: TStream; Graphic: TGraphic);
var
  CName: string[63];
begin
CName := Graphic.ClassName;
Destin.WriteBuffer( CName[0], 1);
Destin.WriteBuffer( CName[1], Integer(CName[0]))
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
