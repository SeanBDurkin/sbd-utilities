unit uLowLevelUtils_Component;
// This is a utility unit belonging to the "SBD Libraries".
// Copyright (c) Sean B. Durkin, 2003 .  All rights reserved.
// sdurkin@siliconrose.com.au

interface uses Classes;
const
  SourceVersion = '1.1';

type
// All the exported components of this package implement the following
//  method, which may be used to indicate the source code version number.
ILowLevelUtils_Component = interface
  ['{7CA59B3C-651F-47ED-BEF3-0FF1D26E67B8}']
  function AboutString: string;
  end;

TLowLevelUtils_Component = class( TComponent, ILowLevelUtils_Component)
  private
    function  AboutString: string;
    procedure Dummy( const NewAbout: string);

  published
    property About: string read AboutString write Dummy stored False;
  end;


implementation




{ TLowLevelUtils_Component }


function TLowLevelUtils_Component.AboutString: string;
begin
result := SourceVersion
end;

procedure TLowLevelUtils_Component.Dummy( const NewAbout: string);
begin
end;


end.
 