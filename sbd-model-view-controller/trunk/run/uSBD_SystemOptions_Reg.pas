{*****************************************************************************}
{                                                                             }
{    SBD Model-View-Controller Framework                                      }
{        Version: 1.0.0.X                                                     }
{                                                                             }
{    Copyright (c) 2010, Sean B. Durkin (sean@seanbdurkin.id.au)         }
{                                                                             }
{*****************************************************************************}

{* ***** BEGIN LICENSE BLOCK *****
This file is part of SBD Low Level Utils.
SBD Model-View-Controller is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

SBD Model-View-Controller is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.

You should have received a copy of the Lesser GNU General Public License
along with SBD Model-View-Controller.  If not, see <http://www.gnu.org/licenses/>.

 * ***** END LICENSE BLOCK ***** *}

unit uSBD_SystemOptions_Reg;
interface
uses TypInfo, Classes, uSBD_AppComponents, Registry, Contnrs;

type
TOption = class
  public
    FProgId: string;
    FKey, FValueName: string;
    FOptionType: TOptionType;
    FEnumTypeInfo: PTypeInfo;
    FisUser: boolean;
    FDefaultString: string;
    FDefaultInteger: integer;
    FDefaultBoolean: boolean;
    FDefaultFloat: double;
    FDefaultBinary: TStream;

    constructor Create( const BaseKey: string;
                        const ProgId: string;
                        const StorageName: string;
                        OptionType: TOptionType;
                        EnumTypeInfo: PTypeInfo;
                        isUser: boolean);
    destructor Destroy; override;
  end;




TSystemOptions_Reg = class( TInterfacedPersistent, ISystemOptions, ISystemOptions_Reg)
  private
    FOptions: TObjectList;  // of TOption
    FReg: TRegistry;
    FCurrentKey: string;
    FisCurrentKeyWritable: boolean;
    FBaseKey: string;
    FisUserRoot: boolean;
    FUpdateObject: TObject;
    FCryptoKey: string;
    FAppInfo: TAppInfo;
//    FCrypto: TDCP_cipher; // TDCP_blowfish;

    procedure DefineOption( const ProgId: string;
                            const StorageName: string;
                            OptionType: TOptionType;
                            EnumTypeInfo: PTypeInfo;
                            isUser: boolean);

    procedure UndefineOption( const ProgId: string);
    function  isUserOption( const ProgId: string): boolean;
    function  isValueDefined( const ProgId: string): boolean;

    function  GetStringOption ( const ProgId: string): string;
    function  GetIntegerOption( const ProgId: string): integer;
    function  GetBooleanOption( const ProgId: string): boolean;
    function  GetFloatOption  ( const ProgId: string): double;
    function  GetBinaryOption ( const ProgId: string): TStream; // ownership transferred to client.

    function  BeginUpdate: IInterface;

    procedure PutStringOption ( const ProgId: string; const Value :string);
    procedure PutIntegerOption( const ProgId: string;       Value :integer);
    procedure PutBooleanOption( const ProgId: string;       Value :boolean);
    procedure PutFloatOption  ( const ProgId: string;       Value :double);
    function  TestPutFloatOption( const ProgId: string): TTestPutResult;
    function  PutBinaryOption ( const ProgId: string): TStream; // ownership transferred to client.
    procedure SetBaseKey( const Key: string);
    procedure EndUpdate;

    function  GetOptionById( const ProgId: string): TOption;
    procedure SetOptionById( const ProgId: string; Value: TOption);

    procedure SetCryptoString( const CryptoKey1: string);
    function  OpenKey( const ProgId: string; NeedWrite: boolean; var W: TOption; var Ok: boolean): IInterface;
    function  isNotWritePermitted( const ProgId: string): boolean;
    function  CheckOpenWritableKey( const ProgId: string; var W: TOption): IInterface;
    procedure CloseKey;

    procedure PutDefaultStringOption ( const ProgId: string; const Value :string);
    procedure PutDefaultIntegerOption( const ProgId: string;       Value :integer);
    procedure PutDefaultBooleanOption( const ProgId: string;       Value :boolean);
    procedure PutDefaultFloatOption  ( const ProgId: string;       Value :double);
    function  PutDefaultBinaryOption ( const ProgId: string): TStream;   // ownership retains with ISystemOptions.

    procedure StartUp( PutDefaultsWhereUndefined: boolean);
    function  Encrypt( const Plaintext: string): ansistring;
    function  Decrypt( const Ciphertext: ansistring): string;

  public
    constructor Create( const CryptoKey1: string; AppInfo1: TAppInfo);
    destructor  Destroy; override;


    property  Option[ const ProgId: string]: TOption
       read GetOptionById write SetOptionById; default;
  end;



implementation








uses SysUtils, StrUtils, windows;






function RemoveEndingBackSlash( const Value: string): string;
var
  L: integer;
begin
result := Value;
L := Length( result);
if (L > 0) and (result[ L] = '\') then
  SetLength( result, L - 1)
end;


function IncludeEndingBackSlash( const Value: string): string;
var
  L: integer;
begin
result := Value;
L := Length( result);
if (L = 0) or (result[ L] <> '\') then
  result := result + '\'
end;


function RemoveBeginningBackSlash( const Value: string): string;
var
  L: integer;
begin
result := Value;
L := Length( result);
if (L > 0) and (result[ 1] = '\') then
  Delete( result, 1, 1)
end;


function IncludeBeginningBackSlash( const Value: string): string;
var
  L: integer;
begin
result := Value;
L := Length( result);
if (L = 0) or (result[ 1] <> '\') then
  result := '\' + result
end;


{ TSystemOptions_Reg }

function TSystemOptions_Reg.CheckOpenWritableKey(
  const ProgId: string; var W: TOption): IInterface;
var
  Ok: boolean;
begin
result := nil;
result := OpenKey( ProgId, True, W, Ok);
if not Ok then
  begin
  result := nil;
  raise Exception.Create( 'Registry write failed.')
  end
end;




procedure TSystemOptions_Reg.CloseKey;
begin
FReg.CloseKey;
FCurrentKey := ''
// The reg key object can be re-used as long as the security aspects
//    (read-only vrs. read/write; user root vrs. machine root) are
//     not changed.
end;


constructor TSystemOptions_Reg.Create(
  const CryptoKey1: string; AppInfo1: TAppInfo);
begin
FOptions := TObjectList.Create( True);
FReg     := TRegistry.Create;
FAppInfo := AppInfo1; // not owned.
FBaseKey := IncludeBeginningBackSlash( RemoveEndingBackSlash( FAppInfo.FBaseKey));
FUpdateObject := nil;
FCryptoKey := CryptoKey1;
FCurrentKey := '';
FisCurrentKeyWritable := False;
//FCrypto := TDCP_blowfish.Create( nil);
//FCrypto.InitStr( FCryptoKey, TDCP_md4)
end;



destructor TSystemOptions_Reg.Destroy;
begin
//FCrypto.Free;
FOptions.Free;
FReg.Free;
FUpdateObject := nil; // detach it.
inherited
end;

type
TUpdateObject = class( TInterfacedObject, IInterface)
  private
    FOwner: TSystemOptions_Reg;
    constructor Create( Owner1: TSystemOptions_Reg);
  public
    destructor  Destroy; override;
  end;


function TSystemOptions_Reg.BeginUpdate: IInterface;
begin
result := nil;
if Supports( FUpdateObject, IInterface, result) then exit;
TUpdateObject.Create( self);
Supports( FUpdateObject, IInterface, result)
end;


constructor TUpdateObject.Create( Owner1: TSystemOptions_Reg);
begin
FOwner := Owner1;
if assigned( FOwner) and (not assigned( FOwner.FUpdateObject)) then
  FOwner.FUpdateObject := self
end;

destructor TUpdateObject.Destroy;
begin
if assigned( FOwner) and (FOwner.FUpdateObject = self) then
  begin
  FOwner.EndUpdate;
  FOwner.FUpdateObject := nil
  end;
inherited
end;


function SaltLength: integer;
begin
result := 6
end;


function SaltA: ansistring;
var
  j: integer;
begin
SetLength( result, SaltLength);
for j := 1 to SaltLength do
  result[j] := AnsiChar( Random( 254) + 1)
end;

{$IFDEF UNICODE}
function SaltW: unicodestring;
{$ELSE}
function SaltW: widestring;
{$ENDIF}
var
  j: integer;
begin
SetLength( result, SaltLength);
for j := 1 to SaltLength do
  begin
  WordRec( result[j]).Lo := Random( 254) + 1;
  WordRec( result[j]).Hi := Random( 254) + 1
  end
end;



function TSystemOptions_Reg.Encrypt( const Plaintext: string): ansistring;
begin
{$IFDEF UNICODE}
if assigned( EncryptFunc) and (FBaseKey <> '') then
    result := EncryptFunc( FBaseKey, SaltW + Plaintext)
  else
    result := UTF8Encode( Plaintext)
{$ELSE}
if assigned( EncryptFunc) and (FBaseKey <> '') then
    result := EncryptFunc( FBaseKey, SaltA + Plaintext)
  else
    result := Plaintext
{$ENDIF}
//result := FCrypto.EncryptString( Salt + PlainText);
//FCrypto.Reset
end;





procedure TSystemOptions_Reg.EndUpdate;
begin
if FCurrentKey <> '' then
  CloseKey
end;


function TSystemOptions_Reg.Decrypt( const Ciphertext: ansistring): string;
begin
//result := FCrypto.DecryptString( Ciphertext);
{$IFDEF UNICODE}
if assigned( DecryptFunc) and (FBaseKey <> '') then
    result := DecryptFunc( FBaseKey, Ciphertext)
  else
    result := UTF8ToString( Ciphertext);
{$ELSE}
if assigned( DecryptFunc) and (FBaseKey <> '') then
    result := DecryptFunc( FBaseKey, Ciphertext)
  else
    result := Ciphertext;
{$ENDIF}
if SaltLength > 0 then
  Delete( result, 1, SaltLength);
//FCrypto.Reset
end;





procedure TSystemOptions_Reg.DefineOption(
  const ProgId, StorageName: string;
  OptionType: TOptionType; EnumTypeInfo: PTypeInfo; isUser: boolean);
var
  W: TOption;
begin
W := TOption.Create( FBaseKey, ProgId, StorageName,
                     OptionType, EnumTypeInfo, isUser);
SetOptionById( ProgId, W)
end;


function TSystemOptions_Reg.GetBinaryOption( const ProgId: string): TStream;
var
  W: TOption;
  Ok: boolean;
  Sz: integer;
  Exists: boolean;
begin
OpenKey( ProgId, False, W, Ok);
result := TMemoryStream.Create;
Exists := Ok and FReg.ValueExists( W.FValueName);
if Exists then
    Sz := FReg.GetDataSize( W.FValueName)
 else if Ok and assigned( W.FDefaultBinary) then
    Sz := W.FDefaultBinary.Size
 else
    Sz := 0;
result.Size := Sz;
if Exists then
    FReg.ReadBinaryData( W.FValueName, TMemoryStream(result).Memory^, Sz)

 else if Ok and assigned( W.FDefaultBinary) then
    begin
    result.Position := 0;
    result.CopyFrom( W.FDefaultBinary, 0)
    end;

result.Position := 0
end;



function TSystemOptions_Reg.GetBooleanOption( const ProgId: string): boolean;
var
  W: TOption;
  Ok: boolean;
  sResult: string;
  FirstLetter: Char;
begin
OpenKey( ProgId, False, W, Ok);
if Ok and FReg.ValueExists( W.FValueName) then
    sResult := FReg.ReadString( W.FValueName)
 else
    sResult := '';
if sResult <> '' then
    FirstLetter := UpCase( sResult[1])
  else
    FirstLetter := ' ';
if (FirstLetter = 'F') or (FirstLetter = 'T') then
    result := FirstLetter = 'T'
  else
    result := W.FDefaultBoolean
end;



function TSystemOptions_Reg.GetFloatOption( const ProgId: string): double;
var
  W: TOption;
  Ok: boolean;
begin
OpenKey( ProgId, False, W, Ok);
if Ok and FReg.ValueExists( W.FValueName) then
    Result := FReg.ReadFloat( W.FValueName)
  else
    Result := W.FDefaultFloat
end;



function TSystemOptions_Reg.GetIntegerOption( const ProgId: string): integer;
var
  W: TOption;
  Ok: boolean;
begin
OpenKey( ProgId, False, W, Ok);
if Ok and FReg.ValueExists( W.FValueName) then
    begin
    if W.FOptionType = otInteger then
        result := FReg.ReadInteger( W.FValueName)
      else
        begin
        Assert( W.FOptionType = otEnum, 'Wrong option type.');
        result := GetEnumValue( W.FEnumTypeInfo,
                    FReg.ReadString( W.FValueName));
        if result = -1 then
          result := W.FDefaultInteger
        end
    end
  else
    result := W.FDefaultInteger
end;



function TSystemOptions_Reg.GetOptionById( const ProgId: string): TOption;
var
  j: integer;
  W: TOption;
begin
result := nil;
for j := 0 to FOptions.Count - 1 do
  begin
  W := FOptions[j] as TOption;
  if W.FProgId <> ProgId then continue;
  result := W;
  break
  end;
end;


{$IFDEF UNICODE}
function WidenCharacters( const Value: ansistring): string;
var
  L, j: integer;
  c: WideChar;
begin
L := Length( Value);
SetLength( result, L);
WordRec( c).Hi := 0;
for j := 1 to L do
  begin
  WordRec( c).Lo := Ord( Value[j]);
  result[ j] := c
  end
end;


function NarrowCharacters( const Value: string): ansistring;
var
  L, j: integer;
  c: WideChar;
begin
L := Length( Value);
SetLength( result, L);
for j := 1 to L do
  begin
  c := Value[ j];
  // Assert: WordRec( c).Hi = 0
  result[ j] := AnsiChar( WordRec( c).Lo)
  end
end;
{$ENDIF}


function TSystemOptions_Reg.GetStringOption( const ProgId: string): string;
var
  W: TOption;
  Ok: boolean;
begin
OpenKey( ProgId, False, W, Ok);
if Ok and FReg.ValueExists( W.FValueName) then
    begin
    result := FReg.ReadString( W.FValueName);
    if W.FOptionType = otEncrypted then
      {$IFDEF UNICODE}
      result := Decrypt( NarrowCharacters( result))
      {$ELSE}
      result := Decrypt( result) //
      {$ENDIF}
    end
  else
    result := W.FDefaultString
end;


function TSystemOptions_Reg.isUserOption( const ProgId: string): boolean;
var
  W: TOption;
begin
W := GetOptionById( ProgId);
result := assigned( W) and W.FisUser
end;


function TSystemOptions_Reg.isValueDefined( const ProgId: string): boolean;
var
  W: TOption;
  Value: TRegDataInfo;

begin
OpenKey( ProgId, False, W, result);
if not result then exit;
result := FReg.GetDataInfo( W.FValueName, Value);
if not result then exit;
case W.FOptionType of
  otString, otEncrypted:  result := Value.RegData in [rdUnknown, rdString, rdExpandString];
  otInteger:  result := Value.RegData = rdInteger;
  otBoolean, otEnum:
              begin
              result := (Value.RegData in [rdUnknown, rdString, rdExpandString]) and
                        (FReg.ReadString( W.FValueName) <> '')
              end;

  otFloat, otBinary:
              result := Value.RegData = rdBinary;
  end;
end;


function TSystemOptions_Reg.isNotWritePermitted( const ProgId: string): boolean;
begin
// Only administrator accounts can write to HKEY_LOCAL_MACHINE.
// But any one can write to thier own HKEY_CURRENT_USER.
result := (not isUserOption( ProgId)) and
          assigned( FAppInfo) and (not FAppInfo.FisAdmin)
end;




type
TOpenKeyObject = class( TInterfacedObject, IInterface)
  private
    FOwner: TSystemOptions_Reg;
    FOption: TOption;

    constructor Create( Owner1: TSystemOptions_Reg; NeedWrite: boolean; Option1: TOption; var Ok: boolean);
  public
    destructor Destroy; override;
  end;


function TSystemOptions_Reg.OpenKey(
  const ProgId: string; NeedWrite: boolean; var W: TOption; var Ok: boolean): IInterface;
begin
result := nil;
W := GetOptionById( ProgId);
if assigned( W) then
    result := TOpenKeyObject.Create( self, NeedWrite, W, Ok)
  else
    Ok := False
end;


constructor TOpenKeyObject.Create(
  Owner1: TSystemOptions_Reg; NeedWrite: boolean;
  Option1: TOption; var Ok: boolean);
var
  NeedRootKey: cardinal;
begin
FOwner  := Owner1;
FOption := Option1;
if (FOwner.FCurrentKey <> '') and
  (((not FOwner.FisCurrentKeyWritable) and NeedWrite) or
   (FOwner.FCurrentKey <> FOption.FKey) or
   (FOwner.FisUserRoot <> FOption.FisUser)) then
    // Wrong key, wrong mode or wrong root key.
    FOwner.CloseKey;
if FOwner.FCurrentKey = '' then
  begin
  FOwner.FisUserRoot := FOption.FisUser;
  if FOwner.FisUserRoot then
      NeedRootKey := HKEY_CURRENT_USER
    else
      NeedRootKey := HKEY_LOCAL_MACHINE;
  if (NeedWrite and (not FOwner.FisCurrentKeyWritable)) or
     (FOwner.FReg.RootKey <> NeedRootKey) then
    begin
    FOwner.FReg.Free;
    FOwner.FReg := TRegistry.Create;
    FOwner.FReg.RootKey := NeedRootKey
    end;
  try
    if NeedWrite then
        Ok := FOwner.FReg.OpenKey( FOption.FKey, True)
      else
        Ok := FOwner.FReg.OpenKeyReadOnly( FOption.FKey)
  except
    begin
    FOwner.CloseKey;
    Ok := False
    end end;
  if Ok then
    begin
    FOwner.FCurrentKey := FOption.FKey;
    FOwner.FisCurrentKeyWritable := NeedWrite
    end;
  end
  else
    Ok := FOwner.FCurrentKey = FOption.FKey
end;


destructor TOpenKeyObject.Destroy;
begin
if assigned( FOwner.FUpdateObject) then exit;
FOwner.CloseKey;
inherited
end;



type TPutBinaryStream = class( TMemoryStream)
  private
    FMaster: TSystemOptions_Reg;
    FOpt: TOption;
    FKeyLock: IInterface;

    constructor Create( const ProgId: string; Master1: TSystemOptions_Reg);

  public
    destructor Destroy; override;
  end;

constructor TPutBinaryStream.Create( const ProgId: string; Master1: TSystemOptions_Reg);
begin
inherited Create;
FMaster  := Master1;
FKeyLock := FMaster.CheckOpenWritableKey( ProgId, FOpt)
end;


function TSystemOptions_Reg.PutBinaryOption( const ProgId: string): TStream;
begin
if isNotWritePermitted( ProgId) then
    result := nil
  else
    result := TPutBinaryStream.Create( ProgId, self)
end;


destructor TPutBinaryStream.Destroy;
var
  W: TOption;
  Ok: boolean;
begin
try
FMaster.OpenKey( FOpt.FProgId, True, W, Ok);
if Ok then
  FMaster.FReg.WriteBinaryData( FOpt.FValueName, Memory^, Size)
finally
  inherited
end end;


procedure TSystemOptions_Reg.PutBooleanOption( const ProgId: string;
  Value: boolean);
var
  W: TOption;
begin
if isNotWritePermitted( ProgId) then exit;
CheckOpenWritableKey( ProgId, W);
FReg.WriteString( W.FValueName, IfThen( Value, 'True', 'False'))
end;



function TSystemOptions_Reg.PutDefaultBinaryOption(
  const ProgId: string): TStream;
var
  W: TOption;
begin
W := GetOptionById( ProgId);
Assert( assigned( W) and (W.FOptionType = otBinary), 'Wrong default type');
if assigned( W) then
    result := W.FDefaultBinary
  else
    result := nil
end;



procedure TSystemOptions_Reg.PutDefaultBooleanOption(
  const ProgId: string; Value: boolean);
var
  W: TOption;
begin
W := GetOptionById( ProgId);
Assert( assigned( W) and (W.FOptionType = otBoolean), 'Wrong default type');
if assigned( W) then
  W.FDefaultBoolean := Value
end;



procedure TSystemOptions_Reg.PutDefaultFloatOption(
  const ProgId: string; Value: double);
var
  W: TOption;
begin
W := GetOptionById( ProgId);
Assert( assigned( W) and (W.FOptionType = otFloat), 'Wrong default type');
if assigned( W) then
  W.FDefaultFloat := Value
end;


procedure TSystemOptions_Reg.PutDefaultIntegerOption(
  const ProgId: string; Value: integer);
var
  W: TOption;
begin
W := GetOptionById( ProgId);
Assert( assigned( W) and (W.FOptionType in [otInteger, otEnum]), 'Wrong default type');
if assigned( W) then
  W.FDefaultInteger := Value
end;



procedure TSystemOptions_Reg.PutDefaultStringOption(
  const ProgId: string; const Value: string);
var
  W: TOption;
begin
W := GetOptionById( ProgId);
Assert( assigned( W) and (W.FOptionType in [otString, otEncrypted]), 'Wrong default type');
if assigned( W) then
  W.FDefaultString := Value
end;


procedure TSystemOptions_Reg.PutFloatOption(
   const ProgId: string; Value: double);
var
  W: TOption;
begin
if isNotWritePermitted( ProgId) then exit;
CheckOpenWritableKey( ProgId, W);
FReg.WriteFloat( W.FValueName, Value)
end;



type
TRegistryHack = class(TObject)
  private
    FCurrentKey: HKEY;
  end;


function TSystemOptions_Reg.TestPutFloatOption(
  const ProgId: string): TTestPutResult;
var
  W: TOption;
  Value: double;
  ErrCode: integer;
begin
Value := -1.0;
if isNotWritePermitted( ProgId) then
    result := putRequiresAdmin
  else
    try
      CheckOpenWritableKey( ProgId, W);
      FReg.WriteFloat( W.FValueName, Value);
      result := putOk
    except
      try
        if assigned( W) and assigned( FReg) then
            ErrCode := RegSetValueEx( TRegistryHack( FReg).FCurrentKey,
                          PChar( W.FValueName), 0, 3, @Value, 8)
          else
            ErrCode := -1
      except
        ErrCode := -1;
        end;
      if ErrCode = 5 then
          result := putAccessDenied
        else
          result := putOtherError
    end
end;



procedure TSystemOptions_Reg.PutIntegerOption(
  const ProgId: string; Value: integer);
var
  W: TOption;
begin
if isNotWritePermitted( ProgId) then exit;
CheckOpenWritableKey( ProgId, W);
case W.FOptionType of
  otInteger: FReg.WriteInteger( W.FValueName, Value);
  otEnum:    FReg.WriteString(  W.FValueName, GetEnumName( W.FEnumTypeInfo, Value));
  else       Assert( False, 'Wrong option type.')
  end
end;


procedure TSystemOptions_Reg.PutStringOption(
  const ProgId: string; const Value: string);
var
  W: TOption;
  s: string;
begin
if isNotWritePermitted( ProgId) then exit;
CheckOpenWritableKey( ProgId, W);
if W.FOptionType = otEncrypted then
    {$IFDEF UNICODE}
    s := WidenCharacters( Encrypt( Value))
    {$ELSE}
    s := Encrypt( Value)
    {$ENDIF}
  else
    s := Value;
FReg.WriteString(  W.FValueName, s)
end;



procedure TSystemOptions_Reg.SetBaseKey( const Key: string);
var
  NewBaseKey: string;
begin
NewBaseKey := IncludeBeginningBackSlash( RemoveEndingBackSlash( Key));
if FBaseKey = NewBaseKey then exit;
if FOptions.Count > 0 then
  raise Exception.Create( 'Cannot change the base with after options have been defined.');
FBaseKey := NewBaseKey
end;


procedure TSystemOptions_Reg.SetCryptoString( const CryptoKey1: string);
begin
FCryptoKey := CryptoKey1;
//FCrypto.InitStr( FCryptoKey, TDCP_md4)
end;



procedure TSystemOptions_Reg.SetOptionById(
  const ProgId: string; Value: TOption);
begin
UndefineOption( ProgId);
FOptions.Add( Value)
end;



procedure TSystemOptions_Reg.StartUp( PutDefaultsWhereUndefined: boolean);
var
  j: integer;
  Opt: TOption;
  I: IInterface;
  Ok: boolean;
  Stream: TStream;
begin
Randomize;
BeginUpdate;
for j := 0 to FOptions.Count - 1 do
  begin
  Opt := FOptions[j] as TOption;
  I := TOpenKeyObject.Create( self, False, Opt, Ok);
  if Ok and FReg.ValueExists( Opt.FValueName) then
      begin
      // In future, possibly cache values here
      end
    else if PutDefaultsWhereUndefined and (not isNotWritePermitted( Opt.FProgId)) then
      begin
      // Write is permitted, but the value does not yet exist.
      // Write the default value in.
      case Opt.FOptionType of
        otString:    PutStringOption ( Opt.FProgId, Opt.FDefaultString);
{$IFDEF UNICODE}
        otEncrypted: PutStringOption ( Opt.FProgId, WidenCharacters( Encrypt( Opt.FDefaultString)));
{$ELSE}
        otEncrypted: PutStringOption ( Opt.FProgId, Encrypt( Opt.FDefaultString));
{$ENDIF}
        otInteger:   PutIntegerOption( Opt.FProgId, Opt.FDefaultInteger);
        otBoolean:   PutBooleanOption( Opt.FProgId, Opt.FDefaultBoolean);
        otFloat:     PutFloatOption  ( Opt.FProgId, Opt.FDefaultFloat);
        otBinary:    begin
                     Stream := PutBinaryOption( Opt.FProgId);
                     if assigned( Stream) and assigned( Opt.FDefaultBinary) then
                       Stream.CopyFrom( Opt.FDefaultBinary, 0);
                     Stream.Free
                     end;
        otEnum:      PutIntegerOption( Opt.FProgId, Opt.FDefaultInteger)
        end
      end;
  I := nil
  end
end;



procedure TSystemOptions_Reg.UndefineOption( const ProgId: string);
var
  W: TOption;
begin
W := GetOptionById( ProgId);
if assigned( W) then
  FOptions.Remove( W); // destroys W
end;



{ TOption }

{$IFDEF UNICODE}
function Pop( const Value: string; var LastElement: string): string;
var
  P: integer;
begin
result := ReverseString( value);
P := Pos( '\', result);
if P > 0 then
    begin
    LastElement := ReverseString( Copy( result, 1, P-1));
    Delete( result, 1, P)
    end
  else
    begin
    LastElement := ReverseString( result);
    result      := ''
    end;
result := ReverseString( result)
end;
{$ELSE}
function Pop( const Value: string; var LastElement: string): string;
var
  P: integer;
begin
result := AnsiReverseString( value);
P := AnsiPos( '\', result);
if P > 0 then
    begin
    LastElement := AnsiReverseString( Copy( result, 1, P-1));
    Delete( result, 1, P)
    end
  else
    begin
    LastElement := AnsiReverseString( result);
    result      := ''
    end;
result := AnsiReverseString( result)
end;
{$ENDIF}

constructor TOption.Create(
  const BaseKey, ProgId, StorageName: string;
  OptionType: TOptionType; EnumTypeInfo: PTypeInfo; isUser: boolean);
begin
FProgId := ProgId;
FKey := Pop( BaseKey + IncludeBeginningBackSlash( StorageName), FValueName);
if FValueName = '@' then
   FValueName := '';
FOptionType   := OptionType;
FEnumTypeInfo := EnumTypeInfo;
FisUser       := isUser;

FDefaultString  := '';
FDefaultInteger := 0;
FDefaultBoolean := False;
FDefaultFloat   := 0.0;
if FOptionType = otBinary then
    FDefaultBinary := TMemoryStream.Create
  else
    FDefaultBinary := nil;
if (FOptionType = otEnum) <> assigned( FEnumTypeInfo) then
  raise Exception.Create( 'Invalid parameters for TOption.Create')
end;



destructor TOption.Destroy;
begin
FDefaultBinary.Free;
inherited
end;

end.
