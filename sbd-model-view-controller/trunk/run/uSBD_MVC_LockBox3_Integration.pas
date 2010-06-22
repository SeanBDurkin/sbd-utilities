unit uSBD_MVC_LockBox3_Integration;
interface

procedure MVC_App__SetTheCryptoFunctionPointers;

implementation









uses uSBD_AppComponents, uTPLb_Codec, uTPLb_AES, uTPLb_CFB_8bit,
     uTPLb_StreamToBlock;


function EncryptFunc_1( const Key, Plaintext: string): ansistring;
var
  Codec: TSimpleCodec;
begin
Codec := TSimpleCodec.Create;
try
Codec.StreamCipher := TStreamToBlock_Adapter.Create;
Codec.BlockCipher  := TAES.Create( 128);
Codec.ChainMode    := TCFB_8Bit.Create;
Codec.Init( Key);
Codec.EncryptString( Plaintext, result);
finally
Codec.Free
end end;


function DecryptFunc_1( const Key: string; const Ciphertext: ansistring): string;
var
  Codec: TSimpleCodec;
begin
Codec := TSimpleCodec.Create;
try
Codec.StreamCipher := TStreamToBlock_Adapter.Create;
Codec.BlockCipher  := TAES.Create( 128);
Codec.ChainMode    := TCFB_8Bit.Create;
Codec.Init( Key);
Codec.DecryptString( result, Ciphertext);
finally
Codec.Free
end end;


{$IFDEF UNICODE}
function EncryptFunc_1A( const Key, Plaintext: ansistring): ansistring;
var
  Codec: TSimpleCodec;
begin
Codec := TSimpleCodec.Create;
try
Codec.StreamCipher := TStreamToBlock_Adapter.Create;
Codec.BlockCipher  := TAES.Create( 128);
Codec.ChainMode    := TCFB_8Bit.Create;
Codec.Init( Key);
Codec.EncryptAnsiString( Plaintext, result);
finally
Codec.Free
end end;


function DecryptFunc_1A( const Key, Ciphertext: ansistring): ansistring;
var
  Codec: TSimpleCodec;
begin
Codec := TSimpleCodec.Create;
try
Codec.StreamCipher := TStreamToBlock_Adapter.Create;
Codec.BlockCipher  := TAES.Create( 128);
Codec.ChainMode    := TCFB_8Bit.Create;
Codec.Init( Key);
Codec.DecryptAnsiString( result, Ciphertext);
finally
Codec.Free
end end;
{$ENDIF}



procedure MVC_App__SetTheCryptoFunctionPointers;
begin
EncryptFunc  := EncryptFunc_1;
DecryptFunc  := DecryptFunc_1;
{$IFDEF UNICODE}
EncryptFuncA := EncryptFunc_1A;
EncryptFuncW := EncryptFunc;
DecryptFuncA := DecryptFunc_1A;
DecryptFuncW := DecryptFunc
{$ELSE}
EncryptFuncA := EncryptFunc_1;
EncryptFuncW := nil;               // Not supported at this time.
DecryptFuncA := DecryptFunc_1;
DecryptFuncW := nil                // Not supported at this time.
{$ENDIF}
end;




end.
