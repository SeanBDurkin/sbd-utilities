unit uCryptoUtils2;
// This is a utility unit belonging to the "SBD Libraries".
// Copyright (c) Sean B. Durkin, 2001-2002 .  All rights reserved.
// sdurkin@siliconrose.com.au

interface

const
  SR_Base = '\Software\SiliconRose\';

function MashAndHash (const PlainText: string): string;
function ApplicationKey (const KeyBase: string): string;
function ChicoryDesktopApplicationKey: string;
function Encrypt (const PlainText: string): string;
function Decrypt (const CipherText: string): string;
function KeyEncrypt (const Key, PlainText: string): string;
function KeyDecrypt (const Key, CipherText: string): string;

implementation







uses uDCPcrypt, DCPRipemd160, DCPTwofish, SysUtils, Forms;

const
  SystemKey = 'The TDCP_cipher component provides the basis of all of the '
            + 'encryption components in the DCPcrypt library.';
  SaltLength = 20;

function MashAndHash (const PlainText: string): string;
var
  Crypto: TDCP_ripemd160;
  Digest: array[0..31] of byte;
  j: Integer;
  sHex: string;

begin
Crypto := TDCP_ripemd160.Create(nil);
try
Crypto.Init;
Crypto.UpdateStr(PlainText);
Crypto.Final(Digest);
SetLength( result, ((Crypto.HashSize + 7) shr 3) * 2) // 20 bytes, 40 chars
finally
Crypto.free
end;
for j := 1 to Length(result) div 2 do
  begin
  sHex := IntToHex(Digest[j],2);
  Move(sHex[1],result[j*2-1],2)
  end
end;





function ApplicationKey (const KeyBase: string): string;
begin
result := KeyBase + MashAndHash( ExtractFilePath( lowercase( Application.EXEName)))
end;


function ChicoryDesktopApplicationKey: string;
const
  Base = SR_Base + 'Chicory\ChicoryDesktop\';

begin
result := ApplicationKey(Base)
end;


function KeyEncrypt (const Key, PlainText: string): string;
var
  crypto: TDCP_twofish;
  Salt: string;
  j: Integer;

begin
crypto := TDCP_twofish.Create(nil);
try
crypto.InitStr( Key, TDCP_ripemd160);
SetLength(Salt,SaltLength);
for j := 1 to SaltLength do
  Salt[j] := Char(Random($FF) + 1);
result := crypto.EncryptString(Salt + PlainText);
finally
crypto.free
end
end;


function KeyDecrypt (const Key, CipherText: string): string;
var
  crypto: TDCP_twofish;

begin
crypto := TDCP_twofish.Create(nil);
try
crypto.InitStr( Key, TDCP_ripemd160);
result := crypto.DecryptString(CipherText);
Delete(result,1,SaltLength)
finally
crypto.free
end
end;


function Encrypt (const PlainText: string): string;
begin
result := KeyEncrypt(SystemKey,PlainText)
end;



function Decrypt (const CipherText: string): string;
begin
result := KeyDecrypt(SystemKey,CipherText)
end;


//initialization
// Randomize is advised.
end.
