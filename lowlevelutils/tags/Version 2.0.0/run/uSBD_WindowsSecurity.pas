unit uSBD_WindowsSecurity;
interface

function IsAdmin: boolean;
// This function should only be called when you're sure you're running
// under Windows 2000/NT or later. Will not work on Win98/ME or earlier.



implementation





uses Windows;



function IsAdmin: Boolean;
// This is based on Borland's Community Article #26752.
{ -------------------------------------------------------------
  Returns a boolean indicating whether or not user has admin
  privileges. (Call only then running under NT.)

  ------------------------------------------------------------- }
var
  hAccessToken       : tHandle;
  ptgGroups          : pTokenGroups;
  dwInfoBufferSize   : DWORD;
  psidAdministrators : PSID;
  int                : integer;            // counter
  blnResult          : boolean;            // return flag

const
 SECURITY_NT_AUTHORITY: SID_IDENTIFIER_AUTHORITY =
    (Value: (0,0,0,0,0,5)); // ntifs
 SECURITY_BUILTIN_DOMAIN_RID: DWORD = $00000020;
 DOMAIN_ALIAS_RID_ADMINS: DWORD = $00000220;
 DOMAIN_ALIAS_RID_USERS : DWORD = $00000221;
 DOMAIN_ALIAS_RID_GUESTS: DWORD = $00000222;
 DOMAIN_ALIAS_RID_POWER_: DWORD = $00000223;

begin
result    := False;
//ptgGroups := nil;
blnResult := OpenThreadToken( GetCurrentThread, TOKEN_QUERY,
                              True, hAccessToken );
if (not blnResult) and (GetLastError = ERROR_NO_TOKEN) then
  blnResult := OpenProcessToken( GetCurrentProcess,
    				                     TOKEN_QUERY, hAccessToken);

if blnResult then
  begin
  GetMem( ptgGroups, 1024);
  try
    blnResult := GetTokenInformation( hAccessToken, TokenGroups,
                                      ptgGroups, 1024,
                                      dwInfoBufferSize );
    CloseHandle( hAccessToken );

    if blnResult then
      begin
      AllocateAndInitializeSid( SECURITY_NT_AUTHORITY, 2,
                                SECURITY_BUILTIN_DOMAIN_RID,
                                DOMAIN_ALIAS_RID_ADMINS,
        			0, 0, 0, 0, 0, 0,
        			psidAdministrators );
      {$R-}
      for int := 0 to ptgGroups.GroupCount - 1 do
        begin
        result := EqualSid( psidAdministrators, ptgGroups.Groups[ int ].Sid);
        if result then break
        end;
      {$R+}

      FreeSid( psidAdministrators)
      end

  finally
    FreeMem( ptgGroups)
    end
  end
end;

end.
