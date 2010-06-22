unit umfmCommandConsolePervert;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TnTForms, ImgList, XPStyleActnCtrls, ActnList, ActnMan, StdCtrls,
  Buttons, ComCtrls, TntStdCtrls, TntComCtrls, AppEvnts, xmldom,
  XMLIntf, XMLDoc, oxmldom, TntClasses, TntWideStrings;

type
  TmfmCommandConsolePervert = class( TTnTForm)
    memoConsoleBox: TTntRichEdit;
    lblIntro: TTntStaticText;
    btnProceed: TBitBtn;
    btnFinish: TBitBtn;
    actmngrMainActions: TActionManager;
    imglstActionGlyphs_16x16: TImageList;
    actProceed: TAction;
    actFinish: TAction;
    appevMainEvents: TApplicationEvents;
    xmlDoc: TXMLDocument;
    procedure TntFormDestroy(Sender: TObject);
    procedure TntFormCreate(Sender: TObject);
    procedure appevMainEventsIdle(Sender: TObject; var Done: Boolean);
    procedure actProceedExecute( Sender: TObject);
    procedure actProceedUpdate( Sender: TObject);
    procedure actFinishExecute( Sender: TObject);
    procedure actFinishUpdate( Sender: TObject);

  private
    procedure ClearLog;
    procedure LogColour( FontColour: TColor; const Line: widestring);
    procedure LogRed( const Line: widestring);
    procedure LogGreen( const Line: widestring);
    procedure DoCommand( const Com: widestring);
    procedure RunCommand_AcceptPervertStringA( const OutLine: string);
    procedure RunCommand_AcceptPervertStringW( const OutLine: widestring);
    procedure Breathe;

  public
    FCommands: TWideStrings;
    FStarted, FFinished: boolean;
  end;

var
  mfmCommandConsolePervert: TmfmCommandConsolePervert;

implementation























uses TntSystem, TntSysUtils, Registry, StrUtils;

{$R *.dfm}





type
  TAcceptPervertStringA = procedure( const Value: string) of object;
  TAcceptPervertStringW = procedure( const Value: widestring) of object;

var
  isUnicodeCommand: boolean = False;

procedure RunCommandA( const Com: string; Output1: TAcceptPervertStringA);
  const
     BufferLen = 2400;
  var
   Security    : TSecurityAttributes;
   ReadPipe, WritePipe : THandle;
   Start       : TStartUpInfoA;
   ProcessInfo : TProcessInformation;
   Buffer      : string;
   BytesRead   : DWord;
   Apprunning  : DWord;
   isMore      : boolean;
   PartialLine : string;
   P: integer;

begin
with Security do
  begin
  nlength              := SizeOf( Security);
  binherithandle       := True;
  lpsecuritydescriptor := nil
  end;
if Createpipe( ReadPipe, WritePipe, @Security, 0) then
    try
      ZeroMemory( @Start, SizeOf( Start));
      Start.cb := SizeOf( Start) ;
      Start.hStdOutput := WritePipe;
      Start.hStdInput  := ReadPipe;
      Start.dwFlags := STARTF_USESTDHANDLES + STARTF_USESHOWWINDOW;
      Start.wShowWindow := SW_HIDE;
      if CreateProcessA( nil, PAnsiChar( Com),
          @Security, @Security, True, NORMAL_PRIORITY_CLASS,
          nil, nil, Start, ProcessInfo) then
          try
            CloseHandle( ProcessInfo.hThread ); ProcessInfo.hThread := 0;
            CloseHandle( WritePipe);            WritePipe := 0;
            Sleep( 500); // Why is this necessary?
            // If the sleep is not done, ReadFile will hang.
            PartialLine := '';
            repeat
              SetLength( Buffer, BufferLen);
              ZeroMemory( @Buffer[1], BufferLen * SizeOf( Char));
              isMore := ReadFile( ReadPipe, (@Buffer[1])^, BufferLen, BytesRead, nil);
              SetLength( Buffer, BytesRead);
              PartialLine := PartialLine + Buffer;
              repeat
                Application.ProcessMessages;
                P := Pos( #13#10, PartialLine);
                if P <> 0 then
                  begin
                  Buffer := PartialLine;
                  SetLength( Buffer, P - 1);
                  Output1( Buffer);
                  Delete( PartialLine, 1, P + 1)
                  end
              until P = 0
            until (not isMore);
            if PartialLine <> '' then
              Output1( PartialLine)
          finally
            while WaitForSingleObject( ProcessInfo.hProcess, 100) = WAIT_TIMEOUT do
              Application.ProcessMessages;
            if ProcessInfo.hProcess <> 0 then
              CloseHandle( ProcessInfo.hProcess);
            if ProcessInfo.hThread <> 0 then
              CloseHandle( ProcessInfo.hThread )
            end
        else
          Output1( Format( 'Error: Could not create process for "%s".', [Com]))
      finally
        if WritePipe <> 0 then
          CloseHandle( WritePipe);
        if ReadPipe <> 0 then
          CloseHandle( ReadPipe);
        end
  else
    Output1( 'Error: Could not create i/o pipes.')
end;



procedure RunCommandW( const Com: widestring; Output1: TAcceptPervertStringW);
  const
     BufferLen = 2400;
  var
   Security    : TSecurityAttributes;
   ReadPipe, WritePipe : THandle;
   Start       : TStartUpInfoW;
   ProcessInfo : TProcessInformation;
   Buffer      : widestring;
   BytesRead   : DWord;
   Apprunning  : DWord;
   isMore      : boolean;
   PartialLine : widestring;
   P: integer;

begin
with Security do
  begin
  nlength              := SizeOf( Security);
  binherithandle       := True;
  lpsecuritydescriptor := nil
  end;
if Createpipe( ReadPipe, WritePipe, @Security, 0) then
    try
      ZeroMemory( @Start, SizeOf( Start));
      Start.cb := SizeOf( Start) ;
      Start.hStdOutput := WritePipe;
      Start.hStdInput  := ReadPipe;
      Start.dwFlags := STARTF_USESTDHANDLES + STARTF_USESHOWWINDOW;
      Start.wShowWindow := SW_HIDE;
      if CreateProcessW( nil, PWideChar( Com),
          @Security, @Security, True, NORMAL_PRIORITY_CLASS,
          nil, nil, Start, ProcessInfo) then
          try
            CloseHandle( ProcessInfo.hThread ); ProcessInfo.hThread := 0;
            CloseHandle( WritePipe);            WritePipe := 0;
            PartialLine := '';
            repeat
              SetLength( Buffer, BufferLen);
              ZeroMemory( @Buffer[1], BufferLen * SizeOf( Char));
              isMore := ReadFile( ReadPipe, (@Buffer[1])^, BufferLen, BytesRead, nil);
              SetLength( Buffer, BytesRead div SizeOf( widechar));
              PartialLine := PartialLine + Buffer;
              repeat
                Application.ProcessMessages;
                P := Pos( #13#10, PartialLine);
                if P <> 0 then
                  begin
                  Buffer := PartialLine;
                  SetLength( Buffer, P - 1);
                  Output1( Buffer);
                  Delete( PartialLine, 1, P + 1)
                  end
              until P = 0
            until (not isMore)
          finally
            while WaitForSingleObject( ProcessInfo.hProcess, 100) = WAIT_TIMEOUT do
              Application.ProcessMessages;
            if ProcessInfo.hProcess <> 0 then
              CloseHandle( ProcessInfo.hProcess);
            if ProcessInfo.hThread <> 0 then
              CloseHandle( ProcessInfo.hThread );
            end
        else
          Output1( Format( 'Error: Could not create process for "%s".', [Com]))
      finally
        if WritePipe <> 0 then
          CloseHandle( WritePipe);
        if ReadPipe <> 0 then
          CloseHandle( ReadPipe);
        end
  else
    Output1( 'Error: Could not create i/o pipes.')
end;


procedure TmfmCommandConsolePervert.actFinishUpdate( Sender: TObject);
begin
(Sender as TAction).Enabled := FFinished
end;





procedure TmfmCommandConsolePervert.actFinishExecute( Sender: TObject);
begin
Close
end;





procedure TmfmCommandConsolePervert.actProceedUpdate( Sender: TObject);
begin
(Sender as TAction).Enabled := not FStarted
end;





procedure TmfmCommandConsolePervert.actProceedExecute( Sender: TObject);
var
  j: integer;
  SaveCursor: TCursor;
  dtStart: TDateTime;
begin
SaveCursor := Screen.Cursor;
Cursor := crHourGlass;
FStarted := True;
actProceedUpdate( Sender);
dtStart := Now;
try
  for j := 0 to FCommands.Count - 1 do
    begin
    Breathe;
    DoCommand( FCommands[ j])
    end;
  FFinished := True
finally
  Screen.Cursor := SaveCursor
  end;
LogRed( Format( 'Execution took %2.2f seconds', [(Now - dtStart) * SecsPerDay]))
end;





procedure TmfmCommandConsolePervert.ClearLog;
begin
memoConsoleBox.Clear
end;




procedure TmfmCommandConsolePervert.LogRed( const Line: widestring);
begin
LogColour( clRed, Line)
end;




procedure TmfmCommandConsolePervert.LogGreen( const Line: widestring);
begin
LogColour( clGreen, Line)
end;





procedure TmfmCommandConsolePervert.LogColour(
  FontColour: TColor; const Line: widestring);
begin
memoConsoleBox.SelAttributes.Color := FontColour;
memoConsoleBox.Lines.Add( Line)
end;





function RemoveBackSlash( const DirValue: string): string;
var
  L: integer;
begin
result := DirValue;
L := Length( result);
if (L > 0) and (result[L] = '\') then
  SetLength( result, L - 1)
end;




procedure TmfmCommandConsolePervert.appevMainEventsIdle(
  Sender: TObject; var Done: Boolean);
var
  FN: widestring;
  s: widestring;
  j: integer;
  Node: IXMLNode;
  Reg: TRegistry;
  DelphiRootDir: string;
begin
appevMainEvents.OnIdle := nil;
ClearLog;
FN := TntSystem.WideParamStr(1);
if FN = '' then
  FN := WideChangeFileExt( Application.ExeName, '.xml');
xmlDoc.LoadFromFile( FN);
s := xmlDoc.DocumentElement.ChildValues['Intro'];
lblIntro.Caption := s;
Node := xmlDoc.DocumentElement.ChildNodes.First;
while assigned( Node) do
  begin
  if Node.NodeName = 'delphi' then
    begin
    s := RemoveBackSlash( Node.GetAttributeNS( 'root', ''));
    s := '\Software\Borland\' + s + '\RootDir';
    Reg := TRegistry.Create;
    try
      Reg.RootKey := HKEY_CURRENT_USER;
      if Reg.OpenKeyReadOnly( s) then
        DelphiRootDir := RemoveBackSlash( Reg.ReadString( 'RootDir'))
    finally
      Reg.Free
      end
    end;
  // </delphi root="Delphi\7.0">
  if Node.NodeName = 'command' then
    begin
    s := Node.GetAttributeNS( 'line', '');
    if DelphiRootDir <> '' then
      s := AnsiReplaceStr( s, '$(DELPHI)', DelphiRootDir);
    FCommands.Add( s)
    end;
  Node := xmlDoc.DocumentElement.ChildNodes.FindSibling( Node, 1)
  end;
if FCommands.Count = 0 then
  begin
  FStarted := True;
  FFinished := True;
  LogRed( 'No commands found')
  end
end;





procedure TmfmCommandConsolePervert.TntFormCreate( Sender: TObject);
begin
FCommands := TTntStringList.Create
end;





procedure TmfmCommandConsolePervert.TntFormDestroy( Sender: TObject);
begin
FCommands.Free
end;




procedure TmfmCommandConsolePervert.DoCommand( const Com: widestring);
var
  Outpt: TWideStrings;
  j: integer;
begin
LogRed( Com);
try
  if isUnicodeCommand then
      RunCommandW( Com, RunCommand_AcceptPervertStringW)
    else
      RunCommandA( Com, RunCommand_AcceptPervertStringA);
  except on E:Exception do
    LogGreen( Format( 'Error: %s', [E.Message]))
    end
end;





procedure TmfmCommandConsolePervert.RunCommand_AcceptPervertStringA(
  const OutLine: string);
begin
LogGreen( OutLine) ;
end;





procedure TmfmCommandConsolePervert.RunCommand_AcceptPervertStringW(
  const OutLine: widestring);
begin
LogGreen( OutLine)
end;

procedure TmfmCommandConsolePervert.Breathe;
begin
Application.ProcessMessages
end;

end.
