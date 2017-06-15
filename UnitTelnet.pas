unit UnitTelnet;

interface

uses
  System.Classes, System.SysUtils, System.DateUtils, System.StrUtils,
  VCL.Graphics,
  WinAPI.Messages,
  IdGlobal, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient,
  IdTelnet;

const
  MY_MSG = WM_USER + 1;

type
  TelnetThread = class(TThread)
  private
    { Private declarations }
  protected
    procedure Execute; override;
    procedure IdTelnet1DataAvailable(Sender: TIdTelnet; const Buffer: TIdBytes);
    procedure IdTelnet1Connected(Sender: TObject);
    procedure IdTelnet1Disconnected(Sender: TObject);
    procedure WriteToLog(instring: string);
  public
    host: string;
    port: Word;
    UID: string;
    connected: boolean;
    logging: boolean;
    SecondsTo: Int64;
  end;

type
  EIdException = class(Exception);

implementation

var
  IdTelnet1: TIdTelnet;
  LastConnectAttempt: TDateTime;
  LastDataSend: TDateTime;

  WasRXEDok: boolean = false;
  WasRXEDerr: boolean = false;

procedure TelnetThread.WriteToLog(instring: string);
var
  tmpStr: string;
  LogFile: TextFile;
begin
  if not logging then
    Exit;

  try
    if not DirectoryExists(extractfilepath(paramstr(0)) + 'telnet_logs') then
      CreateDir(extractfilepath(paramstr(0)) + 'telnet_logs');

    tmpStr := extractfilepath(paramstr(0)) + 'telnet_logs\telnet_' +
      FormatDateTime('yyyy-mm-dd', Now()) + '.log';
    AssignFile(LogFile, tmpStr);
    if FileExists(tmpStr) then
    begin
      Append(LogFile);
    end
    else
    begin
      ReWrite(LogFile);
    end;
    Writeln(LogFile, FormatDateTime('hh:nn:ss', Now()) + ' ' + instring);
    Flush(LogFile);
    CloseFile(LogFile);
  finally

  end;
end;

procedure TelnetThread.Execute;
var
  tmpStr: string;
  PrevStatus: boolean;
begin
  IdTelnet1 := TIdTelnet.Create;
  IdTelnet1.host := host;
  IdTelnet1.port := port;
  IdTelnet1.OnDataAvailable := IdTelnet1DataAvailable;
  IdTelnet1.OnConnected := IdTelnet1Connected;
  IdTelnet1.OnDisconnected := IdTelnet1Disconnected;

  connected := false;

  WriteToLog('Telnet started');

  try
    WriteToLog('Connection attempt');
    IdTelnet1.Connect;
    tmpStr := '!#CUE:' + UID + ':' + inttostr(SecondsTo) + '#!';

    IdTelnet1.IOHandler.WriteLn(tmpStr);
    WriteToLog('>> ' + tmpStr);

    Sleep(100);
    IdTelnet1.Disconnect;
  except
    on E: Exception do
    begin
      WriteToLog(E.ClassName + ' error raised, with message : ' + E.Message);
    end;
  end;
  WriteToLog('Telnet stopped');
end;

procedure TelnetThread.IdTelnet1DataAvailable(Sender: TIdTelnet;
  const Buffer: TIdBytes);
var
  tmpStr: string;
begin
  try
    tmpStr := BytesToString(Buffer);
    if logging then
      WriteToLog('<< ' + tmpStr);
    if pos('result="ERR"', tmpStr) > 0 then
    begin
      WasRXEDerr := true;
    end;
    if pos('result="OK"', tmpStr) > 0 then
    begin
      WasRXEDok := true;
    end;
  except
    on E: Exception do
    begin
      WriteToLog('Exception in Data');
    end;
  end;
end;

procedure TelnetThread.IdTelnet1Connected(Sender: TObject);
begin
  connected := true;
  WriteToLog('Orad connected');
end;

procedure TelnetThread.IdTelnet1Disconnected(Sender: TObject);
begin
  connected := false;
  WriteToLog('Orad disconnected');
end;

end.
