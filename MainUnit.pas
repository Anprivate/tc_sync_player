unit MainUnit;

interface

uses
  System.SysUtils, System.Variants, System.Classes, System.IniFiles,
  System.Types, System.StrUtils, System.Win.ComObj,
  Winapi.Windows, Winapi.Messages, Winapi.MMsystem, Winapi.ShellAPI,
  Winapi.SHLobj, Winapi.ActiveX, Winapi.DirectShow9,
  Data.dsutil,
  VCL.Graphics, VCL.Controls, VCL.Forms, VCL.Dialogs, VCL.StdCtrls, VCL.Menus,
  VCL.ExtCtrls,
  GMFBridgeLib_TLB, DeckLink,
  reader_thread, UnitChannel, UnitClip, UnitTelnet, VCL.ComCtrls;

const
  WM_TC = WM_USER + 1;
  WM_BRIDGE = WM_USER + 2;

type
  TSdiParams = record
    UseSDI: boolean;
    Channel1, Channel2: integer;
    CapWidth, CapHeight: integer;
    NeededFrameInterval: integer;
    DLV_CLSID: TGUID;
    DLA_CLSID: TGUID;
  end;

  TGroupBox = class(VCL.StdCtrls.TGroupBox)
  protected
    procedure WMDropFiles(var Msg: TWMDROPFILES); message WM_DROPFILES;
  end;

  TFormTCreader = class(TForm)
    Memo1: TMemo;
    PaintBoxWaveform: TPaintBox;
    Timer1: TTimer;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Settings1: TMenuItem;
    Exit1: TMenuItem;
    N1: TMenuItem;
    Channel0Sel: TMenuItem;
    Channel1sel: TMenuItem;
    ChannelDiffSel: TMenuItem;
    N2: TMenuItem;
    Autolevel1: TMenuItem;
    GroupBoxTC: TGroupBox;
    PanelTC: TPanel;
    Label1: TLabel;
    PanelTCSyncpoint: TPanel;
    PanelRAW: TPanel;
    Label4: TLabel;
    Label3: TLabel;
    Mode1: TMenuItem;
    Automatic1: TMenuItem;
    Mainonlynoresync1: TMenuItem;
    Idleonly1: TMenuItem;
    ButtonOffsetMinus: TButton;
    ButtonOffsetPlus: TButton;
    LabelOffset: TLabel;
    LabelTelnet: TLabel;
    TelnetSend: TButton;
    procedure WndProc(var Msg: TMessage); override;
    procedure MemoOut(instring: string);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure Timer1Timer(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    function ReOpenFiles(channel: integer): boolean;
    procedure PanelTCSyncpointClick(Sender: TObject);
    procedure PanelPlayerSyncpointClick(Sender: TObject);
    procedure ButtonResyncNowClick(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure AudioDeviceClick(Sender: TObject);
    procedure ChannelSelClick(Sender: TObject);
    procedure RestartAudio;
    procedure Automatic1Click(Sender: TObject);
    procedure Mainonlynoresync1Click(Sender: TObject);
    procedure Idleonly1Click(Sender: TObject);
    procedure LabelOffsetDblClick(Sender: TObject);
    procedure ButtonOffsetMinusClick(Sender: TObject);
    procedure ButtonOffsetPlusClick(Sender: TObject);
    procedure TelnetSendClick(Sender: TObject);
  private
    SdiParams: TSdiParams;
    procedure OnTC(var Message: TMessage); message WM_TC;
  public
  end;

type
  TLocalOneChannel = class(TOnechannel)
  public
    // clips
    MainClip: TOneClip;
    IdleClip: TOneClip;
    MainClipName: string;
    IdleClipName: string;
    // for sync
    Syncpoint: Int64;
    Duration: Int64;
    LSP, PreLSP, CutDur, PreCutDur: Int64;
    LastDiff: Int64;
    IdleDiff: Int64;
    ActualTolerance: Int64;
    MustResync: boolean;
    // panel
    GroupBox: TGroupBox;
    PositionLabel: TLabel;
    PositionPanel: TPanel;
    SyncPointLabel: TLabel;
    SyncPointPanel: TPanel;
    DiffLabel: TLabel;
    DiffPanel: TPanel;
    TmpLabel: TLabel;
    ButtonResyncNow: TButton;
  end;

  EMyOwnException = class(Exception);

  TTC = record
    Hours: byte;
    Minutes: byte;
    Seconds: byte;
    Frames: byte;
  end;

  TModes = (Automatic, MainOnly, IdleOnly);

var
  FormTCreader: TFormTCreader;

  audio_process: AudioTCReaderThread;

  PlayChannels: array of TLocalOneChannel;

  TC_SP: Int64;
  PrevTC: Int64;
  GoodTC: Int64 = 0;
  FinalTC: Longint;
  TC_offset: Int64;
  AudioDebug: boolean;
  FromLastTC: Int64;
  TCisGood: boolean = false;
  IdleMode: boolean = true;
  CurrentMode: TModes;

  AudioDevices: array of TMenuItem;
  AudioDeviceName: string;

implementation

{$R *.dfm}

uses UnitEntry;

function CheckRegistered: boolean;
var
  K: HKEY;
begin
  result := Winapi.Windows.RegOpenKey(HKEY_CLASSES_ROOT,
    PChar('\TypeLib\{5CE27AC5-940C-4199-8746-01FE1F12A12E}'), K)
    = ERROR_SUCCESS;
  if result then
    Winapi.Windows.RegCloseKey(K)
end;

function TCtoString(inTC: Longint): string;
var
  iHours, iMinutes, iSeconds, iFrames, tmp: Longint;
begin
  tmp := inTC;
  iHours := tmp div 90000;
  tmp := tmp mod 90000;
  iMinutes := tmp div 1500;
  tmp := tmp mod 1500;
  iSeconds := tmp div 25;
  iFrames := tmp mod 25;
  TCtoString := format('%.2u', [iHours]) + ':' + format('%.2u', [iMinutes]) +
    ':' + format('%.2u', [iSeconds]) + ':' + format('%.2u', [iFrames]);
end;

function TryStringToTC(instring: string; var TC: Longint): boolean;
var
  tmpStr: string;
  iHours, iMin, iSec, iFrames: Longint;
  position: integer;
begin
  iHours := 0;
  iMin := 0;
  iSec := 0;
  tmpStr := StringReplace(ReverseString(instring), ' ', '0', [rfReplaceAll]);
  tmpStr := StringReplace(tmpStr, '_', '0', [rfReplaceAll]);
  position := pos(':', tmpStr);
  if position > 0 then
  begin
    if not TryStrToInt(ReverseString(LeftStr(tmpStr, position - 1)), iFrames)
    then
      iFrames := -1;
    tmpStr := MidStr(tmpStr, position + 1, 1000);
    position := pos(':', tmpStr);
    if position > 0 then
    begin
      if not TryStrToInt(ReverseString(LeftStr(tmpStr, position - 1)), iSec)
      then
        iSec := -1;
      tmpStr := MidStr(tmpStr, position + 1, 1000);
      position := pos(':', tmpStr);
      if position > 0 then
      begin
        if not TryStrToInt(ReverseString(LeftStr(tmpStr, position - 1)), iMin)
        then
          iMin := -1;
        tmpStr := MidStr(tmpStr, position + 1, 1000);
        if not TryStrToInt(ReverseString(tmpStr), iHours) then
          iHours := -1;
      end
      else if not TryStrToInt(ReverseString(tmpStr), iMin) then
        iMin := -1;
    end
    else if not TryStrToInt(ReverseString(tmpStr), iSec) then
      iSec := -1;
  end
  else if not TryStrToInt(ReverseString(tmpStr), iFrames) then
    iFrames := -1;
  TC := iFrames + 25 * (iSec + 60 * (iMin + 60 * iHours));
  TryStringToTC := (iFrames >= 0) and (iSec >= 0) and (iMin >= 0) and
    (iHours >= 0);
end;

procedure TFormTCreader.FormCreate(Sender: TObject);
var
  ini: TIniFile;
  i, i1: integer;
  tmpStr: string;
  tmpTC: Longint;
  //
  RResult: MMRESULT;
  DeviceCaps: TWaveInCaps;
begin
  Memo1.Clear;

  i := GetFileVersion(Application.ExeName);
  Self.Caption := 'TC sync player V' + format('%d.%d',
    [i div $10000, i mod $10000]);

  Self.DoubleBuffered := true;

  CoInitialize(nil);

  try
    if not CheckRegistered then
      raise EMyOwnException.Create('GFMBridge.dll не зарегистрирован');

    ini := TIniFile.Create(extractfilepath(Application.ExeName) +
      'settings.ini');
    try
      FormTCreader.Left := ini.ReadInteger('common', 'left', 0);
      FormTCreader.Top := ini.ReadInteger('common', 'top', 0);
      FormTCreader.Width := ini.ReadInteger('common', 'width', 0);
      FormTCreader.Height := ini.ReadInteger('common', 'height', 0);
      SdiParams.UseSDI := ini.ReadBool('common', 'use_sdi', true);
      tmpStr := ini.ReadString('common', 'syncpoint', '00:00:00:00');
      if TryStringToTC(tmpStr, tmpTC) then
        TC_SP := tmpTC
      else
        TC_SP := 0;
      PanelTCSyncpoint.Caption := TCtoString(TC_SP);

      if SdiParams.UseSDI then
      begin
        SdiParams.Channel1 := ini.ReadInteger('sdi', 'Channel1', 1) - 1;
        SdiParams.Channel2 := ini.ReadInteger('sdi', 'Channel2', 2) - 1;
        SdiParams.CapWidth := ini.ReadInteger('sdi', 'width', 720);
        SdiParams.CapHeight := ini.ReadInteger('sdi', 'height', 576);
        SdiParams.NeededFrameInterval :=
          ini.ReadInteger('sdi', 'frameinterval', 40000);

        i := ini.ReadInteger('sdi', 'card_no', 1);

        case i of
          1:
            begin
              SdiParams.DLV_CLSID := CLSID_DecklinkVideoCaptureFilter;
              SdiParams.DLA_CLSID := CLSID_DecklinkAudioCaptureFilter;
            end;
          2:
            begin
              SdiParams.DLV_CLSID := CLSID_DecklinkVideoCaptureFilter2;
              SdiParams.DLA_CLSID := CLSID_DecklinkAudioCaptureFilter2;
            end;
          3:
            begin
              SdiParams.DLV_CLSID := CLSID_DecklinkVideoCaptureFilter3;
              SdiParams.DLA_CLSID := CLSID_DecklinkAudioCaptureFilter3;
            end;
          4:
            begin
              SdiParams.DLV_CLSID := CLSID_DecklinkVideoCaptureFilter4;
              SdiParams.DLA_CLSID := CLSID_DecklinkAudioCaptureFilter4;
            end;
          5:
            begin
              SdiParams.DLV_CLSID := CLSID_DecklinkVideoCaptureFilter5;
              SdiParams.DLA_CLSID := CLSID_DecklinkAudioCaptureFilter5;
            end;
          6:
            begin
              SdiParams.DLV_CLSID := CLSID_DecklinkVideoCaptureFilter6;
              SdiParams.DLA_CLSID := CLSID_DecklinkAudioCaptureFilter6;
            end;
          7:
            begin
              SdiParams.DLV_CLSID := CLSID_DecklinkVideoCaptureFilter7;
              SdiParams.DLA_CLSID := CLSID_DecklinkAudioCaptureFilter7;
            end;
          8:
            begin
              SdiParams.DLV_CLSID := CLSID_DecklinkVideoCaptureFilter8;
              SdiParams.DLA_CLSID := CLSID_DecklinkAudioCaptureFilter8;
            end;
        else
          raise EMyOwnException.Create('Неправильно задан номер карты');
        end;

        TC_offset := ini.ReadInteger('sdi', 'tc_offset', 0);
      end
      else
      begin
        // audio parameters
        AudioDeviceName := ini.ReadString('audio', 'device_id', '');
        GroupBoxTC.Caption := AudioDeviceName;

        if ini.ReadBool('audio', 'diff_mode', false) then
          ChannelDiffSel.Checked := true
        else if ini.ReadInteger('audio', 'in_channel', 0) = 0 then
          Channel0Sel.Checked := true
        else
          Channel1sel.Checked := true;

        TC_offset := ini.ReadInteger('audio', 'tc_offset', 0);

        Autolevel1.Checked := ini.ReadBool('audio', 'autolevel', false);

        AudioDebug := ini.ReadBool('audio', 'debug', false);
      end;
      LabelOffset.Caption := inttostr(TC_offset);

      if ini.SectionExists('telnet') then
      begin
        TelnetSend.Visible := true;
        LabelTelnet.Visible := true;
        if not TryStringToTC(ini.ReadString('telnet', 'final_tc',
          '01:00:00:00'), FinalTC) then
          TryStringToTC('01:00:00:00', FinalTC);
      end
      else
      begin
        TelnetSend.Visible := false;
        LabelTelnet.Visible := false;
      end;

      // Playchannels
      i := 1;
      while ini.SectionExists('video' + inttostr(i)) do
      begin
        SetLength(PlayChannels, i);
        PlayChannels[i - 1] := TLocalOneChannel.Create;

        i1 := ini.ReadInteger('video' + inttostr(i), 'card_no', 1);

        case i1 of
          1:
            begin
              PlayChannels[i - 1].VideoBMGUID :=
                CLSID_DecklinkVideoRenderFilter;
              PlayChannels[i - 1].AudioBMGUID :=
                CLSID_DecklinkAudioRenderFilter;
            end;
          2:
            begin
              PlayChannels[i - 1].VideoBMGUID :=
                CLSID_DecklinkVideoRenderFilter2;
              PlayChannels[i - 1].AudioBMGUID :=
                CLSID_DecklinkAudioRenderFilter2;
            end;
          3:
            begin
              PlayChannels[i - 1].VideoBMGUID :=
                CLSID_DecklinkVideoRenderFilter3;
              PlayChannels[i - 1].AudioBMGUID :=
                CLSID_DecklinkAudioRenderFilter3;
            end;
          4:
            begin
              PlayChannels[i - 1].VideoBMGUID :=
                CLSID_DecklinkVideoRenderFilter4;
              PlayChannels[i - 1].AudioBMGUID :=
                CLSID_DecklinkAudioRenderFilter4;
            end;
          5:
            begin
              PlayChannels[i - 1].VideoBMGUID :=
                CLSID_DecklinkVideoRenderFilter5;
              PlayChannels[i - 1].AudioBMGUID :=
                CLSID_DecklinkAudioRenderFilter5;
            end;
          6:
            begin
              PlayChannels[i - 1].VideoBMGUID :=
                CLSID_DecklinkVideoRenderFilter6;
              PlayChannels[i - 1].AudioBMGUID :=
                CLSID_DecklinkAudioRenderFilter6;
            end;
          7:
            begin
              PlayChannels[i - 1].VideoBMGUID :=
                CLSID_DecklinkVideoRenderFilter7;
              PlayChannels[i - 1].AudioBMGUID :=
                CLSID_DecklinkAudioRenderFilter7;
            end;
          8:
            begin
              PlayChannels[i - 1].VideoBMGUID :=
                CLSID_DecklinkVideoRenderFilter8;
              PlayChannels[i - 1].AudioBMGUID :=
                CLSID_DecklinkAudioRenderFilter8;
            end;
        else
          raise EMyOwnException.Create('Неправильно задан номер карты');
        end;

        PlayChannels[i - 1].ClipCount := 2;

        tmpStr := ini.ReadString('video' + inttostr(i), 'syncpoint',
          '00:00:00:00');
        if TryStringToTC(tmpStr, tmpTC) then
          PlayChannels[i - 1].Syncpoint := tmpTC
        else
          PlayChannels[i - 1].Syncpoint := 0;

        PlayChannels[i - 1].GroupBox := TGroupBox.Create(FormTCreader);
        with PlayChannels[i - 1].GroupBox do
        begin
          Parent := FormTCreader;
          Left := 5;
          Top := i * 111;
          Width := 849;
          Height := 103;
          Caption := 'Player group box';
          Font.Size := 12;
          Enabled := true;
          Visible := true;
          Tag := i - 1;
          DragAcceptFiles(PlayChannels[i - 1].GroupBox.Handle, true);
        end;

        PlayChannels[i - 1].PositionLabel := TLabel.Create(FormTCreader);
        with PlayChannels[i - 1].PositionLabel do
        begin
          Parent := PlayChannels[i - 1].GroupBox;
          Left := 19;
          Top := 22;
          Width := 36;
          Height := 20;
          Caption := 'Position';
          Font.Size := 12;
        end;

        PlayChannels[i - 1].PositionPanel := TPanel.Create(FormTCreader);
        with PlayChannels[i - 1].PositionPanel do
        begin
          Parent := PlayChannels[i - 1].GroupBox;
          Left := 19;
          Top := 48;
          Width := 217;
          Height := 41;
          Caption := '00:00:00:00';
          Font.Pitch := fpFixed;
          Font.Size := 24;
          Font.Name := 'Arial';
        end;

        PlayChannels[i - 1].SyncPointLabel := TLabel.Create(FormTCreader);
        with PlayChannels[i - 1].SyncPointLabel do
        begin
          Parent := PlayChannels[i - 1].GroupBox;
          Left := 251;
          Top := 22;
          Width := 36;
          Height := 20;
          Caption := 'Syncpoint';
          Font.Size := 12;
        end;

        PlayChannels[i - 1].SyncPointPanel := TPanel.Create(FormTCreader);
        with PlayChannels[i - 1].SyncPointPanel do
        begin
          Parent := PlayChannels[i - 1].GroupBox;
          Left := 251;
          Top := 48;
          Width := 217;
          Height := 41;
          Caption := TCtoString(PlayChannels[i - 1].Syncpoint);
          Font.Pitch := fpFixed;
          Font.Size := 24;
          Font.Name := 'Arial';
          Tag := i - 1;
          OnClick := PanelPlayerSyncpointClick;
        end;

        PlayChannels[i - 1].DiffLabel := TLabel.Create(FormTCreader);
        with PlayChannels[i - 1].DiffLabel do
        begin
          Parent := PlayChannels[i - 1].GroupBox;
          Left := 483;
          Top := 22;
          Width := 36;
          Height := 20;
          Caption := 'Difference';
          Font.Size := 12;
        end;

        PlayChannels[i - 1].DiffPanel := TPanel.Create(FormTCreader);
        with PlayChannels[i - 1].DiffPanel do
        begin
          Parent := PlayChannels[i - 1].GroupBox;
          Left := 483;
          Top := 48;
          Width := 217;
          Height := 41;
          Caption := '00:00:00:00';
          Font.Pitch := fpFixed;
          Font.Size := 24;
          Font.Name := 'Arial';
        end;

        PlayChannels[i - 1].TmpLabel := TLabel.Create(FormTCreader);
        with PlayChannels[i - 1].TmpLabel do
        begin
          Parent := PlayChannels[i - 1].GroupBox;
          Left := 714;
          Top := 22;
          Width := 36;
          Height := 20;
          Caption := 'Tmp';
          Font.Size := 12;
        end;

        PlayChannels[i - 1].ButtonResyncNow := TButton.Create(FormTCreader);
        with PlayChannels[i - 1].ButtonResyncNow do
        begin
          Parent := PlayChannels[i - 1].GroupBox;
          Left := 714;
          Top := 64;
          Width := 121;
          Height := 25;
          Font.Size := 12;
          Caption := 'Resync Now';
          Tag := i - 1;
          OnClick := ButtonResyncNowClick;
        end;

        tmpStr := ini.ReadString('video' + inttostr(i), 'filename', '');
        PlayChannels[i - 1].MainClipName := tmpStr;

        tmpStr := ini.ReadString('video' + inttostr(i), 'idle', '');
        PlayChannels[i - 1].IdleClipName := tmpStr;

        ReOpenFiles(i - 1);

        inc(i);
      end;

      Memo1.Top := i * 111;

      for i := 0 to Length(PlayChannels) - 1 do
        PlayChannels[i].RunRenderGraph;

    finally
      ini.Free;
    end;
  except
    on E: Exception do
    begin
      ShowMessage(E.ClassName + ' ошибка с сообщением : ' + E.Message);
      Application.Terminate;
    end;
  end;

  if SdiParams.UseSDI then
  begin
  end
  else
  begin
    for i := 0 to waveInGetNumDevs() - 1 do
    begin
      RResult := waveInGetDevCaps(i, @DeviceCaps, SizeOf(TWaveInCaps));
      if RResult = 0 then
      begin
        i1 := Length(AudioDevices);
        SetLength(AudioDevices, i + 1);
        AudioDevices[i1] := TMenuItem.Create(MainMenu1.Items[1]);
        AudioDevices[i1].Caption := DeviceCaps.szPname;
        AudioDevices[i1].Tag := i;
        AudioDevices[i1].OnClick := AudioDeviceClick;
        if SameText(AudioDeviceName, DeviceCaps.szPname) then
        begin
          AudioDevices[i1].Checked := true;
        end;
        MainMenu1.Items[1].Insert(i1, AudioDevices[i1]);
      end;
    end;

  end;

  // start audio processing
  RestartAudio();

  Timer1.Enabled := true;
end;

procedure TFormTCreader.FormDestroy(Sender: TObject);
var
  ini: TIniFile;
  i: integer;
begin
  ini := TIniFile.Create(extractfilepath(Application.ExeName) + 'settings.ini');
  try
    ini.WriteInteger('common', 'left', FormTCreader.Left);
    ini.WriteInteger('common', 'top', FormTCreader.Top);
    ini.WriteInteger('common', 'width', FormTCreader.Width);
    ini.WriteInteger('common', 'height', FormTCreader.Height);

    for i := 1 to Length(PlayChannels) do
    begin
      PlayChannels[i - 1].StopRenderGraph;
      if PlayChannels[i - 1].MainClip <> nil then
        ini.WriteString('video' + inttostr(i), 'filename',
          PlayChannels[i - 1].MainClip.FileName);
      if PlayChannels[i - 1].IdleClip <> nil then
        ini.WriteString('video' + inttostr(i), 'idle',
          PlayChannels[i - 1].IdleClip.FileName);
      ini.WriteString('video' + inttostr(i), 'syncpoint',
        TCtoString(PlayChannels[i - 1].Syncpoint));
      DragAcceptFiles(PlayChannels[i - 1].GroupBox.Handle, false);
      FreeAndNil(PlayChannels[i - 1]);
    end;

    ini.WriteString('common', 'syncpoint', TCtoString(TC_SP));

    {
      if AudioDeviceName <> '' then
      ini.WriteString('audio', 'device_id', AudioDeviceName);

      if Channel0Sel.Checked then
      begin
      ini.WriteBool('audio', 'diff_mode', false);
      ini.WriteInteger('audio', 'in_channel', 0);
      end
      else if Channel1sel.Checked then
      begin
      ini.WriteBool('audio', 'diff_mode', false);
      ini.WriteInteger('audio', 'in_channel', 1);
      end
      else if ChannelDiffSel.Checked then
      begin
      ini.WriteBool('audio', 'diff_mode', true);
      ini.WriteInteger('audio', 'in_channel', 0);
      end;

      ini.WriteBool('audio', 'autolevel', Autolevel1.Checked);

      ini.WriteInteger('audio', 'tc_offset', TC_offset);
    }
  finally
    ini.Free;
  end;

  CoUninitialize();
end;

procedure TFormTCreader.AudioDeviceClick(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to Length(AudioDevices) - 1 do
    AudioDevices[i].Checked := false;
  (Sender as TMenuItem).Checked := true;

  AudioDeviceName := StringReplace((Sender as TMenuItem).Caption, '&', '',
    [rfReplaceAll]);
  GroupBoxTC.Caption := AudioDeviceName;

  RestartAudio;
end;

procedure TFormTCreader.Automatic1Click(Sender: TObject);
var
  i: integer;
begin
  Mode1.Caption := 'Mode: Automatic';
  CurrentMode := Automatic;
  for i := 0 to Length(PlayChannels) - 1 do
    PlayChannels[i].ButtonResyncNow.Enabled := true;
end;

procedure TFormTCreader.ButtonOffsetMinusClick(Sender: TObject);
begin
  Dec(TC_offset);
  LabelOffset.Caption := inttostr(TC_offset);
end;

procedure TFormTCreader.ButtonOffsetPlusClick(Sender: TObject);
begin
  inc(TC_offset);
  LabelOffset.Caption := inttostr(TC_offset);
end;

procedure TFormTCreader.ButtonResyncNowClick(Sender: TObject);
var
  CurChannel: integer;
begin
  if CurrentMode = Automatic then
  begin
    CurChannel := (Sender as TButton).Tag;
    PlayChannels[CurChannel].MustResync := true;
    PlayChannels[CurChannel].ButtonResyncNow.Enabled := false;
  end;
end;

procedure TFormTCreader.ChannelSelClick(Sender: TObject);
var
  change: boolean;
begin
  if (Sender as TMenuItem).Tag <> 3 then
  begin
    Channel0Sel.Checked := false;
    Channel1sel.Checked := false;
    ChannelDiffSel.Checked := false;
    change := not(Sender as TMenuItem).Checked;
    (Sender as TMenuItem).Checked := true;
  end
  else
  begin
    change := true;
    (Sender as TMenuItem).Checked := not(Sender as TMenuItem).Checked;
  end;
  if change then
    RestartAudio;
end;

procedure TFormTCreader.RestartAudio;
var
  i: integer;
  thh: NativeUINT;
begin
  if audio_process <> nil then
  begin
    thh := audio_process.Handle;
    audio_process.Terminate;
    WaitForSingleObject(thh, 0);
    FreeAndNil(audio_process);
  end;

  audio_process := AudioTCReaderThread.Create(true);
  audio_process.FreeOnTerminate := false;
  audio_process.Priority := tpTimeCritical;
  audio_process.MainHandle := Self.Handle;

  if SdiParams.UseSDI then
  begin
    audio_process.CapWidth := SdiParams.CapWidth;
    audio_process.CapHeight := SdiParams.CapHeight;
    audio_process.SDIChannel1 := SdiParams.Channel1;
    audio_process.SDIChannel2 := SdiParams.Channel2;
    audio_process.NeededFrameInterval := SdiParams.NeededFrameInterval;
    audio_process.DLV_CLSID := SdiParams.DLV_CLSID;
    audio_process.DLA_CLSID := SdiParams.DLA_CLSID;
  end
  else
  begin
    audio_process.SelectedDevice := 0;

    for i := 0 to Length(AudioDevices) - 1 do
      if AudioDevices[i].Checked then
        audio_process.SelectedDevice := AudioDevices[i].Tag;

    if Channel0Sel.Checked then
    begin
      audio_process.DiffMode := false;
      audio_process.UseChannel := 0;
    end
    else if Channel1sel.Checked then
    begin
      audio_process.DiffMode := false;
      audio_process.UseChannel := 1;
    end
    else if ChannelDiffSel.Checked then
    begin
      audio_process.DiffMode := true;
      audio_process.UseChannel := 0;
    end;

    audio_process.AutoLevelEnabled := Autolevel1.Checked;
    audio_process.DebugEnabled := AudioDebug;
  end;

  audio_process.Start;
end;

procedure TFormTCreader.Exit1Click(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TFormTCreader.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  thh: NativeUINT;
begin
  thh := audio_process.Handle;
  audio_process.Terminate;
  WaitForSingleObject(thh, 0);
  FreeAndNil(audio_process);
end;

procedure TFormTCreader.TelnetSendClick(Sender: TObject);
var
  ini: TIniFile;
  telnet_process: TelnetThread;
begin
  ini := TIniFile.Create(extractfilepath(Application.ExeName) + 'settings.ini');
  try
    if ini.SectionExists('telnet') then
    begin
      telnet_process := TelnetThread.Create(true);
      telnet_process.FreeOnTerminate := true;
      telnet_process.Priority := tpNormal;

      telnet_process.host := ini.ReadString('telnet', 'host', '127.0.0.1');
      telnet_process.port := ini.ReadInteger('telnet', 'port', 23);
      telnet_process.UID := ini.ReadString('telnet', 'uid', '123456');
      telnet_process.logging := ini.ReadBool('telnet', 'logging', true);
      telnet_process.connected := false;

      telnet_process.SecondsTo := (FinalTC - PrevTC + 23) div 25;
      LabelTelnet.Caption := inttostr(telnet_process.SecondsTo);

      telnet_process.Start;
    end;
  finally
    ini.Free;
  end;
end;

procedure TFormTCreader.Timer1Timer(Sender: TObject);
type
  TPointArr = array [0 .. 16383] of TPoint;
  PPointArr = ^TPointArr;
var
  TC: TTC;
  XScale, YScale: single;
  Hquarter: single;
  i: integer;
  points_l, points_r: PPointArr;

  tmpPos: Int64;
begin
  if audio_process <> nil then
  begin
    if audio_process.OutputText.Count > 0 then
    begin
      Memo1.Lines.AddStrings(audio_process.OutputText);
      audio_process.OutputText.Clear;
    end;

    with audio_process do
    begin
      if TCisReady then
      begin
        PanelRAW.Caption := format('%4.4x %4.4x %4.4x %4.4x',
          [TCData[3], TCData[2], TCData[1], TCData[0]]);
        TC.Hours := (TCData[3] and $000F) + 10 * ((TCData[3] and $0300) shr 8);
        TC.Minutes := (TCData[2] and $000F) + 10 *
          ((TCData[2] and $0700) shr 8);
        TC.Seconds := (TCData[1] and $000F) + 10 *
          ((TCData[1] and $0700) shr 8);
        TC.Frames := (TCData[0] and $000F) + 10 * ((TCData[0] and $0300) shr 8);
        PanelTC.Caption := format('%2.2d:%2.2d:%2.2d:%2.2d',
          [TC.Hours, TC.Minutes, TC.Seconds, TC.Frames]);

        TelnetSend.Caption := 'Send: ' +
          inttostr((FinalTC - PrevTC + 23) div 25);

        TCisReady := false;
      end;

      if audio_process.AudioDataCntr > 0 then
      begin
        GetMem(points_l, audio_process.AudioDataCntr * SizeOf(TPoint));
        GetMem(points_r, audio_process.AudioDataCntr * SizeOf(TPoint));

        XScale := PaintBoxWaveform.Width / audio_process.AudioDataCntr;
        YScale := PaintBoxWaveform.Height / (1 shl 17);
        Hquarter := PaintBoxWaveform.Height / 4;

        for i := 0 to audio_process.AudioDataCntr - 1 do
        begin
          points_l^[i] := Point(round(i * XScale),
            round(Hquarter - audio_process.AudioData^[i * 2] * YScale));
          points_r^[i] := Point(round(i * XScale),
            round(3 * Hquarter - audio_process.AudioData^[i * 2 + 1] * YScale));
        end;

        with PaintBoxWaveform.Canvas do
        begin
          Brush.Color := clWhite;
          FillRect(ClipRect);

          Polyline(Slice(points_l^, audio_process.AudioDataCntr));
          Polyline(Slice(points_r^, audio_process.AudioDataCntr));
        end;

        FreeMem(points_l);
        FreeMem(points_r);
        audio_process.AudioDataCntr := 0;
      end;
    end;

    for i := 0 to Length(PlayChannels) - 1 do
    begin
      if (PlayChannels[i] <> nil) and (PlayChannels[i].CurrentStatus = Play)
      then
      begin
        tmpPos := PlayChannels[i].GetCurrentTime;
        if tmpPos < 0 then
          tmpPos := PlayChannels[i].PreCutDur + tmpPos
        else
        begin
          PlayChannels[i].PreCutDur := PlayChannels[i].CutDur;
          PlayChannels[i].PreLSP := PlayChannels[i].LSP;
        end;
        PlayChannels[i].PositionPanel.Caption :=
          TCtoString(tmpPos + PlayChannels[i].PreLSP);

        if PlayChannels[i].ActiveClip = PlayChannels[i].IdleClip then
        begin
          if i = 0 then
            PlayChannels[i].IdleDiff := tmpPos + PlayChannels[i].PreLSP
          else
            PlayChannels[i].IdleDiff := PlayChannels[0].IdleDiff -
              (tmpPos + PlayChannels[i].PreLSP);

          PlayChannels[i].TmpLabel.Caption :=
            inttostr(PlayChannels[i].IdleDiff);
        end;
      end;
    end;

    if FromLastTC = 10 then
    begin
      PanelTC.Font.Color := clRed;
      TCisGood := false;
    end;
    if FromLastTC < 100 then
    begin
      inc(FromLastTC);
      if FromLastTC = 100 then
      begin
        IdleMode := true;
      end;
    end;
  end;
end;

procedure TFormTCreader.WndProc(var Msg: TMessage);
var
  NewStartPos: Int64;
  CurChannel: integer;
  Dur: Int64;
begin
  if (Msg.Msg < WM_BRIDGE) or (Msg.Msg > (WM_BRIDGE + Length(PlayChannels) - 1))
  then
  begin
    inherited WndProc(Msg);
    exit;
  end;

  CurChannel := Msg.Msg - WM_BRIDGE;

  if (PlayChannels[CurChannel] <> nil) and
    (PlayChannels[CurChannel].CurrentStatus = Play) then
  begin
    with PlayChannels[CurChannel] do
    begin
      if IdleMode or (CurrentMode = IdleOnly) then
      begin
        NextClip := IdleClip;
        if NextClip <> ActiveClip then
        begin
          // after clip switching - playing out from start
          NewStartPos := 0
        end
        else
        begin
          if CurChannel = 0 then
          begin
            NewStartPos := ActiveClip.StartPos + 10000000;
          end
          else
          begin
            if Abs(IdleDiff) > 1 then
            begin
              NewStartPos := ActiveClip.StartPos + 10000000 + IdleDiff * 400000;
              IdleDiff := 0;
              IdleClip.pSeeking.GetDuration(Dur);

              if (NewStartPos < 0) or (NewStartPos > Dur - 20000000) then
                NewStartPos := ActiveClip.StartPos + 10000000;
            end
            else
              NewStartPos := ActiveClip.StartPos + 10000000;
          end;
        end;
      end
      else
      begin
        if ActiveClip <> MainClip then
        begin
          // after clip switching - playing out from syncpoint?
          NextClip := MainClip;
          if CurrentMode = Automatic then
          begin
            NewStartPos := Int64((PrevTC - TC_SP) + Syncpoint) * 400000;
            MainClip.pSeeking.GetDuration(Dur);

            if (NewStartPos < 0) or (NewStartPos > (Dur - 20000000)) then
            begin
              NextClip := IdleClip;
              NewStartPos := IdleClip.StartPos + 10000000;
            end;
          end
          else // MainOnly
          begin
            NewStartPos := 0;
          end;
        end
        else
        begin
          // continue playing
          NextClip := MainClip;
          if CurrentMode = Automatic then
          begin
            MainClip.pSeeking.GetDuration(Dur);
            if TCisGood and ((Abs(LastDiff) > ActualTolerance) or MustResync)
            then
            begin
              NewStartPos := MainClip.StartPos + LastDiff * 400000 + 10000000;
              if (NewStartPos < 0) or (NewStartPos > (Dur - 20000000)) then
              begin // new tc is out of range
                NextClip := IdleClip;
                NewStartPos := 0;
              end
              else
              begin // resyncing
                MemoOut('Channel ' + inttostr(CurChannel) + ': Уход на ' +
                  inttostr(LastDiff) + ' кадров. Пересинхронизация. Новый TC=' +
                  TCtoString(NewStartPos div 400000));
                ActualTolerance := 2;
              end;
            end
            else
              NewStartPos := MainClip.StartPos + 10000000;
          end
          else // MainOnly mode
          begin
            NewStartPos := MainClip.StartPos + 10000000;
          end;
        end;
      end;

      NextClip.StartPos := NewStartPos;
      NextClip.pSeeking.GetDuration(Dur);
      if (Dur - NewStartPos) < 10000000 then
      begin
        NextClip.StartPos := 0;
        NextClip.StopPos := 10000000;
      end
      else
      begin
        if (Dur - NewStartPos) < 20000000 then
          NextClip.StopPos := Dur
        else
          NextClip.StopPos := NextClip.StartPos + 10000000; // 1 sec step
      end;

      LSP := NextClip.StartPos div 400000;
      CutDur := (NextClip.StopPos - NextClip.StartPos) div 400000;

      if NextClip = ActiveClip then
        JumpToNext(true)
      else
      begin
        NextClip.pControl.Pause;

        NextClip.pSeeking.SetPositions(NextClip.StartPos,
          AM_SEEKING_AbsolutePositioning, NextClip.StopPos,
          AM_SEEKING_AbsolutePositioning);

        JumpToNext(false);
      end;

      if MustResync then
      begin
        MustResync := false;
        ButtonResyncNow.Enabled := true;
      end;
    end;
  end;
end;

procedure TFormTCreader.FormResize(Sender: TObject);
var
  w_ch: integer;
  w_l: integer;
begin
  if Self.Width < 870 then
    Self.Width := 870;
  w_ch := Self.Width - 870;
  Memo1.Width := 849 + w_ch;
  PaintBoxWaveform.Width := 849 + w_ch;

  w_l := 500 + Length(PlayChannels) * 111;
  if Self.Height < w_l then
    Self.Height := w_l;

  PaintBoxWaveform.Top := Self.ClientHeight - PaintBoxWaveform.Height - 10;
  Memo1.Height := PaintBoxWaveform.Top - Memo1.Top - 10;
end;

procedure TFormTCreader.Idleonly1Click(Sender: TObject);
var
  i: integer;
begin
  Mode1.Caption := 'Mode: Idle only';
  CurrentMode := IdleOnly;
  for i := 0 to Length(PlayChannels) - 1 do
    PlayChannels[i].ButtonResyncNow.Enabled := false;
end;

procedure TFormTCreader.LabelOffsetDblClick(Sender: TObject);
begin
  if ButtonOffsetMinus.Enabled then
  begin
    ButtonOffsetMinus.Enabled := false;
    ButtonOffsetPlus.Enabled := false;
  end
  else
  begin
    ButtonOffsetMinus.Enabled := true;
    ButtonOffsetPlus.Enabled := true;
  end;
end;

procedure TFormTCreader.Mainonlynoresync1Click(Sender: TObject);
var
  i: integer;
begin
  Mode1.Caption := 'Mode: Main only (no resync)';
  CurrentMode := MainOnly;
  for i := 0 to Length(PlayChannels) - 1 do
    PlayChannels[i].ButtonResyncNow.Enabled := false;
end;

procedure TFormTCreader.MemoOut(instring: string);
begin
  Memo1.Lines.Append(FormatDateTime('hh:nn:ss:zzz ', Now()) + instring);
end;

procedure TFormTCreader.OnTC(var Message: TMessage);
var
  RealTC: Int64;
  tmpPos: Int64;
  i: integer;
begin
  RealTC := Message.WParam + TC_offset;

  if RealTC = (PrevTC + 1) then
  begin
    if GoodTC >= 12 then
    begin
      PanelTC.Font.Color := clGreen;
      TCisGood := true;
    end
    else
    begin
      inc(GoodTC);
      PanelTC.Font.Color := clBlack;
      TCisGood := false;
    end;
  end
  else
  begin
    GoodTC := 0;
    PanelTC.Font.Color := clRed;
    TCisGood := false;
    MemoOut('Подрыв c ' + TCtoString(PrevTC) + ' на ' + TCtoString(RealTC));
  end;
  PrevTC := RealTC;

  if TCisGood then
  begin
    for i := 0 to Length(PlayChannels) - 1 do
    begin
      if (PlayChannels[i] <> nil) and (PlayChannels[i].CurrentStatus = Play) and
        (PlayChannels[i].ActiveClip = PlayChannels[i].MainClip) and Timer1.Enabled
      then
      begin
        tmpPos := PlayChannels[i].GetCurrentTime;
        if tmpPos < 0 then
          tmpPos := PlayChannels[i].PreCutDur + tmpPos
        else
        begin
          PlayChannels[i].PreCutDur := PlayChannels[i].CutDur;
          PlayChannels[i].PreLSP := PlayChannels[i].LSP;
        end;
        PlayChannels[i].LastDiff := (RealTC - TC_SP) -
          (tmpPos + PlayChannels[i].PreLSP - PlayChannels[i].Syncpoint);

        if PlayChannels[i].LastDiff < 0 then
          PlayChannels[i].DiffPanel.Caption :=
            '-' + TCtoString(Abs(PlayChannels[i].LastDiff))
        else
          PlayChannels[i].DiffPanel.Caption :=
            '+' + TCtoString(Abs(PlayChannels[i].LastDiff));
      end;
    end;
  end;

  FromLastTC := 0;
  IdleMode := false;
end;

function TFormTCreader.ReOpenFiles(channel: integer): boolean;
var
  tmpClip: TOneClip;
  tr: boolean;
  ots: boolean;
  Dur64: Int64;
begin
  ots := Timer1.Enabled;
  Timer1.Enabled := false;

  tr := false;
  try
    PlayChannels[channel].ReCreate;
    PlayChannels[channel].SetNotify(Self.Handle, WM_BRIDGE + channel);

    PlayChannels[channel].MainClip := nil;
    if fileexists(PlayChannels[channel].MainClipName) then
    begin
      tmpClip := PlayChannels[channel].GetUnusedClip;
      tmpClip.FileName := PlayChannels[channel].MainClipName;
      tmpClip.StartPos := 0;
      tmpClip.StopPos := 10000000;
      tmpClip.IsLoaded := true;

      PlayChannels[channel].PrepareSourceGraph(tmpClip);
      PlayChannels[channel].MainClip := tmpClip;
    end;

    PlayChannels[channel].IdleClip := nil;
    if fileexists(PlayChannels[channel].IdleClipName) then
    begin
      tmpClip := PlayChannels[channel].GetUnusedClip;
      tmpClip.FileName := PlayChannels[channel].IdleClipName;
      tmpClip.StartPos := 0;
      tmpClip.StopPos := 10000000;
      tmpClip.IsLoaded := true;

      PlayChannels[channel].PrepareSourceGraph(tmpClip);
      PlayChannels[channel].IdleClip := tmpClip;
    end;

    if PlayChannels[channel].IdleClip <> nil then
    begin
      PlayChannels[channel].IdleClip.pSeeking.GetDuration(Dur64);
      PlayChannels[channel].Duration := Dur64 div 400000;

      PlayChannels[channel].PrepareRenderGraph(PlayChannels[channel].IdleClip);
    end
    else
    begin
      if PlayChannels[channel].MainClip <> nil then
      begin
        PlayChannels[channel].MainClip.pSeeking.GetDuration(Dur64);
        PlayChannels[channel].Duration := Dur64 div 400000;

        PlayChannels[channel].PrepareRenderGraph
          (PlayChannels[channel].MainClip);
      end;
    end;

    PlayChannels[channel].PauseRenderGraph;

    PlayChannels[channel].LSP := 0;
    PlayChannels[channel].PreLSP := 0;
    PlayChannels[channel].CutDur := 25;
    PlayChannels[channel].PreCutDur := 25;

    PlayChannels[channel].GroupBox.Caption :=
      ExtractFileName(PlayChannels[channel].MainClipName) + ' / ' +
      ExtractFileName(PlayChannels[channel].IdleClipName);
    tr := true;
  except
    PlayChannels[channel].ReCreate;
    PlayChannels[channel].GroupBox.Caption := 'Empty';
  end;
  ReOpenFiles := tr;
  Timer1.Enabled := ots;
end;

procedure TFormTCreader.PanelPlayerSyncpointClick(Sender: TObject);
var
  tmpStr: string;
  tmpTC: Longint;
  CurChannel: integer;
begin
  CurChannel := (Sender as TPanel).Tag;
  FormEntry.MaskEdit1.Text := PlayChannels[CurChannel].SyncPointPanel.Caption;
  FormEntry.Caption := 'Sync point on channel ' + inttostr(CurChannel + 1);
  if FormEntry.ShowModal = mrOK then
  begin
    tmpStr := FormEntry.MaskEdit1.Text;
    tmpStr := StringReplace(tmpStr, '_', '0', [rfReplaceAll]);
    if TryStringToTC(tmpStr, tmpTC) then
    begin
      PlayChannels[CurChannel].Syncpoint := tmpTC;
      PlayChannels[CurChannel].SyncPointPanel.Caption := TCtoString(tmpTC);
    end;
  end;
end;

procedure TFormTCreader.PanelTCSyncpointClick(Sender: TObject);
var
  tmpStr: string;
  tmpTC: Longint;
begin
  FormEntry.MaskEdit1.Text := PanelTCSyncpoint.Caption;
  FormEntry.Caption := 'Sync point on TC source';
  if FormEntry.ShowModal = mrOK then
  begin
    tmpStr := FormEntry.MaskEdit1.Text;
    tmpStr := StringReplace(tmpStr, '_', '0', [rfReplaceAll]);
    if TryStringToTC(tmpStr, tmpTC) then
    begin
      TC_SP := tmpTC;
      PanelTCSyncpoint.Caption := TCtoString(tmpTC);
    end;
  end;
end;

{ TGroupBox }
procedure TGroupBox.WMDropFiles(var Msg: TWMDROPFILES);
var
  NumFiles: Longint;
  buffer: array [0 .. 255] of char;
  i: integer;
  KeyState: word;
begin
  { How many files are being dropped }
  NumFiles := DragQueryFile(Msg.Drop, $FFFFFFFF, nil, 0);
  { Accept the dropped files }
  for i := 0 to (NumFiles - 1) do
  begin
    DragQueryFile(Msg.Drop, i, @buffer, SizeOf(buffer));
    // if fileexists(buffer) and (UpperCase(ExtractFileExt(buffer)) = '.AVI') then
    if fileexists(buffer) then
    begin
      KeyState := GetKeyState(VK_SHIFT);
      if KeyState and $8000 = $8000 then
        PlayChannels[Self.Tag].IdleClipName := buffer
      else
        PlayChannels[Self.Tag].MainClipName := buffer;
      FormTCreader.ReOpenFiles(Self.Tag);
      PlayChannels[Self.Tag].RunRenderGraph;
      break;
    end;
  end; // for
end;

end.
