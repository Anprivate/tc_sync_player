unit reader_thread;

interface

uses
  System.Classes, System.Types, System.SysUtils,
  WinAPI.Windows, WinAPI.Messages, WinAPI.MMsystem,
  WinAPI.ActiveX, WinAPI.DirectShow9,
  VCL.ExtCtrls;

const
  MY_MSG = WM_USER + 1;

  fs = 48000;
  // длина буфера в семплах
  bufsize = (fs div 25);
  // количество буферов
  bufnum = 4;

type
  TData16 = array [0 .. 16383] of smallint;
  PData16 = ^TData16;

  AudioTCReaderThread = class(TThread)
  private
    hBufHead: array [0 .. bufnum - 1] of THANDLE;
    hBufHeadMem: array [0 .. bufnum - 1] of THANDLE;
    //
    pGraph: IGraphBuilder;
    pBuild: ICaptureGraphBuilder2;
    pControl: IMediaControl;
    GraphID: integer;
    ErrMsg: string;
    //
    procedure InitAudioDevice;
    procedure DeInitAudioDevice;
    procedure InitSDIDevice;
    procedure DeInitSDIDevice;
    procedure AudioDataToTC;
  protected
    procedure Execute; override;
  public
    // mode selecter
    UseSdiIn: boolean;
    // input parameters
    // common parameters
    DiffMode: boolean;
    // audio in parameters
    SelectedDevice: Word;
    UseChannel: integer;
    AutoLevelEnabled: boolean;
    DebugEnabled: boolean;
    // for SDI
    CapWidth, CapHeight: integer;
    PreviewOn: boolean;
    NeededFrameInterval: integer;
    VideoPanel: TPanel;
    SDIChannel1: integer;
    SDIChannel2: integer;
    DLV_CLSID: TGUID;
    DLA_CLSID: TGUID;
    //
    MainHandle: HWND;
    // output parameters
    TCdata: array [0 .. 3] of Word;
    TCisReady: boolean;
    OutputText: TStringList;
    // output parameters
    AudioData: PData16;
    AudioDataCntr: integer;
  end;

  TAudioGrabCB = class(TComponent, ISampleGrabberCB)
  private
    // Capturer: TObject;
    function SampleCB(SampleTime: Double; pSample: IMediaSample)
      : HResult; stdcall;
    function BufferCB(SampleTime: Double; pBuffer: PByte; BufferLen: longint)
      : HResult; stdcall;
    function QueryInterface(const IID: TGUID; out Obj): HResult;
      reintroduce; stdcall;
    function _AddRef: integer; stdcall;
    function _Release: integer; stdcall;
  end;

  EMyOwnException = class(Exception);

procedure OnWaveIn(hwi: HWAVEIN; uMsg, dwInstance, dwParam1,
  dwParam2: DWORD); stdcall;

implementation

var
  pAudioGrabCB: TAudioGrabCB;

  WaveIn: HWAVEIN;

  pBufHead: array [0 .. bufnum - 1] of PWaveHdr;

  data16: PData16;
  SampleBuffer: PData16;

  LastJump: integer = 0;
  WasOne: boolean = false;
  WasPlus: boolean = false;
  WasMinus: boolean = false;
  SampleSumm: Int64 = 0;
  BuffCount: Int64 = 0;

  wData: Word = 0;
  wDataOut: array [0 .. 3] of Word;
  WordCounter: integer = -1;
  BitCounter: integer = -1;

  SavedFramesCounter: integer = 0;

  MixerHandler: HMixer;
  MixerMax, MixerMin: Cardinal;
  MixerLevel: TMixerControlDetailsUnsigned;
  MixerControlDetails: TMixerControlDetails;

  TCisReaded: boolean = false;

  close_request: boolean = false;
  prev_tc_int: integer;
  normal_tc_cntr: integer;
  [volatile]
  DataIsReady: boolean;

  _SDIDiffMode: boolean;
  _SDIChannel1: integer;
  _SDIChannel2: integer;
  _SDIAudioData: PData16;

function AddGraphToRot(Graph: IFilterGraph; out ID: integer): HResult;
var
  Moniker: IMoniker;
  ROT: IRunningObjectTable;
  wsz: WideString;
begin
  Result := GetRunningObjectTable(0, ROT);
  if (Result <> S_OK) then
    exit;
  wsz := format('FilterGraph %p pid %x',
    [pointer(Graph), GetCurrentProcessId()]);
  Result := CreateItemMoniker('!', PWideChar(wsz), Moniker);
  if (Result <> S_OK) then
    exit;
  Result := ROT.Register(0, Graph, Moniker, ID);
  Moniker := nil;
end;

function RemoveGraphFromRot(ID: integer): HResult;
var
  ROT: IRunningObjectTable;
begin
  Result := GetRunningObjectTable(0, ROT);
  if (Result <> S_OK) then
    exit;
  Result := ROT.Revoke(ID);
  ROT := nil;
end;

{ AudioTCReaderThread }

procedure AudioTCReaderThread.AudioDataToTC;
var
  i, i1, i2: integer;
  SampleValue: integer;
  SummPlus, SummMinus, CntrPlus, CntrMinus: Int64;
  AvrgPlus, AvrgMinus, MaxPlus, MaxMinus: integer;
  WasJump: boolean;
  Interval: integer;
  tc_int: integer;
  procedure WriteFrame();
  var
    FileName: string;
    FileHandle: THANDLE;
  begin
    if SavedFramesCounter < 50 then
    begin
      FileName := format('frame%7.7d', [BuffCount]) + '.raw';
      if not FileExists(FileName) then
      begin
        FileHandle := FileCreate(FileName);
        FileWrite(FileHandle, SampleBuffer^, bufsize * 2);
        FileClose(FileHandle);
      end;
      inc(SavedFramesCounter);
    end;
  end;

begin
  // считаем среднее значение
  SummPlus := 0;
  CntrPlus := 0;
  SummMinus := 0;
  CntrMinus := 0;
  MaxPlus := 0;
  MaxMinus := 0;

  // get sample value
  for i := 0 to bufsize - 1 do
  begin
    SampleValue := SampleBuffer^[i];
    if SampleValue > 0 then
    begin
      SummPlus := SummPlus + SampleValue;
      if MaxPlus < SampleValue then
        MaxPlus := SampleValue;
      inc(CntrPlus);
    end; // if SampleValue > 0
    if SampleValue < 0 then
    begin
      SummMinus := SummMinus + SampleValue;
      if MaxMinus > SampleValue then
        MaxMinus := SampleValue;
      inc(CntrMinus);
    end; // if SampleValue < 0
  end; // for

  // calculating average plus and minus value (divided by 4 for trigger value)
  if CntrPlus > 0 then
    AvrgPlus := SummPlus div CntrPlus div 4
  else
    AvrgPlus := 10;

  if CntrMinus > 0 then
    AvrgMinus := SummMinus div CntrMinus div 4
  else
    AvrgMinus := -10;

  // autolevel adjustment
  if AutoLevelEnabled then
  begin
    if MaxPlus > 30000 then
    begin
      if MixerLevel.dwValue > (MixerMax div 256) then
      begin
        MixerLevel.dwValue := MixerLevel.dwValue - (MixerMax div 256);
        mixerSetControlDetails(MixerHandler, @MixerControlDetails,
          MIXER_GETCONTROLDETAILSF_VALUE or MIXER_OBJECTF_HMIXER);
      end;
    end;

    if MaxPlus < 20000 then
    begin
      MixerLevel.dwValue := MixerLevel.dwValue + (MixerMax div 256);
      if MixerLevel.dwValue > MixerMax then
        MixerLevel.dwValue := MixerMax;
      mixerSetControlDetails(MixerHandler, @MixerControlDetails,
        MIXER_GETCONTROLDETAILSF_VALUE or MIXER_OBJECTF_HMIXER);
    end;
  end;

  // decoding
  for i := 0 to bufsize - 1 do
  begin
    SampleSumm := SampleSumm + SampleBuffer^[i];

    // zero crossing detection
    WasJump := false;
    if (SampleBuffer^)[i] > AvrgPlus then
    begin
      if WasMinus then
        WasJump := true;
      WasMinus := false;
      WasPlus := true;
    end
    else
    begin
      if (SampleBuffer^)[i] < AvrgMinus then
      begin
        if WasPlus then
          WasJump := true;
        WasPlus := false;
        WasMinus := true;
      end;
    end;

    //
    if WasJump then
    begin
      // i1 - distance between transitions
      if LastJump > i then
        i1 := i + (bufsize - LastJump)
      else
        i1 := i - LastJump;
      //
      if ((i1 > 30) or (i1 < 10)) and (BuffCount > 1) then
      begin
        if normal_tc_cntr > 125 then
        begin
          if DebugEnabled then
          begin
            OutputText.Append(format('Strange in period %d: %d %d %d',
              [BuffCount, i, LastJump, i1]));
            WriteFrame();
          end;
        end;
        normal_tc_cntr := 0;
      end;
      //
      if i1 >= 16 then
      begin // period
        Interval := i1;
        if (Interval <= 30) or (BuffCount = 0) then
        begin // normal interval
          wData := wData shr 1;
          if Abs(SampleSumm div Interval) < ((AvrgPlus - AvrgMinus) div 2) then
            // '1' detected
            wData := wData or $8000;

          if wData = $BFFC then
          begin // sync word detected
            if (WordCounter <> 4) and (BuffCount > 1) then
            begin
              if DebugEnabled then
              begin
                OutputText.Append(FormatDateTime('hh:nn:ss:zzz', Now()) +
                  format(' Синхрослово в странный момент. Счётчик слов: %d Счётчик бит: %d',
                  [WordCounter, BitCounter]));
                WriteFrame();
              end;
            end
            else
            begin
              if TCisReaded then
              begin
                for i2 := 0 to 3 do
                  TCdata[i2] := wDataOut[i2];
                TCisReady := true;
                tc_int := (wDataOut[0] and $000F) + 10 *
                  ((wDataOut[0] and $0300) shr 8) + 25 * (wDataOut[1] and $000F)
                  + 250 * ((wDataOut[1] and $0700) shr 8) + 25 * 60 *
                  (wDataOut[2] and $000F) + 250 * 60 *
                  ((wDataOut[2] and $0700) shr 8) + 25 * 60 * 60 *
                  (wDataOut[3] and $000F) + 250 * 60 * 60 *
                  ((wDataOut[3] and $0300) shr 8);

                if (prev_tc_int <> 0) and ((tc_int - prev_tc_int) <> 1) then
                begin
                  OutputText.Append(FormatDateTime('hh:nn:ss:zzz', Now()) +
                    ' Скачок на ' + inttostr(tc_int - prev_tc_int) + ' кадров');
                  prev_tc_int := 0;
                end;
                prev_tc_int := tc_int;

                if MainHandle <> 0 then
                begin
                  SendMessage(MainHandle, MY_MSG, tc_int, 0);
                end;

                if normal_tc_cntr < 1000 then
                  inc(normal_tc_cntr);

                TCisReaded := false;
              end;
            end;
            WordCounter := 0;
            BitCounter := 0;
          end
          else
          begin // not sync word
            inc(BitCounter);
            if BitCounter = 16 then
            begin // word is rxed
              BitCounter := 0;
              if WordCounter < 0 then
                WordCounter := 0
              else
              begin
                if WordCounter < 4 then
                begin
                  wDataOut[WordCounter] := wData;
                  if WordCounter = 3 then
                  begin
                    TCisReaded := true;
                  end; // if WordCounter = 3
                end; // if WordCounter < 4
                if WordCounter < 10 then
                  inc(WordCounter);
              end; // if WordCounter < 0
            end; // if BitCounter = 16
          end; // if wData = $BFFC
        end; // if Interval > 30
        LastJump := i;
        SampleSumm := 0;
      end; // if i1 >= 16
    end; // if WasJump
  end; // for i
  inc(BuffCount);

  DataIsReady := false;
end;

procedure AudioTCReaderThread.DeInitAudioDevice;
var
  i: integer;
begin
  WaveInStop(WaveIn);

  close_request := true;
  Sleep(500);

  WaveInReset(WaveIn);

  for i := 0 to bufnum - 1 do
    WaveInUnPrepareHeader(WaveIn, pBufHead[i], SizeOf(TWaveHdr));

  WaveInClose(WaveIn);

  MixerClose(MixerHandler);

  for i := 0 to bufnum - 1 do
  begin
    GlobalUnlock(hBufHeadMem[i]);
    GlobalFree(hBufHeadMem[i]);
    GlobalUnlock(hBufHead[i]);
    GlobalFree(hBufHead[i]);
  end;
end;

procedure AudioTCReaderThread.DeInitSDIDevice;
var
  hr: HResult;
  pEnum: IEnumFilters;
  pFilter: IBaseFilter;
  ErrMsg: string;
begin
  if Assigned(pControl) then
    pControl.Stop;

  if not Assigned(pGraph) then
    exit;

  hr := pGraph.EnumFilters(pEnum);
  if FAILED(hr) then
  begin
    SetLength(ErrMsg, 512);
    AMGetErrorText(hr, PChar(ErrMsg), 512);
    OutputText.Append('GraphDestroy: не могу создать Filter Enum - ' +
      Trim(ErrMsg));
  end;

  while SUCCEEDED(pEnum.Next(1, pFilter, nil)) do
  begin
    if pFilter = nil then
      break;
    pGraph.RemoveFilter(pFilter);
    pEnum.Reset;
    pFilter := nil;
  end;

  pEnum := nil;

  RemoveGraphFromRot(GraphID);
  CoUninitialize();

  OutputText.Append('Граф разобран');

  if Assigned(pAudioGrabCB) then
    pAudioGrabCB.Free;

  SetLength(ErrMsg, 0);
end;

procedure AudioTCReaderThread.Execute;
begin
  OutputText := TStringList.Create;
  normal_tc_cntr := 0;
  prev_tc_int := 0;
  DataIsReady := false;
  _SDIDiffMode := DiffMode;
  _SDIChannel1 := SDIChannel1;
  _SDIChannel2 := SDIChannel2;

  // получаем буферы под обработку
  SampleBuffer := GetMemory(bufsize * 2);
  // main buffer
  data16 := GetMemory(bufsize * 4);
  // indication buffer
  AudioData := GetMemory(bufsize * 4);
  _SDIAudioData := AudioData;

  if UseSdiIn then
    InitSDIDevice()
  else
    InitAudioDevice();

  while not Terminated do
  begin
    if DataIsReady then
      AudioDataToTC();
    Sleep(1);
  end;

  if UseSdiIn then
    DeInitSDIDevice()
  else
    DeInitAudioDevice();

  if Assigned(SampleBuffer) then
    FreeMemory(SampleBuffer);
  if Assigned(data16) then
    FreeMemory(data16);
  if Assigned(AudioData) then
    FreeMemory(AudioData);
end;

// callback процедура - вызывается по заполнению буфера (событие MM_WIM_DATA)
procedure OnWaveIn(hwi: HWAVEIN; uMsg, dwInstance, dwParam1,
  dwParam2: DWORD); stdcall;
var
  i, i1: integer;
  SampleValue: int16;
  _DiffMode: boolean;
begin
  if (uMsg <> WIM_DATA) or close_request then
    exit;

  // copy rxed data to temporary buffer for decoding
  Move(PWaveHdr(dwParam1)^.lpData^, data16^, bufsize * 4);

  // copy rxed data to temporary buffer for indication
  if AudioTCReaderThread(dwInstance).AudioDataCntr = 0 then
  begin
    Move(PWaveHdr(dwParam1)^.lpData^, AudioTCReaderThread(dwInstance)
      .AudioData^, bufsize * 4);
    AudioTCReaderThread(dwInstance).AudioDataCntr := bufsize;
  end;

  // i := pWaveHdr(dwParam1)^.dwBytesRecorded;
  // marking buffer as processed
  PWaveHdr(dwParam1)^.dwFlags := (PWaveHdr(dwParam1)^.dwFlags) and
    not WHDR_DONE;
  // copy buffer in empty
  WaveInAddBuffer(hwi, pBufHead[PWaveHdr(dwParam1)^.dwUser], SizeOf(TWaveHdr));

  _DiffMode := AudioTCReaderThread(dwInstance).DiffMode;
  if _DiffMode or (AudioTCReaderThread(dwInstance).UseChannel = 0) then
    i1 := 0
  else
    i1 := 1;

  for i := 0 to bufsize - 1 do
  begin
    if _DiffMode then
    begin
      SampleValue := data16^[i1] div 2;
      inc(i1);
      SampleValue := SampleValue - (data16^[i1] div 2);
      inc(i1);
    end
    else
    begin
      SampleValue := data16^[i1];
      inc(i1, 2);
    end; // if DiffMode

    SampleBuffer^[i] := SampleValue;
  end; // for

  DataIsReady := true;
end;

procedure AudioTCReaderThread.InitAudioDevice;
var
  i: integer;
  InDevNum: Word;
  SelectedMixer: Cardinal;
  MixerCaps: TMixerCaps;
  MyMixerLine: TMixerLine;
  MixerLineControls: TMixerLineControls;
  MyMixerControl: array of TMixerControl;
  Mixer_Lists: array of TMixerControlDetailsListText;
  Mixer_Value: array of TMixerControlDetailsBoolean;
  //
  SourceLineFound: boolean;
  SourceLineID: Cardinal;
  SourceLineName: string;
  //
  pWaveFormatHeader: PWaveFormatEx;
  hWaveFormatHeader: THANDLE;
  BufLen: Word;
  //
  ErrTxt: array [0 .. 511] of char;
  RResult: MMRESULT;
begin
  InDevNum := waveInGetNumDevs();

  if SelectedDevice >= InDevNum then
    SelectedDevice := 0;

  if AutoLevelEnabled then
  begin
    try
      // get WaveIn mixer ID
      RResult := mixerGetID(SelectedDevice, SelectedMixer,
        MIXER_OBJECTF_WAVEIN);
      if RResult <> 0 then
      begin
        waveInGetErrorText(RResult, ErrTxt, 512);
        raise EMyOwnException.Create('Can not get mixer for selected device: '
          + ErrTxt);
      end;

      // open mixer device
      RResult := MixerOpen(@MixerHandler, SelectedDevice, 0, 0,
        MIXER_OBJECTF_WAVEIN);
      if RResult <> 0 then
      begin
        waveInGetErrorText(RResult, ErrTxt, 512);
        raise EMyOwnException.Create('MixerOpen failed: ' + ErrTxt);
      end;

      // get mixer capabilities
      RResult := mixerGetDevCaps(MixerHandler, @MixerCaps, SizeOf(TMixerCaps));
      if RResult <> 0 then
      begin
        waveInGetErrorText(RResult, ErrTxt, 512);
        raise EMyOwnException.Create('Mixer #' + inttostr(SelectedMixer) +
          ' - can not get properties: ' + ErrTxt);
      end;

      OutputText.Append('Selected mixer device: ' + MixerCaps.szPname);

      // get line information for destination line for wave in
      MyMixerLine.dwComponentType := MIXERLINE_COMPONENTTYPE_DST_WAVEIN;
      MyMixerLine.cbStruct := SizeOf(TMixerLine);

      RResult := mixerGetLineInfo(MixerHandler, @MyMixerLine,
        MIXER_GETLINEINFOF_COMPONENTTYPE);
      if RResult <> 0 then
      begin
        waveInGetErrorText(RResult, ErrTxt, 512);
        raise EMyOwnException.Create('MixerGetLineInfo for DST failed: '
          + ErrTxt);
      end;

      if MyMixerLine.cConnections = 1 then
      begin // only one line as source - direct connection
        // get line information for source line
        MyMixerLine.dwSource := 0;
        MyMixerLine.cbStruct := SizeOf(TMixerLine);

        RResult := mixerGetLineInfo(MixerHandler, @MyMixerLine,
          MIXER_GETLINEINFOF_SOURCE);
        if RResult <> 0 then
        begin
          waveInGetErrorText(RResult, ErrTxt, 512);
          raise EMyOwnException.Create('MixerGetLineInfo for SRC failed: '
            + ErrTxt);
        end;

        SourceLineID := MyMixerLine.dwLineID;
        SourceLineName := MyMixerLine.szName;
      end
      else
      begin // not only 1 source - working with muxer
        // get controls for destination line
        SetLength(MyMixerControl, MyMixerLine.cControls);
        ZeroMemory(Addr(MyMixerControl[0]), SizeOf(TMixerControl) *
          MyMixerLine.cControls);

        MixerLineControls.cbStruct := SizeOf(TMixerLineControls);
        MixerLineControls.dwLineID := MyMixerLine.dwLineID;
        MixerLineControls.cControls := MyMixerLine.cControls;
        MixerLineControls.cbmxctrl := SizeOf(TMixerControl);
        MixerLineControls.pamxctrl := @MyMixerControl[0];

        RResult := mixerGetLineControls(MixerHandler, @MixerLineControls,
          MIXER_GETLINECONTROLSF_ALL);
        if RResult <> 0 then
        begin
          waveInGetErrorText(RResult, ErrTxt, 512);
          raise EMyOwnException.Create('MixerGetLineControls - error: '
            + ErrTxt);
        end;

        // get list of source lines for source muxer
        SetLength(Mixer_Lists, MyMixerControl[0].cMultipleItems);
        ZeroMemory(Addr(Mixer_Lists[0]), SizeOf(TMixerControlDetailsListText) *
          MyMixerControl[0].cMultipleItems);
        MixerControlDetails.cbStruct := SizeOf(TMixerControlDetails);
        MixerControlDetails.dwControlID := MyMixerControl[0].dwControlID;
        MixerControlDetails.cMultipleItems := MyMixerControl[0].cMultipleItems;
        MixerControlDetails.cChannels := MyMixerLine.cChannels;
        MixerControlDetails.cbDetails := SizeOf(TMixerControlDetailsListText);
        MixerControlDetails.paDetails := Addr(Mixer_Lists[0]);

        RResult := mixerGetControlDetails(MixerHandler, @MixerControlDetails,
          MIXER_GETCONTROLDETAILSF_LISTTEXT or MIXER_OBJECTF_HMIXER);
        if RResult <> 0 then
        begin
          waveInGetErrorText(RResult, ErrTxt, 512);
          raise EMyOwnException.Create
            ('MixerGetControlDetails for mux list failed: ' + ErrTxt);
        end;

        // get values for source muxer
        SetLength(Mixer_Value, MyMixerControl[0].cMultipleItems);
        MixerControlDetails.cbStruct := SizeOf(TMixerControlDetails);
        MixerControlDetails.dwControlID := MyMixerControl[0].dwControlID;
        MixerControlDetails.cMultipleItems := MyMixerControl[0].cMultipleItems;
        MixerControlDetails.cChannels := MyMixerLine.cChannels;
        MixerControlDetails.cbDetails := SizeOf(Mixer_Value);
        MixerControlDetails.paDetails := Addr(Mixer_Value[0]);

        RResult := mixerGetControlDetails(MixerHandler, @MixerControlDetails,
          MIXER_GETCONTROLDETAILSF_VALUE or MIXER_OBJECTF_HMIXER);
        if RResult <> 0 then
        begin
          waveInGetErrorText(RResult, ErrTxt, 512);
          raise EMyOwnException.Create
            ('MixerGetControlDetails for mux value failed: ' + ErrTxt);
        end;

        // search for connected source line
        SourceLineID := 0;
        SourceLineFound := false;
        for i := 0 to MixerControlDetails.cMultipleItems - 1 do
        begin
          if integer(Mixer_Value[i]) <> 0 then
          begin
            SourceLineID := Mixer_Lists[i].dwParam1;
            SourceLineName := Mixer_Lists[i].szName;
            SourceLineFound := true;
            break;
          end;
        end;

        if not SourceLineFound then
          raise EMyOwnException.Create('Can not find active Muxer Line ');
      end;

      // get information for selected source line
      ZeroMemory(Addr(MyMixerLine), SizeOf(MyMixerLine));
      MyMixerLine.dwLineID := SourceLineID;
      MyMixerLine.cbStruct := SizeOf(TMixerLine);

      RResult := mixerGetLineInfo(MixerHandler, @MyMixerLine,
        MIXER_GETLINEINFOF_LINEID);
      if RResult <> 0 then
      begin
        waveInGetErrorText(RResult, ErrTxt, 512);
        raise EMyOwnException.Create('MixerGetLineInfo for LINEID failed: '
          + ErrTxt);
      end;

      // get volume control for selected source line
      SetLength(MyMixerControl, 1);
      ZeroMemory(Addr(MyMixerControl[0]), SizeOf(TMixerControl));
      ZeroMemory(Addr(MixerLineControls), SizeOf(TMixerLineControls));
      MixerLineControls.cbStruct := SizeOf(TMixerLineControls);
      MixerLineControls.dwLineID := SourceLineID;
      MixerLineControls.cControls := 1;
      MixerLineControls.dwControlType := MIXERCONTROL_CONTROLTYPE_VOLUME;
      MixerLineControls.cbmxctrl := SizeOf(TMixerControl);
      MixerLineControls.pamxctrl := Addr(MyMixerControl[0]);

      RResult := mixerGetLineControls(MixerHandler, @MixerLineControls,
        MIXER_GETLINECONTROLSF_ONEBYTYPE);
      if RResult <> 0 then
      begin
        waveInGetErrorText(RResult, ErrTxt, 512);
        raise EMyOwnException.Create('MixerGetLineControls - error: ' + ErrTxt);
      end;

      // get volume control details for selected source line
      MixerMin := MyMixerControl[0].Bounds.dwMinimum;
      MixerMax := MyMixerControl[0].Bounds.dwMaximum;

      MixerControlDetails.cbStruct := SizeOf(TMixerControlDetails);
      MixerControlDetails.dwControlID := MyMixerControl[0].dwControlID;
      MixerControlDetails.cMultipleItems := MyMixerControl[0].cMultipleItems;
      MixerControlDetails.cChannels := MyMixerLine.cChannels;
      MixerControlDetails.cbDetails := SizeOf(MixerLevel);
      MixerControlDetails.paDetails := Addr(MixerLevel);

      RResult := mixerGetControlDetails(MixerHandler, @MixerControlDetails,
        MIXER_GETCONTROLDETAILSF_VALUE or MIXER_OBJECTF_HMIXER);
      if RResult <> 0 then
      begin
        waveInGetErrorText(RResult, ErrTxt, 512);
        raise EMyOwnException.Create('MixerGetControlDetails - error: '
          + ErrTxt);
      end;

      OutputText.Append
        (format('Выбрана линия на запись: "%s", регулятор %s на: %d (%d..%d)',
        [SourceLineName, MyMixerControl[0].szName, MixerLevel.dwValue, MixerMin,
        MixerMax]));

      OutputText.Append('Авторегулировка уровня включена');
    except
      on E: EMyOwnException do
      begin
        OutputText.Append(E.Message);
        AutoLevelEnabled := false;
      end;
      else
      begin
        OutputText.Append('Unknown error');
        AutoLevelEnabled := false;
      end;
    end;
  end;

  if DiffMode then
    OutputText.Append('Differential input mode')
  else if UseChannel = 0 then
    OutputText.Append('TC from left channel')
  else
    OutputText.Append('TC from right channel');

  try
    hWaveFormatHeader := LocalAlloc(LMEM_MOVEABLE, SizeOf(TWaveFormatEx));
    if hWaveFormatHeader = 0 then
      raise EMyOwnException.Create('WaveFormatEx local alloc error');
    pWaveFormatHeader := LocalLock(hWaveFormatHeader);
    if pWaveFormatHeader = nil then
      raise EMyOwnException.Create('WaveFormatEx local lock error');

    // заполняем хидер
    with pWaveFormatHeader^ do
    begin
      wFormatTag := WAVE_FORMAT_PCM;
      nChannels := 2;
      nSamplesPerSec := fs;
      wBitsPerSample := 16;
      nBlockAlign := nChannels * (wBitsPerSample div 8);
      nAvgBytesPerSec := nSamplesPerSec * nBlockAlign;
      cbSize := 0;
    end;

    RResult := WaveInOpen(@WaveIn, SelectedDevice, pWaveFormatHeader,
      DWORD(@OnWaveIn), DWORD(Self), CALLBACK_FUNCTION);
    if RResult <> MMSYSERR_NOERROR then
    begin
      LocalUnlock(hWaveFormatHeader);
      LocalFree(hWaveFormatHeader);
      waveInGetErrorText(RResult, ErrTxt, 512);
      raise EMyOwnException.Create('WaveInOpen error: ' + ErrTxt);
    end;

    // длина буфера в байтах
    BufLen := pWaveFormatHeader^.nBlockAlign * bufsize;

    LocalUnlock(hWaveFormatHeader);
    LocalFree(hWaveFormatHeader);

    // получаем память под заголовки буферов и сами буфера
    for i := 0 to bufnum - 1 do
    begin
      hBufHead[i] := GlobalAlloc(GMEM_MOVEABLE or GMEM_SHARE, SizeOf(TWaveHdr));
      if hBufHead[i] = 0 then
        raise EMyOwnException.Create('Buffer header #' + inttostr(i) +
          ' global alloc error');
      pBufHead[i] := GlobalLock(hBufHead[i]);
      if pBufHead[i] = nil then
        raise EMyOwnException.Create('Buffer header #' + inttostr(i) +
          ' global lock error');
    end;

    // заполняем заголовок буферов
    for i := 0 to bufnum - 1 do
    begin
      with pBufHead[i]^ do
      begin
        hBufHeadMem[i] := GlobalAlloc(GMEM_MOVEABLE or GMEM_SHARE, BufLen);
        if hBufHeadMem[i] = 0 then
          raise EMyOwnException.Create('Buffer memory header #' + inttostr(i) +
            ' global alloc error');
        lpData := GlobalLock(hBufHeadMem[i]);
        if lpData = nil then
          raise EMyOwnException.Create('Buffer memory header #' + inttostr(i) +
            ' global lock error');
        dwBufferLength := BufLen;
        dwFlags := 0;
        dwUser := i;
      end;
    end;

    // подготавливаем буферы
    for i := 0 to bufnum - 1 do
    begin
      RResult := WaveInPrepareHeader(WaveIn, pBufHead[i], SizeOf(TWaveHdr));
      if RResult <> MMSYSERR_NOERROR then
      begin
        waveInGetErrorText(RResult, ErrTxt, 512);
        raise EMyOwnException.Create('WaveInPrepareHeader error: ' + ErrTxt);
      end;
    end;

    // подключаем буферы к устройству
    for i := 0 to bufnum - 1 do
    begin
      RResult := WaveInAddBuffer(WaveIn, pBufHead[i], SizeOf(TWaveHdr));
      if RResult <> MMSYSERR_NOERROR then
      begin
        waveInGetErrorText(RResult, ErrTxt, 512);
        raise EMyOwnException.Create('WaveInAddBuffer error: ' + ErrTxt);
      end;
    end;

    // запускаем захват потока
    RResult := WaveInStart(WaveIn);
    if RResult <> MMSYSERR_NOERROR then
    begin
      waveInGetErrorText(RResult, ErrTxt, 512);
      raise EMyOwnException.Create('WaveInStart error: ' + ErrTxt);
    end;

  except
    on E: EMyOwnException do
    begin
      OutputText.Append(E.Message);
    end;
    else
      OutputText.Append('Unknown error');
  end;

end;

procedure AudioTCReaderThread.InitSDIDevice;
var
  i: integer;
  hr: HResult;
  iCount, iSize: integer;
  ModeOk: boolean;
  NeededSubType: TGUID;
  was_any_error: boolean;
  //
  CaptureName: String;
  VendorInfo: PChar;
  //
  pAMStreamConfig: IAMStreamConfig;
  currentMediaType: PAMMEDIATYPE;
  vscc: VIDEO_STREAM_CONFIG_CAPS;
  //
  enumPins: IEnumPins;
  pin: IPin;
  pinDir: _PinDirection;

  ascc: AUDIO_STREAM_CONFIG_CAPS;

  pVCap: IBaseFilter;
  DLVoutpin: IPin;

  pACap: IBaseFilter;
  DLAoutpin: IPin;

  pSGAudio: IBaseFilter;
  SGAudioinpin, SGAudiooutpin: IPin;
  SampleGrabberAudio: ISampleGrabber;
  pAudioGrabCB: TAudioGrabCB;

  // null renderer filter related
  pNullRenderer: IBaseFilter;
  NullRendererInPin: IPin;

  procedure ConnectPins(PinFrom, PinTo: IPin;
    SuccessString, ErrorString: string);
  var
    hr: HResult;
  begin
    hr := pGraph.Connect(PinFrom, PinTo);
    if SUCCEEDED(hr) then
      OutputText.Append(SuccessString)
    else
    begin
      AMGetErrorText(hr, PWideChar(ErrMsg), 512);
      raise EMyOwnException.Create(ErrorString + Trim(ErrMsg));
    end;
  end;

begin

  was_any_error := true;

  pAudioGrabCB := TAudioGrabCB.Create(nil);

  try
    CoInitialize(nil);

    pGraph := nil;
    pBuild := nil;

    // create Capture Graph Builder
    hr := CoCreateInstance(CLSID_CaptureGraphBuilder2, nil,
      CLSCTX_INPROC_SERVER, IID_ICaptureGraphBuilder2, pBuild);
    if FAILED(hr) then
    begin
      AMGetErrorText(hr, PChar(ErrMsg), 512);
      raise EMyOwnException.Create('Не могу создать Capture Graph Builder 2 - '
        + ErrMsg);
    end;

    // create graph
    hr := CoCreateInstance(CLSID_FilterGraph, nil, CLSCTX_INPROC_SERVER,
      IID_IGraphBuilder, pGraph);
    if FAILED(hr) then
    begin
      AMGetErrorText(hr, PChar(ErrMsg), 512);
      raise EMyOwnException.Create('Не могу создать Filter Graph - ' +
        Trim(ErrMsg));
    end;

    // link graph to builder
    hr := pBuild.SetFiltergraph(pGraph);
    if FAILED(hr) then
    begin
      AMGetErrorText(hr, PChar(ErrMsg), 512);
      raise EMyOwnException.Create('Не могу привязать Graph to Builder - ' +
        Trim(ErrMsg));
    end;

    hr := CoCreateInstance(DLV_CLSID, nil, CLSCTX_INPROC_SERVER,
      IID_IBaseFilter, pVCap);
    if FAILED(hr) then
    begin
      AMGetErrorText(hr, PChar(ErrMsg), 512);
      raise EMyOwnException.Create('Не могу создать Decklink Video Capture - ' +
        Trim(ErrMsg));
    end;
    if pVCap = nil then
    begin
      raise EMyOwnException.Create('Не могу найти video capture device');
    end;

    hr := pVCap.QueryVendorInfo(VendorInfo);
    if FAILED(hr) then
    begin
      AMGetErrorText(hr, PWideChar(ErrMsg), 512);
      raise EMyOwnException.Create('Не могу получить video VendorInfo - ' +
        Trim(ErrMsg));
    end;

    CaptureName := VendorInfo;
    OutputText.Append('Video capture device - ' + CaptureName);

    hr := pGraph.AddFilter(pVCap, PChar(CaptureName));
    if FAILED(hr) then
    begin
      AMGetErrorText(hr, PWideChar(ErrMsg), 512);
      raise EMyOwnException.Create
        ('Не могу добавить в граф video capture device - ' + Trim(ErrMsg));
    end;

    hr := pBuild.FindInterface(@PIN_CATEGORY_CAPTURE, @MEDIATYPE_Video, pVCap,
      IID_IAMStreamConfig, pAMStreamConfig);
    if FAILED(hr) then
    begin
      AMGetErrorText(hr, PWideChar(ErrMsg), 512);
      raise EMyOwnException.Create('Не могу получить video StreamConfig - ' +
        Trim(ErrMsg));
    end;

    hr := pAMStreamConfig.GetNumberOfCapabilities(iCount, iSize);
    if FAILED(hr) then
    begin
      AMGetErrorText(hr, PWideChar(ErrMsg), 512);
      raise EMyOwnException.Create
        ('Не могу получить video NumberOfCapabilities - ' + Trim(ErrMsg));
    end;

    if CapWidth > 720 then
      NeededSubType := StringToGuid('{43594448-0000-0010-8000-00AA00389B71}')
    else
      NeededSubType := MEDIASUBTYPE_UYVY;

    ModeOk := false;
    for i := 0 to iCount - 1 do
    begin
      hr := pAMStreamConfig.GetStreamCaps(i, currentMediaType, vscc);
      if SUCCEEDED(hr) then
      begin
        if IsEqualGUID(currentMediaType.subtype, NeededSubType) and
          (vscc.InputSize.cx = CapWidth) and (vscc.InputSize.cy = CapHeight) and
          ((vscc.MaxFrameInterval div 10) = NeededFrameInterval) then
        begin
          hr := pAMStreamConfig.SetFormat(currentMediaType);
          if SUCCEEDED(hr) then
          begin
            ModeOk := true;
            break;
          end;
        end;
      end;
    end;

    if not ModeOk then
    begin
      raise EMyOwnException.Create
        ('Не могу установить требуемый режим видеозахвата');
    end;

    // search Decklink video output pin
    DLVoutpin := nil;
    hr := pVCap.enumPins(enumPins);
    if SUCCEEDED(hr) then
    begin
      while (enumPins.Next(1, pin, nil) = S_OK) do
      begin
        pin.QueryDirection(pinDir);
        if pinDir = pindir_output then
        begin
          DLVoutpin := pin;
          break;
        end;
      end;
    end
    else
    begin
      AMGetErrorText(hr, PWideChar(ErrMsg), 512);
      raise EMyOwnException.Create('Не могу enum пины decklink video - ' +
        Trim(ErrMsg));
    end;

    if DLVoutpin = nil then
    begin
      raise EMyOwnException.Create('Не могу найти video capture output pin');
    end;

    // create audio capture device
    hr := CoCreateInstance(DLA_CLSID, nil, CLSCTX_INPROC_SERVER,
      IID_IBaseFilter, pACap);
    if FAILED(hr) then
    begin
      AMGetErrorText(hr, PWideChar(ErrMsg), 512);
      raise EMyOwnException.Create('Не могу создать audio capture  - ' +
        Trim(ErrMsg));
    end;

    if pACap = nil then
      raise EMyOwnException.Create('Не могу найти audio capture device');

    hr := pACap.QueryVendorInfo(VendorInfo);
    if FAILED(hr) then
    begin
      AMGetErrorText(hr, PWideChar(ErrMsg), 512);
      raise EMyOwnException.Create('Не могу получить audio VendorInfo - ' +
        Trim(ErrMsg));
    end;

    CaptureName := VendorInfo;
    OutputText.Append('Audio capture device - ' + CaptureName);

    hr := pGraph.AddFilter(pACap, PChar(CaptureName));
    if FAILED(hr) then
    begin
      AMGetErrorText(hr, PWideChar(ErrMsg), 512);
      raise EMyOwnException.Create
        ('Не могу добавить в граф audio capture device - ' + Trim(ErrMsg));
    end;

    hr := pBuild.FindInterface(@PIN_CATEGORY_CAPTURE, @MEDIATYPE_Audio, pACap,
      IID_IAMStreamConfig, pAMStreamConfig);
    if FAILED(hr) then
    begin
      AMGetErrorText(hr, PWideChar(ErrMsg), 512);
      raise EMyOwnException.Create('Не могу получить audio StreamConfig - ' +
        Trim(ErrMsg));
    end;

    hr := pAMStreamConfig.GetNumberOfCapabilities(iCount, iSize);
    if FAILED(hr) then
    begin
      AMGetErrorText(hr, PWideChar(ErrMsg), 512);
      raise EMyOwnException.Create
        ('Не могу получить audio NumberOfCapabilities - ' + Trim(ErrMsg));
    end;

    ModeOk := false;
    for i := 0 to iCount - 1 do
    begin
      hr := pAMStreamConfig.GetStreamCaps(i, currentMediaType, ascc);
      if SUCCEEDED(hr) then
      begin
        if (ascc.MinimumBitsPerSample = 16) and
          (ascc.MinimumSampleFrequency = 48000) and (ascc.MinimumChannels = 16)
        then
        begin
          hr := pAMStreamConfig.SetFormat(currentMediaType);
          if SUCCEEDED(hr) then
          begin
            ModeOk := true;
            break;
          end;
        end;
      end;
    end;

    if not ModeOk then
    begin
      raise EMyOwnException.Create
        ('Не могу установить требуемый режим аудиозахвата');
    end;

    // search Decklink audio output pin
    DLAoutpin := nil;
    hr := pACap.enumPins(enumPins);
    if SUCCEEDED(hr) then
    begin
      while (enumPins.Next(1, pin, nil) = S_OK) do
      begin
        pin.QueryDirection(pinDir);
        if pinDir = pindir_output then
        begin
          DLAoutpin := pin;
          break;
        end;
      end;
    end
    else
    begin
      AMGetErrorText(hr, PWideChar(ErrMsg), 512);
      raise EMyOwnException.Create('Не могу enum пины decklink audio - ' +
        Trim(ErrMsg));
    end;
    if DLAoutpin = nil then
    begin
      raise EMyOwnException.Create('Не могу найти audio capture output pin');
    end;

    hr := CoCreateInstance(CLSID_SampleGrabber, nil, CLSCTX_INPROC_SERVER,
      IID_IBaseFilter, pSGAudio);
    if FAILED(hr) then
    begin
      AMGetErrorText(hr, PChar(ErrMsg), 512);
      raise EMyOwnException.Create('Не могу создать Audio SampleGrabber - ' +
        Trim(ErrMsg));
    end; // CLSID_SampleGrabber

    // adjusting audio sample grabber
    hr := pSGAudio.QueryInterface(IID_ISampleGrabber, SampleGrabberAudio);
    if FAILED(hr) then
    begin
      AMGetErrorText(hr, PChar(ErrMsg), 512);
      raise EMyOwnException.Create
        ('Не могу получить интерфейс Audio SampleGrabber - ' + Trim(ErrMsg));
    end;

    // set buffer on
    hr := SampleGrabberAudio.SetBufferSamples(true);
    if FAILED(hr) then
    begin
      AMGetErrorText(hr, PChar(ErrMsg), 512);
      raise EMyOwnException.Create(Trim(ErrMsg));
    end;

    // set oneshot false
    hr := SampleGrabberAudio.SetOneShot(false);
    if FAILED(hr) then
    begin
      AMGetErrorText(hr, PChar(ErrMsg), 512);
      raise EMyOwnException.Create(Trim(ErrMsg));
    end;

    // set call back
    hr := SampleGrabberAudio.SetCallback(pAudioGrabCB, 0);
    if FAILED(hr) then
    begin
      AMGetErrorText(hr, PChar(ErrMsg), 512);
      raise EMyOwnException.Create(Trim(ErrMsg));
    end;

    hr := pGraph.AddFilter(pSGAudio, 'Audio sample grabber');
    if SUCCEEDED(hr) then
      OutputText.Append('Audio sample grabber added')
    else
    begin
      AMGetErrorText(hr, PChar(ErrMsg), 512);
      raise EMyOwnException.Create
        ('Не могу добавить Audio sample grabber в граф - ' + Trim(ErrMsg));
    end;

    // search Audio sample Grabber input and output pins
    SGAudioinpin := nil;
    SGAudiooutpin := nil;

    hr := pSGAudio.enumPins(enumPins);
    if SUCCEEDED(hr) then
    begin
      while (enumPins.Next(1, pin, nil) = S_OK) do
      begin
        pin.QueryDirection(pinDir);
        if pinDir = pindir_input then
          SGAudioinpin := pin
        else
          SGAudiooutpin := pin;
      end;
    end
    else
    begin
      AMGetErrorText(hr, PChar(ErrMsg), 512);
      raise EMyOwnException.Create('Не могу enum пины audio sample grabber - ' +
        Trim(ErrMsg));
    end;

    if not Assigned(SGAudioinpin) then
      raise EMyOwnException.Create
        ('Не могу найти Audio sample grabber input pin');

    if not Assigned(SGAudiooutpin) then
      raise EMyOwnException.Create
        ('Не могу найти Audio sample grabber output pin');

    hr := CoCreateInstance(CLSID_NullRenderer, nil, CLSCTX_INPROC_SERVER,
      IID_IBaseFilter, pNullRenderer);
    if FAILED(hr) then
    begin
      AMGetErrorText(hr, PChar(ErrMsg), 512);
      raise EMyOwnException.Create('Не могу создать NullRenderer - ' +
        Trim(ErrMsg));
    end;

    ConnectPins(DLAoutpin, SGAudioinpin,
      'Decklink audio out -> Audio sample grabber in connected',
      'Decklink audio out -> Audio sample grabber in failed - ');

    hr := pGraph.AddFilter(pNullRenderer, 'NullRenderer');
    if SUCCEEDED(hr) then
    begin
      OutputText.Append('Null Renderer added');
    end
    else
    begin
      AMGetErrorText(hr, PChar(ErrMsg), 512);
      raise EMyOwnException.Create('Не могу добавить Null Renderer в граф - ' +
        Trim(ErrMsg));
    end;

    // search renderer input pin
    NullRendererInPin := nil;
    hr := pNullRenderer.enumPins(enumPins);
    if SUCCEEDED(hr) then
    begin
      while (enumPins.Next(1, pin, nil) = S_OK) do
      begin
        pin.QueryDirection(pinDir);
        if pinDir = pindir_input then
        begin
          NullRendererInPin := pin;
          break;
        end;
      end;
    end
    else
    begin
      AMGetErrorText(hr, PChar(ErrMsg), 512);
      raise EMyOwnException.Create('Не могу enum пины render - ' +
        Trim(ErrMsg));
    end;

    if not Assigned(NullRendererInPin) then
      raise EMyOwnException.Create('Не могу найти null renderer input pin');

    ConnectPins(SGAudiooutpin, NullRendererInPin,
      'Audio sample grabber out -> Null renderer in connected',
      'Audio sample grabber out -> Null renderer in failed - ');

    hr := pGraph.QueryInterface(IID_IMediaControl, pControl);
    if FAILED(hr) then
    begin
      AMGetErrorText(hr, PWideChar(ErrMsg), 512);
      raise EMyOwnException.Create('Не могу получить IID_IMediaControl - ' +
        Trim(ErrMsg));
    end;

    AddGraphToRot(pGraph, GraphID);

    was_any_error := false;
  except
    on E: EMyOwnException do
    begin
      OutputText.Append(E.Message);
    end;
    else
      OutputText.Append('Неизвестная ошибка');
  end;

  OutputText.Append('Граф собран');
  CoFreeUnusedLibraries();

  if not was_any_error then
    pControl.Run;
end;

{ TAudioGrabCB }

function TAudioGrabCB.BufferCB(SampleTime: Double; pBuffer: PByte;
  BufferLen: integer): HResult;
begin
  Result := S_OK;
end;

function TAudioGrabCB.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := inherited QueryInterface(IID, Obj);
end;

function TAudioGrabCB.SampleCB(SampleTime: Double;
  pSample: IMediaSample): HResult;
var
  pBuffer: PByte;
  pCh1, pCh2: PSmallInt;
  sample_size: integer;
  iTmp1, iTmp2: Int64;
  i, i1: integer;
  SampleValue: smallint;
begin
  pSample.GetTime(iTmp1, iTmp2);
  sample_size := pSample.GetActualDataLength;
  pSample.GetPointer(pBuffer);

  pCh1 := PSmallInt(pBuffer);
  pCh2 := PSmallInt(pBuffer);

  if (_SDIChannel1 > 0) and (_SDIChannel1 < 16) then
    inc(pCh1, _SDIChannel1);

  if (_SDIChannel2 = 0) and (_SDIChannel2 < 16) then
    inc(pCh2, _SDIChannel2);

  i1 := 0;
  for i := 0 to bufsize - 1 do
  begin
    if _SDIDiffMode then
      SampleValue := (pCh1^ div 2) - (pCh2^ div 2)
    else
      SampleValue := pCh1^;

    SampleBuffer^[i] := SampleValue;

    data16^[i1] := pCh1^;
    _SDIAudioData^[i1] := pCh1^;
    inc(i1);

    data16^[i1] := pCh2^;
    _SDIAudioData^[i1] := pCh2^;
    inc(i1);

    inc(pCh1, 16);
    inc(pCh2, 16);
  end; // for

  DataIsReady := true;

  Result := S_OK;
end;

function TAudioGrabCB._AddRef: integer;
begin
  Result := 2;
end;

function TAudioGrabCB._Release: integer;
begin
  Result := 1;
end;

end.
