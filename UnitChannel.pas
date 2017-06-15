unit UnitChannel;

interface

uses System.SysUtils, System.UITypes, WinApi.Windows, Vcl.Forms,
  WinApi.DirectShow9, WinApi.ActiveX,
  GMFBridgeLib_TLB, UnitClip;

type
  TStats = (Stop, Pause, Play);

  TOneChannel = class
  private
    AllClips: array of TOneClip;

    MyBridge: TGMFBridgeController;

    RenderGraph: TOneClip;

    pRendererVideo: IBaseFilter;
    pRendererAudio: IBaseFilter;
    pRenderSourceFilter: IBaseFilter;

    LastSettedPosition: Int64;

    function ReadCount: integer;
    procedure SetCount(Value: integer);
  public
    ActiveClip: TOneClip;
    NextClip: TOneClip;

    VideoBMGUID: TGuid;
    AudioBMGUID: TGuid;

    CurrentStatus: TStats;
    PrevStatus: TStats;

    Channel: integer;
    Color: TColor;
    AutoAdvanceCounter: integer;

    property ClipCount: integer read ReadCount write SetCount;
    procedure ReCreate();
    procedure SetNotify(hwnd: LONG_PTR; msg: integer);
    // main working actions
    procedure ClearRenderGraph;
    procedure PrepareRenderGraph(CurClip: TOneClip);
    procedure RunRenderGraph;
    procedure PauseRenderGraph;
    procedure StopRenderGraph;
    procedure PrepareSourceGraph(WhatClip: TOneClip);
    procedure JumpToNext(IsLoop: boolean = false);
    procedure StopOnEnd;
    procedure StopSources(unload: boolean = false);
    procedure PauseSources;
    function GetCurrentTime: Int64;
    procedure SetPosition(Position: Int64);
    // preparing actions
    procedure ListClear;
    function MarkClipAsUsed(WhatClip: TOneClip): boolean;
    procedure ListPurgeUnused;
    function GetUnusedClip: TOneClip;
    //
    constructor Create();
    destructor Destroy(); override;
  end;

implementation

{ OneChannel }
procedure RaiseIfFailed(hr: Hresult; MString: String);
var
  ErrMsg: string;
begin
  if FAILED(hr) then
  begin
    AMGetErrorText(hr, PWideChar(ErrMsg), 512);
    raise EMyOwnException.Create(MString + Trim(ErrMsg));
  end;
end;

procedure TOneChannel.ClearRenderGraph;
begin
  MyBridge.BridgeGraphs(nil, nil);

  if RenderGraph <> nil then
  begin
    RenderGraph.pControl.Stop;
    RenderGraph.ClearGraph;
  end;
end;

procedure TOneChannel.PrepareRenderGraph(CurClip: TOneClip);
var
  hr: Hresult;
begin
  ClearRenderGraph;

  hr := CoCreateInstance(VideoBMGUID, nil, CLSCTX_INPROC_SERVER,
    IID_IBaseFilter, pRendererVideo);
  RaiseIfFailed(hr, 'Не могу создать blackmagic video renderer - ');

  RenderGraph.AddFilter(pRendererVideo, 'Video');

  hr := CoCreateInstance(AudioBMGUID, nil, CLSCTX_INPROC_SERVER,
    IID_IBaseFilter, pRendererAudio);
  RaiseIfFailed(hr, 'Не могу создать blackmagic audio renderer - ');

  RenderGraph.AddFilter(pRendererAudio, 'Audio');

  RenderGraph.pControl.Stop;
  CurClip.pControl.Stop;
  // StopSources;

  pRenderSourceFilter := MyBridge.CreateRenderGraph(CurClip.pSinkFilter,
    RenderGraph.pGraph) as IBaseFilter;
  CurClip.pSeeking.SetPositions(CurClip.StartPos,
    AM_SEEKING_AbsolutePositioning, CurClip.StopPos,
    AM_SEEKING_AbsolutePositioning);
  MyBridge.BridgeGraphs(CurClip.pSinkFilter, pRenderSourceFilter);

  RenderGraph.pControl.Pause;
  PauseSources;

  ActiveClip := CurClip;
  LastSettedPosition := 0;

  AutoAdvanceCounter := 0;
end;

procedure TOneChannel.PrepareSourceGraph(WhatClip: TOneClip);
begin
  WhatClip.pSinkFilter := MyBridge.CreateSourceGraph(WhatClip.filename,
    WhatClip.pGraph) as IBaseFilter;
  WhatClip.pControl.Pause;
end;

constructor TOneChannel.Create;
begin
  RenderGraph := TOneClip.Create;

  // preparing bridge controller
  MyBridge := TGMFBridgeController.Create(nil);

  MyBridge.AddStream(1, eUncompressed, 0);
  MyBridge.AddStream(0, eUncompressed, 0);
  MyBridge.SetBufferMinimum(500);
end;

destructor TOneChannel.Destroy;
var
  i: integer;
begin
  ClearRenderGraph;

  freeandnil(MyBridge);
  freeandnil(RenderGraph);

  for i := 0 to Length(AllClips) - 1 do
  begin
    AllClips[i].pControl.Stop;
    AllClips[i].ClearGraph;
    freeandnil(AllClips[i]);
    Application.ProcessMessages;
  end;

  inherited Destroy();
end;

function TOneChannel.GetCurrentTime: Int64;
var
  tmp: Int64;
begin
  tmp := round(MyBridge.GetSegmentTime * 25) + LastSettedPosition;
  GetCurrentTime := tmp;
end;

procedure TOneChannel.JumpToNext(IsLoop: boolean = false);
begin
  MyBridge.BridgeGraphs(nil, nil);
  if IsLoop then
  begin
    ActiveClip.pControl.Pause;
    ActiveClip.pSeeking.SetPositions(ActiveClip.StartPos,
      AM_SEEKING_AbsolutePositioning, ActiveClip.StopPos,
      AM_SEEKING_AbsolutePositioning);
    MyBridge.BridgeGraphs(ActiveClip.pSinkFilter, pRenderSourceFilter);
  end
  else
  begin
    MyBridge.BridgeGraphs(NextClip.pSinkFilter, pRenderSourceFilter);
    ActiveClip := NextClip;
  end;
  LastSettedPosition := 0;
end;

procedure TOneChannel.ListClear;
var
  i: integer;
begin
  for i := 0 to ClipCount - 1 do
    AllClips[i].IsUsed := false;
end;

procedure TOneChannel.ListPurgeUnused;
var
  i: integer;
begin
  for i := 0 to ClipCount - 1 do
  begin
    with AllClips[i] do
      if (AllClips[i] <> nil) and (not IsUsed) and IsLoaded then
        ClearGraph;
    Application.ProcessMessages;
  end;
end;

function TOneChannel.MarkClipAsUsed(WhatClip: TOneClip): boolean;
var
  i: integer;
begin
  MarkClipAsUsed := false;
  if WhatClip = nil then
    Exit;
  for i := 0 to Length(AllClips) - 1 do
  begin
    if WhatClip = AllClips[i] then
    begin
      AllClips[i].IsUsed := true;
      MarkClipAsUsed := true;
      break;
    end;
  end;
end;

function TOneChannel.ReadCount: integer;
begin
  ReadCount := Length(AllClips);
end;

procedure TOneChannel.ReCreate;
var
  i: integer;
begin
  ClearRenderGraph;

  freeandnil(MyBridge);
  freeandnil(RenderGraph);

  for i := 0 to Length(AllClips) - 1 do
  begin
    AllClips[i].pControl.Stop;
    AllClips[i].ClearGraph;
    freeandnil(AllClips[i]);
    AllClips[i] := TOneClip.Create;
    AllClips[i].Channel := Channel;
    Application.ProcessMessages;
  end;

  RenderGraph := TOneClip.Create;

  // preparing bridge controller
  MyBridge := TGMFBridgeController.Create(nil);

  MyBridge.AddStream(1, eUncompressed, 0);
  MyBridge.AddStream(0, eUncompressed, 0);
  MyBridge.SetBufferMinimum(500);

  ActiveClip := nil;
  NextClip := nil;
  CurrentStatus := Stop;
end;

procedure TOneChannel.RunRenderGraph;
begin
  RenderGraph.pControl.Run;
  CurrentStatus := Play;
  AutoAdvanceCounter := 0;
end;

procedure TOneChannel.PauseRenderGraph;
begin
  RenderGraph.pControl.Pause;
  CurrentStatus := Pause;
  AutoAdvanceCounter := 0;
end;

procedure TOneChannel.StopRenderGraph;
begin
  RenderGraph.pControl.Stop;
  CurrentStatus := Stop;
  AutoAdvanceCounter := 0;
end;

procedure TOneChannel.SetCount(Value: integer);
var
  i: integer;
begin
  if Length(AllClips) > Value then
  begin
    repeat
      i := Length(AllClips) - 1;
      freeandnil(AllClips[i]);
      SetLength(AllClips, i);
    until i <= Value;
  end
  else
    while Length(AllClips) < Value do
    begin
      i := Length(AllClips);
      SetLength(AllClips, i + 1);
      AllClips[i] := TOneClip.Create;
      AllClips[i].Channel := Channel;
    end;
end;

procedure TOneChannel.SetNotify(hwnd: LONG_PTR; msg: integer);
begin
  MyBridge.SetNotify(hwnd, msg);
end;

procedure TOneChannel.SetPosition(Position: Int64);
var
  NewStart: Int64;
begin
  if (CurrentStatus = Pause) and (ActiveClip <> nil) then
  begin
    NewStart := ActiveClip.StartPos + 400000 * Position;
    if NewStart < (ActiveClip.StopPos - 10000000) then
    begin
      ActiveClip.pSeeking.SetPositions(NewStart, AM_SEEKING_AbsolutePositioning,
        NewStart, AM_SEEKING_NoPositioning);
      LastSettedPosition := Position;
    end;
  end;
end;

procedure TOneChannel.StopOnEnd;
var
  EventCode: integer;
begin
  MyBridge.NoMoreSegments;
  MyBridge.BridgeGraphs(nil, nil);
  while RenderGraph.pEvent.WaitForCompletion(0, EventCode) <> S_OK do
    Application.ProcessMessages;
  RenderGraph.pControl.Pause;
  CurrentStatus := Stop;
end;

procedure TOneChannel.StopSources(unload: boolean = false);
var
  i: integer;
begin
  for i := 0 to ClipCount - 1 do
    if AllClips[i] <> nil then
    begin
      AllClips[i].pControl.Stop;
      if unload then
        AllClips[i].ClearGraph;
    end;
{$IFDEF DEBUG}
  OutputDebugString(PWideChar('StopSources'));
{$ENDIF}
end;

function TOneChannel.GetUnusedClip: TOneClip;
var
  i: integer;
begin
  GetUnusedClip := nil;
  for i := 0 to ClipCount - 1 do
    if not AllClips[i].IsLoaded then
    begin
      GetUnusedClip := AllClips[i];
      break;
    end;
end;

procedure TOneChannel.PauseSources;
var
  i: integer;
begin
  for i := 0 to ClipCount - 1 do
    if AllClips[i] <> nil then
      AllClips[i].pControl.Pause;
{$IFDEF DEBUG}
  OutputDebugString(PWideChar('PauseSources'));
{$ENDIF}
end;

end.
