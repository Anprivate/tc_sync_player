unit UnitClip;

interface

uses
  System.Sysutils, VCL.Forms, Winapi.Windows, Winapi.ActiveX,
  Winapi.DirectShow9, dsutil,
  MMsystem;

type
  TOneClip = class
    pGraph: IGraphBuilder;
    pControl: IMediaControl;
    pSeeking: IMediaSeeking;
    pSinkFilter: IBaseFilter;
    pEvent: IMediaEvent;
    FileName: string;
    GraphID: Integer;
    IsLoaded: boolean;
    IsUsed: boolean;
    StartPos: Int64;
    StopPos: Int64;
    Channel: Integer;
    constructor Create();
    destructor Destroy();
    procedure ClearGraph();
    procedure AddFilter(pFilter: IBaseFilter; FilterName: string);
    function AddGraphToRot(Graph: IFilterGraph; out ID: Integer): HRESULT;
    function RemoveGraphFromRot(ID: Integer): HRESULT;
  private
    ErrMsg: string;
  end;

  EMyOwnException = class(Exception);

implementation

{ TOneClip }

procedure TOneClip.ClearGraph;
var
  hr: HRESULT;
  pEnum: IEnumFilters;
  pFilter: IBaseFilter;
begin
  pControl.Stop;
  hr := pGraph.EnumFilters(pEnum);
  if FAILED(hr) then
  begin
    AMGetErrorText(hr, PWideChar(ErrMsg), 512);
    raise EMyOwnException.Create('Не могу создать Filter Enum - ' +
      Trim(ErrMsg));
  end;

  while Succeeded(pEnum.Next(1, pFilter, nil)) do
  begin
    if pFilter = nil then
      break;
    pGraph.RemoveFilter(pFilter);
    pEnum.Reset;
    pFilter := nil;
    Application.ProcessMessages;
  end;

  pEnum := nil;

  IsLoaded := false;
{$IFDEF DEBUG}
  OutputDebugString(PWideChar('ClearGraph on ch.' + inttostr(Channel) + ':' +
    FileName));
{$ENDIF}
end;

constructor TOneClip.Create;
var
  hr: HRESULT;
begin
  // create graph
  hr := CoCreateInstance(CLSID_FilterGraph, nil, CLSCTX_INPROC_SERVER,
    IID_IGraphBuilder, pGraph);
  if FAILED(hr) then
  begin
    AMGetErrorText(hr, PWideChar(ErrMsg), 512);
    raise EMyOwnException.Create('Не могу создать Filter Graph - ' +
      Trim(ErrMsg));
  end;

  AddGraphToRot(pGraph, GraphID);

  hr := pGraph.QueryInterface(IID_IMediaControl, pControl);
  if FAILED(hr) then
  begin
    AMGetErrorText(hr, PWideChar(ErrMsg), 512);
    raise EMyOwnException.Create('Не могу получить IID_IMediaControl - ' +
      Trim(ErrMsg));
  end;

  hr := pGraph.QueryInterface(IID_IMediaSeeking, pSeeking);
  if FAILED(hr) then
  begin
    AMGetErrorText(hr, PWideChar(ErrMsg), 512);
    raise EMyOwnException.Create('Не могу получить IID_IMediaSeeking - ' +
      Trim(ErrMsg));
  end;

  hr := pGraph.QueryInterface(IID_IMediaEvent, pEvent);
  if FAILED(hr) then
  begin
    AMGetErrorText(hr, PWideChar(ErrMsg), 512);
    raise EMyOwnException.Create('Не могу получить IID_IMediaEvent - ' +
      Trim(ErrMsg));
  end;

  IsLoaded := false;
end;

destructor TOneClip.Destroy;
begin
  RemoveGraphFromRot(GraphID);
  ClearGraph();
end;

procedure TOneClip.AddFilter(pFilter: IBaseFilter; FilterName: string);
var
  hr: HRESULT;
begin
  hr := pGraph.AddFilter(pFilter, PWideChar(FilterName));
  if FAILED(hr) then
  begin
    AMGetErrorText(hr, PWideChar(ErrMsg), 512);
    raise EMyOwnException.Create('Не могу получить IID_IMediaSeeking - ' +
      Trim(ErrMsg));
  end;
end;

function TOneClip.AddGraphToRot(Graph: IFilterGraph; out ID: Integer): HRESULT;
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

function TOneClip.RemoveGraphFromRot(ID: Integer): HRESULT;
var
  ROT: IRunningObjectTable;
begin
  Result := GetRunningObjectTable(0, ROT);
  if (Result <> S_OK) then
    exit;
  Result := ROT.Revoke(ID);
  ROT := nil;
end;

end.
