unit Fetching.OfflineHTTPClient;

interface
uses Classes, SysUtils, System.Net.HttpClient, System.Net.URLClient;

type
  IHTTPResponse = System.Net.HttpClient.IHTTPResponse;
  IHTTPRequest = System.Net.HttpClient.IHTTPRequest;

  TOfflineHTTPClient = class
  private
    Client: THTTPClient;
    function GetReceiveDataCallback: TReceiveDataCallback;
    procedure SetReceiveDataCallback(const Value: TReceiveDataCallback);
    function GetCustomHeaderValue(const AName: string): string;
    procedure SetCustomHeaderValue(const AName, Value: string);
  public
    constructor Create;
    destructor Destroy; override;

    property ReceiveDataCallback: TReceiveDataCallback read GetReceiveDataCallback write SetReceiveDataCallback;
    function GetRequest(const ARequestMethod, AURL: string): IHTTPRequest; overload;
    function Execute(const ARequest: IHTTPRequest; const AContentStream: TStream = nil;
      const AHeaders: TNetHeaders = nil): IHTTPResponse; overload;

    property CustomHeaders[const AName: string]: string read GetCustomHeaderValue write SetCustomHeaderValue;


  end;

implementation
uses Testing.Globals;

{ TOfflineHTTPClient }

constructor TOfflineHTTPClient.Create;
begin
  Client := THTTPClient.Create;
end;

destructor TOfflineHTTPClient.Destroy;
begin
  Client.Free;
  inherited;
end;

function TOfflineHTTPClient.Execute(const ARequest: IHTTPRequest;
  const AContentStream: TStream; const AHeaders: TNetHeaders): IHTTPResponse;
begin
{$IFDEF DEBUG}
  TestParameters.CheckOffline('TOfflineHTTPClient.Execute');
{$ENDIF}

  Result := Client.Execute(ARequest, AContentStream, AHeaders)
end;

function TOfflineHTTPClient.GetCustomHeaderValue(const AName: string): string;
begin
  Result := Client.CustomHeaders[AName];
end;

function TOfflineHTTPClient.GetReceiveDataCallback: TReceiveDataCallback;
begin
  Result := Client.ReceiveDataCallBack;
end;

function TOfflineHTTPClient.GetRequest(const ARequestMethod,
  AURL: string): IHTTPRequest;
begin
  Result := Client.GetRequest(ARequestMethod, AURL);
end;

procedure TOfflineHTTPClient.SetCustomHeaderValue(const AName, Value: string);
begin
  Client.CustomHeaders[AName] := Value;
end;

procedure TOfflineHTTPClient.SetReceiveDataCallback(
  const Value: TReceiveDataCallback);
begin
  Client.ReceiveDataCallBack := Value;
end;

end.
