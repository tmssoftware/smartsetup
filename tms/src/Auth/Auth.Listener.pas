unit Auth.Listener;

{$i ../../tmssetup.inc}

interface

uses
  SysUtils, Classes, SyncObjs,
  IdHTTPServer, IdCustomHTTPServer, IdContext, IdSocketHandle;

type
  TLoopbackOutcome = (Received, TimedOut, Canceled);

  TLoopbackResult = record
    Outcome: TLoopbackOutcome;
    CallbackUrl: string;
  end;

  /// <summary>
  /// Minimal local HTTP listener that captures a single OAuth redirect on
  /// http://127.0.0.1:&lt;port&gt;/callback. It is generic protocol plumbing:
  /// it knows nothing about servers, configuration or console output.
  /// The response pages are static on purpose: nothing coming from the
  /// request is ever echoed back to the browser.
  /// </summary>
  TLoopbackListener = class
  strict private const
    CallbackPath = '/callback';
    PollMilliseconds = 250;
  strict private
    FServer: TIdHTTPServer;
    FEvent: TEvent;
    FLock: TCriticalSection;
    FCallbackUrl: string; // protected by FLock; written once by the Indy worker thread
    FSuccessHtml: string;
    FErrorHtml: string;
    procedure HandleCommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
  public
    constructor Create;
    destructor Destroy; override;

    // Binds to 127.0.0.1 on an OS-assigned ephemeral port and starts listening.
    procedure Start;
    function Port: Word;
    function RedirectUri: string;

    // Waits until the browser hits the callback, the timeout elapses, or
    // IsCanceled returns true. Polls in short slices so cancel stays responsive.
    // Call Stop (or destroy the listener) after this returns; both are done
    // from the caller's thread, never from the Indy worker thread.
    function WaitForCallback(const TimeoutSeconds: integer; const IsCanceled: TFunc<boolean>): TLoopbackResult;
    procedure Stop;

    property SuccessHtml: string read FSuccessHtml write FSuccessHtml;
    property ErrorHtml: string read FErrorHtml write FErrorHtml;
  end;

implementation

const
  DefaultSuccessHtml =
    '<!DOCTYPE html><html><head><meta charset="utf-8"><title>Signed in</title></head>'
    + '<body style="font-family: sans-serif; text-align: center; margin-top: 15%">'
    + '<h1>You are signed in</h1>'
    + '<p>You can close this window and return to the application.</p>'
    + '</body></html>';

  DefaultErrorHtml =
    '<!DOCTYPE html><html><head><meta charset="utf-8"><title>Sign in failed</title></head>'
    + '<body style="font-family: sans-serif; text-align: center; margin-top: 15%">'
    + '<h1>Sign in failed</h1>'
    + '<p>You can close this window and return to the application for details.</p>'
    + '</body></html>';

{ TLoopbackListener }

constructor TLoopbackListener.Create;
begin
  inherited Create;
  FServer := TIdHTTPServer.Create(nil);
  FServer.OnCommandGet := HandleCommandGet;
  FEvent := TEvent.Create(nil, True, False, '');
  FLock := TCriticalSection.Create;
  FSuccessHtml := DefaultSuccessHtml;
  FErrorHtml := DefaultErrorHtml;
end;

destructor TLoopbackListener.Destroy;
begin
  Stop;
  FServer.Free;
  FEvent.Free;
  FLock.Free;
  inherited;
end;

procedure TLoopbackListener.Start;
begin
  FServer.Bindings.Clear;
  var Binding := FServer.Bindings.Add;
  Binding.IP := '127.0.0.1';
  Binding.Port := 0; // let the OS choose an ephemeral port
  FServer.Active := True;
end;

function TLoopbackListener.Port: Word;
begin
  if not FServer.Active then
    raise Exception.Create('The OAuth callback listener is not active');
  Result := FServer.Bindings[0].Port;
end;

function TLoopbackListener.RedirectUri: string;
begin
  Result := 'http://127.0.0.1:' + IntToStr(Port) + CallbackPath;
end;

procedure TLoopbackListener.Stop;
begin
  if (FServer <> nil) and FServer.Active then
    FServer.Active := False;
end;

procedure TLoopbackListener.HandleCommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
begin
  // Ignore favicon requests, browser prefetching, etc.
  if not SameText(ARequestInfo.Document, CallbackPath) then
  begin
    AResponseInfo.ResponseNo := 404;
    exit;
  end;

  var IsError: Boolean;
  FLock.Acquire;
  try
    // Only the first callback wins; later hits just get the page again.
    if FCallbackUrl = '' then
    begin
      FCallbackUrl := 'http://127.0.0.1:' + IntToStr(Port) + ARequestInfo.Document;
      if ARequestInfo.UnparsedParams <> '' then
        FCallbackUrl := FCallbackUrl + '?' + ARequestInfo.UnparsedParams;
    end;
    IsError := ARequestInfo.Params.Values['error'] <> '';
  finally
    FLock.Release;
  end;

  AResponseInfo.ContentType := 'text/html';
  AResponseInfo.CharSet := 'utf-8';
  if IsError then
    AResponseInfo.ContentText := FErrorHtml
  else
    AResponseInfo.ContentText := FSuccessHtml;
  AResponseInfo.CloseConnection := True;

  // Indy only sends the response after this handler returns, but signaling the
  // event wakes the caller, which may Stop the server before that happens and
  // reset the connection, leaving the browser with an error or a blank page.
  // Flush the page to the browser first, so the event means "the browser got it".
  // WriteContent clears ContentText, so Indy will not send the body twice.
  try
    AResponseInfo.WriteContent;
  finally
    // Signal even if the browser dropped the connection mid-write: the callback
    // URL was already captured above and the sign in must still complete.
    FEvent.SetEvent;
  end;
end;

function TLoopbackListener.WaitForCallback(const TimeoutSeconds: integer;
  const IsCanceled: TFunc<boolean>): TLoopbackResult;
begin
  Result := Default(TLoopbackResult);
  var Deadline := TThread.GetTickCount64 + UInt64(TimeoutSeconds) * 1000;
  while True do
  begin
    if FEvent.WaitFor(PollMilliseconds) = wrSignaled then
    begin
      FLock.Acquire;
      try
        Result.CallbackUrl := FCallbackUrl;
      finally
        FLock.Release;
      end;
      Result.Outcome := TLoopbackOutcome.Received;
      exit;
    end;

    if Assigned(IsCanceled) and IsCanceled then
    begin
      Result.Outcome := TLoopbackOutcome.Canceled;
      exit;
    end;

    if TThread.GetTickCount64 >= Deadline then
    begin
      Result.Outcome := TLoopbackOutcome.TimedOut;
      exit;
    end;
  end;
end;

end.
