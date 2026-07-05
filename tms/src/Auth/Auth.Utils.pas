unit Auth.Utils;

interface

uses
  System.Classes, System.JSON, System.SysUtils;

function Sha256(const Value: string): string;
function ParamsFromUri(const ResponseUri: string): TStrings;
function BuildQueryParams(Params: TStrings): string;
function GetJwtPayload(const AToken: string): TJSONObject;
function ParseJsonObject(const Json: string): TJSONObject;
function RandomString(ByteLength: Integer = 32): string;
function BasicAuthHeaderValue(const UserName, Password: string): string;
function AddUrlParams(const Url: string; Params: TStrings): string;

implementation

uses
  System.Hash, System.Types, System.StrUtils, System.NetEncoding, System.NET.URLClient;

// Cryptographically secure random bytes: values are used for OAuth state and
// PKCE code verifiers, which must not be predictable.
{$IFDEF MSWINDOWS}
function BCryptGenRandom(hAlgorithm: Pointer; pbBuffer: PByte; cbBuffer: Cardinal;
  dwFlags: Cardinal): Integer; stdcall; external 'bcrypt.dll';

function RandomBytes(Length: Integer): TBytes;
const
  BCRYPT_USE_SYSTEM_PREFERRED_RNG = $00000002;
var
  Status: Integer;
begin
  SetLength(Result, Length);
  if Length = 0 then Exit;
  Status := BCryptGenRandom(nil, @Result[0], Length, BCRYPT_USE_SYSTEM_PREFERRED_RNG);
  if Status <> 0 then
    raise Exception.CreateFmt('Could not generate secure random data, status %d', [Status]);
end;
{$ELSE}
function RandomBytes(Length: Integer): TBytes;
var
  Stream: TFileStream;
begin
  SetLength(Result, Length);
  if Length = 0 then Exit;
  Stream := TFileStream.Create('/dev/urandom', fmOpenRead or fmShareDenyNone);
  try
    if Stream.Read(Result[0], Length) <> Length then
      raise Exception.Create('Could not generate random bytes');
  finally
    Stream.Free;
  end;
end;
{$ENDIF}

function RandomString(ByteLength: Integer = 32): string;
begin
  Result := TNetEncoding.Base64URL.EncodeBytesToString(RandomBytes(ByteLength));
end;

function ParseJsonObject(const Json: string): TJSONObject;
begin
  var Value := TJSONObject.ParseJSONValue(Json);
  if Assigned(Value) and (Value is TJSONObject) then
    Result := Value as TJSONObject
  else
    Result := nil;
end;

function Sha256(const Value: string): string;
var
  Hash: THashSHA2;
begin
  Hash := THashSHA2.Create;
  Hash.Update(Value);
  Result := TNetEncoding.Base64URL.EncodeBytesToString(Hash.HashAsBytes);
end;

function BasicAuthHeaderValue(const UserName, Password: string): string;
begin
  Result := 'Basic ' + TNETEncoding.Base64.EncodeBytesToString(TEncoding.UTF8.GetBytes(Format('%s:%s', [UserName, Password])));
end;

procedure ParseQueryParams(const Query: string; Params: TStrings);
var
  I: Integer;
begin
  Params.Delimiter := '&';
  Params.StrictDelimiter := True;
  Params.DelimitedText := Query;
  for I := 0 to Params.Count - 1 do
    Params.Strings[I] := TNetEncoding.URL.Decode(Params.Strings[I]);
end;

function ParamsFromUri(const ResponseUri: string): TStrings;
var
  CalledUrl: TUri;
  Params: string;
  Query: string;
begin
  CalledUrl := TUri.Create(ResponseUri);
  Params := CalledUrl.Fragment;
  if (Length(Params) > 0) and (Params[1] = '#') then
    Params := Copy(Params, 2);

  Query := CalledUrl.Query;
  if (Length(Query) > 0) and (Query[1] = '?') then
    Query := Copy(Query, 2);

  if Query <> '' then
  begin
    if Params <> '' then
      Params := Params + '&';
    Params := Params + Query;
  end;

  Result := TStringList.Create;
  try
    ParseQueryParams(Params, Result);
  except
    Result.Free;
    raise
  end;
end;

function BuildQueryParams(Params: TStrings): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Params.Count - 1 do
    if Params.ValueFromIndex[I] <> '' then
    begin
      if Result <> '' then
        Result := Result + '&';
      Result := Result + Params.Names[I] + '=' + TNetEncoding.URL.EncodeQuery(Params.ValueFromIndex[I]);
    end;
end;

function GetJwtPayload(const AToken: string): TJSONObject;
var
  Parts: TStringDynArray;
begin
  Parts := SplitString(AToken, '.');
  if Length(Parts) <> 3 then
    Exit(nil);

  Result := ParseJsonObject(TNETEncoding.Base64URL.Decode(Parts[1]));
  if Result = nil then
    raise Exception.Create('Token payload is not a valid JSON');
end;

function AddUrlParams(const Url: string; Params: TStrings): string;
begin
  var URI: TURI := TURI.Create(Url);
  var URIParams: TURIParameters := [];
  for var I := 0 to Params.Count - 1 do
    URIParams := URIParams + [TURIParameter.Create(Params.Names[I], Params.ValueFromIndex[I])];
  URI.Params := URIParams;
  Result := URI.ToString;
end;

end.
