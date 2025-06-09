unit URepositoryInfo;

interface

uses
  System.Generics.Collections, System.SysUtils;

type
  IRepositoryInfo = interface
  ['{3E4B5627-17F4-4729-8775-92C19FD5A1F6}']
    function ApiUrl: string;
    function AdminUrl: string;
    function AuthUrl: string;
  end;

  TLocalRepositoryInfo = class(TInterfacedObject, IRepositoryInfo)
  strict private const
    CApiUrl = 'http://localhost:2001/tms/api';
    CAdminUrl = 'http://localhost:2001/tms/admin';
    CAuthUrl = 'http://localhost:2001/tms/auth';
  public
    function ApiUrl: string;
    function AdminUrl: string;
    function AuthUrl: string;
  end;

  TProductionRepositoryInfo = class(TInterfacedObject, IRepositoryInfo)
  strict private const
    CApiUrl = 'https://api.landgraf.dev/tms/api';
    CAdminUrl = 'https://api.landgraf.dev/tms/admin';
    CAuthUrl = 'https://api.landgraf.dev/tms/auth';
  public
    function ApiUrl: string;
    function AdminUrl: string;
    function AuthUrl: string;
  end;

  TSandboxRepositoryInfo = class(TInterfacedObject, IRepositoryInfo)
  strict private const
    CApiUrl = 'https://api.landgraf.dev/tms/sandbox/api';
    CAdminUrl = 'https://api.landgraf.dev/tms/sandbox/admin';
    CAuthUrl = 'https://api.landgraf.dev/tms/sandbox/auth';
  public
    function ApiUrl: string;
    function AdminUrl: string;
    function AuthUrl: string;
  end;

function GetRepositoryInfo(const Name: string): IRepositoryInfo;

implementation

var
  Repos: TDictionary<string, IRepositoryInfo>;

function GetRepositoryInfo(const Name: string): IRepositoryInfo;
begin
  if Name = '' then
    raise Exception.Create('Repository name not provided');

  if not Repos.TryGetValue(Name, Result) then
    raise Exception.CreateFmt('Repository "%s" does not exist', [Name]);
end;

{ TLocalRepositoryInfo }

function TLocalRepositoryInfo.AdminUrl: string;
begin
  Result := CAdminUrl;
end;

function TLocalRepositoryInfo.ApiUrl: string;
begin
  Result := CApiUrl
end;

function TLocalRepositoryInfo.AuthUrl: string;
begin
  Result := CAuthUrl;
end;

{ TProductionRepositoryInfo }

function TProductionRepositoryInfo.AdminUrl: string;
begin
  Result := CAdminUrl;
end;

function TProductionRepositoryInfo.ApiUrl: string;
begin
  Result := CApiUrl;
end;

function TProductionRepositoryInfo.AuthUrl: string;
begin
  Result := CAuthUrl;
end;

{ TSandboxRepositoryInfo }

function TSandboxRepositoryInfo.AdminUrl: string;
begin
  Result := CAdminUrl;
end;

function TSandboxRepositoryInfo.ApiUrl: string;
begin
  Result := CApiUrl;
end;

function TSandboxRepositoryInfo.AuthUrl: string;
begin
  Result := CAuthUrl;
end;

initialization
  Repos := TDictionary<string, IRepositoryInfo>.Create;
  Repos.Add('production', TProductionRepositoryInfo.Create);
  Repos.Add('sandbox', TSandboxRepositoryInfo.Create);
  Repos.Add('local', TLocalRepositoryInfo.Create);

finalization
  Repos.Free;
end.
