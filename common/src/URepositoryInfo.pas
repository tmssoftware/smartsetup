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

  TRepositoryInfo = class(TInterfacedObject)
  private
    RootUrl: string;
  protected
    function GetUrl(const Suffix: string): string;
  public
    constructor Create(const aRootUrl: string);
  end;

  TLocalRepositoryInfo = class(TRepositoryInfo, IRepositoryInfo)
  strict private const
    CApiUrl = 'http://localhost:2001/tms/api';
    CAdminUrl = 'http://localhost:2001/tms/admin';
    CAuthUrl = 'http://localhost:2001/tms/auth';
  public
    function ApiUrl: string;
    function AdminUrl: string;
    function AuthUrl: string;
  end;

  TProductionRepositoryInfo = class(TRepositoryInfo, IRepositoryInfo)
  strict private const
    CApiUrlSuffix = 'api';
    CAdminUrlSuffix = 'admin';
    CAuthUrlSuffix = 'auth';
  public
    function ApiUrl: string;
    function AdminUrl: string;
    function AuthUrl: string;
  end;

  TSandboxRepositoryInfo = class(TRepositoryInfo, IRepositoryInfo)
  strict private const
    CApiUrlSuffix = 'sandbox/api';
    CAdminUrlSuffix = 'sandbox/admin';
    CAuthUrlSuffix = 'sandbox/auth';
  public
    function ApiUrl: string;
    function AdminUrl: string;
    function AuthUrl: string;
  end;

function GetRepositoryInfo(const Name, Url: string): IRepositoryInfo;

implementation

function GetRepositoryInfo(const Name, Url: string): IRepositoryInfo;
begin
  if Name = '' then
    raise Exception.Create('Repository name not provided');

  if Url = '' then
    raise Exception.Create('Url not provided');


  if Name = 'production' then exit(TProductionRepositoryInfo.Create(Url));
  if Name = 'sandbox' then exit(TSandboxRepositoryInfo.Create(Url));
  if Name = 'local' then exit(TLocalRepositoryInfo.Create(Url));

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
  Result := GetUrl(CAdminUrlSuffix);
end;

function TProductionRepositoryInfo.ApiUrl: string;
begin
  Result := GetUrl(CApiUrlSuffix);
end;

function TProductionRepositoryInfo.AuthUrl: string;
begin
  Result := GetUrl(CAuthUrlSuffix);
end;

{ TSandboxRepositoryInfo }

function TSandboxRepositoryInfo.AdminUrl: string;
begin
  Result := GetUrl(CAdminUrlSuffix);
end;

function TSandboxRepositoryInfo.ApiUrl: string;
begin
  Result := GetUrl(CApiUrlSuffix);
end;

function TSandboxRepositoryInfo.AuthUrl: string;
begin
  Result := GetUrl(CAuthUrlSuffix);
end;

{ TRepositoryInfo }

constructor TRepositoryInfo.Create(const aRootUrl: string);
begin
    if RootUrl.EndsWith('/')
      then RootUrl := aRootUrl
      else RootUrl := aRootUrl + '/';
end;

function TRepositoryInfo.GetUrl(const Suffix: string): string;
begin
  Result := RootUrl + Suffix;
end;

end.
