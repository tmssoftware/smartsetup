unit Fetching.Options;

interface

uses
  System.IOUtils, System.SysUtils,
{$IFDEF MSWINDOWS}
  Winapi.Windows,
{$ENDIF}
  URepositoryInfo;

type
  TFetchOptions = class
  private
    FRootFolder: string;
    FTargetRepository: string;
  public
    constructor Create;
    function RepositoryInfo(const Url: string): IRepositoryInfo;
    property TargetRepository: string read FTargetRepository write FTargetRepository;
  end;

implementation

{ TFetchOptions }

constructor TFetchOptions.Create;
begin
  inherited;
  FRootFolder := TDirectory.GetCurrentDirectory;
  // default value
  TargetRepository := GetEnvironmentVariable('TMSSETUP_DEFAULTREPO');
  if TargetRepository = '' then
    TargetRepository := 'production'; // production repository by default
end;

function TFetchOptions.RepositoryInfo(const Url: string): IRepositoryInfo;
begin
  Result := GetRepositoryInfo(TargetRepository, Url);
end;

end.
