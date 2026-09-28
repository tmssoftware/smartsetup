unit BBError;

interface
type
  TErrorInfo = class
  private
    FIgnoreOtherFiles: boolean;
  public
    property IgnoreOtherFiles: boolean read FIgnoreOtherFiles;
    constructor Create(const aIgnoreOtherFiles: boolean);
  end;

implementation

{ TErrorInfo }

constructor TErrorInfo.Create(const aIgnoreOtherFiles: boolean);
begin
  FIgnoreOtherFiles := aIgnoreOtherFiles;
end;


end.
