program tmstest_util;


{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  UTmsBuildSystemUtils in '..\..\..\..\common\src\System\UTmsBuildSystemUtils.pas',
  Util.Dispatcher in 'src\Util.Dispatcher.pas',
  FileSystem.DeleteFolder in 'src\FileSystem.DeleteFolder.pas',
  UCommandLine in '..\..\..\..\common\src\UCommandLine.pas',
  FileSystem.CleanLockedFolder in 'src\FileSystem.CleanLockedFolder.pas',
  VSoft.CommandLine.CommandDef in '..\..\..\..\externals\VSoft.CommandLineParser\Src\VSoft.CommandLine.CommandDef.pas',
  VSoft.CommandLine.OptionDef in '..\..\..\..\externals\VSoft.CommandLineParser\Src\VSoft.CommandLine.OptionDef.pas',
  VSoft.CommandLine.Options in '..\..\..\..\externals\VSoft.CommandLineParser\Src\VSoft.CommandLine.Options.pas',
  VSoft.CommandLine.Parser in '..\..\..\..\externals\VSoft.CommandLineParser\Src\VSoft.CommandLine.Parser.pas',
  VSoft.CommandLine.Utils in '..\..\..\..\externals\VSoft.CommandLineParser\Src\VSoft.CommandLine.Utils.pas';

begin
  try
    Dispatch;
{$IFDEF DEBUG}
{$IFDEF MSWINDOWS}
{$WARNINGS OFF}
  if (DebugHook <> 0) then ReadLn;
{$WARNINGS ON}
{$ENDIF}
{$ENDIF}

  except
    on E: Exception do
    begin
      if ExitCode = 0 then
        ExitCode := 10;

      Writeln('Error: ' + E.Message);
    end;
  end;
end.
