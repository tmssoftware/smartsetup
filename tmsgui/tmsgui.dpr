program tmsgui;

{$R *.dres}

uses
  Vcl.Forms,
  Forms.Main in 'src\Forms.Main.pas' {MainForm},
  UTmsRunner in 'src\UTmsRunner.pas',
  Deget.CommandLine in '..\common\src\Deget\Deget.CommandLine.pas',
  UProductInfo in 'src\UProductInfo.pas',
  UMultiLogger in '..\common\src\Logger\UMultiLogger.pas',
  UAppTerminated in '..\common\src\UAppTerminated.pas',
  ULogger in '..\common\src\Logger\ULogger.pas',
  Deget.Version in '..\common\src\Deget\Deget.Version.pas',
  UMemoLogger in '..\common\src\Logger\UMemoLogger.pas',
  Deget.CoreTypes in '..\common\src\Deget\Deget.CoreTypes.pas',
  GUI.Environment in 'src\GUI.Environment.pas',
  Forms.Credentials in 'src\Forms.Credentials.pas' {CredentialsForm},
  UCommonTypes in 'src\UCommonTypes.pas',
  Forms.VersionPicker in 'src\Forms.VersionPicker.pas' {VersionPickerForm},
  Forms.Start in 'src\Forms.Start.pas' {StartForm},
  Forms.Config.Servers in 'src\Forms.Config.Servers.pas' {ServerConfigForm},
  Forms.Config in 'src\Forms.Config.pas' {ConfigForm};

{$R *.res}

begin
{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
