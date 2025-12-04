unit Deget.Compilation;

interface

uses
  Deget.CoreTypes,
  Deget.Nullable;


type
  TSearchPathMode = (
    ProjectSettings,     // uses the settings in project, which includes library path defined in Delphi IDE
    DelphiLib            // includes only Delphi Lib files and nothing more
  );

  TBuildMode = (
    BuildAll,
    Compile
  );

  TCompilationSettings = class
  private
    FBuildMode: TBuildMode;
    FExtraSearchPath: Nullable<string>;
    FSearchPathsToPreserve: TArray<string>;
    FConditionalDefines: Nullable<string>;
    FSearchPathMode: TSearchPathMode;
    FWarningsAsErrors: Nullable<boolean>;
    FTargetConfig: Nullable<string>;
  public
    property BuildMode: TBuildMode read FBuildMode write FBuildMode;
    property TargetConfig: Nullable<string> read FTargetConfig write FTargetConfig;
    property Defines: Nullable<string> read FConditionalDefines write FConditionalDefines;
    property WarningsAsErrors: Nullable<boolean> read FWarningsAsErrors write FWarningsAsErrors;

    // search directories
    property SearchPathMode: TSearchPathMode read FSearchPathMode write FSearchPathMode;
    property ExtraSearchPath: Nullable<string> read FExtraSearchPath write FExtraSearchPath;
    property SearchPathsToPreserve: TArray<string> read FSearchPathsToPreserve write FSearchPathsToPreserve;

  end;

implementation

end.
