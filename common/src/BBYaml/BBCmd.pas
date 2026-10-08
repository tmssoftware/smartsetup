unit BBCmd;
{$i ../tmscommon.inc}

// A companion for BBYaml that allows us to parse command line commands with the same classes we
// defined for the Yaml. The syntax for command line parameters is:
//
// section:other-section:value=value
//
// If section contains a ":" (or what you define as section separator), then you can use a # instead. For example:
//
//  section#subsection:other-section:value=value
//
//  Arrays can also be used, with:
//
//  section.value=[a,b,c]


interface
uses Classes, SysUtils, BBError, BBClasses, BBFlow, Generics.Collections, BBStrings;

type

TBBCmdReader = class
  private
    class procedure ParseParameter(const Parameter: string; const SectionSeparator: string; const ErrorInfo: TErrorInfo; out Sections: TArray<string>; out Value: string);
    class procedure ProcessArray(const ArrayStr: string; const Section: TSection; const ErrorInfo: TErrorInfo);
    class procedure ProcessOneParameter(const Parameter, SectionSeparator: string; const MainSection: TSection; const OnlyValidate: boolean);
    class function IsSeparator(const Parameter: string; const Position: integer;
      const SectionSeparator: string): boolean; static;
  public
    class procedure ProcessCommandLine(const Parameters: array of string; const MainSection: TSection; const SectionSeparator: string; const OnlyValidate: boolean);
    class function AdaptForCmd(const s, SectionSeparator: string): string; static;
    class function EscapeForCmd(const s: string): string; static;
end;

implementation
type
TReplacementFunction = reference to function(const s: string; var Index: integer): string;


TCMDErrorInfo = class(TErrorInfo)
private
  Parameter: string;
public
  constructor Create(const aIgnoreOtherFiles: boolean; const aParameter: string);
  function ToString: string; override;
end;

{ TBBCmdReader }

class procedure TBBCmdReader.ProcessCommandLine(const Parameters: array of string;
  const MainSection: TSection; const SectionSeparator: string; const OnlyValidate: boolean);
begin
  for var Parameter in Parameters do
  begin
    ProcessOneParameter(Parameter.Trim, SectionSeparator, MainSection, OnlyValidate);
  end;
end;

class procedure TBBCmdReader.ProcessArray(const ArrayStr: string;
  const Section: TSection; const ErrorInfo: TErrorInfo);
begin
  TBBFlowParser.GetFlowArray(ArrayStr.Trim, Section, ErrorInfo);
end;

class function TBBCmdReader.IsSeparator(const Parameter: string; const Position: integer; const SectionSeparator: string): boolean;
begin
  if Length(SectionSeparator) + Position - 1 > Length(Parameter) then exit(false);

  for var i := 1 to Length(SectionSeparator) do
  begin
    if Parameter[i + Position - 1] <> SectionSeparator[i] then exit(false);
  end;
  Result := true;
end;

class procedure TBBCmdReader.ParseParameter(const Parameter, SectionSeparator: string; const ErrorInfo: TErrorInfo; out Sections: TArray<string>;
  out Value: string);
begin
  Value := '';
  var SectionList := TList<String>.Create;
  try
    var Start := 0;
    var i := 0;
    var EndParameter := Length(Parameter);
    while (true) do
    begin
      inc(i);
      if i > Length(Parameter) then break;

      if Parameter[i] = ' ' then continue;

      if IsSeparator(Parameter, i, SectionSeparator) then
      begin
        SectionList.Add(Parameter.Substring(Start, i - Start - 1));
        Start := i + Length(SectionSeparator) - 1;
        continue;
      end;
      if Parameter[i] = '=' then
      begin
        Value := BBYamlUnescapeString(Parameter.Substring(i).Trim, ErrorInfo);
        EndParameter := i - 1;
        break;
      end;
    end;

    if (Start < Length(Parameter)) then SectionList.Add(Parameter.Substring(Start, EndParameter - Start));

    Sections := SectionList.ToArray;
  finally
    SectionList.Free;
  end;
end;


function StringEscape(const s: string; const EscapeChars: TSysCharSet; const OnEscape: TReplacementFunction): string;
begin
  var Start := 0;
  var Builder: TStringBuilder := nil;
  try
    var i := -1;
    while (i < s.Length - 1) do
    begin
      Inc(i);
      if (CharInSet(s.Chars[i], EscapeChars)) then
      begin
        if Builder = nil then
          Builder := TStringBuilder.Create(Round(s.Length + 32));

        Builder.Append(s, Start, i - Start);
        Builder.Append(OnEscape(s, i));
        Start := i + 1;
      end
    end;

    if Builder = nil then
      Exit(s);

    Builder.Append(s, Start, s.Length - Start);
    Result := Builder.ToString;
  finally
    Builder.Free;
  end;

end;

class function TBBCmdReader.AdaptForCmd(const s, SectionSeparator: string): string;
begin
  Result := StringEscape(s, ['_','-','#'],
    function(const data: string; var Index: integer): string
    begin
      if (Index + 1 < Data.Length) and (Data.Chars[Index] = Data.Chars[Index + 1]) then
      begin
        Inc(Index);
        exit(Data.Chars[Index]);
      end
      else if Data.Chars[Index] = '#' then exit(SectionSeparator)
      else exit(' ');

    end).Trim;
end;

class function TBBCmdReader.EscapeForCmd(const s: string): string;
begin
  Result := StringEscape(s, ['_','-','#'],
    function(const data: string; var Index: integer): string
    begin
        exit(Data.Chars[Index] + Data.Chars[Index]);
    end);
end;

class procedure TBBCmdReader.ProcessOneParameter(const Parameter, SectionSeparator: string;
  const MainSection: TSection; const OnlyValidate: boolean);
begin
  var SectionsStr: TArray<string> := nil;
  var Value: string;

  var ErrorInfo := TCMDErrorInfo.Create(true, Parameter);
  try
    ParseParameter(Parameter, SectionSeparator, ErrorInfo, SectionsStr, Value);
    var Section := MainSection;
    for var i := Low(SectionsStr) to High(SectionsStr) - 1 do
    begin
      Section := Section.GotoChild(AdaptForCmd(SectionsStr[i], SectionSeparator), ErrorInfo);
    end;

    if Length(SectionsStr) = 0 then
    begin
      raise Exception.Create('Invalid parameter: "' + Parameter + '".');
    end;

    var ActionStr := AdaptForCmd(SectionsStr[Length(SectionsStr) - 1], SectionSeparator);
    var Action: TActionNameValue;

    if ((Section.Actions <> nil) and Section.Actions.TryGetValue(ActionStr, Action)) then
    begin
      if not OnlyValidate then Action(ActionStr, Value, ErrorInfo);
    end
    else
    begin
      Section := Section.GotoChild(ActionStr, ErrorInfo);
      if Section.ContainsArrays then
      begin
        if not OnlyValidate then
        begin
          ProcessArray(Value, Section, ErrorInfo);
        end;
      end

      else if not OnlyValidate then raise Exception.Create('Can''t access section: ' + ActionStr + ' from the command line. ' + ErrorInfo.ToString);
    end;

  finally
    ErrorInfo.Free;
  end;

end;

{ TCMDErrorInfo }

constructor TCMDErrorInfo.Create(const aIgnoreOtherFiles: boolean; const aParameter: string);
begin
  inherited Create(aIgnoreOtherFiles);
  Parameter := aParameter;
end;

function TCMDErrorInfo.ToString: string;
begin
  Result := 'In parameter: "' + Parameter  + '".';
end;

end.
