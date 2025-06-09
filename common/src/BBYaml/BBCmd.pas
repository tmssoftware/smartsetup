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
uses BBClasses, Generics.Collections;

type

TBBCmdReader = class
  private
    class procedure ParseParameter(const Parameter: string; const SectionSeparator: string; out Sections: TArray<string>; out Value: string);
    class procedure ProcessArray(const ArrayStr: string; const ArrayAction: TActionNameValue; const ErrorInfo: TErrorInfo);
    class procedure ProcessOneParameter(const Parameter, SectionSeparator: string; const MainSection: TSection);
    class function AdaptForCmd(const s, SectionSeparator: string): string; static;
    class function AdaptToCmd(const s, SectionSeparator: string): string; static;
    class function IsSeparator(const Parameter: string; const Position: integer;
      const SectionSeparator: string): boolean; static;
  public
    class procedure ProcessCommandLine(const Parameters: array of string; const MainSection: TSection; const SectionSeparator: string);
    class function GetVariableName(const Parameter, SectionSeparator: string;
      const MainSection: TSection; const ExtraInfos: TList<string>): string; static;
end;

implementation
uses Classes, SysUtils;

type

TCMDErrorInfo = class(TErrorInfo)
private
  Parameter: string;
public
  constructor Create(const aParameter: string);
  function ToString: string; override;
end;

{ TBBCmdReader }

class procedure TBBCmdReader.ProcessCommandLine(const Parameters: array of string;
  const MainSection: TSection; const SectionSeparator: string);
begin
  for var Parameter in Parameters do
  begin
    ProcessOneParameter(Parameter.Trim, SectionSeparator, MainSection);
  end;
end;

class procedure TBBCmdReader.ProcessArray(const ArrayStr: string;
  const ArrayAction: TActionNameValue; const ErrorInfo: TErrorInfo);
begin
  TSection.GetArray(ArrayStr.Trim, nil,
    procedure(Value: string; ErrorInfo: TErrorInfo)
    begin
      var ElementName := Value;
      var ElementValue := '';
      var idx := Value.IndexOf(':');
      if idx > 0 then
      begin
        ElementName := Value.Substring(0, idx).Trim;
        ElementValue := Value.Substring(idx + 1).Trim;
      end;


      ArrayAction(ElementName, ElementValue, ErrorInfo);
    end
    , ErrorInfo);
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

class procedure TBBCmdReader.ParseParameter(const Parameter, SectionSeparator: string; out Sections: TArray<string>;
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
        Value := Parameter.Substring(i).Trim;
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

class function TBBCmdReader.AdaptForCmd(const s, SectionSeparator: string): string;
begin
  Result := s.Replace('_', ' ').Replace('-', ' ').Replace('#', SectionSeparator);
end;

class function TBBCmdReader.AdaptToCmd(const s, SectionSeparator: string): string;
begin
  Result := s.Trim;
  while Result.IndexOf('  ') >= 0 do
  begin
    Result := Result.Replace('  ', ' ');
  end;
  Result := Result.Replace(' ', '-');
end;

class procedure TBBCmdReader.ProcessOneParameter(const Parameter, SectionSeparator: string;
  const MainSection: TSection);
begin
  var SectionsStr: TArray<string> := nil;
  var Value: string;
  ParseParameter(Parameter, SectionSeparator, SectionsStr, Value);

  var ErrorInfo := TCMDErrorInfo.Create(Parameter);
  try
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
    var Action: TAction;

    if ((Section.Actions <> nil) and Section.Actions
      .TryGetValue(ActionStr, Action)) then
    begin
      Action(Value, ErrorInfo);
    end
    else
    begin
      Section := Section.GotoChild(ActionStr, ErrorInfo);
      if (Assigned(Section.ArrayMainAction)) then
      begin
        ProcessArray(Value, Section.ArrayMainAction, ErrorInfo);
      end
      else if Section.ContainsArrays then
      begin
        ProcessArray(value, procedure(N, V: string; ErrorInfo: TErrorInfo)
        begin
          if ((Section.Actions <> nil) and Section.Actions.TryGetValue(N, Action)) then
          begin
            Action(V, ErrorInfo);
          end
          else Section.ThrowInvalidTag(N, ErrorInfo);
         end, ErrorInfo);
      end
      else raise Exception.Create('Can''t access section: ' + ActionStr + ' from the command line. ' + ErrorInfo.ToString);
    end;

  finally
    ErrorInfo.Free;
  end;

end;

class function TBBCmdReader.GetVariableName(const Parameter, SectionSeparator: string;
  const MainSection: TSection; const ExtraInfos: TList<string>): string;
begin
  var SectionsStr: TArray<string> := nil;
  var Value: string;
  ParseParameter(Parameter, SectionSeparator, SectionsStr, Value);

  var ErrorInfo := TCMDErrorInfo.Create(Parameter);
  try
    var VarPrefix := '';
    var Section := MainSection;
    for var i := Low(SectionsStr) to High(SectionsStr) - 1 do
    begin
      Section := Section.GotoChild(AdaptForCmd(SectionsStr[i], SectionSeparator), ErrorInfo, true);
      if Section.VarPrefix <> '' then VarPrefix := VarPrefix + Section.VarPrefix;
      if Section.ExtraInfo <> '' then ExtraInfos.Add(Section.ExtraInfo);      
    end;

    if Length(SectionsStr) = 0 then
    begin
      raise Exception.Create('Invalid parameter: "' + Parameter + '".');
    end;

    var ActionStr := AdaptForCmd(SectionsStr[Length(SectionsStr) - 1], SectionSeparator);
    Result := VarPrefix + AdaptToCmd(ActionStr, SectionSeparator);
    var Action: TAction;

    if ((Section.Actions <> nil) and Section.Actions
      .TryGetValue(ActionStr, Action)) then
    begin
      exit;
    end
    else
    begin
      Section := Section.GotoChild(ActionStr, ErrorInfo, true);
      if Section.VarPrefix <> '' then
      begin
        VarPrefix := VarPrefix + Section.VarPrefix;
        Result := VarPrefix + AdaptToCmd(ActionStr, SectionSeparator);
      end;

      if (Assigned(Section.ArrayMainAction)) then
      begin
        exit;
      end
      else if Section.ContainsArrays then
      begin
        exit;
      end
      else raise Exception.Create('Can''t access section: ' + ActionStr + ' from the command line. ' + ErrorInfo.ToString);
    end;

  finally
    ErrorInfo.Free;
  end;

end;

{ TCMDErrorInfo }

constructor TCMDErrorInfo.Create(const aParameter: string);
begin
  Parameter := aParameter;
end;

function TCMDErrorInfo.ToString: string;
begin
  Result := 'In parameter: "' + Parameter  + '".';
end;

end.
