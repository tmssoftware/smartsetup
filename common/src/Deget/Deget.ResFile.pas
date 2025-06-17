unit Deget.ResFile;

interface
uses Deget.Version, UProjectDefinition;
procedure CreateResFile(const FileName: string; const Application: TApplicationDefinition; const BRCC32Path: string);

implementation
uses Classes, SysUtils, IOUtils, Deget.CommandLine, ULogger, UMultiLogger;

function Escape(const s: string): string;
begin
  Result := StringReplace(s, '"', '\"', [TReplaceFlag.rfReplaceAll]);
end;

procedure CreateResFile(const FileName: string; const Application: TApplicationDefinition; const BRCC32Path: string);
begin
  var RCFileName := TPath.ChangeExtension(FileName, '.rc');
  Logger.Trace('Creating res file ' + RCFileName);
  var Writer := TStreamWriter.Create(RCFileName, false, TEncoding.ASCII);
  try
    // We could create ICON and MAINICON here too as Delphi 7 to XE do, but it makes no sense.
    // Most of our components have been shipping with the standard D7-XE icons, and they aren't used anywhere.
    // XE2 or newer also don't include them anymore.

    var Version: TVersion := Application.Version;
    var VersionDots := Version.Normalized;
    var VersionCommas := VersionDots.Replace('.',',', [rfReplaceAll]);

    Writer.WriteLine('1 VERSIONINFO');
    Writer.WriteLine('FILEVERSION ' + VersionCommas);
    Writer.WriteLine('PRODUCTVERSION ' + VersionCommas);
    Writer.WriteLine('FILEOS 0x4');
    Writer.WriteLine('FILETYPE 0x1');
    Writer.WriteLine('{');
    Writer.WriteLine('BLOCK "StringFileInfo"');
    Writer.WriteLine('{');
    Writer.WriteLine('	BLOCK "040904E4"');
    Writer.WriteLine('	{');
    Writer.WriteLine('		VALUE "CompanyName", "' + '' +'\0"');
    Writer.WriteLine('		VALUE "FileDescription", "' + Escape(Application.Description) +'\0"');
    Writer.WriteLine('		VALUE "FileVersion", "' + VersionDots + '\0"');
    Writer.WriteLine('		VALUE "InternalName", "\0"');
    Writer.WriteLine('		VALUE "LegalCopyright", "' + Escape(Application.Copyright) +'\0"');
    Writer.WriteLine('		VALUE "LegalTrademarks", "\0"');
    Writer.WriteLine('		VALUE "OriginalFilename", "\0"');
    Writer.WriteLine('		VALUE "ProductName", "' + Escape(Application.Name) +'\0"');
    Writer.WriteLine('		VALUE "ProductVersion", "' + VersionDots + '\0"');
    Writer.WriteLine('		VALUE "Comments", "\0"');
    Writer.WriteLine('	}');
    Writer.WriteLine('}');
    Writer.WriteLine('');
    Writer.WriteLine('BLOCK "VarFileInfo"');
    Writer.WriteLine('{');
    Writer.WriteLine('	VALUE "Translation", 0x0409 0x04E4  ');
    Writer.WriteLine('}');
    Writer.WriteLine('}');


  finally
    Writer.Free;
  end;

  if not ExecuteCommand(BRCC32Path + ' "' + RCFileName + '"') then raise Exception.Create('Cannot create res file ' + RCFileName);
end;
end.
