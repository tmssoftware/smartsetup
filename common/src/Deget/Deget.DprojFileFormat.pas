unit Deget.DprojFileFormat;
{$i ../tmscommon.inc}

interface
procedure CopyDproj(const Source, Dest, Add, OutputPath: string);


implementation
uses Xml.XMLDoc, Xml.XMLIntf,
     {$IFDEF MSWINDOWS}
     ActiveX,
     {$ENDIF}
     SysUtils, IOUtils;

procedure Process(const Node: IXmlNode; const Add, OutputPath: string; const Level: integer);
begin
  if Level > 30 then raise Exception.Create('Too many nested levels in xml file.');

  if (Node.LocalName = 'DCCReference') or (Node.LocalName = 'RcCompile') then
  begin
    var val := Node.Attributes['Include'];
    var s: string := val;
    if (s <> '') and s.StartsWith('..\') then
    begin
       Node.Attributes['Include'] := Add + s;
    end;
    exit;
  end;

  if   (Node.LocalName = 'DCC_DcuOutput')
    or (Node.LocalName = 'DCC_ExeOutput')
    or (Node.LocalName = 'DCC_ObjOutput')
    or (Node.LocalName = 'DCC_BplOutput')
    or (Node.LocalName = 'DCC_BpiOutput')
    or (Node.LocalName = 'DCC_DcpOutput')
    or (Node.LocalName = 'DCC_HppOutput')
    or (Node.LocalName = 'BRCC_OutputDir')
  then
  begin
    Node.NodeValue := OutputPath;
  end;

  for var i := 0 to Node.ChildNodes.Count - 1 do Process(Node.ChildNodes[i], Add, OutputPath, Level + 1);
end;

procedure DoCopyDproj(const Source, Dest, Add, OutputPath: string);
var
  XML : IXMLDocument;
begin
  if not TFile.Exists(Source) then raise Exception.Create('Can''t find package "' + Source + '".');

  XML := TXMLDocument.Create(Source);
  Process(XML.DocumentElement, Add, OutputPath, 0);
  XML.SaveToFile(Dest);
  //IXMLDocument frees automatically, and has to be done before CoUninitialize.
  //this is why this is on its own method.
end;

procedure CopyDproj(const Source, Dest, Add, OutputPath: string);
begin
{$IFDEF MSWINDOWS}
  Coinitialize(nil);
{$ENDIF}
  try
    DoCopyDproj(Source, Dest, Add, OutputPath);
  finally
{$IFDEF MSWINDOWS}
    CoUninitialize;
{$ENDIF}
  end;
end;

end.
