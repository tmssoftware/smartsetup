#Check sbom generation
. test.setup
tms server-enable community true

tms fetch tms.biz.bcl c4d.conn tms.fmx.mcl

Test-tmsgui

$relativePath = ".\Products\tms.biz.bcl\tms.biz.bcl.cdx.json"
$filePath = (Resolve-Path $relativePath).Path
$fileStream = [System.IO.File]::Open($filePath, 'Open', 'Read', 'None')
try
{
   Test-tmsgui locked
}
finally
{
  $fileStream.Close()
}



