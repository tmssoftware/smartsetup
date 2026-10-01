# make sure we can self-update even when the yaml files have fields the current version of tms.exe does not know about. This is important for forward compatibility, so that we can add new fields to the yaml files without breaking older versions of tms.exe.

. test.setup

#create zip file with a tmsbuild.yaml that has a field that the current version of tms.exe does not know about.
Push-Location wrongrepos
$repoFolder = Get-Location
$zipFilePath = Join-Path -Path $repoFolder -ChildPath "wrongrepos.zip"
$zip = [System.IO.Compression.ZipFile]::Open($zipFilePath, [System.IO.Compression.ZipArchiveMode]::Create)
    try {
        [System.IO.Compression.ZipFileExtensions]::CreateEntryFromFile($zip, (Join-Path -Path $PWD -ChildPath "tmsbuild.yaml"), "tmsbuild.yaml") | Out-Null
    } finally {
        $zip.Dispose()
    }

Pop-Location
tms server-add wrongserver zipfile file:///$($repoFolder.ToString().Replace('\', '/'))/wrongrepos.zip

$tmsexe = Get-Alias tms
Copy-Item $tmsexe.Definition "./tms.exe" -Force


$log = ./tms.exe self-update -test-force-self-update -test-skip-self-update-signature-verification
if ($log -like "*TMS Smart Setup has been updated from version*") {
    Write-Host "Self-update reported a successful update."
}
else {
    throw "Unexpected output from self-update: $log"
}