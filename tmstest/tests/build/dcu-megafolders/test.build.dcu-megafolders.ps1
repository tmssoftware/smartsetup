# Check megafolders

. test.setup

tms config-write -p:"tms smart setup options:dcu megafolders=[tms:tms.*, other:*]"
tms server-enable community
tms config-write -p:"configuration for all products:options:skip register=false"
$libfolder = ".tmssetup\lib"
tms install tms.biz.bcl bero.boldfordelphi
$tmsfilesBefore = (Get-ChildItem -Path $libFolder -File -Recurse).Count
if ($tmsfilesBefore -eq 0) {
    throw "No files were found in '$libFolder' after the install."
}

tms install yankovsky.delphi.ast


$filesBefore = (Get-ChildItem -Path $libFolder -File -Recurse).Count
if ($filesBefore -eq 0 -or $filesBefore -eq $tmsfilesBefore) {
    throw "No files were found in '$libFolder' after the install."
}

tms build tms.* -full

$filesAfter = (Get-ChildItem -Path $libFolder -File -Recurse).Count
if ($filesBefore -ne $filesAfter) {
    throw "The number of files in '$libFolder' changed after a full build of tms.*. Before: $filesBefore, after: $filesAfter."
}

tms uninstall tms.* bero.boldfordelphi
$yankovskyfilesAfter = (Get-ChildItem -Path $libFolder -File -Recurse).Count
$filesExpected = $filesBefore - $tmsfilesBefore + 1
if ($yankovskyfilesAfter -ne $filesExpected) { 
    throw "The number of files in '$libFolder' changed after an uninstall of tms.*. Before: $filesExpected, after: $yankovskyfilesAfter."
}

tms install tms.biz.bcl
$filesMotherBefore = (Get-ChildItem -Path $libFolder -File -Recurse).Count
tms config-write -p:"tms smart setup options:dcu megafolders=[mother:*]"
tms build

$filesMotherAfter = (Get-ChildItem -Path $libFolder\mother -File -Recurse).Count + 1
if ($filesMotherBefore -ne $filesMotherAfter) {
    throw "The number of files in '$libFolder\mother' changed after a build. Before: $filesMotherBefore, after: $filesMotherAfter."
}

$filesAllAfter = (Get-ChildItem -Path $libFolder -File -Recurse).Count
if ($filesAllAfter -ne $filesMotherAfter) {
    throw "The number of files in '$libFolder' changed after a build. Before: $filesMotherAfter, after: $filesAllAfter."
}


uninstall_and_check(0)

$noFilesAfter = @((Get-ChildItem -Path $libFolder -File -Recurse)).Count
if ($noFilesAfter -ne 1) {
    throw "there should be no files in '$libFolder' besides the megafolders.txt one, after uninstalling all products."
}

