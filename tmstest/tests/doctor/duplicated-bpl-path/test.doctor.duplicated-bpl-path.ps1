# Checks that if a path entry is duplicated, we don't search it twice.

. test.setup

$currentFolder = Join-Path -Path (Get-Location).Path -ChildPath "fake-bpl"

$doctorDiagnostic = tms doctor -test-windows-path:"$currentFolder;$currentFolder" | Out-String

if ($doctorDiagnostic -notmatch "Checking Duplicated BPLs\s+OK") {
    throw "Doctor diagnostic should contain 'Checking Duplicated BPLs\nOK', but it is '$doctorDiagnostic'."
}

if ($doctorDiagnostic -match "build_events.bpl") {
    throw "Doctor diagnostic should not contain 'build_events.bpl', but it is '$doctorDiagnostic'."
}

$currentFolder2 = Join-Path -Path (Get-Location).Path -ChildPath "fake-bpl-2"

$doctorDiagnostic = tms doctor -test-windows-path:"$currentFolder;$currentFolder2" | Out-String

if ($doctorDiagnostic -match "Checking Duplicated BPLs\s+OK") {
    throw "Doctor diagnostic should not contain 'Checking Duplicated BPLs\nOK', but it is '$doctorDiagnostic'."
}

if ($doctorDiagnostic -notmatch "Checking Duplicated BPLs") {
    throw "Doctor diagnostic should contain 'Checking Duplicated BPLs', but it is '$doctorDiagnostic'."
}
if ($doctorDiagnostic -notmatch 'contain the following duplicated files: "build_events.bpl"') {
    throw "Doctor diagnostic should contain 'contain the following duplicated files: build_events.bpl', but it is '$doctorDiagnostic'."
}
