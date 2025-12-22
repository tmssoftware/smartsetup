# Test pin and unpin commands

. test.setup
tmscredentials
#use an older delphi version, as the latest might not be supported by more than 1 version.
tms config-write -p:"configuration for all products:delphi versions=[$($PreviousDelphiVersionId)]"


function Prettify {
    param(
        [version] $ver
    )
    if ($ver.Revision -eq 0) {
        if ($ver.Build -eq 0) {
            return "$($ver.Major).$($ver.Minor)"
        } else {
            return "$($ver.Major).$($ver.Minor).$($ver.Build)"
        }
    }
    return "$($ver.Major).$($ver.Minor).$($ver.Build).$($ver.Revision)"
}   

# try to keep recent. If we just used an arbitrary old version, it will stop working when we test in newer delphis.
$versions = tms versions-remote tms.biz.aurelius -json | ConvertFrom-Json
$sortedVersions = $versions.psobject.Properties.Name | Sort-Object {[version]$_}
$lastVersion = $sortedVersions[-1]
$secondLastVersion = $sortedVersions[-2]
$secondLastVersionPretty = Prettify([version]$secondLastVersion)

$bclVersions = tms versions-remote tms.biz.bcl -json | ConvertFrom-Json
$sortedBclVersions = $bclVersions.psobject.Properties.Name | Sort-Object {[version]$_}
$lastBclVersion = $sortedBclVersions[-1]
$secondLastBclVersion = $sortedBclVersions[-2]
$secondLastBclVersionPretty = Prettify([version]$secondLastBclVersion)

tms install tms.biz.aurelius:$secondLastVersion tms.biz.bcl:$secondLastBclVersion

$list = tms list -json | ConvertFrom-Json -AsHashtable
if ($list["tms.biz.aurelius"].version -ne $secondLastVersion) {
    throw "tms.biz.aurelius should be version $secondLastVersion, but it is $($list["tms.biz.aurelius"].version)."
}
if ($list["tms.biz.bcl"].version -ne $secondLastBclVersion) {
    throw "tms.biz.bcl should be version $secondLastBclVersion, but it is $($list["tms.biz.bcl"].version)."
}

$pin = tms pin * -verbose
$testOk = Test-Result -CommandResult $pin -Message "*Pinned tms.biz.aurelius to version $($secondLastVersionPretty)*" "tms pin"
if (-not ($testOk)) {
    Write-Error "The pin command did not report pinning tms.biz.aurelius to version $secondLastVersionPretty. Actual message: $($pin)"
}
$testOk = Test-Result -CommandResult $pin -Message "*Pinned tms.biz.bcl to version $(([version]$secondLastBclVersionPretty).ToString())*" "tms pin"
if (-not ($testOk)) {
    Write-Error "The pin command did not report pinning tms.biz.bcl to version $secondLastBclVersionPretty. Actual message: $($pin)"
}

tms update
$list = tms list -json | ConvertFrom-Json -AsHashtable
if ($list["tms.biz.aurelius"].version -ne $secondLastVersion) {
    throw "tms.biz.aurelius should be version $secondLastVersion, but it is $($list["tms.biz.aurelius"].version)."
}
if ($list["tms.biz.bcl"].version -ne $secondLastBclVersion) {
    throw "tms.biz.bcl should be version $secondLastBclVersion, but it is $($list["tms.biz.bcl"].version)."
}

tms install tms.biz.aurelius:$lastVersion tms.biz.bcl:$secondLastBclVersion
$list = tms list -json | ConvertFrom-Json -AsHashtable
if ($list["tms.biz.aurelius"].version -ne $secondLastVersion) {
    throw "tms.biz.aurelius should be version $secondLastVersion, but it is $($list["tms.biz.aurelius"].version)."
}
if ($list["tms.biz.bcl"].version -ne $secondLastBclVersion) {
    throw "tms.biz.bcl should be version $secondLastBclVersion, but it is $($list["tms.biz.bcl"].version)."
}

tms install tms.biz.bcl:$lastBclVersion
$list = tms list -json | ConvertFrom-Json -AsHashtable
if ($list["tms.biz.aurelius"].version -ne $secondLastVersion) {
    throw "tms.biz.aurelius should be version $secondLastVersion, but it is $($list["tms.biz.aurelius"].version)."
}
if ($list["tms.biz.bcl"].version -ne $secondLastBclVersion) {
    throw "tms.biz.bcl should be version $secondLastBclVersion, but it is $($list["tms.biz.bcl"].version)."
}

tms unpin tms.biz.aurelius
tms update
$list = tms list -json | ConvertFrom-Json -AsHashtable
if ($list["tms.biz.aurelius"].version -ne $lastVersion) {
    throw "tms.biz.aurelius should be version $lastVersion, but it is $($list["tms.biz.aurelius"].version)."
}
if ($list["tms.biz.bcl"].version -ne $secondLastBclVersion) {
    throw "tms.biz.bcl should be version $secondLastBclVersion, but it is $($list["tms.biz.bcl"].version)."
}


tms install tms.biz.aurelius:$secondLastVersion tms.biz.bcl:$lastBclVersion
$list = tms list -json | ConvertFrom-Json -AsHashtable
if ($list["tms.biz.aurelius"].version -ne $secondLastVersion) {
    throw "tms.biz.aurelius should be version $secondLastVersion, but it is $($list["tms.biz.aurelius"].version)."
}
if ($list["tms.biz.bcl"].version -ne $secondLastBclVersion) {
    throw "tms.biz.bcl should be version $secondLastBclVersion, but it is $($list["tms.biz.bcl"].version)."

}

tms server-add testserver zipfile "file:///$($tmsTestRootDir.Replace('\', '/'))/tmp-run/test-repos/tmsbuild_test_repos.zip"

tms install tmstest.a:v1.0.0 tmstest.c:v1.0.0
$list = tms list -json | ConvertFrom-Json -AsHashtable
if ($list["tmstest.c"].version -ne "v1.0.0") {
    throw "tmstest.c should be version v1.0.0, but it is $($list["tmstest.c"].version)."
}
if ($list["tmstest.a"].version -ne "v1.0.0") {
    throw "tmstest.a should be version v1.0.0, but it is $($list["tmstest.a"].version)."
}

$pin = tms pin tmstest.c -verbose
$testOk = Test-Result -CommandResult $pin -Message "*Pinned tmstest.c to version v1.0.0*" "tms pin tmstest.c"
if (-not ($testOk)) {
    Write-Error "The pin command did not report pinning tmstest.c to version v1.0.0. Actual message: $($pin)"
}

tms update
$list = tms list -json | ConvertFrom-Json -AsHashtable
if ($list["tmstest.c"].version -ne "v1.0.0") {
    throw "tmstest.c should be version v1.0.0, but it is $($list["tmstest.c"].version)."
}
if ($list["tmstest.a"].version -ne "v1.1.0") {
    throw "tmstest.a should be version v1.1.0, but it is $($list["tmstest.a"].version)."
}

$updateResult = tms update tmstest.a:v1.0.0
$testOk = Test-Result -CommandResult $updateResult -Message "*Skipping tmstest.c because it is pinned.*" "tms update tmstest.a"
if (-not ($testOk)) {
    Write-Error "The update command did not report skipping tmstest.c because it is pinned. Actual message: $($updateResult)"
}   

$list = tms list -json | ConvertFrom-Json -AsHashtable
if ($list["tmstest.c"].version -ne "v1.0.0") {
    throw "tmstest.c should be version v1.0.0, but it is $($list["tmstest.c"].version)."
}
if ($list["tmstest.a"].version -ne "v1.0.0") {
    throw "tmstest.a should be version v1.0.0, but it is $($list["tmstest.a"].version)."
}
