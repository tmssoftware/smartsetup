# Check that even if the user has the verbosity in trace, json output doesn't have the trace information.

function Test-JsonOutput {
    param(
        [Parameter(Mandatory = $true)]
        [object[]]$Result
    )

    try {
        return $Result | ConvertFrom-Json -AsHashtable
    } catch {
        throw "The output is not valid JSON: $Result"
    }
}

. test.setup

tms config-write -p:configuration-for-all-products:options:verbosity=trace
tms install tms.biz.bcl

$result = tms list -json -test-alert-new-versions -test-low-disk-space
$json = Test-JsonOutput -Result $result
if ($json["tms.biz.bcl"].name -ne "TMS BIZ Core Library") {
    throw "tms.biz.bcl should be installed, but it is not."
}

$result = tms list-remote -json -test-alert-new-versions -test-low-disk-space
$json = Test-JsonOutput -Result $result
if ($json["tms.flexcel.vcl"].name -ne "TMS FlexCel Studio for VCL and Firemonkey") {
    throw "tms.flexcel.vcl should be available to install, but it is not."
}

$result = tms info -json -test-no-credentials -test-alert-new-versions -test-low-disk-space
$json = Test-JsonOutput -Result $result
if ($json["has credentials"] -ne $false) {
    throw "The user should not have credentials, but the info command reports that they do."
}
if ($json["folder initialized"] -ne $true) {
    throw "The folder should be initialized, but it is not."
}

$result = tms config-read -json -test-alert-new-versions -test-low-disk-space
$json = Test-JsonOutput -Result $result
if ($json["configuration for all products"]["options"]["verbosity"] -ne "trace") {
    throw "The verbosity should be trace, but it is $($json["configuration for all products"]["options"]["verbosity"])."
}

$result = tms server-list -json -test-alert-new-versions -test-low-disk-space
$json = Test-JsonOutput -Result $result
if ($json["community"].url -ne "https://github.com/tmssoftware/smartsetup-registry/archive/refs/heads/main.zip") {
    throw "The community server should be available, but it is not."
}

$result =  tms versions-remote tms.webcore -json -test-alert-new-versions -test-low-disk-space
$json = Test-JsonOutput -Result $result
if ((Get-Date $json["3.0.3.0"].release_date -Format "yyyy-MM-dd") -ne "2026-08-18") {
    throw "versions remote not working."
}

tms spec -non-interactive -json -template:.\Products\tms.biz.bcl\tmsbuild.yaml  -test-alert-new-versions -test-low-disk-space
$result = Get-Content -Path .\tmsbuild.json
$json = Test-JsonOutput -Result $result

if ($json["paths"]."extra library paths" -ne "source\extra") {
   throw "spec command not working."
}
