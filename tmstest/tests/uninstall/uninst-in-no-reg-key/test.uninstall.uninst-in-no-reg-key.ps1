# When running without an alternate registry key, the windows path should be modified

. test.setup
$regkey = "BDS"

function Get-UserWindowsPath {
	$windowsPath = [System.Environment]::GetEnvironmentVariable("Path", "User")
	if ($null -eq $windowsPath) {
		return ""
	}

	return $windowsPath
}

function Test-AlternateRegistryPathEntry {
    param(
        [Parameter(Mandatory)] [bool] $ShouldContainEntry
    )

    foreach ($ver in $Global:AllDelphiVersions) {
        $regPaths = @(
            "HKCU:\Software\Embarcadero\$regkey\$ver\Environment Variables",
            "HKCU:\Software\Embarcadero\$regkey\$ver\Environment Variables x64"
        )

        foreach ($regPath in $regPaths) {
            if ($ver -eq '23.0') {
                # For Delphi 12, there is no x64 registry path.
                if ($regPath.Contains("x64")) {
                    continue
                }
            }
            if (-not (Test-Path $regPath)) {
                throw "Registry path '$regPath' should exist."
            }

            $entry = "uninst-in-no-reg-key\.tmssetup\build\bpl\Win32"
            if ($regPath.Contains("x64")) {
                $entry = "uninst-in-no-reg-key\.tmssetup\build\bpl\Win64"
            }
            $pathValue = (Get-ItemProperty -Path $regPath).Path
            $containsEntry = $pathValue.Contains($entry)
            if ($ShouldContainEntry -and -not $containsEntry) {
                throw "Registry path '$regPath' should contain $($entry)."
            }
            if (-not $ShouldContainEntry -and $containsEntry) {
                throw "Registry path '$regPath' should not contain $($entry)."
            }
        }
    }
}

tms config-write -p:"configuration for all products:options:skip register=false"
tms config-write -p:"configuration for all products:platforms=[win32intel,win64intel]"

$windowsPathBeforeInstall = Get-UserWindowsPath

if ($windowsPathBeforeInstall.Contains("uninst-in-no-reg-key")) {
    #try installing a dummy product and uninstalling it, to remove ourselves from the path.
    tms install tms.biz.bcl 
    tms uninstall *
    $windowsPathBeforeInstall = Get-UserWindowsPath
}

$windowsPathBeforeInstall | Assert-ValueNotContains -expected "uninst-in-no-reg-key" 
tms install tms.biz.echo

$windowsPathAfterInstall = Get-UserWindowsPath
if ($windowsPathAfterInstall -eq $windowsPathBeforeInstall) {
    throw "Windows path should have been modified."
}

$windowsPathAfterInstall | Assert-ValueContains -expected "uninst-in-no-reg-key\.tmssetup\build\bpl\Win64"
$windowsPathAfterInstall | Assert-ValueContains -expected "uninst-in-no-reg-key\.tmssetup\build\bpl\Win32"


Test-AlternateRegistryPathEntry -ShouldContainEntry $false
tms uninstall *
Test-AlternateRegistryPathEntry -ShouldContainEntry $false

$windowsPathAfterUnInstall = Get-UserWindowsPath
$windowsPathAfterUnInstall | Assert-ValueIs -expected $windowsPathBeforeInstall



