# When running in an alternate registry key, the windows path should not be modified, and the entry should be added to delphi itself.

. test.setup
$regkey = "tmstest\uninst-in-alt-reg-key"

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

            $entry = "uninst-in-alt-reg-key\.tmssetup\build\bpl\Win32"
            if ($regPath.Contains("x64")) {
                $entry = "uninst-in-alt-reg-key\.tmssetup\build\bpl\Win64"
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

Set-AlternateRegistryKey -RegKey $regkey -RegisterAllDelphiVersions $true
tms config-write -p:"configuration for all products:options:skip register=false"
tms config-write -p:"configuration for all products:platforms=[win32intel,win64intel, win64xintel, winarm64ec]" 

$windowsPathBeforeInstall = Get-UserWindowsPath

if ($windowsPathBeforeInstall.Contains("uninst-in-alt-reg-key")) {
    #try installing a dummy product and uninstalling it, to remove ourselves from the path.
    tms install tms.biz.bcl 
    tms uninstall *
    $windowsPathBeforeInstall = Get-UserWindowsPath
}

$windowsPathBeforeInstall | Assert-ValueNotContains -expected "uninst-in-alt-reg-key" 
tms install tms.biz.echo tms.fnc.uipack

$windowsPathAfterInstall = Get-UserWindowsPath
$windowsPathAfterInstall | Assert-ValueIs -expected $windowsPathBeforeInstall

$products = tms list -json -detailed | ConvertFrom-Json -AsHashtable
$echo = $products["tms.biz.echo"]
if ($null -eq $echo) {
	throw "tms.biz.echo should be installed."
}

$hasRegisteredPlatform = $false
foreach ($ide in $echo.ides.Keys) {
	foreach ($platform in $echo.ides[$ide].platforms.Keys) {
		if ($echo.ides[$ide].platforms[$platform].registered) {
			$hasRegisteredPlatform = $true
			break
		}
	}

	if ($hasRegisteredPlatform) {
		break
	}
}

if (-not $hasRegisteredPlatform) {
	throw "tms.biz.echo should be registered in the alternate Delphi registry key."
}

Test-AlternateRegistryPathEntry -ShouldContainEntry $true
tms uninstall *
Test-AlternateRegistryPathEntry -ShouldContainEntry $false




