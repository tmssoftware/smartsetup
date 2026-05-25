# See what happens if we are using semver in git and auto-generating the packages.

. test.setup

tms server-enable tms false
tms server-add testserver zipfile "file:///$($tmsTestRootDir.Replace('\', '/'))/tmp-run/test-repos/tmsbuild_test_repos.zip"
tms config-write -p:"tms smart setup options:error if skipped=false"

$currentVersions = tms list-remote -json | ConvertFrom-Json -AsHashtable
$currentVersion = $currentVersions["test.double.trouble.git"].version

tms install test.double.trouble.git
$currentVersions = tms list -json | ConvertFrom-Json -AsHashtable
$currentVersion = $currentVersions["test.double.trouble.git"].version

if ($currentVersion -ne "1.1.0") {
    throw "Expected version 1.1.0, but got $currentVersion"
}

$allVersions = tms versions-remote test.double.trouble.git
if ($allVersions.Count -ne 7) {
    throw "Expected 7 versions, but got $($allVersions.Count)"
}

$expectedVersions = @{
    "v1.1.0_beta.2" = "0.0.0.0"
    "1.1.0" = "1.1.0.0"
    "1.0.1" = "1.0.1.0"
    "1.1.0_beta+1" = "0.0.0.0"
    "1.0.1-beta.1" = "1.0.1.0"
    "1.0.1-beta.2" = "1.0.1.0"
    "v1.0.0" = "0.0.0.0"
}
$expectedSpecialVersions = @{
    "1.0.1-beta.1" = "1.0.1.0-beta.1"
    "1.0.1-beta.2" = "1.0.1.0-beta.2"
}

foreach ($version0 in $allVersions) {
    $version = $version0.Trim();
    tms install test.double.trouble.git:$version
    $currentVersions = tms list -json | ConvertFrom-Json -AsHashtable
    $currentVersion = $currentVersions["test.double.trouble.git"].version
    if ($currentVersion -ne $version) {
        throw "Expected version $version, but got $currentVersion"
    }

    $result = ./triple-trouble/Win32/Release/tripletrouble.exe
    if ($result -ne $version) {
        throw "Expected output ' $version', but got '$result'"
    }


    #check the DoubleTrouble*.bpl files have correct versioninfo
    $bplFiles = @(Get-ChildItem -Path "Products\test.double.trouble.git\src\packages" -Filter "DoubleTrouble*.bpl" -Recurse -ErrorAction SilentlyContinue)
    if ($bplFiles.Count -lt 1) {
        throw "Expected at least one DoubleTrouble*.bpl file in 'Products\test.double.trouble.git\src\packages', but found none"
    }
    $expectedPrerelease = $version -match '-'
    $expectedProductVersion = $expectedVersions[$version]
    # $expectedSpecialBuild should be true if the version has something which isn't digits or dots
    $expectedSpecialBuild = $version -match '[^0-9\.\+]'
    foreach ($bpl in $bplFiles) {
        $versionInfo = $bpl.VersionInfo
        if ($versionInfo.IsPreRelease -ne $expectedPrerelease) {
            throw "File '$($bpl.FullName)' has IsPreRelease=$($versionInfo.IsPreRelease), expected $expectedPrerelease"
        }
        if ($versionInfo.ProductVersion -ne $expectedProductVersion) {
            throw "File '$($bpl.FullName)' has ProductVersion='$($versionInfo.ProductVersion)', expected '$expectedProductVersion'"
        }
        if ($expectedSpecialBuild) {
            if (-not $versionInfo.IsSpecialBuild) {
                throw "File '$($bpl.FullName)' has IsSpecialBuild=$($versionInfo.IsSpecialBuild), expected True"
            }
            $specialVersion = $version
            if ($expectedSpecialVersions[$version]) {
                $specialVersion = $expectedSpecialVersions[$version]
            }
            if ($versionInfo.SpecialBuild -ne $specialVersion) {
                throw "File '$($bpl.FullName)' has SpecialBuild='$($versionInfo.SpecialBuild)', expected '$specialVersion'"
            }
        }
        else
        {
            if ($versionInfo.IsSpecialBuild) {
                throw "File '$($bpl.FullName)' has IsSpecialBuild=$($versionInfo.IsSpecialBuild), expected False"
            }
        }
    }
}
