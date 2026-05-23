#Check we can use a -beta version.

. test.setup

function Normalize-Version {
    param(
        [Parameter(Mandatory=$true)]
        [string]$Version
    )
    # Split version and prerelease
    $parts = $Version -split '-', 2
    $versionPart = $parts[0]
    $prereleasePart = if ($parts.Count -gt 1) { $parts[1] } else { $null }
    
    # Normalize version part to 4-part format (X.X.X.X)
    $versionComponents = @($versionPart -split '\.')
    while ($versionComponents.Count -lt 4) {
        $versionComponents += "0"
    }
    $normalizedVersion = $versionComponents -join '.'
    
    # Append prerelease if present
    if ($prereleasePart) {
        return "$normalizedVersion-$prereleasePart"
    }
    return $normalizedVersion
}

function New-Bundles {
    param(
        [Parameter(Mandatory=$true)]
        [string]$ProductVersion
    )
    $fileName = "test.double.trouble-$ProductVersion.zip"
    #create a zip file with the contents of the directory .Components/DoubleTrouble/v$ProductVersion
    $sourceDir = ".Components/DoubleTrouble/v$ProductVersion"

    $versionFile = Join-Path -Path $sourceDir -ChildPath "version.txt"
    "test.double.trouble: $ProductVersion" | Out-File -FilePath $versionFile -Encoding utf8

    Copy-Item -Path ".Components\yaml\tmsbuild.template.yaml" -Destination $sourceDir\tmsbuild.yaml -Force
    
     if (Test-Path $fileName) {
        Remove-Item $fileName
    }

    if (Test-Path $fileName) {
        Remove-Item $fileName
    }
    Compress-Archive -Path "$sourceDir\*" -DestinationPath $fileName
        
}

function New-ProductInSandbox {
    param(
        [Parameter(Mandatory=$true)]
        [string]$ProductVersion,
        [boolean]$Release = $false
    )
    $fileName = "test.double.trouble-$ProductVersion.zip"
    
    tmsrepo version-create test.double.trouble -repo:sandbox -version:$ProductVersion
    tmsrepo version-upload test.double.trouble -repo:sandbox -version:$ProductVersion -file:$fileName
    if ($Release) {
        tmsrepo version-release test.double.trouble -repo:sandbox -version:$ProductVersion
    }
    write-host "Uploaded version $ProductVersion to sandbox"
}

tms server-enable tms false
tms server-add sandbox api https://api.landgraf.dev/tms/sandbox ⁠ 
tms config-write -p:"tms smart setup options:error if skipped=false"


$products = tms list-remote -json | ConvertFrom-Json -AsHashtable
$directories = Get-ChildItem -Directory -Path ".Components/DoubleTrouble"

if (-not $products.ContainsKey("test.double.trouble")) {
    #If we can't find the product, create it in the sandbox and try again.
    #remember that for it to work, you need to have the credentials for tmsrepo set up.
    Write-Host "No product found, creating product in sandbox and trying again..."
    tmsrepo product-add test.double.trouble -repo:sandbox -name:"Double Trouble" -tmsid:_free_
}

$existingVersions = (tms versions-remote test.double.trouble -json | ConvertFrom-Json -AsHashtable)
#Get all directories in .Components/DoubleTrouble
foreach ($dir in $directories) {
    $version = $dir.Name -replace '^v', ''
    $versionNormalized = Normalize-Version -Version $version
    if ($existingVersions.ContainsKey($versionNormalized)) {
        Write-Host "Version $version already exists, skipping..."
        continue
    }
    New-Bundles -ProductVersion $version
}

foreach ($dir in $directories) {
    $version = $dir.Name -replace '^v', ''
    $versionNormalized = Normalize-Version -Version $version
    if ($existingVersions.ContainsKey($versionNormalized)) {
        Write-Host "Version $version already exists, skipping..."
        continue
    }
    New-ProductInSandbox -ProductVersion $version -Release:($version -notmatch '-')
}


$currentVersions = tms list-remote -json | ConvertFrom-Json -AsHashtable
$currentVersion = $currentVersions["test.double.trouble"].version
if ($currentVersion -ne "1.1.0.0") {
    throw "Expected version 1.1.0.0, but got $currentVersion"
}

tms install test.double.trouble
$currentVersions = tms list -json | ConvertFrom-Json -AsHashtable
$currentVersion = $currentVersions["test.double.trouble"].version

if ($currentVersion -ne "1.1.0.0") {
    throw "Expected version 1.1.0.0, but got $currentVersion"
}

 foreach ($dir in $directories) {
    $version = $dir.Name -replace '^v', ''
    $versionNormalized = Normalize-Version -Version $version
    if ($versionNormalized -eq "1.0.0.0-rc") {
        #skip the rc version, because it doesn't compile.
        continue
    }

    tms install test.double.trouble:$versionNormalized
    $currentVersions = tms list -json | ConvertFrom-Json -AsHashtable
    $currentVersion = $currentVersions["test.double.trouble"].version
    if ($currentVersion -ne $versionNormalized) {
        throw "Expected version $versionNormalized, but got $currentVersion"
    }
}

$expectedOutput = @{
    "1.0.0.0" = "2.000000 | -2.000000 | 0.000000";
    "1.0.0.0-beta" = "123.456000 | -2.000000 | 0.000000";
    "1.0.0.0-beta.2" = "2.000000 | -2.000000 | NAN";
    "1.0.0.0-rc" = "2.000000 | 2.000000 | 0.000000";
    "1.1.0.0" = "2.000000 | -2.000000 | 0.000000";
    "1.1.0.0-beta.1" = "3.000000 | -3.000000 | 0.000000";
    "1.2.0.0-alpha.1" = "17.000000 | -17.000000 | 0.000000";

}

$previousVersions = @()
foreach ($dir in $directories) {
    $version = $dir.Name -replace '^v', ''
    $versionNormalized = Normalize-Version -Version $version
    if ($versionNormalized -eq "1.0.0.0-rc") {
        #skip the rc version, because it doesn't compile.
        continue
    }
    tms install test.double.trouble:$version
    $currentVersions = tms list -json | ConvertFrom-Json -AsHashtable
    $currentVersion = $currentVersions["test.double.trouble"].version
    if ($currentVersion -ne $versionNormalized) {
        throw "Expected version $versionNormalized, but got $currentVersion"
    }

    $result = ./triple-trouble/Win32/Release/tripletrouble.exe
    if ($result -ne $expectedOutput[$versionNormalized]) {
        throw "Expected output ' $expectedOutput[$versionNormalized]', but got '$result'"
    }

    $expectedFile = ".\Downloads\CurrentVersions\test.double.trouble_production_$versionNormalized.zip"
    if (-Not (Test-Path $expectedFile)) {
        throw "Expected file '$expectedFile' does not exist"
    }
    #check there is only one file in the directory .\Downloads\CurrentVersions
    $files = @(Get-ChildItem -Path ".\Downloads\CurrentVersions")
    if ($files.Count -ne 1) {
        throw "Expected only one file in the directory '.\Downloads\CurrentVersions', but found $($files.Count)"
    }

    #check the current file is not in OldVersions
    $oldVersionFile = ".\Downloads\OldVersions\test.double.trouble_production_$versionNormalized.zip"
    if (Test-Path $oldVersionFile) {
        throw "File '$oldVersionFile' should not exist in OldVersions"
    }

    #check OldVersions contains all the previously tested files
    foreach ($prevVersion in $previousVersions) {
        $expectedOldFile = ".\Downloads\OldVersions\test.double.trouble_production_$prevVersion.zip"
        if (-Not (Test-Path $expectedOldFile)) {
            throw "Expected file '$expectedOldFile' does not exist in OldVersions"
        }
    }

    $previousVersions += $versionNormalized

    #check the DoubleTrouble*.bpl files have correct versioninfo
    $bplFiles = @(Get-ChildItem -Path "Products\test.double.trouble\packages" -Filter "DoubleTrouble*.bpl" -Recurse -ErrorAction SilentlyContinue)
    if ($bplFiles.Count -lt 1) {
        throw "Expected at least one DoubleTrouble*.bpl file in 'Products\test.double.trouble\packages', but found none"
    }
    $expectedPrerelease = $versionNormalized -match '-'
    $expectedProductVersion = ($versionNormalized -split '-')[0]
    foreach ($bpl in $bplFiles) {
        $versionInfo = $bpl.VersionInfo
        if ($versionInfo.IsPreRelease -ne $expectedPrerelease) {
            throw "File '$($bpl.FullName)' has IsPreRelease=$($versionInfo.IsPreRelease), expected $expectedPrerelease"
        }
        if ($versionInfo.ProductVersion -ne $expectedProductVersion) {
            throw "File '$($bpl.FullName)' has ProductVersion='$($versionInfo.ProductVersion)', expected '$expectedProductVersion'"
        }
    }
}