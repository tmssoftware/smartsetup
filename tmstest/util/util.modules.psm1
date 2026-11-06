function Invoke-WithExitCodeIgnored {
  param([Parameter(Mandatory, Position=0)] [scriptblock] $ScriptBlock)
  $PSNativeCommandUseErrorActionPreference = $false
  $ErrorActionPreference = "Continue"
  try{
    & $ScriptBlock
  } catch {
 }
 if ($global:LASTEXITCODE -eq 0) {
    throw "The command '$ScriptBlock' was expected to fail, but it succeeded."
    }
 $global:LASTEXITCODE = 0
}

function Test-CommandFails {
    param(
        [Parameter(Mandatory, Position=0)] [scriptblock] $ScriptBlock,
        [Parameter(Mandatory, Position=1)] [string] $Message,
        [Parameter(Position=2)] [string] $ExtraInfo = ""
    )
    $opResult = Invoke-WithExitCodeIgnored $ScriptBlock
    $testOk = Test-Result -CommandResult $opResult -Message $Message  
    if (-not ($testOk)) {   
        throw "$($ExtraInfo): The error message should mention '$($Message)'.'. Actual message: $($opResult)"
    }
}

function Test-CommandOk {
    param(
        [Parameter(Mandatory, Position=0)] [scriptblock] $ScriptBlock,
        [Parameter(Mandatory, Position=1)] [string] $Message,
        [Parameter(Position=2)] [string] $ExtraInfo = ""
    )
    $opResult = & $ScriptBlock
    $testOk = Test-Result -CommandResult $opResult -Message $Message  
    if (-not ($testOk)) {   
        throw "$($ExtraInfo): The message should mention '$($Message)'.'. Actual message: $($opResult)"
    }
}

# pass negative values to not check for that count.
function Test-BuildResultCounts {
    param (
        [Parameter(Mandatory)]
        [array]$BuildResult,
        [Parameter(Mandatory)]
        [int]$expectedNotModifiedCount,
        [Parameter(Mandatory)]
        [int]$expectedIgnoreCount,    
        [Parameter(Mandatory)]
        [int]$expectedOkCount    
    )
    
    $ignoreCount = 0
    $notModifiedCount = 0
    $okCount = 0

    $SummaryReached = $false
    
    foreach ($line in $BuildResult) {
        if ($line -like "*=== Build Summary ===*") {
            $SummaryReached = $true
            continue
        }
        if (-not $SummaryReached) {
            continue
        }

        if ($line.Trim().EndsWith("]")) {
            break
        }
        if ($line -like "*-> IGNORED*") {
            $ignoreCount++
        }
        if ($line -like "*-> NOT MODIFIED*") {
            $notModifiedCount++
        }
        if ($line -like "*-> OK*") {
            $okCount++
        }
    }
    if ($expectedIgnoreCount -ge 0 -and $ignoreCount -ne $expectedIgnoreCount) {
        throw "There should be $expectedIgnoreCount lines with '-> IGNORED' in the build output, but there are $ignoreCount."
    }
    if ($expectedNotModifiedCount -ge 0 -and $notModifiedCount -ne $expectedNotModifiedCount) {
        throw "There should be $expectedNotModifiedCount lines with '-> NOT MODIFIED' in the build output, but there are $notModifiedCount."
    }
    if ($expectedOkCount -ge 0 -and $okCount -ne $expectedOkCount) {
        throw "There should be $expectedOkCount lines with '-> OK' in the build output, but there are $okCount."
    }
    
    
}

function Test-FetchResultCounts {
    param (
        [Parameter(Mandatory)]
        [array]$FetchResult,
        [Parameter(Mandatory)]
        [AllowEmptyCollection()]
        [array]$expectedLines  
    )
    
    $SummaryReached = $false
    $TotalLines = 0
    foreach ($line in $FetchResult) {
        if ($line -like "*=== Fetch Summary ===*") {
            $SummaryReached = $true
            continue
        }
        if (-not $SummaryReached) {
            continue
        }

        if ($line.Trim().EndsWith("]")) {
            break
        }
        $TotalLines++
        $found = $false
        foreach ($expectedLine in $expectedLines) {
            if ($line -like $expectedLine) {
                $found = $true
                break
            }
        }
        if (-not $found) {
            throw "The line '$line' was not expected in the fetch output. $($FetchResult)"
        }   
    }
    if ($TotalLines -ne $expectedLines.Count) {
        throw "There should be $($expectedLines.Count) lines in the fetch output, but there are $TotalLines."
    }
    
}

function Test-RepoFetchResultCounts {
    param (
        [Parameter(Mandatory)]
        [array]$FetchResult,
        [Parameter(Mandatory)]
        [AllowEmptyCollection()]
        [array]$expectedLines  
    )
    
    $SummaryReached = $false
    $TotalLines = 0
    foreach ($line in $FetchResult) {
        if ($line -like "*=== Repository Summary ===*") {
            $SummaryReached = $true
            continue
        }
        if (-not $SummaryReached) {
            continue
        }

        if ($line.Trim().EndsWith("]")) {
            break
        }
        $TotalLines++
        $found = $false
        foreach ($expectedLine in $expectedLines) {
            if ($line -like $expectedLine) {
                $found = $true
                break
            }
        }
        if (-not $found) {
            throw "The line '$line' was not expected in the fetch output: $($FetchResult)"
        }   
    }
    if ($TotalLines -ne $expectedLines.Count) {
        throw "There should be $($expectedLines.Count) lines in the fetch output, but there are $TotalLines."
    }
    
}
function Test-Result {
    param(
        [string[]]$CommandResult,
        [string]$Message
    )

    foreach ($line in $CommandResult) {
        if ($line -like $Message) {
            return $true
        }
    }
    return $false
}

function CheckLogHasString {
    param(
        [Parameter(Mandatory, Position=0)] [string] $Message
    )
    # Reads the most recent tms log and verifies it contains $Message.
    # Useful for tests that assert that a specific message reached the log
    # (for example, evidence that an injected payload was actually attempted
    # against the underlying tool before being blocked by a second layer of defense).
    $logFileHtml = tms log-view -print
    #remove .html extension
    $logFile = [System.IO.Path]::ChangeExtension($logFileHtml, $null)
    $logContent = Get-Content -Path ($logFile) -Raw
    if ($logContent -notmatch $Message) {
        throw "Expected log content to contain '$Message', but it did not. Log content: $logContent"
    }
}

function CheckTextHasString {
    param(
        [Parameter(Mandatory, Position=0)] [string[]] $Text,
        [Parameter(Mandatory, Position=1)] [string] $Message
    )

    if (-not (Test-Result  -CommandResult $Text -Message $Message)) {
        throw "Error: Text '$($Text)' doesn't contain '$($Message)'"
    }

}

function bds {
    param(
        [Parameter(Mandatory, Position=0)] [string] $ProjectFile,
        [Parameter(Mandatory = $false, Position=1)] [string[]] $RegistryKey = ""
    )

    #if testapp isn't in the latest dproj version, it will ask for saving the changes when closing.
    #so we set ProjectVersion to a high number to avoid that.
    $fullProjectFile = (Resolve-Path $ProjectFile).Path
    $xml = New-Object System.Xml.XmlDocument
    $xml.PreserveWhitespace = $true
    $xml.Load($fullProjectFile)
    $versionNodes = $xml.SelectNodes("//*[local-name()='ProjectVersion']")
    foreach ($node in $versionNodes) {
        $node.InnerText = "99.0"
    }
    $xml.Save($fullProjectFile)

    $regCommand = ""
    if ($RegistryKey -ne "") {
        $regCommand = "-r $RegistryKey"
    }
    Start-Process -FilePath "$Env:TMS_BDS" -ArgumentList @("$ProjectFile", "-ns", $regCommand, "-b") -NoNewWindow -Wait
}

function tmscredentials {
#Not used anymore. now tms credentials are stored globally, and should be set before running the tests.
}

function uninstall_and_check {
    param([Parameter(Mandatory, Position=0)] [int] $expectedRemaining)

    tms uninstall *

    $remaining = tms list -json | ConvertFrom-Json -AsHashtable
    if ($remaining.Count -ne $expectedRemaining) {
        throw "There should be $expectedRemaining results remaining, but there are $($remaining.Count)."
    }
}

function compare-files {
    param(
        [Parameter(Mandatory, Position=0)] [string] $file1,
        [Parameter(Mandatory, Position=1)] [string] $file2
    )

    if (-not (Test-Path $file1)) {
        throw "File not found: $file1"
    }
    if (-not (Test-Path $file2)) {
        throw  "File not found: $file2"
    }

    $content1 = Get-FileHash -Path $file1 -Algorithm SHA256
    $content2 = Get-FileHash -Path $file2 -Algorithm SHA256

    if ($content1.Hash -ne $content2.Hash) {
        throw "Files '$file1' and '$file2' do not match."
    } else {
        Write-Output "Files '$file1' and '$file2' match." -ForegroundColor Green
    }
}

function compare-files_diff {
    param(
        [Parameter(Mandatory, Position=0)] [string] $file1,
        [Parameter(Mandatory, Position=1)] [string] $file2
    )

    if (-not (Test-Path $file1)) {
        throw "File not found: $file1"
    }
    if (-not (Test-Path $file2)) {
        throw  "File not found: $file2"
    }

    $content1 = Get-FileHash -Path $file1 -Algorithm SHA256
    $content2 = Get-FileHash -Path $file2 -Algorithm SHA256

    if ($content1.Hash -eq $content2.Hash) {
        throw "Files '$file1' and '$file2' are the same and should be different."
    } else {
        Write-Output "Files '$file1' and '$file2' are different as expected." -ForegroundColor Green
    }
}

filter Assert-ValueIs
{
  param([string]$expected)

  $same = ($expected -eq $_)
  if (!$same) { 
    throw "Actual value: '$_' is not equal to expected value: '$expected'"
  }
}

filter Assert-ValueContains
{
  param([string]$expected)
  
  $ok = ($_.Contains($expected))
  if (!$ok) { 
    throw "'$_' does not contain '$expected'"
  }
}

filter Assert-ValueNotContains
{
  param([string]$expected)
  
  $ok = (-not $_.Contains($expected))
  if (!$ok) { 
    throw "'$_' contains '$expected'"
  }
}

function Set-AlternateRegistryKey {
    param(
        [Parameter(Mandatory, Position=0)] [string] $RegKey,
        [Parameter(Mandatory, Position=1)] [bool] $RegisterAllDelphiVersions
    )

    $fullRegPath = "HKCU:\Software\Embarcadero\$RegKey"
    if (Test-Path $fullRegPath) {
        Remove-Item -Path $fullRegPath -Recurse -Force
    }

    if ($RegisterAllDelphiVersions) {
        foreach ($ver in $Global:AllDelphiVersions) {
            # Create the empty registry entries in $regkey so smart setup finds them.
            Start-Process -FilePath "$($Global:ALL_BDS_ROOT_DIRS[$ver])\bin\bds.exe" -ArgumentList @("-ns", "-r$RegKey", "-ProductInfo:Trial") -NoNewWindow -Wait -RedirectStandardOutput "NUL"
        }
    }
    else {
    # Create the empty registry entries in $regkey so smart setup finds them.
    Start-Process -FilePath $Env:TMS_BDS -ArgumentList @("-ns", "-r$RegKey", "-ProductInfo:Trial") -NoNewWindow -Wait -RedirectStandardOutput "NUL"
    }

    tms config-write -p:"tms smart setup options:alternate registry key=$RegKey"
}


function Test-tmsgui {
    param(
        [string]$suffixParam = "",
        [string]$tmsguiexe = "",
        [string]$extraArgs = ""
    )
    $suffix = $suffixParam
    if ($suffix -ne "") {
        $suffix = "." + $suffix
    }
    if ("" -eq $tmsguiexe){ $tmsguiexe = Get-Alias tmsgui | Select-Object -ExpandProperty Definition }

    $CallingScriptFolderName = Split-Path -Path (Split-Path -Path $MyInvocation.ScriptName -Parent) -Leaf
    $CallingScriptFolder = Join-Path -Path $tmsTestRootDir -ChildPath "tmp-run\$CallingScriptFolderName"


    $automationScriptFileName = "$CallingScriptFolder\auto-scripts\automate.script$($suffix).txt" 
    $automationLogFileName = "$CallingScriptFolder\auto-scripts-result\tmsgui.automation$($suffix).log" 
    # Don't use -Wait: it waits for child processes too, and tmsgui can relaunch itself after a self-update.
    $process = Start-Process -FilePath $tmsguiexe -ArgumentList "-automation-script:`"$automationScriptFileName`"", "-automation-log:`"$automationLogFileName`"", $extraArgs -PassThru
    $null = $process.Handle # cache the handle so ExitCode is available after the process exits.
    $process.WaitForExit()
    if ($process.ExitCode -ne 0) {
        throw "tmsgui exited with error code $($process.ExitCode)"
    }
    Compare-Files $CallingScriptFolder\reference\tmsgui.automation$($suffix).log $automationLogFileName
}


# Copies tms.exe and tmsgui.exe to $Folder, patches the version in their VS_FIXEDFILEINFO resource
# so their hashes change but they still run, and writes tms.smartsetup.hashes.txt with the new hashes.
function New-AutoUpdateBundle {
    param(
        [Parameter(Position=0)] [version] $Version = "99.0.0.0",
        [Parameter(Position=1)] [string] $Algorithm = "SHA256",
        [Parameter(Position=2)] [bool] $AddHashes = $true
    )

     $aliases = @("tms", "tmsgui")

    $actualFiles = @()
    foreach ($alias in $aliases) {
        $exe = Get-Alias $alias
        Copy-Item $exe.Definition "./$(Split-Path $exe.Definition -Leaf)" -Force
        $actualFiles += (Resolve-Path "./$(Split-Path $exe.Definition -Leaf)").Path
    }

    $Folder = Join-Path -Path $PWD -ChildPath "fake-auto-update-bundle"
    New-Item -ItemType Directory -Force $Folder | Out-Null
    $hashLines = @()
    foreach ($exe in $actualFiles) {
        $target = Join-Path $Folder (Split-Path $exe -Leaf)
        Copy-Item $exe $target -Force
        Set-ItemProperty -Path $target -Name IsReadOnly -Value $false
        Set-ExeFixedFileVersion -FileName $target -Version $Version
        $hash = (Get-FileHash -Path $target -Algorithm $Algorithm).Hash.ToLowerInvariant()
        $hashLines += "$(Split-Path $target -Leaf): $hash"
    }

    if ($AddHashes){ Set-Content -Path (Join-Path $Folder "tms.smartsetup.hashes.txt") -Value $hashLines -Encoding utf8 }

    $zipFolder = Join-Path -Path $PWD -ChildPath "Downloads\CurrentVersions"
    New-Item -ItemType Directory -Force $zipFolder | Out-Null
    $zipFile = Join-Path -Path $zipFolder -ChildPath "tms.smartsetup.windows_production_$($Version).zip"
    
    [System.IO.Compression.ZipFile]::CreateFromDirectory($Folder, $zipFile)
}

function Restore-AutoupdateBundle {
 $aliases = @("tms", "tmsgui")
    foreach ($alias in $aliases) {
        $exe = Get-Alias $alias
        Copy-Item $exe.Definition "./$(Split-Path $exe.Definition -Leaf)" -Force
    }
}

# Patches the file and product version in the VS_FIXEDFILEINFO block in place. The file size doesn't change,
# so the exe keeps working (an Authenticode signature, if any, becomes invalid).
function Set-ExeFixedFileVersion {
    param(
        [Parameter(Mandatory, Position=0)] [string] $FileName,
        [Parameter(Mandatory, Position=1)] [version] $Version
    )

    $fullName = (Resolve-Path $FileName).Path
    $bytes = [System.IO.File]::ReadAllBytes($fullName)
    # VS_FIXEDFILEINFO starts with dwSignature = 0xFEEF04BD followed by dwStrucVersion = 0x00010000
    $signature = [byte[]](0xBD, 0x04, 0xEF, 0xFE, 0x00, 0x00, 0x01, 0x00)
    $idx = [Array]::IndexOf($bytes, $signature[0])
    while ($idx -ge 0) {
        $found = $idx + $signature.Length -le $bytes.Length
        for ($i = 1; $found -and $i -lt $signature.Length; $i++) {
            if ($bytes[$idx + $i] -ne $signature[$i]) { $found = $false }
        }
        if ($found) { break }
        $idx = [Array]::IndexOf($bytes, $signature[0], $idx + 1)
    }
    if ($idx -lt 0) {
        throw "Can't find a version resource in '$FileName'."
    }

    $ms = [BitConverter]::GetBytes([uint32](($Version.Major -shl 16) -bor $Version.Minor))
    $ls = [BitConverter]::GetBytes([uint32](([math]::Max($Version.Build, 0) -shl 16) -bor [math]::Max($Version.Revision, 0)))
    # dwFileVersionMS, dwFileVersionLS, dwProductVersionMS, dwProductVersionLS
    foreach ($offset in @(8, 16)) {
        [Array]::Copy($ms, 0, $bytes, $idx + $offset, 4)
        [Array]::Copy($ls, 0, $bytes, $idx + $offset + 4, 4)
    }
    [System.IO.File]::WriteAllBytes($fullName, $bytes)
}
