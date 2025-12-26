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

function bds {
    param(
        [Parameter(Mandatory, Position=0)] [string] $ProjectFile,
        [Parameter(Mandatory = $false, Position=1)] [string[]] $RegistryKey = ""
    )

    Start-Process -FilePath $Env:TMS_BDS -ArgumentList @("-ns", "-r$RegistryKey", "-b", $ProjectFile) -NoNewWindow -Wait
}

function tmscredentials {
    if (-not $env:TMSTEST_CODE -or -not $env:TMSTEST_EMAIL) {
        throw "The environment variables TMSTEST_CODE and TMSTEST_EMAIL must be set to run the tests."
    }
    tms credentials -code:$env:TMSTEST_CODE -email:$env:TMSTEST_EMAIL
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
    throw "'$_' is not equal to '$expected'"
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

