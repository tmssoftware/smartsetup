function Invoke-WithExitCodeIgnored {
  param([Parameter(Mandatory, Position=0)] [scriptblock] $ScriptBlock)
  $PSNativeCommandUseErrorActionPreference = $false
  $ErrorActionPreference = "Continue"
  try{
    & $ScriptBlock
  } catch {
 }
}

function tmscredentials {
    if (-not $env:TMSTEST_CODE -or -not $env:TMSTEST_EMAIL) {
        Write-Error "The environment variables TMSTEST_CODE and TMSTEST_EMAIL must be set to run the tests."
        return
    }
    tms credentials -code:$env:TMSTEST_CODE -email:$env:TMSTEST_EMAIL
}

function uninstall_and_check {
    param([Parameter(Mandatory, Position=0)] [int] $expectedRemaining)

    tms uninstall *

    $remaining = tms list -json | ConvertFrom-Json -AsHashtable
    if ($remaining.Count -ne $expectedRemaining) {
        Write-Error "There should be $expectedRemaining results remaining, but there are $($remaining.Count)."
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
