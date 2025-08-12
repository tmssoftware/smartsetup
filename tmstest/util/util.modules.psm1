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

function uninstall-and-check {
    param([Parameter(Mandatory, Position=0)] [int] $expectedRemaining)

    tms uninstall *

    $remaining = tms list -json | ConvertFrom-Json -AsHashtable
    if ($remaining.Count -ne $expectedRemaining) {
        Write-Error "There should be $expectedRemaining results remaining, but there are $($remaining.Count)."
    }
}
