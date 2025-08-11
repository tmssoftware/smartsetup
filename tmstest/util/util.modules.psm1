function Invoke-WithExitCodeIgnored {
  param([Parameter(Mandatory, Position=0)] [scriptblock] $ScriptBlock)
  $PSNativeCommandUseErrorActionPreference = $false
  $ErrorActionPreference = "Continue"
  try{
    & $ScriptBlock
  } catch {
 }
}

function uninstall-and-check {
    param([Parameter(Mandatory, Position=0)] [int] $expectedRemaining)

    tms uninstall *

    $remaining = tms list -json | ConvertFrom-Json -AsHashtable
    if ($remaining.Count -ne $expectedRemaining) {
        Write-Error "There should be $expectedRemaining results remaining, but there are $($remaining.Count)."
    }
}
