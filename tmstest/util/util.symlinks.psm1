# Helpers for tests that check we never follow symlinks that point outside the product folder.

function Get-DeepWorkFolderLevels {
    param([Parameter(Mandatory)] [string]$Root)
    $levels = @($Root)
    $current = $Root
    foreach ($i in 1..4) {
        $current = Join-Path -Path $current -ChildPath "$i"
        $levels += $current
    }
    return $levels
}

function Get-CanaryFolders {
    param([Parameter(Mandatory)] [string]$Root)
    $levels = Get-DeepWorkFolderLevels -Root $Root
    $folders = @($levels | ForEach-Object { Join-Path -Path $_ -ChildPath "evil" })
    # zip and tar.zst products are extracted to Products\<id>, so a link to ..\evil there resolves to Products\evil.
    $folders += Join-Path -Path $levels[-1] -ChildPath "Products/evil"
    return $folders
}

# Creates <Root>\1\2\3\4, moves tms.config.yaml there and returns that folder.
# Every level gets an "evil" folder with a canary file inside. If tms follows a symlink to ..\evil,
# it will touch a canary, but whatever it deletes will still be inside the test folder.
function Initialize-DeepWorkFolder {
    param([Parameter(Mandatory)] [string]$Root)
    foreach ($folder in Get-CanaryFolders -Root $Root) {
        New-Item -ItemType Directory -Path $folder -Force | Out-Null
        Set-Content -Path (Join-Path -Path $folder -ChildPath "canary.txt") -Value "canary"
    }
    $workFolder = (Get-DeepWorkFolderLevels -Root $Root)[-1]
    Move-Item -Path (Join-Path -Path $Root -ChildPath "tms.config.yaml") -Destination $workFolder
    return $workFolder
}

# Checks that no canary was deleted or modified, and nothing was written into the canary folders.
function Assert-CanariesIntact {
    param(
        [Parameter(Mandatory)] [string]$Root,
        [Parameter(Mandatory)] [string]$Stage
    )
    foreach ($folder in Get-CanaryFolders -Root $Root) {
        $canary = Join-Path -Path $folder -ChildPath "canary.txt"
        if (-not (Test-Path -LiteralPath $canary -PathType Leaf)) {
            throw "$($Stage): the canary '$canary' was deleted."
        }
        if ((Get-Content -LiteralPath $canary -Raw).Trim() -ne "canary") {
            throw "$($Stage): the canary '$canary' was modified."
        }
        $children = @(Get-ChildItem -LiteralPath $folder -Force)
        if ($children.Count -ne 1) {
            throw "$($Stage): something was written into '$folder': $($children.Name -join ', ')"
        }
    }
}

# Checks there are no symlinks or junctions inside Path. Get-ChildItem -Recurse doesn't follow links.
function Assert-NoReparsePoints {
    param([Parameter(Mandatory)] [string]$Path)
    $links = @(Get-ChildItem -LiteralPath $Path -Recurse -Force -Attributes ReparsePoint)
    if ($links.Count -gt 0) {
        throw "Found links inside '$Path': $(($links | ForEach-Object { "$($_.FullName) -> $($_.Target)" }) -join ', ')"
    }
}

# Checks that Path was fully removed, including any links inside it.
function Assert-FolderRemoved {
    param(
        [Parameter(Mandatory)] [string]$Path,
        [Parameter(Mandatory)] [string]$Stage
    )
    if (Test-Path -LiteralPath $Path) {
        $leftovers = @(Get-ChildItem -LiteralPath $Path -Recurse -Force | ForEach-Object { $_.FullName })
        throw "$($Stage): '$Path' should have been removed, but it still exists. Leftovers: $($leftovers -join ', ')"
    }
}

# Runs tms with the arguments, and returns the output and exit code without stopping if it fails.
function Invoke-TmsAllowingFailure {
    $PSNativeCommandUseErrorActionPreference = $false
    $ErrorActionPreference = "Continue"
    $output = & tms @args 2>&1 | ForEach-Object { "$_" }
    $exitCode = $global:LASTEXITCODE
    $global:LASTEXITCODE = 0
    return [pscustomobject]@{ Output = $output; ExitCode = $exitCode }
}

Export-ModuleMember -Function Initialize-DeepWorkFolder, Assert-CanariesIntact, Assert-NoReparsePoints, Assert-FolderRemoved, Invoke-TmsAllowingFailure
