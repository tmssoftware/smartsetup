function Test-DownloadFolders {
    param(
        [Parameter(Mandatory=$true)]
        [string]$Product,
        
        [Parameter(Mandatory=$false)]
        [string[]]$CurrentDownloads = @(),
        
        [Parameter(Mandatory=$false)]
        [string[]]$OldDownloads = @()
    )
    
    # Test CurrentVersions folder
    $currentPath = "./Downloads/CurrentVersions"
    $currentFiles = @(Get-ChildItem -Path $currentPath -Filter "$Product*" -File)
    
    if ($CurrentDownloads.Count -gt 0) {
        foreach ($version in $CurrentDownloads) {
            $matchingFiles = $currentFiles | Where-Object { $_.Name -match [regex]::Escape($version) }
            if (-not $matchingFiles) {
                throw "Expected file matching '$Product' and version '$version' not found in $currentPath"
            }
        }
    }
    
    $expectedCount = $CurrentDownloads.Count
    if ($currentFiles.Count -ne $expectedCount) {
        throw "Expected $expectedCount file(s) matching '$Product' in $currentPath, but found $($currentFiles.Count)"
    }
    
    # Test OldVersions folder
    $oldPath = "./Downloads/OldVersions"
    if (-not (Test-Path -Path $oldPath)) {
        if ($OldDownloads.Count -eq 0) {
            return
        } else {
            throw "Expected OldVersions folder at $oldPath does not exist."
        }
    }
    $oldFiles = @(Get-ChildItem -Path $oldPath -Filter "$Product*" -File)
    
    if ($OldDownloads.Count -gt 0) {
        foreach ($version in $OldDownloads) {
            $matchingFiles = $oldFiles | Where-Object { $_.Name -match [regex]::Escape($version) }
            if (-not $matchingFiles) {
                throw "Expected file matching '$Product' and version '$version' not found in $oldPath"
            }
        }
    }
    
    $expectedOldCount = $OldDownloads.Count
    if ($oldFiles.Count -ne $expectedOldCount) {
        throw "Expected $expectedOldCount file(s) matching '$Product' in $oldPath, but found $($oldFiles.Count)"
    }
}

function Test-CachedFiles {
    param(
        [Parameter(Mandatory=$true)]
        [string]$Product,
        
        [Parameter(Mandatory=$false)]
        [bool]$WasCached = $true
    )
    
    #Open the file ./Logs/tms.log and search for the string "Cached download found for $Product, no need to download again"
    $logPath = "./Logs/tms.log"
    $logContent = Get-Content -Path $logPath
    $searchString = "Cached download found for $Product, no need to download again"
    $found = $logContent | Where-Object { $_ -like "*$searchString*" }
    if ($WasCached -and (-not $found)) {
        throw "Expected cached download message for '$Product' not found in log."
    }
    if (-not $WasCached -and $found) {
        throw "Did not expect cached download message for '$Product', but it was found in log."
    }
}