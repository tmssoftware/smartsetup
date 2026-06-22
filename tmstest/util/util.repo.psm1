function Update-TmsbuildYamlWithLocalUrl {
    param(
        [string]$TmsbuildFilePath
    )
    
    $tmsbuildFile = Get-Item -Path $TmsbuildFilePath
    $productFolder = $tmsbuildFile.Directory
    $inApplicationSection = $false
    $foundNameLine = $false
    $content = Get-Content -Path $tmsbuildFile
    $newContent = @()
    foreach ($line in $content) {
        $newContent += $line
        if ($foundNameLine) {
            continue
        }
        if ($line -match '^\s*application:\s*$') {
            $inApplicationSection = $true
            continue
        }
        if (-not $inApplicationSection) {
            continue
        }
        if ($line -match '^\w*:\$') {
            # we reached a new section
            $inApplicationSection = $false
            continue
        }
        if ($line -match '  name:\s*.*') {
            $fileUrl = "file:///" + ($productFolder.FullName -replace '\\', '/')
            $newContent += "  url: `"$fileUrl`""
            $foundNameLine = $true
            continue
        }
    }
    if (-not $foundNameLine) {
        throw "Could not find application name line in $($tmsbuildFile.FullName)"
    }
    Set-Content -Path $tmsbuildFile -Value $newContent
}