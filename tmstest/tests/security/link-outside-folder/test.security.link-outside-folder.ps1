# Check both  that we can't link our file outside our folders, and that we can't link an outside file to our folders.

. test.setup

tms config-write -p:configuration-for-all-products:options:skip-register=false

$files = Get-ChildItem -Path "dangerous\tmsbuild.fail.*.yaml"
foreach ($file in $files) {
    Write-Host "Testing with config file: '$($file.FullName)'" -ForegroundColor Yellow
    Copy-Item -Path $file.FullName -Destination "dangerous\tmsbuild.yaml" -Force
    $results = Invoke-WithExitCodeIgnored{tms build}
    $errFile = [System.IO.Path]::ChangeExtension($file.FullName, ".error.txt")
    if (-not ($results | Out-String).Contains((Get-Content -Raw $errFile))) {
        Write-Error "Unexpected error message: $results."
    }
}

$files = Get-ChildItem -Path "dangerous\tmsbuild.ok.*.yaml"
foreach ($file in $files) {
    Copy-Item -Path $file.FullName -Destination "dangerous\tmsbuild.yaml" -Force
    tms build 
    $fileFile = [System.IO.Path]::ChangeExtension($file.FullName, ".file.txt")
    $linkedFile = Get-Content -Raw $fileFile 
    $linkedFile = $linkedFile.Trim()
     Write-Host "Checking that linked file '$linkedFile' exists." -ForegroundColor Green
    if (-not (Test-Path -Path $linkedFile)) {
        Write-Error "Expected linked file '$linkedFile' does not exist."
    }
}


tms build -unregister