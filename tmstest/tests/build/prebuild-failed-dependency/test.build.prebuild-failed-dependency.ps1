# When the pre-build step of a product fails (here, because its defines filename doesn't end in .inc),
# that product never gets anything to run. The products that depended on it waited for it forever,
# and the build hung at 100% CPU. Now the failed product releases its dependents, and the build finishes.

. test.setup

tms config-write -p:"configuration for test.prebuild.base:advanced options:modify sources=true"

# If the bug comes back, tms build never returns. Run it with a timeout so the test fails instead of hanging.
$timeoutMinutes = 3
$tmsExe = (Get-Command tms).Definition
$stdout = Join-Path (Get-Location) "build-output.txt"
$stderr = Join-Path (Get-Location) "build-errors.txt"
$process = Start-Process -FilePath $tmsExe -ArgumentList "build" -NoNewWindow -PassThru `
    -RedirectStandardOutput $stdout -RedirectStandardError $stderr
$null = $process.Handle # without this, ExitCode is empty after the process finishes.

if (-not $process.WaitForExit($timeoutMinutes * 60 * 1000)) {
    $process.Kill($true)
    $process.WaitForExit()
    Get-Content $stdout
    throw "tms build didn't finish in $timeoutMinutes minutes. A project is probably waiting for a dependency whose pre-build failed."
}

$buildOutput = Get-Content $stdout
$buildOutput
Get-Content $stderr

if ($process.ExitCode -eq 0) {
    throw "tms build should have failed, since the pre-build of test.prebuild.base fails."
}

if (-not ($buildOutput | Out-String).Contains('The defines filename must have a ".inc" extension')) {
    throw "The build didn't fail for the expected reason (a defines filename without .inc extension)."
}

if (($buildOutput | Out-String).Contains('waiting for dependencies that will never be built')) {
    throw "The build ended with an internal error: some projects were waiting for dependencies that never finished."
}

$results = $buildOutput | tmsutil summary-to-json | ConvertFrom-Json -AsHashtable

$dependentOk = $false
foreach ($product in $results.Keys) {
    if ($product.Contains('Prebuild Dependent')) {
        if ($results[$product] -ne 'OK.') {
            throw "$product should have been built, but the result is '$($results[$product])'."
        }
        $dependentOk = $true
    }
    if ($product.Contains('Prebuild Base')) {
        if ($results[$product] -eq 'OK.') {
            throw "$product should have failed, since its pre-build step fails."
        }
    }
}

if (-not $dependentOk) {
    throw "Prebuild Dependent isn't in the build summary: $($results | Out-String)"
}

exit 0
