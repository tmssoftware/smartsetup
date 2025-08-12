[CmdletBinding()]
param(
    [Parameter(Mandatory = $false, Position = 0)]
    [string]$testsToRunParam = "",
    [Alias("skip-slow")]    
    [switch]$skipSlow = $false,
    [Alias("failed")]
    [switch]$openFailedFiles = $false
)
. $PSScriptRoot/util/util.errors.ps1

if (-not $tmsTestRootDir) {
    Write-Error "The variable `tmsTestRootDir` is not set. You need to setup the environment, see $($PSScriptRoot)/README.md."
}

if (! $env:TMSTEST_CODE -or ! $env:TMSTEST_EMAIL) {
    Write-Error "The environment variables TMSTEST_CODE and TMSTEST_EMAIL must be set to run the tests."
}
. $PSScriptRoot/util/util.set_tmstest_util.ps1

$testsToRun = $testsToRunParam

# If the parameter is ourselves, Stop. This is likely because we are debugging from vscode.
if ($testsToRun -eq $PSCommandPath) {
    # $testsToRun = ""
    Write-Error "To run the tests, select the one you want to run, and press F5. If you want to run all tests, run tmstest from the console."
}

if (Test-Path -LiteralPath $testsToRun) {
    # if the parameter is a file, run that file.
    $tests = @(Get-Item($testsToRun))
}
else {
    # By default, the parameter testsToRun is a pattern to match test files.
    $tests = [array](Get-ChildItem -Path $PSScriptRoot/tests -Recurse -Include test.*$testsToRun*.ps1)
}

if ($null -eq $tests -or $tests.Count -eq 0) {
    Write-Error "No tests found matching the pattern '$testsToRun'."
    Exit 1
}   

$successfulTests = @()
$failedTests = @()
$skippedTests = @()
$index = 0
$opened = 0

$IsSingleTest = $tests.Count -eq 1
tmsutil clean-locked  | Out-Null
$stopWatch = New-Object -TypeName 'System.Diagnostics.Stopwatch'
$stopWatch.Start()

foreach ($test in $tests) {
    $index++
    if ($skipSlow -and $test.Name -like "test.slow.*") {
        Write-Host "Skipping slow test: $($test.Name)" -ForegroundColor Yellow
        $skippedTests += $test.Name
        continue
    }
        
    $testDir = "$tmsTestRootDir\tmp-run\$($test.Directory.Name)"

    if ($openFailedFiles) {
        if (Test-Path -Path "$testDir\ok.txt") { continue }
        if ($opened -lt 5) {
            Write-Output "Opening test results for $($test.Directory.Name)."
            $logPath = "$testDir\output2.log"
            if (Test-Path -Path $logPath) {
                $opened++
                Start-Process -FilePath $logPath
            }
        }
        else {
            Write-Output "Maximum number of opened log files reached."
        }
        continue
    }

    Write-Host "Running test: $($test.Name)"  -NoNewline -ForegroundColor Cyan
    try {
        # Delete the test directory if it exists
        if (Test-Path -Path $testDir) {
            #Remove-Item -Path $testDir -Recurse -Force
            tmsutil delete-folder -keep-root-folder -folder:"$testDir" | Out-Null
        }

        # Copy the folder to the test directory
        Copy-Item -Path $test.Directory.FullName -Destination "$tmsTestRootDir\tmp-run\" -Recurse -Force -Exclude "test.*.ps1"
        Copy-Item -Path "$tmsTestRootDir\util\tms.starting.config.yaml" -Destination "$testDir\tms.config.yaml" -Force

        Set-Location $testDir
        try {
            # Execute the test script
            Start-Transcript -Path "$testDir\output.log" -UseMinimalHeader | Out-Null
            try {
                & $test.FullName *>&1 | Out-File -FilePath "$testDir\output2.log" -Encoding utf8
                if ($LASTEXITCODE -ne 0) {
                    Write-Error "Test script '$($test.Name)' failed with exit code $LASTEXITCODE."
                }
            }
            finally {
                Stop-Transcript | Out-Null
            }
            $successfulTests += $test.Name
            Write-Output "Ok" > "$testDir\ok.txt"
            Write-Host " -> OK" -ForegroundColor Green

        }
        catch {
            Write-Host " -> FAILED" -ForegroundColor Red
            #Only show the exception if we are running a single test. If not, the output can be too verbose.
            Write-Output $_.Exception.Message >> "$testDir\output2.log"
            if ($IsSingleTest) { Write-Host "Error executing test script: $($_.Exception.Message)" }
            $failedTests += $test.Name
            continue
        }
        finally {
            Set-Location -
            $percent = ($index / $tests.Length) * 100
            Write-Progress -Activity "Running tests" -Status "Test $($index) of $($tests.Length)." -PercentComplete $percent -CurrentOperation "$($test.Name)"
        }
        
    }
    catch {
        Write-Host " -> FAILED" -ForegroundColor Red
        if ($IsSingleTest) { Write-Host "Error setting up test: $($_.Exception.Message)" }
        $failedTests += $test.Name
        continue
    }
}

$stopWatch.Stop()
$elapsedTime = $stopWatch.Elapsed
Write-Progress -Activity "Running tests" -Status "Completed!" -PercentComplete 100 -Completed

Write-Host "Tests completed in $($elapsedTime.TotalSeconds.ToString("F2")) seconds." -ForegroundColor Cyan
Write-Host ""
Write-Host "Successful tests: $($successfulTests.Count)" -ForegroundColor Green
Write-Host "Failed tests: $($failedTests.Count)" -ForegroundColor $(if ($failedTests.Count -eq 0) {'Green'} else {'Red'})
Write-Host "Skipped tests: $($skippedTests.Count)" -ForegroundColor $(if ($skippedTests.Count -eq 0) {'Green'} else {'Yellow'})

if ($failedTests.Count -gt 0) {
    Write-Host "Failed tests: $($failedTests -join ', ')" -ForegroundColor Red
    Exit 1
}
else {
    Write-Host "All tests passed!" -ForegroundColor Green
    Exit 0
}

