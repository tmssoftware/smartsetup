# Test we can retrieve the logs given the session id.
# Also verify that we truncate the logs after 10, so they don't grow indefinitely.
. test.setup

function Get-SessionIdLine([string[]]$lines) {
    # Lines look like "[timestamp] Session Id: <id>"
    return $lines | Where-Object { $_ -match '^\s*(\[[^\]]*\]\s*)?Session Id: ' } | Select-Object -First 1
}

function Assert-SessionLogFileExists([string]$sessionId) {
    $logFile = (tms log-view $sessionId -print -text | Out-String).Trim()
    if (-not $logFile) {
        throw "The log file path for session '$sessionId' is empty."
    }

    if (-not (Test-Path $logFile)) {
        throw "The log file '$logFile' for session '$sessionId' does not exist."
    }

    $logContent = Get-Content -Path $logFile -Raw
    if (-not $logContent.Contains($sessionId)) {
        throw "The log file '$logFile' does not contain the session id '$sessionId'."
    }

    $logFileHtml = (tms log-view $sessionId -print | Out-String).Trim()

    if (-not (Test-Path "$logFileHtml")) {
        throw "The file '$logFileHtml' for session '$sessionId' does not exist."
    }

    if ($logFileHtml -eq $logFile) {
        throw "The log file path '$logFileHtml' for the html file is the same as the log path '$logFile' for session '$sessionId'."
    }
}

function Assert-SessionLogFileDoesntExist([string]$sessionId) {
    $logFile = (tms log-view $sessionId -print -text | Out-String).Trim()
    if (Test-Path "$logFile") {
        throw "Expected no log file path for session '$sessionId', but found '$logFile'."
    }
    $logFileHtml = (tms log-view $sessionId -print | Out-String).Trim()
    if (Test-Path "$logFileHtml") {
        throw "Expected no log file path for session '$sessionId', but found '$logFileHtml'."
    }

    if ($logFileHtml -eq $logFile) {
        throw "The log file path '$logFileHtml' for the html file is the same as the log path '$logFile' for session '$sessionId'."
    }
    
}


function CheckLog([scriptblock]$command) {
    $output = & $command 2>&1 | ForEach-Object { "$_" }
    $sessionLine = Get-SessionIdLine $output
    if (-not $sessionLine) {
        throw "Expected a 'Session Id: ' line in the output of '$command', but none was found."
    }

    $sessionId = ($sessionLine -replace '^.*?Session Id: ', '').Trim()
    if (-not $sessionId) {
        throw "The 'Session Id: ' line in the output of '$command' is empty."
    }

    Assert-SessionLogFileExists -sessionId $sessionId
    return $sessionId
}

function CheckNoLog([scriptblock]$command) {
    $output = & $command 2>&1 | ForEach-Object { "$_" }
    $sessionLine = Get-SessionIdLine $output
    if ($sessionLine) {
        throw "Expected no 'Session Id: ' line in the output of '$command', but found '$sessionLine'."
    }
}

$sessionIds = @("", "", "", "")
$sessionIds[0] = CheckLog { tms install tms.vcl.crypto }
$sessionIds[1] = CheckLog { tms update}
CheckNoLog { tms config -print }
$sessionIds[2] = CheckLog { tms doctor }
$sessionIds[3] = CheckLog { tms uninstall * }

for ($i = 3; $i -ge 0; $i--) {
    Assert-SessionLogFileExists -sessionId $sessionIds[$i]
}

for ($i = 0; $i -le 5; $i++) {
    tms build
}

for ($i = 0; $i -le 3; $i++) {
    CheckLog {tms build}
    Assert-SessionLogFileDoesntExist -sessionId $sessionIds[$i]  

    if ($i -le 2) {
       Assert-SessionLogFileExists -sessionId $sessionIds[$i + 1]  
    }
}