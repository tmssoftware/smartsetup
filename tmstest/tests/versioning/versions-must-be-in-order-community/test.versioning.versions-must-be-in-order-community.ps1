# Versions-remote must return versions in order, latest first.
# We must ensure that the result of versions-remote always has the latest version first.

. test.setup
tms server-enable community
tms server-enable tms false

$versions = tms versions-remote sglienke.Spring4D -json | ConvertFrom-Json

## check they are sorted by date descending
$lastDate = $null
foreach ($versionDate in $versions.psobject.Properties.Value) {
    $date = [datetime]$versionDate.release_date
    if ($date -eq $null) {
        Write-Error "Version release date is null"
    }   
    if ($lastDate -ne $null) {
        if ($date -gt $lastDate) {
            Write-Error "Versions are not in order by date: $date is after $lastDate"
        }
    }
    $lastDate = $date
}