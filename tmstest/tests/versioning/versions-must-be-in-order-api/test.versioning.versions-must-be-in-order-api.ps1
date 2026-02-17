# We must ensure that the result of versions-remote always has the latest version first. 
# If we have a version like 2.10 and 2.9, 2.10 must be before 2.9.

. test.setup
tmscredentials
$versions = tms versions-remote tms.flexcel.vcl -json | ConvertFrom-Json

## check they are sorted by date descending
$lastVersion = $null
foreach ($versionName in $versions.psobject.Properties.Name) {
    $version = [version]$versionName
    if ($lastVersion -ne $null) {
        if ($version -gt $lastVersion) {
            Write-Error "Versions are not in order: $version is after $lastVersion"
        }
    }
    $lastVersion = $version
}

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
