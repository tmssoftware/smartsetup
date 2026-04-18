# see if all.installed.components is working for apps

. test.setup

Set-Location myapp\All
$BuildResult = tms restore -skip-register tms.snapshot.yaml

Test-BuildResultCounts -BuildResult $BuildResult -expectedNotModifiedCount 0 -expectedIgnoreCount 0 -expectedOkCount 3

$BuildResult = tms build *ast* -full
Test-BuildResultCounts -BuildResult $BuildResult -expectedNotModifiedCount 1 -expectedIgnoreCount 0 -expectedOkCount 2
