#Check that if the working folder is inside our current folder, projects aren't found twice

. test.setup
mv tms.config.yaml app\tms.config.yaml
Set-Location app
$workingFolder = "..\build"
tms config-write -p:"tms smart setup options:working folder=$workingFolder"
tms server-enable tms false
tms server-enable community true

$BuildResult = tms restore -skip-register tms.snapshot.yaml
Test-BuildResultCounts -BuildResult $BuildResult -expectedNotModifiedCount 0 -expectedIgnoreCount 0 -expectedOkCount 3

$BuildResult = tms build yankovsky.delphi.ast -full
Test-BuildResultCounts -BuildResult $BuildResult -expectedNotModifiedCount 1 -expectedIgnoreCount 0 -expectedOkCount 2
