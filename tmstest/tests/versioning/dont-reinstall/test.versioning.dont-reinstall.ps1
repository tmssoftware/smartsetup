# check that if we reinstall a certain version, it is not recompiled or redownloaded.
. test.setup
tmscredentials
tms server-enable community
tms server-add testserver zipfile "file:///$($tmsTestRootDir.Replace('\', '/'))/tmp-run/test-repos/tmsbuild_test_repos.zip"


$BuildResult = tms install tms.biz.aurelius:5.19 tmstest.a:v1.0.0
Test-BuildResultCounts -BuildResult $BuildResult -expectedNotModifiedCount 0 -expectedIgnoreCount 0 -expectedOkCount 8

$BuildResult = tms install tms.biz.aurelius:5.19 tmstest.a:v1.0.0

Test-BuildResultCounts -BuildResult $BuildResult -expectedNotModifiedCount 8 -expectedIgnoreCount 0 -expectedOkCount 0
Test-FetchResultCounts -FetchResult $BuildResult -expectedLines @("*tms.biz.aurelius -> SKIPPED (Up to date)*", "**tms.biz.bcl -> SKIPPED (Up to date)*")
