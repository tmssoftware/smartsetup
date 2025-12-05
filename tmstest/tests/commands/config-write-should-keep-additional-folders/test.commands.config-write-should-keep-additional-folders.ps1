# see https://github.com/tmssoftware/tms-smartsetup/issues/269

. test.setup

Copy-Item -Path tms.config.old.yaml -Destination tms.config.yaml -Force
tms server-enable community

$productFolders = tms config-read "tms smart setup options:additional products folders:"

if ($productFolders -ne "[..\landgraf\aws-sdk-delphi,..\landgraf\tms-biz,..\tms\delphi-graphql]") {
    Write-Error "Test failed: Expected additional products folders to be '[..\landgraf\aws-sdk-delphi,..\landgraf\tms-biz,..\tms\delphi-graphql]', but got '$productFolders'."
}