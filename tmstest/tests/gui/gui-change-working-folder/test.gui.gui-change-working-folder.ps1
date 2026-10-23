# check that we show a welcome screen in an empty folder

. test.setup

function Test-FolderResults {
    param(
        [Parameter(Position=0)] [string] $folder,
        [Parameter(Position=1)] [string] $product
    )
    Push-Location $folder
    $list = tms list -json -detailed | ConvertFrom-Json -AsHashtable
    if ($list.Count -ne 1) {
        throw "There should be 1 products in the list, but there are $($list.Count)."
    }
    if (-not $list.Contains($product)) {
        throw "The product '$product' should be in the list, but it is not."
    }
    tms uninstall *
    Pop-Location
    $list = tms list -json -detailed | ConvertFrom-Json -AsHashtable
    if ($list.Count -ne 0) {
        throw "There should be 0 products in the list, but there are $($list.Count)."
    }
}


Test-tmsgui not-existing-folder
# no welcome screen as we have tms.config.yaml
mkdir initialized-folder
Copy-Item -Path .\tms.config.yaml -Destination .\initialized-folder\tms.config.yaml
Test-tmsgui no-welcome-screen

Test-FolderResults "initialized-folder" "tms.vcl.uipack"

mkdir not-initialized-folder
Test-tmsgui welcome-screen-no-changes
Test-FolderResults "not-initialized-folder" "tms.graphql"

mkdir wrong-folder-cancel
New-Item -ItemType File -Path 'wrong-folder-cancel\do-not-touch.txt' -Value "this folder is not a smartsetup folder! 🤬 #&%^&*("
Test-tmsgui wrong-folder-cancel


mkdir wrong-folder-but-keep-going
New-Item -ItemType File -Path 'wrong-folder-but-keep-going\do-not-touch.txt' -Value "this folder is not a smartsetup folder! 🤬 #&%^&*("
Test-tmsgui wrong-folder-but-keep-going

mkdir wrong-folder-but-really-keep-going
New-Item -ItemType File -Path 'wrong-folder-but-really-keep-going\do-not-touch.txt' -Value "this folder is not a smartsetup folder! 🤬 #&%^&*("
Test-tmsgui wrong-folder-but-really-keep-going
Test-FolderResults "wrong-folder-but-really-keep-going" "turbopack.doscommand"
