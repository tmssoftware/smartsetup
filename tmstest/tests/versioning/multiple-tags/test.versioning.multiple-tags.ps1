# When there are multiple tags in the same commit, we should try to pick the one that resembles a version number.
. test.setup
tms server-enable tms false
tms server-add testserver zipfile "file:///$($tmsTestRootDir.Replace('\', '/'))/tmp-run/test-repos/tmsbuild_test_repos.zip"

tms install tmstest.c:"first.release"

$list = tms list -json | ConvertFrom-Json -AsHashtable
if ($list["tmstest.c"].version -ne "first.release") {
    throw "tmstest.c should be version first release, but it is $($list["tmstest.c"].version)."
}

tms update tmstest.c:"a_lot_of_tags.on.this.commit"
$list = tms list -json | ConvertFrom-Json -AsHashtable
if ($list["tmstest.c"].version -ne "a_lot_of_tags.on.this.commit") {
    throw "tmstest.c should be version a_lot_of_tags.on.this.commit, but it is $($list["tmstest.c"].version)."
}

tms update tmstest.c
$list = tms list -json | ConvertFrom-Json -AsHashtable
if ($list["tmstest.c"].version -ne "v1.1.0")    {
    throw "tmstest.c should be version v1.1.0, but it is $($list["tmstest.c"].version)."
}