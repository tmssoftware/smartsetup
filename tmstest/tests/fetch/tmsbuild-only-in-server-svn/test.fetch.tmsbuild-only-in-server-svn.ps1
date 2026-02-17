# Similar to tmsbuild-only-in-server-git, but using SVN instead of Git

tms server-enable tms false
tms server-enable community true

tms fetch zeos.zeoslib

# Store the contents of tmsbuild.yaml for later comparison
$FetchedTmsBuildPath = "./Products/zeos.zeoslib/src/tmsbuild.yaml"
$FetchedTmsBuildContent = Get-Content $FetchedTmsBuildPath -Raw

# modify tmsbuild.yaml locally to add a comment at the top
$ModifiedTmsBuildContent = "# This is a local modification`n" + $FetchedTmsBuildContent
Set-Content -Path $FetchedTmsBuildPath -Value $ModifiedTmsBuildContent

tms fetch zeos.zeoslib
$AfterFetchTmsBuildContent = Get-Content $FetchedTmsBuildPath -Raw
if ($AfterFetchTmsBuildContent -ne $FetchedTmsBuildContent) {
    throw "tmsbuild.yaml should have been modified by fetch, because it is not in version control."
}

Set-Content -Path $FetchedTmsBuildPath -Value $ModifiedTmsBuildContent
svn add $FetchedTmsBuildPath # add to version control, so now fetch should not update it.
tms fetch zeos.zeoslib
$AfterFetchTmsBuildContent = Get-Content $FetchedTmsBuildPath -Raw
if ($AfterFetchTmsBuildContent.Trim() -ne $ModifiedTmsBuildContent.Trim()) {
    throw "tmsbuild.yaml should not have been modified by fetch, because it is in version control."
}
if ($AfterFetchTmsBuildContent.Trim() -eq $FetchedTmsBuildContent.Trim()) {
    throw "Internal error: Files should be different."
}
