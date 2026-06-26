#Check that Skip and Rename events behave similarly for zstd and zip files

. test.setup

$resultZip = tms uncompress files_production.zip zip  -test-log-uncompress
$resultZst = tms uncompress files_production.tar.zst zst  -test-log-uncompress

# Keep only the SKIP/RENAMER lines and drop the timestamp prefix
# ("[00:13:34] " for zip, "[00:13:23 - 100%] " for zstd) so both can be compared.
function Get-SkipRenameLines {
    param([string[]]$Output)
    @($Output |
        Where-Object { $_ -match 'SKIP:|RENAMER:' } |
        ForEach-Object { ($_ -replace '^\[[^\]]*\]\s*', '').Trim() })
}

$expected = @(
    'SKIP: Base.txt'
    'RENAMER: Base.txt'
    'SKIP: Nested/'
    'RENAMER: Nested/'
    'SKIP: Nested/MoreNested/'
    'RENAMER: Nested/MoreNested/'
    'SKIP: Nested/MoreNested/child.txt'
    'RENAMER: Nested/MoreNested/child.txt'
)

$zipLines = Get-SkipRenameLines $resultZip
$zstLines = Get-SkipRenameLines $resultZst

if (($zipLines -join "`n") -ne ($zstLines -join "`n")) {
    throw "SKIP/RENAMER lines differ between zip and zstd.`nZip:`n$($zipLines -join "`n")`nZst:`n$($zstLines -join "`n")"
}

if (($zipLines -join "`n") -ne ($expected -join "`n")) {
    throw "SKIP/RENAMER lines don't match the expected values.`nActual:`n$($zipLines -join "`n")`nExpected:`n$($expected -join "`n")"
}

# The renamer flattens every file into AllFiles\<filename>, so both archives
# must produce the same set of files under their target folder.
function Get-RelativeFiles {
    param([string]$Root)
    $rootFull = (Resolve-Path $Root).Path
    @(Get-ChildItem -Path $Root -Recurse -File |
        ForEach-Object { $_.FullName.Substring($rootFull.Length).TrimStart('\', '/').Replace('\', '/') } |
        Sort-Object)
}

$expectedFiles = @(
    'files/AllFiles/Base.txt'
    'files/AllFiles/child.txt'
)

$zipFiles = Get-RelativeFiles zip
$zstFiles = Get-RelativeFiles zst

if (($zipFiles -join "`n") -ne ($zstFiles -join "`n")) {
    throw "Extracted files differ between zip and zstd.`nZip:`n$($zipFiles -join "`n")`nZst:`n$($zstFiles -join "`n")"
}

if (($zipFiles -join "`n") -ne ($expectedFiles -join "`n")) {
    throw "Extracted files don't match the expected values.`nActual:`n$($zipFiles -join "`n")`nExpected:`n$($expectedFiles -join "`n")"
}
