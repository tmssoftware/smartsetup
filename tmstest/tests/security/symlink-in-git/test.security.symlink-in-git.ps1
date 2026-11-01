# Test that symlinks inside a git repo are never created in the product folder, so we can't read, write or delete outside it.
# There are two protections:
#   1. tms calls git with "-c core.symlinks=false", so git checks out links as plain files, whatever the system, global or repo config say.
#   2. After fetching, tms removes any link in the product folder. This covers a user who sets "-c core.symlinks=true" in the clone command.
# The repo has two links:
#   evil-link -> ..\evil: in the first commit. The clone command has "-c core.symlinks=true", so git creates it and tms must remove it.
#   evil-dir-link -> ..\..\evil: in the second commit, pulled by "tms update" with "core.symlinks=true" in the repo config.
#                                It resolves to the existing Products\evil, so it would be a directory link if git created it.
# We run tms 4 folders deep inside the test folder, so if something follows the links, it can't escape the test.

. test.setup
Import-Module -Name $tmsTestRootDir/util/util.repo.psm1 -Force
Import-Module -Name $tmsTestRootDir/util/util.symlinks.psm1 -Force

# Adds a symlink directly to the index, so we don't need rights to create symlinks here.
function Add-GitSymlink {
    param([string]$LinkName, [string]$Target)
    $linkTargetFile = Join-Path -Path $testRoot -ChildPath "link-target.tmp"
    [System.IO.File]::WriteAllText($linkTargetFile, $Target)
    $blob = git hash-object -w -- $linkTargetFile
    Remove-Item -Path $linkTargetFile -Force
    git update-index --add --cacheinfo "120000,$blob,$LinkName"
    git -c user.name=tmstest -c user.email=tmstest@example.com commit --quiet -m "Add $LinkName -> $Target"
}

# Checks the link was checked out as a plain file with the target inside.
function Assert-LinkIsPlainFile {
    param([string]$LinkName, [string]$Target)
    $file = Join-Path -Path $productFolder -ChildPath "src/$LinkName"
    $item = Get-Item -LiteralPath $file -Force -ErrorAction SilentlyContinue
    if (-not $item) {
        throw "$LinkName was not found in the cloned repo at '$file'."
    }
    if ($item.LinkType) {
        throw "$LinkName should be a plain file, but it is a $($item.LinkType) to '$($item.Target)'."
    }
    $content = [System.IO.File]::ReadAllText($file)
    if ($content -ne $Target) {
        throw "$LinkName should contain '$Target', but it contains '$content'."
    }
}

$testRoot = (Get-Location).Path
$repo = Join-Path -Path $testRoot -ChildPath ".repo"
$serverZip = Join-Path -Path $testRoot -ChildPath "server.zip"

Update-TmsbuildYamlWithLocalUrl -TmsbuildFilePath "$repo/tmsbuild.yaml"
Push-Location -Path $repo
try {
    git init --quiet
    git add tmsbuild.yaml packages
    Add-GitSymlink -LinkName "evil-link" -Target '..\evil'
} finally {
    Pop-Location
}

$zip = [System.IO.Compression.ZipFile]::Open($serverZip, [System.IO.Compression.ZipArchiveMode]::Create)
try {
    [System.IO.Compression.ZipFileExtensions]::CreateEntryFromFile($zip, "$repo/tmsbuild.yaml", "tmsbuild.yaml") | Out-Null
} finally {
    $zip.Dispose()
}

$workFolder = Initialize-DeepWorkFolder -Root $testRoot
Set-Location -Path $workFolder
$productFolder = Join-Path -Path $workFolder -ChildPath "Products/tmstest.evil"

tms server-enable tms false
tms server-add tmstest.evil zipfile "file:///$($serverZip -replace '\\', '/')"
# The user can override our core.symlinks=false here. In that case git creates the link and tms must remove it after fetching.
tms config-write -p:"tms smart setup options:git:clone command = -c core.symlinks=true clone"

$result = tms install tmstest.evil
Write-Output $result
Assert-CanariesIntact -Root $testRoot -Stage "Install"
Assert-NoReparsePoints -Path $productFolder
if (-not (Test-Result -CommandResult $result -Message "*Removed the link*evil-link*")) {
    throw "Install should have removed evil-link, created by git because the clone command has core.symlinks=true."
}

# Enable symlinks in the repo config, as if the machine allowed them. Our "-c core.symlinks=false" must win.
git -C (Join-Path -Path $productFolder -ChildPath "src") config core.symlinks true
Push-Location -Path $repo
try {
    Add-GitSymlink -LinkName "evil-dir-link" -Target '..\..\evil'
} finally {
    Pop-Location
}

$result = tms update
Write-Output $result
Assert-CanariesIntact -Root $testRoot -Stage "Update"
Assert-NoReparsePoints -Path $productFolder
if (Test-Result -CommandResult $result -Message "*Removed the link*") {
    throw "git should have checked out the links as plain files, so there shouldn't be any link to remove after the update."
}
# The update cleans and resets the repo, so evil-link, removed after install, comes back too.
Assert-LinkIsPlainFile -LinkName "evil-link" -Target '..\evil'
Assert-LinkIsPlainFile -LinkName "evil-dir-link" -Target '..\..\evil'

tms uninstall tmstest.evil
Assert-CanariesIntact -Root $testRoot -Stage "Uninstall"
Assert-FolderRemoved -Path $productFolder -Stage "Uninstall"
