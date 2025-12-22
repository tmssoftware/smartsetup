# we use this script to generate some test server and avoid creating lots of public repos in github (one per product and dependency)

# copy all folders from $PSScriptRoot\products to a temp folder, and create git repos in each folder
$testReposSource = Join-Path -Path $PSScriptRoot -ChildPath "test-repos"
$testReposTarget = Join-Path -Path $tmsTestRootDir\tmp-run\ -ChildPath "test-repos" -AdditionalChildPath "products"
if (Test-Path -Path $testReposTarget) {
    # Remove-Item -Path $testReposTarget -Recurse -Force
    # If it exists, we won't create it again. This way, we can speed up tests that call this script multiple times.
    Write-Output "Test repositories already exist at: $testReposTarget -Skipping"
    return
}

Copy-Item -Path $testReposSource -Destination $testReposTarget -Recurse

# edit the tmsbuild.yaml files to set the repository url to a local file url
$tmsbuildFiles = Get-ChildItem -Path $testReposTarget -Filter 'tmsbuild.yaml' -Recurse -File
foreach ($tmsbuildFile in $tmsbuildFiles) {
    $productFolder = $tmsbuildFile.Directory
    $inApplicationSection = $false
    $foundNameLine = $false
    $content = Get-Content -Path $tmsbuildFile
    $newContent = @()
    foreach ($line in $content) {
        $newContent += $line
        if ($foundNameLine) {
            continue
        }
        if ($line -match '^\s*application:\s*$') {
            $inApplicationSection = $true
            continue
        }
        if (-not $inApplicationSection) {
            continue
        }
        if ($line -match '^\w*:\$') {
            # we reached a new section
            $inApplicationSection = $false
            continue
        }
        if ($line -match '  name:\s*".*"') {
            $fileUrl = "file:///" + ($productFolder.FullName -replace '\\', '/')
            $newContent += "  url: `"$fileUrl`""
            $foundNameLine = $true
            continue
        }
    }
    if (-not $foundNameLine) {
        throw "Could not find application name line in $($tmsbuildFile.FullName)"
    }
    Set-Content -Path $tmsbuildFile -Value $newContent
}

# create a zip file with all the tmsbuild.yaml files inside their corresponding product folder
$zipFilePath = Join-Path -Path $tmsTestRootDir\tmp-run -ChildPath "test-repos" -AdditionalChildPath "tmsbuild_test_repos.zip"
if (Test-Path -Path $zipFilePath) {
    Remove-Item -Path $zipFilePath -Force
}
Add-Type -AssemblyName System.IO.Compression.FileSystem
$zip = [System.IO.Compression.ZipFile]::Open($zipFilePath, [System.IO.Compression.ZipArchiveMode]::Create)
try {
    $yamlFiles = Get-ChildItem -Path $testReposTarget -Filter 'tmsbuild.yaml' -Recurse -File
    foreach ($file in $yamlFiles) {
        # keep folder structure relative to $testReposTarget inside the zip
        $relative = $file.FullName.Substring($testReposTarget.Length).TrimStart('\','/')
        $entryName = $relative.Replace('\','/')
        [System.IO.Compression.ZipFileExtensions]::CreateEntryFromFile($zip, $file.FullName, $entryName) | Out-Null
    }
} finally {
    $zip.Dispose()
}

#remove tmsbuild.yaml from folders "E-F" and "F" to test tmsbuild.yaml hosted only in the server.
Remove-Item -Path (Join-Path -Path $testReposTarget -ChildPath "E-F\tmsbuild.yaml") -Force
Remove-Item -Path (Join-Path -Path $testReposTarget -ChildPath "F\tmsbuild.yaml") -Force

# loop over all products in the folder and create git repos for each one
$productFolders = Get-ChildItem -Path $testReposTarget -Directory
foreach ($productFolder in $productFolders) {
    Write-Output "Creating git repo for product: $($productFolder.Name)"
    Set-Location -Path $productFolder.FullName

    git init | Out-Null
    git config user.name "Test User"
    git config user.email "testuser@example.com"
    git add .
    git commit -m "Initial commit"
    #create a v1.0.0 tag
    git tag v1.0.0
}

# create a new version for the products by changing the .pas files to be all in uppercase
foreach ($productFolder in $productFolders) {
    Write-Output "Creating new version for product: $($productFolder.Name)"
    Set-Location -Path $productFolder.FullName

    $pasFiles = Get-ChildItem -Path $productFolder.FullName -Filter *.pas -Recurse
    foreach ($pasFile in $pasFiles) {
        $content = Get-Content -Path $pasFile.FullName
        $contentUpper = $content.ToUpper()
        Set-Content -Path $pasFile.FullName -Value $contentUpper
    }

    git add .
    git commit -m "Updated .pas files to uppercase for new version"
    #create a v1.1.0 tag
    git tag "first.release"
    git tag v1.1.0
    git tag "a_lot_of_tags.on.this.commit"
}

Write-Output "Test repositories created at: $testReposTarget"
Write-Output "Zip file created at: $zipFilePath"
