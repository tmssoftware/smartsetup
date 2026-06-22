# we use this script to generate some test server and avoid creating lots of public repos in github (one per product and dependency)
Import-Module -Name $tmsTestRootDir/util/util.repo.psm1 -Force

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
    Update-TmsbuildYamlWithLocalUrl -TmsbuildFilePath $tmsbuildFile.FullName
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

$betaVersions = @{
    "DoubleTrouble" = @("1.0.1-beta.1", "1.0.1-beta.2", "1.0.1", "1.1.0_beta+1", "v1.1.0_beta.2", "1.1.0")
    "TagTagAndBranch" = @("2.0.0-beta.1", "2.0.0-beta.2", "2.0.0", "2.1.0_beta+1", "v2.1.0_beta.2", "2.1.0")
}

$branches = @{
    "TagTagAndBranch" = @("", "2.1.0", "", "", "main", "")
}

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
    git tag "v1.0.0"
}

# create a new version for the products
foreach ($productFolder in $productFolders) {
    if ($betaVersions.ContainsKey($productFolder.Name)) {
        continue
    }
    Write-Output "Creating new version for product: $($productFolder.Name)"
    Set-Location -Path $productFolder.FullName

    $pasFiles = Get-ChildItem -Path $productFolder.FullName -Filter *.pas -Recurse
    foreach ($pasFile in $pasFiles) {
        $content = Get-Content -Path $pasFile.FullName
        $contentUpper = $content.ToUpper()
        #replace '1.0.0' with '1.1.0' in the content too, to simulate a version change in the code
        $contentUpper = $contentUpper -replace '1\.0\.0', '1.1.0'
        Set-Content -Path $pasFile.FullName -Value $contentUpper
    }

    git add .
    git commit -m "Updated .pas files to uppercase for new version"
    #create a v1.1.0 tag
    git tag "first.release"
    git tag v1.1.0
    git tag "a_lot_of_tags.on.this.commit"
}

foreach ($productFolder in $productFolders) {
    if (-not $betaVersions.ContainsKey($productFolder.Name)) {
        continue
    }
    Write-Output "Creating new version for product: $($productFolder.Name)"
    Set-Location -Path $productFolder.FullName

    $pasFiles = Get-ChildItem -Path $productFolder.FullName -Filter *.pas -Recurse
    foreach ($version in $betaVersions[$productFolder.Name]) {
        foreach ($pasFile in $pasFiles) {
            $content = Get-Content -Path $pasFile.FullName
            $contentUpper = $content -replace "Result := '.*';", "Result := '$version';"
            Set-Content -Path $pasFile.FullName -Value $contentUpper
        }
        #create version.txt
        #$versionTxtPath = Join-Path -Path $productFolder.FullName -ChildPath "version.txt"
        #"version: $version" | Out-File -FilePath $versionTxtPath -Encoding utf8
        
        if ($branches.ContainsKey($productFolder.Name)) {
            $branchName = $branches[$productFolder.Name][[Array]::IndexOf($betaVersions[$productFolder.Name], $version)]
            if ($branchName -ne "") {
                git branch $branchName | Out-Null
            }
        }

        git add .
        git commit -m "Updated .pas files to uppercase for new version $version"
        git tag "$version"

    }

}
Write-Output "Test repositories created at: $testReposTarget"
Write-Output "Zip file created at: $zipFilePath"
