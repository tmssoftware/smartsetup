# basic sbom generation test
. test.setup
tms fetch tms.biz.*

$sbomFiles = tms sbom-generate tms.biz.echo

# the sbom files are in lines that start with "[timestamp] SBOM created: " followed by the file path quoted, so we need to extract the file paths from the output
$sbomFiles = @($sbomFiles | Where-Object { $_ -like "*SBOM created: *" } | ForEach-Object { $_ -replace ".*SBOM created: ", "" })

if ($sbomFiles.Count -ne 1) {
    throw "Only one SBOM file should be generated, but found $($sbomFiles.Count)."
}
$sbomFile = $sbomFiles[0].Trim('"')
#check the values in the sbom file
if (-not (Test-Path -Path $sbomFile)) {
    throw "The SBOM file '$sbomFile' does not exist."
}
# check that the contents match those of the file "./tms.biz.echo.cdx.expected.json", but ignore 
# the fields "timestamp" and "serialNumber" in the SBOM file, as they are generated dynamically 
# and will not match the expected file.
$sbomContent = Get-Content -Path $sbomFile | ConvertFrom-Json
$expectedContent = Get-Content -Path "$PSScriptRoot/tms.biz.echo.cdx.expected.json" | ConvertFrom-Json
$sbomContent.metadata.timestamp = $null
$sbomContent.serialNumber = $null
$expectedContent.metadata.timestamp = $null
$expectedContent.serialNumber = $null
$sbomJson = $sbomContent | ConvertTo-Json -Depth 100
$expectedJson = $expectedContent | ConvertTo-Json -Depth 100
if ($sbomJson -ne $expectedJson) {
    throw "The contents of the SBOM file '$sbomFile' do not match the expected contents."
}


$sbomFiles = tms sbom-generate tms.biz.echo
$sbomFiles = @($sbomFiles | Where-Object { $_ -like "*SBOM created: *" } | ForEach-Object { $_ -replace ".*SBOM created: ", "" })
if ($sbomFiles.Count -ne 0) {
    throw "No SBOM file should be generated, but found $($sbomFiles.Count)."
}

$sbomFiles = tms sbom-generate tms.biz.echo -force
$sbomFiles = @($sbomFiles | Where-Object { $_ -like "*SBOM created: *" } | ForEach-Object { $_ -replace ".*SBOM created: ", "" })
if ($sbomFiles.Count -ne 1) {
    throw "Only one SBOM file should be generated, but found $($sbomFiles.Count)."
}

$sbomFiles = tms sbom-generate *
$sbomFiles = @($sbomFiles | Where-Object { $_ -like "*SBOM created: *" } | ForEach-Object { $_ -replace ".*SBOM created: ", "" })
$products = tms list
if ($sbomFiles.Count -ne $products.Count - 1) {
    throw "One SBOM file should be generated for each product except echo, but found $($sbomFiles.Count) for $($products.Count) products."
}
