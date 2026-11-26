# check backups from installed sources
#  * See webcore which is binary and includes dcus
#  * See abbrevia which has a strange folder structure
#  * See delphi < d+ folders
#  * See pinned products
#  * See exes
#  * See versions to keep = 0
#  * dependencies are included too
#  * see a package with root package folder

. test.setup
tms server-enable community
New-Item -ItemType Directory -Path "Products" -Force | Out-Null
Move-Item -Path "pkg - inside" -Destination "Products\pkg - inside" -Force

tms config-write -p:"tms smart setup options:versions to keep=0"
#tms config-write -p:"configuration for all products:add delphi versions = [delphirio]"
tms config-write -p:"configuration for amelander.map2pdb:delphi versions = [$Global:CurrentDelphiVersionId]"

tms install tms.webcore tms.biz.aurelius c4d.conn turbopack.abbrevia tms.flexcel.vcl jedi.jcl xor-el.crypto-lib-4-pascal bero.boldfordelphi amelander.map2pdb
tms pin *

New-Item -ItemType Directory -Path "Products\tms.webcore\Packages\d13\test" -Force | Out-Null
tms backup ./all.snapshot.zip -as-installed

uninstall_and_check(3)
Remove-Item -Path "Products" -Recurse -Force


tms restore ./all.snapshot.zip -test-offline -skip-register

# Only webcore ships binary and should include dcus. No other product zip should have them.
$outerZip = [System.IO.Compression.ZipFile]::OpenRead((Join-Path -Path $PWD -ChildPath "all.snapshot.zip"))
try {
    # Check the snapshot has exactly the expected files. Versions and git hashes change between runs, so compare only the product ids.
    $expectedEntries = @(
        "tms.snapshot.yaml",
        "c4d.conn",
        "jedi.jcl",
        "tms.biz.aurelius",
        "tms.biz.bcl",
        "tms.flexcel.vcl",
        "tms.webcore",
        "turbopack.abbrevia"
        "my_awesome_pkg_inside"
        "bero.boldfordelphi" 
        "xor-el.crypto-lib-4-pascal"
        "xor-el.hash-lib-4-pascal" 
        "xor-el.simple-base-lib-4-pascal"
        "amelander.map2pdb"
    ) | Sort-Object

    $actualEntries = @($outerZip.Entries | ForEach-Object {
        if ($_.FullName -match '^(.+)_production_.*\.zip$') { $Matches[1] } else { $_.FullName }
    }) | Sort-Object

    $diff = Compare-Object -ReferenceObject $expectedEntries -DifferenceObject $actualEntries
    if ($diff) {
        $missing = ($diff | Where-Object SideIndicator -eq "<=" | ForEach-Object InputObject) -join ", "
        $extra = ($diff | Where-Object SideIndicator -eq "=>" | ForEach-Object InputObject) -join ", "
        throw "all.snapshot.zip doesn't have the expected files. Missing: [$missing]. Unexpected: [$extra]."
    }

    foreach ($entry in $outerZip.Entries) {
        if (-not $entry.Name.EndsWith(".zip", [System.StringComparison]::OrdinalIgnoreCase)) { continue }
        if ($entry.Name -like "tms.webcore_*.zip") { continue }

        $stream = $entry.Open()
        try {
            $innerZip = New-Object System.IO.Compression.ZipArchive($stream, [System.IO.Compression.ZipArchiveMode]::Read)
            try {
                $dcus = @($innerZip.Entries | Where-Object { $_.Name.EndsWith(".dcu", [System.StringComparison]::OrdinalIgnoreCase) })
                if ($dcus.Count -gt 0) {
                    throw "$($entry.FullName) should not contain .dcu files, but it contains $($dcus.Count). First one: $($dcus[0].FullName)"
                }
            }
            finally {
                $innerZip.Dispose()
            }
        }
        finally {
            $stream.Dispose()
        }
    }
}
finally {
    $outerZip.Dispose()
}