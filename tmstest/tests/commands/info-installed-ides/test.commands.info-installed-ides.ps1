# Checks "installed ides" in tms info -json: the IDEs found in the registry, or with a compiler path in the config.

. test.setup

function Get-InstalledIdes {
    $info = tms info -json | ConvertFrom-Json
    if (-not ($info.PSObject.Properties.Name -contains "installed ides")) {
        throw "tms info -json doesn't have ""installed ides""."
    }
    return ($info."installed ides" -join ",")
}

# An alternate registry key should not affect installed ides.
tms config-write -p:"tms smart setup options:alternate registry key=tmstest-no-ides"
$ides = Get-InstalledIdes 
CheckTextHasString -Text $ides -Message "*$CurrentDelphiVersionId*"
CheckTextHasString -Text $ides -Message "*$PreviousDelphiVersionId*"

# A compiler path in the config counts if the compiler is there. For Delphi XE3 that is bin\rsvars.bat.
New-Item -ItemType File -Force -Path ".\fake-delphi\bin\rsvars.bat" | Out-Null
tms config-write -p:"configuration for all products:compiler paths:delphixe3=fake-delphi"
$ides = Get-InstalledIdes 
CheckTextHasString -Text $ides -Message "*$CurrentDelphiVersionId*"
CheckTextHasString -Text $ides -Message "*$PreviousDelphiVersionId*"
CheckTextHasString -Text $ides -Message "*delphixe3*"

# A path where the compiler isn't doesn't count.
tms config-write -p:"configuration for all products:compiler paths:delphixe4=missing-delphi"
Get-InstalledIdes | Assert-ValueNotContains "delphixe4"

# A path set only for one product doesn't count either.
tms config-write -p:"configuration for tms.test:compiler paths:delphixe5=fake-delphi"
Get-InstalledIdes | Assert-ValueNotContains "delphixe5"

# The text output lists them too.
tms info | Out-String | Assert-ValueContains "delphixe3"
