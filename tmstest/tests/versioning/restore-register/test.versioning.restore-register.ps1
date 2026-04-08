# Tests that tms restore requires -auto-register or -skip-register, and that each option works correctly.

$regkey = "tmstest\restore-test"

. test.setup
tmscredentials
tms server-enable community

Set-AlternateRegistryKey -RegKey $regkey -RegisterAllDelphiVersions $true

tms fetch tms.biz.echo sglienke.spring4d

tms snapshot "tms.snapshot.yaml"

# Restore without -auto-register or -skip-register should fail
Test-CommandFails { tms restore "tms.snapshot.yaml" } "*specify -auto-register or -skip-register*"

function Test-RegistrationState {
    param(
        [bool]$ShouldBeRegistered,
        [string]$RestoreOption,
        [string]$HasSkip
    )
    tms restore "tms.snapshot.yaml" $RestoreOption
    $products = tms list -json -detailed | ConvertFrom-Json -AsHashtable
    foreach ($product in $products.values) {
        foreach ($ide in $product.ides.keys) {
            foreach ($plat in $product.ides[$ide].platforms.keys) {
                $isRegistered = $product.ides[$ide].platforms[$plat].registered
                if ($isRegistered -ne $ShouldBeRegistered) {
                    $state = if ($ShouldBeRegistered) { "registered" } else { "NOT registered" }
                    throw "Skip State: $($HasSkip): Product $($product.name) IDE $($ide) platform $($plat) should be $state after restore $RestoreOption, but it is not."
                }
            }
        }
    }
    tms uninstall *
}

tms config-write -p:configuration-for-all-products:options:skip-register=true

# Restore with -skip-register should succeed and NOT register components
Test-RegistrationState -ShouldBeRegistered $false -RestoreOption "-skip-register" -HasSkip  "skip-register=true"

# Restore with -auto-register should succeed and register components
Test-RegistrationState -ShouldBeRegistered $false -RestoreOption "-auto-register" -HasSkip  "skip-register=true"

tms config-write -p:configuration-for-test:options:skip-register=false

# Restore with -skip-register should succeed and NOT register components
Test-RegistrationState -ShouldBeRegistered $false -RestoreOption "-skip-register" -HasSkip  "skip-register=false"

# Restore with -auto-register should succeed and register components
Test-RegistrationState -ShouldBeRegistered $true -RestoreOption "-auto-register" -HasSkip  "skip-register=false"
