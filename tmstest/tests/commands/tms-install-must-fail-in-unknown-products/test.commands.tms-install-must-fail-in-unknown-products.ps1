# if there is an invalid product id, we should always crash

. test.setup

function Test-UnknownProductError {
    param(
        [string] $Case
    )
    $ProductId = "potato.salad"
    $installResult = Invoke-WithExitCodeIgnored{ tms install $ProductId }
    $testOk = Test-Result -CommandResult $installResult -Message "*Error: Could not find any products matching $ProductId*"  
    if (-not ($testOk)) {   
        throw "Case $($Case): The error message should mention 'Error: Could not find any products matching $ProductId'.'. Actual message: $($installResult)"
    }
}

Test-UnknownProductError "none"


tms server-enable community
Test-UnknownProductError "community, no credentials"

tmscredentials
Test-UnknownProductError "community, with credentials"

tms server-enable community false
Test-UnknownProductError

tms server-enable tms false
Test-UnknownProductError "tms disabled"
