# if there is an invalid product id, we should always crash

. test.setup

function Test-UnknownProductError {
    param(
        [string] $Case
    )
    $ProductId = "potato.salad"
    Test-CommandFails { tms install $ProductId } "*Error: Could not find any products matching $ProductId*" "Case $Case"
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
