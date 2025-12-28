# if there is an invalid product id, we should always crash

. test.setup

function Test-UnknownProductError {
    param(
        [string] $Case
    )
    $ProductId = "potato.salad"
    Test-CommandFails { tms install $ProductId } "*Error: Could not find any products matching $ProductId*" "Case $Case"
}

tms server-enable tms false
#the real one, which should ask for credentials.
Test-CommandFails { tms credentials } "*Server tms isn't defined or is not enabled*" "unknown product"
#the one we use in tests, which wasn't failing even if the real one was.
Test-CommandFails { tmscredentials } "*Server tms isn't defined or is not enabled*" "unknown product"

Test-CommandFails { tms install potato.salad } "*There are no servers enabled. To fetch a product, enable a server first*" "no servers enabled"

tms server-enable community
Test-UnknownProductError "community, no credentials"

tms server-enable tms
tmscredentials
Test-UnknownProductError "community, with credentials"

tms server-enable community false
Test-UnknownProductError

