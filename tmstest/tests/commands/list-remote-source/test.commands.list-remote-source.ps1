# Checks list-remote -source returns only from that source.
# See https://github.com/tmssoftware/tms-smartsetup/issues/265
. test.setup

try
 {
    tms list-remote -server:community
    Write-Host "The command 'tms list-remote shouldn't run in an empty folder."
    exit -1
 }
 catch {
 }

 #JSON
tms credentials -code:$env:TMSTEST_CODE -email:$env:TMSTEST_EMAIL
tms server-enable community
$JsonCommunityProducts = tms list-remote -server:community -json | ConvertFrom-Json -AsHashtable

if ($JsonCommunityProducts.Count -eq 0) {
    Write-Host "No products found in the 'community' server."
    exit -1
}

foreach ($Product in $JsonCommunityProducts.Values) {
    if ($Product.server -ne "community") {
        Write-Host "The product '$($Product.name)' should be from the 'community' server, but it is from '$($Product.server)'."
        exit -1
    }
}   

$JsonTmsProducts = tms list-remote -server:tms -json | ConvertFrom-Json -AsHashtable
if ($JsonTmsProducts.Count -eq 0) {
    Write-Host "No products found in the 'tms' server. This is expected if you are not logged in."
    exit -1
}
foreach ($Product in $JsonTmsProducts.Values) {
    if ($Product.server -ne "tms") {
        Write-Host "The product '$($Product.name)' should be from the 'tms' server, but it is from '$($Product.server)'."
        exit -1
    }
}   

#TEXT
$TextCommunityProducts = tms list-remote -server:community 

if ($TextCommunityProducts.Count -eq 0) {
    Write-Host "No products found in the 'community' server, text listing."
    exit -1
}

foreach ($Product in $TextCommunityProducts) {
    if (! $Product.EndsWith("-> community") ) {
        Write-Host "The product '$Product' should be from the 'community' server, but it is not."
        exit -1
    }
}

$TextTmsProducts = tms list-remote -server:tms

if ($TextTmsProducts.Count -eq 0) {
    Write-Host "No products found in the 'tms' server, text listing."
    exit -1
}

foreach ($Product in $TextTmsProducts) {
    if (! $Product.EndsWith("-> tms") ) {
        Write-Host "The product '$Product' should be from the 'tms' server, but it is not."
        exit -1
    }
}

if ($JsonCommunityProducts.Count -ne $TextCommunityProducts.Count) {
    Write-Host "The number of products in the JSON listing is different from the text listing for the 'community' server."
    exit -1
}
if ($JsonTmsProducts.Count -ne $TextTmsProducts.Count) {
    Write-Host "The number of products in the JSON listing is different from the text listing for the 'tms' server."
    exit -1
}

$JsonAllProducts = tms list-remote -json | ConvertFrom-Json -AsHashtable
if ($JsonAllProducts.Count -ne ($JsonCommunityProducts.Count + $JsonTmsProducts.Count)) {
    Write-Host "The number of products in the JSON listing is different from the sum of the 'community' and 'tms' servers."
    exit -1
}

$TextAllProducts = tms list-remote
if ($TextAllProducts.Count -ne ($TextCommunityProducts.Count + $TextTmsProducts.Count)) {
    Write-Host "The number of products in the text listing is different from the sum of the 'community' and 'tms' servers."
    exit -1
}

