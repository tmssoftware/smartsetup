# Checks list-remote -source returns only from that source.
# See https://github.com/tmssoftware/tms-smartsetup/issues/265
. test.setup

Remove-Item ".\tms.config.yaml" -Force
try
 {
    tms list-remote -server:community
    Write-Output "The command 'tms list-remote shouldn't run in an empty folder."
    exit -1
 }
 catch {
 }

 #JSON
tmscredentials
tms server-enable community
$JsonCommunityProducts = tms list-remote -server:community -json | ConvertFrom-Json -AsHashtable

if ($JsonCommunityProducts.Count -eq 0) {
    Write-Error "No products found in the 'community' server."
}

foreach ($Product in $JsonCommunityProducts.Values) {
    if ($Product.server -ne "community") {
        Write-Error "The product '$($Product.name)' should be from the 'community' server, but it is from '$($Product.server)'."
    }
}   

$JsonTmsProducts = tms list-remote -server:tms -json | ConvertFrom-Json -AsHashtable
if ($JsonTmsProducts.Count -eq 0) {
    Write-Error "No products found in the 'tms' server. This is expected if you are not logged in."
}
foreach ($Product in $JsonTmsProducts.Values) {
    if ($Product.server -ne "tms") {
        Write-Error "The product '$($Product.name)' should be from the 'tms' server, but it is from '$($Product.server)'."
    }
}

#TEXT
$TextCommunityProducts = tms list-remote -server:community 

if ($TextCommunityProducts.Count -eq 0) {
    Write-Error "No products found in the 'community' server, text listing."
}

foreach ($Product in $TextCommunityProducts) {
    if (! $Product.EndsWith("-> community") ) {
        Write-Error "The product '$Product' should be from the 'community' server, but it is not."
    }
}

$TextTmsProducts = tms list-remote -server:tms

if ($TextTmsProducts.Count -eq 0) {
    Write-Error "No products found in the 'tms' server, text listing."
}

foreach ($Product in $TextTmsProducts) {
    if (! $Product.EndsWith("-> tms") ) {
        Write-Error "The product '$Product' should be from the 'tms' server, but it is not."
    }
}

if ($JsonCommunityProducts.Count -ne $TextCommunityProducts.Count) {
    Write-Error "The number of products in the JSON listing is different from the text listing for the 'community' server."
}
if ($JsonTmsProducts.Count -ne $TextTmsProducts.Count) {
    Write-Error "The number of products in the JSON listing is different from the text listing for the 'tms' server."
}

$JsonAllProducts = tms list-remote -json | ConvertFrom-Json -AsHashtable
if ($JsonAllProducts.Count -ne ($JsonCommunityProducts.Count + $JsonTmsProducts.Count)) {
    Write-Error "The number of products in the JSON listing is different from the sum of the 'community' and 'tms' servers."
}

$TextAllProducts = tms list-remote
if ($TextAllProducts.Count -ne ($TextCommunityProducts.Count + $TextTmsProducts.Count)) {
    Write-Error "The number of products in the text listing is different from the sum of the 'community' and 'tms' servers."
}

