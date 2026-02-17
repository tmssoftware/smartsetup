# If I install tms install tms.biz.aurelius:5.22 tms.biz.bcl:1.44,
# when it installs aurelius it will also add tms.biz.bcl:latest as it is a dependency
# Even when you have both 1.44 and latest as products to install, it should not fail
# saying that two different versions are requested. It should install 1.44 only.

. test.setup
tmscredentials

tms install tms.biz.aurelius:5.22 tms.biz.bcl:1.44

$installed = tms list -json | ConvertFrom-Json -AsHashtable
if ($installed["tms.biz.bcl"].version -ne "1.44.0.0") {
    throw "tms.biz.bcl should be version 1.44.0.0, but it is $($installed["tms.biz.bcl"].version)." 
}


tms install tms.biz.aurelius:5.22 tms.biz.bcl:1.43
$installed = tms list -json | ConvertFrom-Json -AsHashtable
if ($installed["tms.biz.bcl"].version -ne "1.43.0.0") {
    throw "tms.biz.bcl should be version 1.43.0.0, but it is $($installed["tms.biz.bcl"].version)." 
}

$availableProducts = tms list-remote -json | ConvertFrom-Json -AsHashtable

tms update tms.biz.aurelius
$installed = tms list -json | ConvertFrom-Json -AsHashtable
if ($installed["tms.biz.bcl"].version -ne $availableProducts["tms.biz.bcl"].version) {
    throw "tms.biz.bcl should be version $($availableProducts["tms.biz.bcl"].version), but it is $($installed["tms.biz.bcl"].version)." 
}
if ($installed["tms.biz.aurelius"].version -ne $availableProducts["tms.biz.aurelius"].version) {
    throw "tms.biz.aurelius should be version $($availableProducts["tms.biz.aurelius"].version), but it is $($installed["tms.biz.aurelius"].version)." 
}

tms install tms.biz.aurelius:* tms.biz.bcl:*
$installed = tms list -json | ConvertFrom-Json -AsHashtable
if ($installed["tms.biz.bcl"].version -ne $availableProducts["tms.biz.bcl"].version) {
    throw "tms.biz.bcl should be version $($availableProducts["tms.biz.bcl"].version), but it is $($installed["tms.biz.bcl"].version)." 
}
if ($installed["tms.biz.aurelius"].version -ne $availableProducts["tms.biz.aurelius"].version) {
    throw "tms.biz.aurelius should be version $($availableProducts["tms.biz.aurelius"].version), but it is $($installed["tms.biz.aurelius"].version)." 
}

tms uninstall tms.biz.aurelius -cascade

tms install tms.biz.aurelius
if ($installed["tms.biz.bcl"].version -ne $availableProducts["tms.biz.bcl"].version) {
    throw "tms.biz.bcl should be version $($availableProducts["tms.biz.bcl"].version), but it is $($installed["tms.biz.bcl"].version)." 
}
if ($installed["tms.biz.aurelius"].version -ne $availableProducts["tms.biz.aurelius"].version) {
    throw "tms.biz.aurelius should be version $($availableProducts["tms.biz.aurelius"].version), but it is $($installed["tms.biz.aurelius"].version)." 
}


