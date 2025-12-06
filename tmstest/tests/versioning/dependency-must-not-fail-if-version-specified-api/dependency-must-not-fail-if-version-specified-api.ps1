# If I install tms install tms.biz.aurelius:5.22 tms.biz.bcl:1.44,
# when it installs aurelius it will also add tms.biz.bcl:latest as it is a dependency
# Even when you have both 1.44 and latest as products to install, it should not fail
# saying that two different versions are requested. It should install 1.44 only.

. test.setup
tmscredentials

tms install tms.biz.aurelius:5.22 tms.biz.bcl:1.44

tms list -json | ConvertFrom-Json | ForEach-Object {
    if ($_.id -eq "tms.biz.bcl") {
        $_.version | Assert-ValueIs "1.44"
    }
}