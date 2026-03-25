# checks that we don't build C++ code on winarm, which is not supported by the toolchain

. test.setup

tms config-write -p:configuration-for-all-products:replace-delphi-versions=[delphi12,delphi13] -p:configuration-for-all-products:platforms=[] -p:configuration-for-all-products:options:skip-register=true

tms build