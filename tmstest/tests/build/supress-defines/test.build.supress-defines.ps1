# Checks that we can suppress existing defines, both in the package definition and in the configuration.

. test.setup

tms config-write -p:"configuration-for-all-products:compilation options:add-defines=[TOMATO=false]"
tms config-write -p:"configuration-for-all-products:compilation options:add-defines=[NewONE=true]"
tms config-write -p:"configuration-for-all-products:advanced options:modify sources=true"
tms build

# changing the define should trigger a rebuild
tms config-write -p:"configuration-for-all-products:compilation options:replace-defines=[TOMATO=true,NewONE=true]"
Invoke-WithExitCodeIgnored { tms build}

CheckLogHasString "Declaration expected but identifier 'tomato' found"

tms config-write -p:"configuration-for-silly__defines:compilation options:replace-defines=[TOMATO=false,NewONE=true,NewTWO=true, NewTHREE=false]"

tms config-write -p:"configuration-for-silly__defines2:compilation options:replace-defines=[TOMATO=false,NewONE=true]"


tms build
CheckLogHasString 'Note: The define "NewONE" is not in the list of allowed defines for silly_defines'
CheckLogHasString 'Note: The define "NewTWO" is not in the list of allowed defines for silly_defines'
CheckLogHasString 'Note: The define "NewTHREE" is not in the list of allowed defines for silly_defines'

Compare-Files "pkg\mydefines.inc" "pkg\expected\mydefines.inc"

tms config-write -p:"configuration-for-silly__defines2:compilation options:replace-defines=[TOMATO=false,POTATO=true]"
Invoke-WithExitCodeIgnored { tms build}
CheckLogHasString "Declaration expected but identifier 'potato' found"



