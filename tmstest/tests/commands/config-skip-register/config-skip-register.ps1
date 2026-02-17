# Check that config: skip-register is preserved correctly.

. test.setup
tmscredentials

tms config-write -p:"configuration for all products:options:skip register=true"
tms config-read "configuration for all products:options:skip register" | Assert-ValueIs "true"

tms config-write -p:"configuration for all products:options:skip register=[all, -startmenu]"
tms config-read "configuration for all products:options:skip register" | Assert-ValueIs "[all,-startmenu]"

Test-CommandFails {tms config-write -p:"configuration for all products:options:skip register=[all, -startmenus]"} "*[all, packages, startmenu, help, windowspath, webcore, registry, filelinks]*"
tms config-read "configuration for all products:options:skip register" | Assert-ValueIs "[all,-startmenu]"