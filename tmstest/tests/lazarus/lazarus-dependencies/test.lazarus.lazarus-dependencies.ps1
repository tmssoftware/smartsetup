#Installs packages with depdencies in lazarus
#Note that to do this correctly, lazarus needs to find the dependent packages, even if we are not registering the components.
. test.setup

tms config-write -p:"configuration for all products:delphi versions = [lazarus]"
tms config-write -p:"configuration for all products:compiler paths:lazarus = C:\fpc\lazarus"
tms build tmslaztest.c