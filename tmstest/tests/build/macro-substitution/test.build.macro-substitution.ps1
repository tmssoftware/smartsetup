# MSBuild doesn't replace macros like $(PRODUCTVERSION) when they are passed as parameters in the command line (using"/p")
# It does replace those macros when they are in the dproj, but we move those to "/p" parameters in order to keep the search path small.
#This test will check that we can work anyway even when there are macros in extra library paths, or in the dproj.

. test.setup

tms config-write -p:"configuration for all products:compilation options:debug dcus=true" #check $(CONFIG) macro
tms config-write -p:configuration-for-all-products:replace_platforms=[win32intel,win64intel,win64xintel] #check $(PLATFORM) macro
tms config-write -p:configuration-for-all-products:replace-delphi-versions=[delphi12,delphi13] #check $(PRODUCTVERSION) macro

tms build