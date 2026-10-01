# MSBuild doesn't replace macros like $(PRODUCTVERSION) when they are passed as parameters in the command line (using"/p")
# It does replace those macros when they are in the dproj, but we move those to "/p" parameters in order to keep the search path small.
#This test will check that we can work anyway even when there are macros in extra library paths, or in the dproj.

. test.setup

tms config-write -p:"configuration for all products:compilation options:debug dcus=true" #check $(CONFIG) macro
tms config-write -p:configuration-for-all-products:replace_platforms=[win32intel,win64intel,win64xintel] #check $(PLATFORM) macro
tms config-write -p:configuration-for-all-products:replace-delphi-versions=[delphi12,delphi13] #check $(PRODUCTVERSION) macro

tms build
#tms build -unregister

#loop in the array of platforms.
foreach ($platform in $('Win32', 'Win64', 'Win64x'))
{
    foreach ($ProductVersion in $('23.0', '37.0'))
    {
        $cmd = $("./MacroAppCpp/$ProductVersion/$platform/Debug/MacroAppCpp.exe")
        $result = & $cmd
        if ($result -ne 42) {
            throw "The exe output folder is not respected for exes. Expected 42, got $result."
        }
    }
}

