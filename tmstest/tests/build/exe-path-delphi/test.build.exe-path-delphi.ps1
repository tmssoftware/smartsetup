#Checks that the exe output folder is respected for exes.

. test.setup

tms build
foreach ($platform in $('Win32'))
{
    foreach ($ProductVersion in $('23.0', '37.0'))
    {
        $cmd = $(".\AppStar\myexe\$ProductVersion\$Platform\Release\AppStar.exe")
        $result = & $cmd
        if ($result -ne 49) {
            throw "The exe output folder is not respected for exes. Expected 42, got $result."
        }
    }
}

