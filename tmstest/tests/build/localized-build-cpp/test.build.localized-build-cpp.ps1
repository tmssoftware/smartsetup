# Test the localized build for C++
# See https://support.tmssoftware.com/t/c-builder-access-violation-with-static-linking/25711
# For this test to actually test something, you need to have the French "Language Packs" installed in Delphi->Tools->Manage Features
# (note: It takes a while to install those packs...)

. test.setup

#build a component using localized bindings.
tms config -print
tms build

Set-Location AV
#copy the output to the root so we can compile without setting search paths.
Copy-Item ..\Tests\test.localized.build.cpp\Packages\d11+\$BaseDelphiVersion\Win32\Release\UBindings.hpp .
Copy-Item ..\Tests\test.localized.build.cpp\Packages\d11+\$BaseDelphiVersion\Win32\Release\LocalizedBindings.lib .

#compile the app
msbuild "set LANGDIR=DE"

#run it and check it writes "Hello" to the console.
$Result = & Win32\Debug\localized.exe
if ($Result -ne "Hello") {
    Write-Error "Test failed: Result was '$Result' and should have been 'hello'."
}
exit 0



