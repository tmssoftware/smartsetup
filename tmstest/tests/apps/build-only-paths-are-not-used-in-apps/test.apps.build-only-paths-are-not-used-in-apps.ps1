# This is actually tested in apps.all-installed-components, 
# because DelphiAST has a build-only path, but that could be changed
# in the future, so we will check it with a local component.

. test.setup

tms build
#test that MyApp\Win32\Release\MyApp.exe exists
$exePath = "MyApp\Win32\Release\MyApp.exe"
if (-Not (Test-Path $exePath)) {
    throw "The expected executable '$exePath' was not found after the build."
}