# Check we escape xml correctly when creating the packages.

. test.setup

tms build

CheckLogHasString '- TMS Example for VCL & FMX -> OK.'

$result = .\app\Win32\Release\xmlencodetest.exe
if ($result -ne "Work!, Rest! / Work2!, Rest2!") {
    throw "Error The app returned $result instead of the expected value."
}
