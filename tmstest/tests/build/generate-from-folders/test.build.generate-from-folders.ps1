# Check the generate from gets the right files when they are in different folders.

. test.setup

tms build

$result = .\app\Win32\Release\generate_fromtest.exe
if ($result -ne "Work!, Rest! / Work2!, Rest2!") {
    throw "Error The app returned $result instead of the expected value."
}


