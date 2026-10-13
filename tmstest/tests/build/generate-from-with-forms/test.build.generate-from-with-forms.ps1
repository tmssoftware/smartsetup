# Check that generate from handles packages with forms and .rc resources: .dfm and .rc files must not go into the dpk contains clause.

. test.setup

tms build

$result = & ".\app\$($Global:BaseDelphiVersion)\Win32\Release\generate_fromtest.exe"
if (($result -join ' ') -ne '31629b2d0e94437fe37fbb0a42e03213') {
    throw "Error The app returned '$result' instead of the expected value."
}
