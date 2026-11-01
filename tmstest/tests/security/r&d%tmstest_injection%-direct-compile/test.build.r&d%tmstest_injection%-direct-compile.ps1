# Check that compiling with msbuild and dcc32 works when the path has & and % characters,
# and that a variable in the path (%tmstest_injection%) can't be used to inject commands.

. test.setup

$env:TMSTEST_INJECTION = 'x" & echo "injected code" > "./injected.txt" & "'

$result = tms install tms.vcl.query
if ((Test-Result -CommandResult $result -Message "*Warning: Can't execute msbuild directly. Falling back to compiling using a bat file.*")) {
    throw "The message should not mention 'Warning: Can't execute msbuild directly. Falling back to compiling using a bat file.'. Actual message: $($result)"
}

if (Get-ChildItem -Recurse -Filter "injected.txt") {
    throw "The file 'injected.txt' should not exist after compiling with msbuild."
}


tms config-write -p:configuration-for-all-products:replace-delphi-versions=[delphi7,delphi2009]
tms install tms.vcl.query:1.18 #keep it frozen in time, because newer  versions might not support delphi7.

if (Get-ChildItem -Recurse -Filter "injected.txt") {
    throw "The file 'injected.txt' should not exist after compiling with dcc32."
}

if (-not (Test-Path "Products\tms.vcl.query\packages\d7\Win32\Release\TMSQueryStudio.dcp")) {
    throw "The TMSQueryStudio.dcp file should have been generated"
}

if (-not (Test-Path "Products\tms.vcl.query\packages\d2009\Win32\Release\TMSQueryStudio.dcp")) {
    throw "The TMSQueryStudio.dcp file should have been generated"
}
tms uninstall *

