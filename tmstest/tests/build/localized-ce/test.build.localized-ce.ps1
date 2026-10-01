# In Delphi CE, we need to check for the "Success" string in the output. 
# But that string is localized, so we need to check for the localized version of "Success" instead.
# This test won't run in a normal machine, to avoid destroying delphi.

. test.setup

# if the user is not WDAGUtilityAccount, print a warning and exit the test
if ($env:USERNAME -ne "WDAGUtilityAccount") {
    throw "This test is designed to run in a Windows Sandbox environment. Exiting the test."    
}

tms config-write -p:configuration-for-all-products:replace-platforms=[]


Set-Alias bdssetlang "$($BDS_ROOT_DIR.RootDir)\bin\BDSSetLang.exe"

bdssetlang de
tms install tms.biz.bcl -test-delphi-ce
CheckLogHasString "Erfolg"
tms uninstall *

bdssetlang ja
tms install tms.biz.bcl -test-delphi-ce
CheckLogHasString "成功"
tms uninstall *

bdssetlang fr
tms install tms.biz.bcl -test-delphi-ce
CheckLogHasString "Succès"
tms uninstall *

bdssetlang en
tms install tms.biz.bcl -test-delphi-ce
CheckLogHasString "Success"
tms uninstall *


