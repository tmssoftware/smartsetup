# tms doctor searches for repeated bpls, but it should ignore bpls coming from Embarcadero.
# if not, a file like dbtest.bpl would be reported as duplicated if both Delphi 10.4 and Delphi 11 are installed.
# we have an array of "CompanyName" values that are in the bpls in the existing versions of Delphi.
# but a new version of Delphi could add a new CompanyName, so we check that the name in
# the Delphi we use for tests is in that array.

. test.setup
#get the folder where the file $Env:TMS_BDS is located
$bdsFolder = Split-Path -Path $Env:TMS_BDS -Parent

tms doctor -test-company-names:$bdsFolder