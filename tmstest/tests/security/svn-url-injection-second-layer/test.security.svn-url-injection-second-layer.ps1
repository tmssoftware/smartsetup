# This is similar to test.security.svn-url-injection, but 
# it tests the second layer of defense, which is adding "--" to the command line.
# So even if some day we remove/relax the first layer of defense that rejects URLs starting with "-", 
# the second layer of defense should still protect us from command injection.

. test.setup

tms server-enable tms false
#Set #workingFolder to the folder "."
$workingFolder = Get-Location
tms server-add testserver zipfile "file:///$($workingFolder.Path.Replace('\', '/'))/svn-poisoned-repos.zip"


Test-CommandFails { tms install tmstest.a -test-allow-vcs-commands-starting-with-minus } "*Errors downloading products from version control*" "version with leading double dash"
CheckLogHasString "Error doing svn checkout from"
Test-CommandFails { tms install tmstest.b -test-allow-vcs-commands-starting-with-minus } "*Errors downloading products from version control*" "version with leading single dash"
CheckLogHasString "Error doing svn checkout from"
Test-CommandFails { tms install tmstest.c -test-allow-vcs-commands-starting-with-minus } "*Errors downloading products from version control*" "version that looks like git --force"
CheckLogHasString "Error doing svn checkout from"

# Whitespace before the dash must not bypass the check (the validator trims first).
Test-CommandFails { tms install tmstest.d -test-allow-vcs-commands-starting-with-minus} "*Errors downloading products from version control*" "version with leading space then dash"
CheckLogHasString "Invalid character in repository URL"
