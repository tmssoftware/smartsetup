# This is similar to test.security.git-url-injection, but 
# it tests the second layer of defense, which is adding "--" to the command line.
# So even if some day we remove/relax the first layer of defense that rejects URLs starting with "-", 
# the second layer of defense should still protect us from command injection.

. test.setup

tms server-enable tms false
#Set #workingFolder to the folder "."
$workingFolder = Get-Location
tms server-add testserver zipfile "file:///$($workingFolder.Path.Replace('\', '/'))/git-poisoned-repos.zip"


# --- Version starting with '-' is rejected ---
# These would, without validation, become "git switch <option>" — e.g.
# --orphan, --discard-changes, etc., which manipulate repo state.

Test-CommandFails { tms install tmstest.a -test-allow-vcs-commands-starting-with-minus } "*Errors downloading products from version control*" "version with leading double dash"
CheckLogHasString "strange hostname"
Test-CommandFails { tms install tmstest.b -test-allow-vcs-commands-starting-with-minus } "*Errors downloading products from version control*" "version with leading single dash"
CheckLogHasString "fatal: repository '-orphan' does not exist"
Test-CommandFails { tms install tmstest.c -test-allow-vcs-commands-starting-with-minus } "*Errors downloading products from version control*" "version that looks like git --force"
CheckLogHasString "fatal: repository '--force' does not exist"

# Whitespace before the dash must not bypass the check (the validator trims first).
Test-CommandFails { tms install tmstest.d -test-allow-vcs-commands-starting-with-minus} "*Errors downloading products from version control*" "version with leading space then dash"
CheckLogHasString "Invalid character in repository URL"
