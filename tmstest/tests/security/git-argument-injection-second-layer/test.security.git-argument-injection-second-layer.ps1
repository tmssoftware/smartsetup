# This is similar to test.security.vcs-argument-injection, but
# it tests the second layer of defense, which is adding "--" to the command line.
# So even if some day we remove/relax the first layer of defense that rejects URLs starting with "-", 
# the second layer of defense should still protect us from command injection.

. test.setup

tms server-enable tms false
tms server-enable community true

# --- Version starting with '-' is rejected ---
# These would, without validation, become "git switch <option>" — e.g.
# --orphan, --discard-changes, etc., which manipulate repo state.

Test-CommandFails { tms install sglienke.spring4d:"--upload-pack=evil" -test-allow-vcs-commands-starting-with-minus} "*(0x3D) is not allowed. Only letters, digits, dots, hyphens, underscores, slashes and plus signs are permitted*" "version with leading double dash"
Test-CommandFails { tms install sglienke.spring4d:"-orphan" -test-allow-vcs-commands-starting-with-minus} "*Error fetching sglienke.spring4d:-orphan from GIT: Can't find the version*" "version with leading single dash"
CheckLogHasString "fatal: invalid reference: -orphan"
Test-CommandFails { tms install sglienke.spring4d:"--force" -test-allow-vcs-commands-starting-with-minus} "*Error fetching sglienke.spring4d:--force from GIT: Can't find the version*" "version that looks like git --force"
CheckLogHasString "fatal: invalid reference: --force"

# Whitespace before the dash must not bypass the check (the validator trims first).
Test-CommandFails { tms install sglienke.spring4d:" --upload-pack-evil" -test-allow-vcs-commands-starting-with-minus} "*Error fetching sglienke.spring4d:--upload-pack-evil from GIT: Can't find the version*" "version with leading space then dash"
CheckLogHasString "fatal: invalid reference: --upload-pack-evil"

# --- Versions that contain '-' but do NOT start with one are still allowed ---
# These should fail because the version doesn't exist, NOT because of validation.

Test-CommandFails { tms install sglienke.spring4d:1.2.3-beta -test-allow-vcs-commands-starting-with-minus} "*Can't find the version*" "semver tag with internal hyphen"
Test-CommandFails { tms install sglienke.spring4d:feature/my-branch -test-allow-vcs-commands-starting-with-minus} "*Can't find the version*" "branch name with internal hyphen"
Test-CommandFails { tms install sglienke.spring4d:release-1.0 -test-allow-vcs-commands-starting-with-minus} "*Can't find the version*" "tag with internal hyphen at start of suffix"
