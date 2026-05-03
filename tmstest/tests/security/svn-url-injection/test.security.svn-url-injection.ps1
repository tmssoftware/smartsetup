# Tests that VCS operations reject URLs, versions and file paths whose first
# character is '-'. Without this check, values like "--upload-pack=cmd" would
# be parsed as command-line options by git/svn instead of as a positional
# argument, leading to remote code execution when a malicious config or
# package server supplies a crafted URL.
#
# See VCS.Sanitizer.pas (ValidateVCSUrl/Version/FilePath) and the matching
# `--` end-of-options separators in VCS.Engine.Git.pas / VCS.Engine.Svn.pas.

. test.setup

tms server-enable tms false
#Set #workingFolder to the folder "."
$workingFolder = Get-Location
tms server-add testserver zipfile "file:///$($workingFolder.Path.Replace('\', '/'))/svn-poisoned-repos.zip"


# --- Version starting with '-' is rejected ---
# These would, without validation, become "git switch <option>" — e.g.
# --orphan, --discard-changes, etc., which manipulate repo state.

Test-CommandFails { tms install tmstest.a } "*must not start with*" "version with leading double dash"
Test-CommandFails { tms install tmstest.b } "*must not start with*" "version with leading single dash"
Test-CommandFails { tms install tmstest.c } "*must not start with*" "version that looks like git --force"

# Whitespace before the dash must not bypass the check (the validator trims first).
Test-CommandFails { tms install tmstest.d } "*must not start with*" "version with leading space then dash"
