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
tms server-enable community true

# --- Version starting with '-' is rejected ---
# These would, without validation, become "git switch <option>" — e.g.
# --orphan, --discard-changes, etc., which manipulate repo state.

Test-CommandFails { tms install sglienke.spring4d:"--upload-pack=evil" } "*must not start with*" "version with leading double dash"
Test-CommandFails { tms install sglienke.spring4d:"-orphan" } "*must not start with*" "version with leading single dash"
Test-CommandFails { tms install sglienke.spring4d:"--force" } "*must not start with*" "version that looks like git --force"

# Whitespace before the dash must not bypass the check (the validator trims first).
Test-CommandFails { tms install sglienke.spring4d:" --upload-pack=evil" } "*must not start with*" "version with leading space then dash"

# --- Versions that contain '-' but do NOT start with one are still allowed ---
# These should fail because the version doesn't exist, NOT because of validation.

Test-CommandFails { tms install sglienke.spring4d:1.2.3-beta } "*Can't find the version*" "semver tag with internal hyphen"
Test-CommandFails { tms install sglienke.spring4d:feature/my-branch } "*Can't find the version*" "branch name with internal hyphen"
Test-CommandFails { tms install sglienke.spring4d:release-1.0 } "*Can't find the version*" "tag with internal hyphen at start of suffix"
