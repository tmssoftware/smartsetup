# Tests that VCS operations reject version strings containing characters
# outside the allowed whitelist (letters, digits, . - _ / +).
# This prevents command injection via shell metacharacters.

. test.setup

tms server-enable tms false
tms server-enable community true

# --- Version injection via shell metacharacters ---

# Semicolon: tries to chain a second command
Test-CommandFails { tms install sglienke.spring4d:"main;echo injected" } "*is not allowed*" "semicolon injection"

# Pipe: tries to pipe output to another command
Test-CommandFails { tms install sglienke.spring4d:"main|echo injected" } "*is not allowed*" "pipe injection"

# Ampersand: tries to run a background/chained command
Test-CommandFails { tms install sglienke.spring4d:"main&echo injected" } "*is not allowed*" "ampersand injection"

# Dollar sign: tries shell variable expansion or subshell
Test-CommandFails { tms install sglienke.spring4d:'main$(echo injected)' } "*is not allowed*" "dollar subshell injection"

# Backtick: tries command substitution
Test-CommandFails { tms install sglienke.spring4d:'main`echo injected`' } "*is not allowed*" "backtick injection"

# Double quote: tries to break out of quoting
Test-CommandFails { tms install sglienke.spring4d:'main"--help' } "*is not allowed*" "double quote injection"

# Exclamation mark: history expansion in some shells
Test-CommandFails { tms install sglienke.spring4d:"main!!" } "*is not allowed*" "exclamation injection"

# --- Ensure valid characters in versions are accepted ---
# These should fail because the versions don't exist, NOT because of validation.

Test-CommandFails { tms install sglienke.spring4d:this_version_does_not_exist } "*Can't find the version*" "normal invalid version"
Test-CommandFails { tms install sglienke.spring4d:1.2.3-beta+build } "*Can't find the version*" "semver with hyphens and plus"
Test-CommandFails { tms install sglienke.spring4d:feature/my_branch } "*Can't find the version*" "branch name with slash"
Test-CommandFails { tms install sglienke.spring4d:v1.0-🚀 } "*Can't find the version*" "version with emoji"
