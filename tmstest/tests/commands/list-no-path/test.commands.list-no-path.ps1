# Checks that the list command runs without anything in the path, specifically snv.exe

. test.setup

tms server-enable community true
# clear the windows PATH
$env:PATH = ""
# Remove any svn alias that may have been defined by the user profile, so svn is truly unreachable
# for the rest of this test. The alias is usually set at Global scope in the user's PowerShell profile.
Remove-Alias -Name svn -Scope Global -Force -ErrorAction SilentlyContinue

if (Get-Command svn -ErrorAction SilentlyContinue) {
    throw "svn should not be reachable after clearing PATH, but Get-Command resolved it."
}

tms install tms.graphql yankovsky.delphi.ast

Remove-Alias -Name git -Scope Global -Force -ErrorAction SilentlyContinue
if (Get-Command git -ErrorAction SilentlyContinue) {
    throw "git should not be reachable after clearing PATH, but Get-Command resolved it."
}


$ListResult = tms list

$ListLines = @($ListResult -split "`r?`n" | Where-Object { $_.Trim() -ne "" })

if ($ListLines.Count -ne 3) {
	throw "list should return exactly 2 lines: $ListResult"
}

$TmsGraphqlLines = @($ListLines | Where-Object { $_ -match "tms\.graphql" })
$AstLines = @($ListLines | Where-Object { $_ -match "yankovsky\.delphi\.ast" })

if ($TmsGraphqlLines.Count -ne 1 -or $AstLines.Count -ne 1) {
	throw "list should contain tms.graphql and yankovsky.delphi.ast on separate lines: $ListResult"
}

if ($TmsGraphqlLines[0] -eq $AstLines[0]) {
	throw "list should show each product on its own line: $ListResult"
}



