# Checks that the list command runs without anything in the path, specifically snv.exe

. test.setup

function Test-ListOutput {
	param([string[]] $ListArgs)

	$ListResult = tms list @ListArgs

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
}

tms server-enable community true

tms install tms.graphql yankovsky.delphi.ast

Test-ListOutput -ListArgs @("-test-no-git")
Test-ListOutput -ListArgs @("-test-no-svn")
Test-ListOutput -ListArgs @("-test-no-git","-test-no-svn")





