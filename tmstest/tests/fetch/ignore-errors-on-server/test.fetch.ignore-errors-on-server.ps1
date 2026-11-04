# Check that a misconfigured file doesn't cause the full tms to stop working.

. test.setup
tms server-enable tms false
$PWDFWD = $PWD.Path.Replace('\', '/')
tms server-add testserver zipfile "file:///$($PWDFWD)/.repo.zip"

$result = tms list-remote

if ($result -ne "product-ok () -> testserver") {
    throw "list-remote should have returned a single product and it returned: $result"
}

tms server-add testserver1 zipfile "file:///$($PWDFWD)/.repo1.zip"

$result = tms list-remote -json | ConvertFrom-Json -AsHashtable
if ($result.Count -ne 1 -or $result["product-ok"].name -ne "product ok") {
    throw "list-remote should have returned a single product (not from testserver1) and it returned: $($result | ConvertTo-Json)"
}

tms server-remove testserver1

tms server-add testserver2 zipfile "file:///$($PWDFWD)/.repo2.zip"

$result = tms list-remote

if ($result -ne "product-ok () -> testserver") {
    throw "list-remote should have returned a single product without warnings and it returned: $result"
}

tms server-add testserver3 zipfile "file:///$($PWDFWD)/.repo3.zip"

$result = tms list-remote

if (($result -join "\n") -notmatch 'Error loading project: The text "Hi! How can I assist you today?') {
    throw "list-remote should have warned about the invalid configuration. It returned: $result"
}

#json should be valid, not contain warnings.
$result = tms list-remote -json | ConvertFrom-Json -AsHashtable

if ($result.Count -ne 1 -or $result["product-ok"].name -ne "product ok") {
    throw "list-remote should have returned a single product (not from testserver2 or testserver3) and it returned: $($result | ConvertTo-Json)"
}
