#Check that all json is utf-8
. test.setup

tms server-enable community
$result = tms list-remote -json

#check no bom characters in the json output
if ($result -match "^\xEF\xBB\xBF") {
    throw "The json output from 'tms list-remote -json' contains a BOM character. It should be UTF-8 without BOM."
}

#Check $result has only valid ascii characters.
if ($result -match '[^\x00-\x7F]') {
    throw "The json output from 'tms list-remote -json' contains non-ASCII characters. Non-ASCII characters should be escaped in the JSON output."
}

