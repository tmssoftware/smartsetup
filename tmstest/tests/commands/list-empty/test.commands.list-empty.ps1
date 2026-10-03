#Check that list returns empty in an empty folder

. test.setup

$result = tms list

if ($null -ne $result -and $result.Trim() -ne "") {
    throw "list should return nothing in an empty folder: $result"
}

mkdir potato
cd potato

$result = tms list

if ($null -ne $result -and $result.Trim() -ne "") {
    throw "list should return nothing in an empty folder: $result"
}

