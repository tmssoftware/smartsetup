#Check that all json is utf-8

$result = tms list-remote -json -detailed

#check no bom characters in the json output
if ($result -match "^\xEF\xBB\xBF") {
    throw "The json output from 'tms list-remote -json' contains a BOM character. It should be UTF-8 without BOM."
}

#Check $result has only valid utf-8 characters. 
try {
    $bytes = [System.Text.Encoding]::UTF8.GetBytes($result)
    $decodedString = [System.Text.Encoding]::UTF8.GetString($bytes)
}
catch {
    throw "The json output from 'tms list-remote -json' contains invalid UTF-8 characters."
}