# Having a flow array of flow arrays is not supported by our YAML,
# but when you are using cmd, it might be needed.

. test.setup

function Check-Write-Array {
    param (
        [string]$value,
        [string[]]$items
    )
    tms spec -non-interactive -s:"registry keys = $value" -json
    
    #read tmsbuild.json
    $tmsbuildJson = Get-Content -Path .\tmsbuild.json -Raw | ConvertFrom-Json -AsHashtable
    $regkeys = $tmsbuildJson['registry keys']
    if ($items.Count -ne $regkeys.Count) {
        throw "Expected registry keys to have $($items.Count) items, but got $($regkeys.Count)"
    }
    for ($i = 0; $i -lt $items.Count; $i++) {
        $expectedItem = $items[$i]
        $foundItem = $regkeys[$i] | ConvertTo-Json -Depth 50
        if ($foundItem -ne $expectedItem) {
            throw "Expected registry key item $($i) to be '$expectedItem', but got '$($foundItem)'"
        }
    }
}

function Check-Write-Array-Error {
    param (
        [string]$value,
        [string]$expectedErrorMessage
    )
    $result = Invoke-WithExitCodeIgnored {tms spec -non-interactive -s:"$value" -json}

    if (-not ($result -join "`n").Contains($expectedErrorMessage)) { throw "Expected error message to contain '$expectedErrorMessage', but got '$($result)'" }

}

Check-Write-Array-Error 'packages = [JOSE = [runtime],JOSE_TaurusTLSProvider = [runtime,exe],JOSE_CryptoLib4PascalProvider = [runtime]' `
'Error: "[JOSE = [runtime],JOSE_TaurusTLSProvider = [runtime,exe],JOSE_CryptoLib4PascalProvider = [runtime]" is not a valid flow item. It must end with a "]"'

Check-Write-Array "[TMS WEB Core = [value ={name = InstallDir,data = '%install- #path%'}, value={name = 'No',type = dword,data = '2'}],Components = [value={name = ' on',data = 'off '}]]" `
@(
    @'
{
  "TMS WEB Core": [
    {
      "value": {
        "name": "InstallDir",
        "data": "%install- #path%"
      }
    },
    {
      "value": {
        "name": "No",
        "type": "dword",
        "data": "2"
      }
    }
  ]
}
'@
@'
{
  "Components": [
    {
      "value": {
        "name": " on",
        "data": "off "
      }
    }
  ]
}
'@

)

Check-Write-Array-Error 'packages = [JOSE = [runtime,rtl,rtl2plus, crypto4pascal],"JOSE_TaurusTLSProvider" = [runtime,taurus],JOSE_CryptoLib4PascalProvider = [runtime,crypto4pascal]]' `
'Error: "rtl" is an invalid child section for "packages:JOSE". It must be one of: ["design", "runtime", "exe"].'