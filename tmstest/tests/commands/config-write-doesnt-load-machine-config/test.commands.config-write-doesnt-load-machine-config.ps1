# Checks the -config-source option of the commands that read and write the configuration.
# Reading defaults to the merged configuration (global, main and local files).
# Writing defaults to tms.config.yaml, and loads only the file being written, so values
# from the other files never end up in it.
# Files passed with -add-config are specified by hand, work like -p, and are written.

. test.setup

$originalUserProfile = $env:USERPROFILE
$globalValues = @('global-machine-key', 'global.excluded', 'file://global-server-url')
$localValues = @('clone-local-machine', 'file://local-server-url')

function Assert-FileDoesntContain {
    param(
        [Parameter(Mandatory, Position = 0)] [string] $Step,
        [Parameter(Mandatory, Position = 1)] [string] $FileName,
        [Parameter(Mandatory, Position = 2)] [string[]] $Values
    )

    $content = Get-Content -Path $FileName -Raw
    foreach ($value in $Values) {
        if ($content.Contains($value)) {
            throw "$($Step): '$FileName' should not contain '$value'."
        }
    }
}

function Assert-FileContains {
    param(
        [Parameter(Mandatory, Position = 0)] [string] $Step,
        [Parameter(Mandatory, Position = 1)] [string] $FileName,
        [Parameter(Mandatory, Position = 2)] [string] $Value
    )

    if (-not (Get-Content -Path $FileName -Raw).Contains($Value)) {
        throw "$($Step): '$FileName' should contain '$Value'."
    }
}

function Assert-NoMachineValues {
    param([Parameter(Mandatory, Position = 0)] [string] $Step)
    Assert-FileDoesntContain $Step 'tms.config.yaml' ($globalValues + $localValues)
}

try {
    # The files are created here so nothing loads them until the test setup finished.
    $globalFolder = Join-Path (Get-Location) 'fake-user-profile' '.smartsetup'
    $globalFile = Join-Path $globalFolder 'tms.config.global.yaml'
    $localFile = 'tms.config.local.yaml'
    New-Item -ItemType Directory -Path $globalFolder -Force | Out-Null
    $env:USERPROFILE = Join-Path (Get-Location) 'fake-user-profile'

    Set-Content -Path $globalFile -Value @'
tms smart setup options:
  alternate registry key: global-machine-key
  excluded products:
    - global.excluded
  servers:
    global-server:
      type: zipfile
      url: file://global-server-url
      enabled: true
'@

    Set-Content -Path $localFile -Value @'
tms smart setup options:
  git:
    clone command: clone-local-machine
  add servers:
    local-server:
      type: zipfile
      url: file://local-server-url
      enabled: true
'@

    Set-Content -Path 'tms.config.extra.yaml' -Value @'
tms smart setup options:
  svn:
    update command: update-extra-config
'@

    # A plain "servers:" section replaces the custom servers from the files loaded before it,
    # so we need "add servers:" to have the servers from the three files in the merged configuration.
    $mainConfig = (Get-Content -Path 'tms.config.yaml' -Raw) -replace '(?m)^  servers:', '  add servers:'
    Set-Content -Path 'tms.config.yaml' -Value $mainConfig -NoNewline

    # Reading without -config-source returns the merged configuration.
    tms config-read "tms smart setup options:alternate registry key" | Assert-ValueIs 'global-machine-key'
    tms config-read "tms smart setup options:git:clone command" | Assert-ValueIs 'clone-local-machine'
    $servers = tms server-list -json | ConvertFrom-Json
    $servers.'global-server'.url | Assert-ValueIs 'file://global-server-url'
    $servers.'local-server'.url | Assert-ValueIs 'file://local-server-url'

    # Reading with -config-source returns only the values in that file.
    tms config-read "tms smart setup options:alternate registry key" -config-source:global | Assert-ValueIs 'global-machine-key'
    tms config-read "tms smart setup options:alternate registry key" -config-source:main | Assert-ValueIs ''
    tms config-read "tms smart setup options:alternate registry key" -config-source:local | Assert-ValueIs ''
    tms config-read "tms smart setup options:git:clone command" -config-source:local | Assert-ValueIs 'clone-local-machine'
    tms config-read "tms smart setup options:git:clone command" -config-source:main | Assert-ValueIs ''

    $mainServers = tms server-list -json -config-source:main | ConvertFrom-Json
    if ($mainServers.PSObject.Properties.Name -contains 'global-server' -or $mainServers.PSObject.Properties.Name -contains 'local-server') {
        throw 'server-list -config-source:main should only show the servers in tms.config.yaml.'
    }
    (tms server-list -json -config-source:local | ConvertFrom-Json).'local-server'.url | Assert-ValueIs 'file://local-server-url'
    (tms server-list -json -config-source:global | ConvertFrom-Json).'global-server'.url | Assert-ValueIs 'file://global-server-url'

    # config-write writes tms.config.yaml by default, without the global or local values.
    tms config-write
    Assert-NoMachineValues 'config-write without parameters'

    tms config-write -p:tms-smart-setup-options:build-cores=3
    Assert-NoMachineValues 'config-write with -p'
    tms config-read "tms smart setup options:build cores" | Assert-ValueIs '3'

    tms config-write -p:tms-smart-setup-options:svn:checkout-command=checkout-main -config-source:main
    Assert-NoMachineValues 'config-write -config-source:main'
    Assert-FileContains 'config-write -config-source:main' 'tms.config.yaml' 'checkout-main'

    # -add-config files are written, but the automatically loaded files still aren't.
    tms config-write -add-config:tms.config.extra.yaml
    Assert-NoMachineValues 'config-write with -add-config'
    Assert-FileContains 'config-write with -add-config' 'tms.config.yaml' 'update-extra-config'

    # server-add, server-enable and server-remove on a server in tms.config.yaml.
    tms server-add main-server zipfile file://main-server-url
    Assert-NoMachineValues 'server-add'
    Assert-FileContains 'server-add' 'tms.config.yaml' 'file://main-server-url'

    tms server-enable main-server false
    Assert-NoMachineValues 'server-enable'
    (tms server-list -json | ConvertFrom-Json).'main-server'.enabled | Assert-ValueIs $false

    tms server-remove main-server
    Assert-NoMachineValues 'server-remove'
    Assert-FileDoesntContain 'server-remove' 'tms.config.yaml' @('file://main-server-url')

    # Without -config-source, servers defined only in the global or local files can't be changed.
    foreach ($server in @('global-server', 'local-server')) {
        Test-CommandFails { tms server-enable $server false } "*Cannot find the server `"$server`"*"
        Test-CommandFails { tms server-remove $server } "*Server `"$server`" is not defined*"
    }
    Assert-NoMachineValues 'failed server commands'

    # server-add doesn't see the local definition, which still has priority when the configuration is loaded.
    tms server-add local-server zipfile file://main-server-url
    Assert-FileContains 'server-add with a local definition' 'tms.config.yaml' 'file://main-server-url'
    Assert-NoMachineValues 'server-add with a local definition'
    (tms server-list -json | ConvertFrom-Json).'local-server'.url | Assert-ValueIs 'file://local-server-url'

    # Removing it from tms.config.yaml leaves the definition in the local file.
    tms server-remove local-server
    (tms server-list -json | ConvertFrom-Json).'local-server'.url | Assert-ValueIs 'file://local-server-url'

    # Writing the local and global files changes only that file, and doesn't copy values from the others.
    $mainBefore = Get-Content -Path 'tms.config.yaml' -Raw

    tms config-write -p:tms-smart-setup-options:git:git-location=c:\local-machine\git.exe -config-source:local
    Assert-FileContains 'config-write -config-source:local' $localFile 'c:\local-machine\git.exe'
    Assert-FileContains 'config-write -config-source:local' $localFile 'clone-local-machine'
    Assert-FileDoesntContain 'config-write -config-source:local' $localFile ($globalValues + @('checkout-main', 'update-extra-config'))
    tms config-read "tms smart setup options:git:git location" | Assert-ValueIs 'c:\local-machine\git.exe'

    tms config-write -p:tms-smart-setup-options:svn:svn-location=c:\global-machine\svn.exe -config-source:global
    Assert-FileContains 'config-write -config-source:global' $globalFile 'c:\global-machine\svn.exe'
    Assert-FileContains 'config-write -config-source:global' $globalFile 'global-machine-key'
    Assert-FileDoesntContain 'config-write -config-source:global' $globalFile ($localValues + @('c:\local-machine\git.exe', 'checkout-main', 'update-extra-config'))
    # Read from the global file: tms.config.yaml is written with every key, so its empty "svn location:" overrides the global value.
    tms config-read "tms smart setup options:svn:svn location" -config-source:global | Assert-ValueIs 'c:\global-machine\svn.exe'

    tms server-add second-local-server zipfile file://second-local-server-url -config-source:local
    Assert-FileContains 'server-add -config-source:local' $localFile 'file://second-local-server-url'

    tms server-enable local-server false -config-source:local
    (tms server-list -json -config-source:local | ConvertFrom-Json).'local-server'.enabled | Assert-ValueIs $false

    tms server-remove global-server -config-source:global
    Assert-FileDoesntContain 'server-remove -config-source:global' $globalFile @('file://global-server-url')

    if ((Get-Content -Path 'tms.config.yaml' -Raw) -ne $mainBefore) {
        throw 'Writing the local or global configuration should not modify tms.config.yaml.'
    }

    # -config-source can only be specified once.
    Test-CommandFails { tms config-write -config-source:main -config-source:local } '*-config-source can only be specified once.*'
    Test-CommandFails { tms config-read -config-source:local -config-source:local } '*-config-source can only be specified once.*'
    if ((Get-Content -Path 'tms.config.yaml' -Raw) -ne $mainBefore) {
        throw 'A failed config-write should not modify tms.config.yaml.'
    }

    # Writing the global configuration creates the .smartsetup folder if it doesn't exist.
    Remove-Item -Path $globalFolder -Recurse -Force
    tms config-write -p:tms-smart-setup-options:build-cores=5 -config-source:global
    Assert-FileContains 'config-write -config-source:global without a .smartsetup folder' $globalFile 'build cores: 5'
}
finally {
    $env:USERPROFILE = $originalUserProfile
}
