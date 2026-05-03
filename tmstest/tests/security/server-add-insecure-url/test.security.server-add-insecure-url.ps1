# Tests that `tms server-add` rejects http:// URLs by default and that the
# scheme allowlist rejects other unsupported schemes. The opt-in for http://
# is the per-server YAML field "allow insecure connections: true" and is
# only set by editing tms.config.yaml directly (no CLI flag) — this matches
# how NuGet exposes its allowInsecureConnections option.
#
# See TServerConfig.ValidateUrlScheme in UConfigDefinition.pas and the call
# sites in Commands.ServerAdd.pas and ZipFile.Download.pas.

. test.setup

function Test-NoAllowInsecureConnections {
    $config = tms config-read -json | ConvertFrom-Json -AsHashtable
    #check $config doesn't contain "allow insecure connections", as it is false by default and should not be emitted in the config file.
    $servers = $config."tms smart setup options".servers
    foreach ($server in $servers.Values) {
        if ($server.ContainsKey("allow insecure connections")) {
            Write-Error "Server has 'allow insecure connections' set, but it should not be set by default."
        }
        
    }
}

# --- http:// is rejected at server-add time ---

Test-CommandFails { tms server-add myserver zipfile "http://example.com/foo.zip" } "*Refusing http://*" "http URL rejected on server-add"
Test-CommandFails { tms server-add myserver zipfile "HTTP://EXAMPLE.COM/foo.zip" } "*Refusing http://*" "http URL rejected case-insensitively"

# Error message must point the user at the YAML opt-in.
Test-CommandFails { tms server-add myserver zipfile "http://example.com/foo.zip" } "*allow insecure connections*" "error message mentions the YAML opt-in"

# --- file:// is only accepted for zipfile-type servers ---

Test-CommandFails { tms server-add myserver api "file:///c:/some/path.zip" } "*file:// URLs are only allowed for servers of type ""zipfile""*" "file:// rejected for api server"

# --- Other schemes are rejected ---

Test-CommandFails { tms server-add myserver zipfile "ftp://example.com/foo.zip" } "*Unsupported URL scheme*" "ftp URL rejected"
Test-CommandFails { tms server-add myserver zipfile "javascript:alert(1)" } "*Unsupported URL scheme*" "javascript URL rejected"
Test-CommandFails { tms server-add myserver zipfile "example.com/foo.zip" } "*Unsupported URL scheme*" "schemeless URL rejected"

# --- The allowed schemes still work ---

tms server-add httpsserver zipfile "https://example.com/foo.zip"
Test-NoAllowInsecureConnections
tms server-remove httpsserver

tms server-add httpsserver zipfile "HTTPS://example.com/foo.zip"
Test-NoAllowInsecureConnections
tms server-remove httpsserver

# Use a plausible local path; the file does not need to exist for server-add to succeed.
$workingFolder = Get-Location
tms server-add fileserver zipfile "file:///$($workingFolder.Path.Replace('\', '/'))/dummy.zip"
Test-NoAllowInsecureConnections
tms server-remove fileserver

tms config-write -p:"tms smart setup options:servers:tmstest.httpserver:enabled=true"
tms config-write -p:"tms smart setup options:add servers:tmstest.httpserver:url=http://example.com/foo.zip"
tms config-write -p:"tms smart setup options:add servers:tmstest.httpserver:type=zipfile"

Test-CommandFails { tms list-remote } "*Refusing http:// URL*" "manually added http URL is rejected by list-remote"

tms config-write -p:"tms smart setup options:add servers:tmstest.httpserver:type=api"
tms server-enable tms false
tms credentials -code:"test" -email:test -server:tmstest.httpserver
Test-CommandFails { tms list-remote } "*Refusing http:// URL*" "manually added http URL is rejected by list-remote"
Test-CommandFails { tms fetch tms.biz.bcl } "*Refusing http:// URL*" "manually added http URL is rejected by list-remote"


#allowed insecure connections
tms config-write -p:"tms smart setup options:add servers:tmstest.httpserver:type=zipfile"
tms config-write -p:"tms smart setup options:add servers:tmstest.httpserver:allow insecure connections=true"

Test-CommandFails { tms list-remote } "*failed with status 404*" "manually added http URL with allow insecure connections is accepted by list-remote"

tms config-write -p:"tms smart setup options:add servers:tmstest.httpserver:type=api"
Test-CommandFails { tms list-remote } "*failed with status code 404*" "manually added http URL with allow insecure connections"
