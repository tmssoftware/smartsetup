. test.setup

function Test-NullableProperty() {
    param (
        [string]$PropertyName,
        [string]$PropertyValue
    )

    tms config-read "configuration for test:$PropertyName" | Assert-ValueIs ""
    tms config-write -p:"configuration for test:$PropertyName="
    tms config-read "configuration for test:$PropertyName" | Assert-ValueIs ""
    tms config-write -p:"configuration for test:$PropertyName=$PropertyValue"
    tms config-read "configuration for test:$PropertyName" | Assert-ValueIs "$PropertyValue"
    tms config-write -p:"configuration for test:$PropertyName="
    tms config-read "configuration for test:$PropertyName" | Assert-ValueIs ""
}

function Test-NullableArray() {
    param (
        [string]$PropertyName,
        [string]$PropertyValue
    )
    #We don't discern between "empty array" or "null array". When reading, empty arrays will be ""
    tms config-read "configuration for test:$PropertyName" | Assert-ValueIs ""
    tms config-write -p:"configuration for test:$PropertyName="
    tms config-read "configuration for test:$PropertyName" | Assert-ValueIs ""
    tms config-write -p:"configuration for test:$PropertyName=$PropertyValue"
    tms config-read "configuration for test:$PropertyName" | Assert-ValueIs "$PropertyValue"
    tms config-write -p:"configuration for test:$PropertyName=[]"
    tms config-read "configuration for test:$PropertyName" | Assert-ValueIs ""
}

tms config -reset -print

Test-NullableProperty -PropertyName "options:verbosity" -PropertyValue "info"
Test-NullableProperty -PropertyName "options:skip-register" -PropertyValue "false"
Test-NullableProperty -PropertyName "options:skip-register" -PropertyValue "true"
Test-NullableProperty -PropertyName "options:skip-register" -PropertyValue "[all,-startmenu]"

Test-NullableProperty -PropertyName "options:dry run" -PropertyValue "true"

Test-NullableArray -PropertyName "delphi versions" -PropertyValue "[delphi12]"
Test-NullableArray -PropertyName "platforms" -PropertyValue "[win32intel,win64xintel]"

Test-NullableProperty -PropertyName "compilation options:debug dcus" -PropertyValue "true"
Test-NullableArray -PropertyName "compilation options:defines" -PropertyValue "[UNICODE,MyTest]"
Test-NullableArray -PropertyName "compilation options:defines" -PropertyValue "[UNICODE,MyTest: false]"

Test-NullableProperty -PropertyName "advanced options:use symlinks" -PropertyValue "false"
Test-NullableProperty -PropertyName "advanced options:keep parallel folders" -PropertyValue "true"
Test-NullableProperty -PropertyName "advanced options:modify sources" -PropertyValue "false"
Test-NullableProperty -PropertyName "advanced options:partial builds" -PropertyValue "true"
Test-NullableProperty -PropertyName "advanced options:add source code to library path" -PropertyValue "false"


Test-NullableProperty -PropertyName "compiler paths:delphi12" -PropertyValue "c:\test"
#CompilerParameters
