# this test should break every time we change the config file.
# if it breaks too much we can review it, but I think it is nice to do a diff
# every time we change something in the config. We shouldn't be changing the config that much anyway

. test.setup

$schemaPath = Join-Path $PSScriptRoot "..\..\..\..\tms\example-config\tms.config.schema.json"
ajv test -s $schemaPath -d tms.config.yaml --valid

tms config -reset -print
ajv test -s $schemaPath -d tms.config.yaml --valid

compare-files tms.config.yaml tms.config.expected.yaml

for ($i = 1; $i -le 2; $i++) {
    tms config-write -p:configuration-for-test:options:skip-register=[all,-startmenu] -p:configuration-for-test:compiler-paths:delphi7=r:\test7 -p:configuration-for-test:compiler-paths:delphixe=r:\testXE -p:configuration-for-test:delphi-versions=[delphi7:false,delphixe] -p:configuration-for-test:platforms=[win64intel] -p:configuration-for-test:compilation-options:defines=[RTTI:false,MyUnicode]
    ajv test -s $schemaPath -d tms.config.yaml --valid

    compare-files tms.config.yaml tms.config.expected2.yaml
}

