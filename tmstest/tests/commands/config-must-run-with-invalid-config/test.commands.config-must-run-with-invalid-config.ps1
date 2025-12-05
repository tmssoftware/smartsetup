# No matter if the yaml is corrupt, tms config should be able to open it or reset it.
. test.setup

"This is not valid yaml." | Out-File -FilePath "tms.config.yaml"

tms config -print
tms config -print -reset
