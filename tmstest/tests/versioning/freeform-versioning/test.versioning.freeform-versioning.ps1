# test versioning in git

. test.setup

tms server-enable tms false
tms server-enable community true

Test-CommandFails { tms install sglienke.spring4d:invalid_version } "*Can't find the version ""invalid_version"" in product sglienke.spring4d*"
