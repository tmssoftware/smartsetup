# smartsetup shouldn't execute build events as that is a security risk
# It shouldn't be possible to execute arbitrary code during the build process
# If calc.exe or notepad.exe are opened, this test failed.

. test.setup
tms config-write -p:"configuration for all products:compilation options:debug dcus=true"
tms build

# uninstall_and_check(0)