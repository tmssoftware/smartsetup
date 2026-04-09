# update works a little different from install, as IgnoreMissing is true.

. test.setup

tmscredentials
tms server-enable community

tms install tms.biz.aurelius vsoft.commandline
tms server-enable tms $false

tms update
