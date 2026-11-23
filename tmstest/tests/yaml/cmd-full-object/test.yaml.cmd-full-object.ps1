#checks replacing the full server section

. test.setup

tms config-write -p:"tms smart setup options:servers={local:{type: api,url: https://E:\tms\smartsetup-registry-pending\pending\pending.ziop},'otro mas':{type:api,url:'https://sd f'}}"
$result = tms config-read "tms smart setup options:servers:" -cmd

DiffText -ReferenceObject (
'-p:"tms smart setup options:servers:tms:enabled = true"',
'-p:"tms smart setup options:servers:community:enabled = false"',
'-p:"tms smart setup options:servers:local:type = api"',
'-p:"tms smart setup options:servers:local:url = https://E:\tms\smartsetup-registry-pending\pending\pending.ziop"',
'-p:"tms smart setup options:servers:local:enabled = true"',
'-p:"tms smart setup options:servers:otro mas:type = api"',
'-p:"tms smart setup options:servers:otro mas:url = https://sd f"',
'-p:"tms smart setup options:servers:otro mas:enabled = true"') -DifferenceObject $result

 tms config-write -p:"tms smart setup options:add-servers={remotes:{type: api,url: https://www.tmssoftware.com}}"
 $result = tms config-read "tms smart setup options:servers:" -cmd

DiffText -ReferenceObject (
'-p:"tms smart setup options:servers:tms:enabled = true"',
'-p:"tms smart setup options:servers:community:enabled = false"',
'-p:"tms smart setup options:servers:local:type = api"',
'-p:"tms smart setup options:servers:local:url = https://E:\tms\smartsetup-registry-pending\pending\pending.ziop"',
'-p:"tms smart setup options:servers:local:enabled = true"',
'-p:"tms smart setup options:servers:otro mas:type = api"',
'-p:"tms smart setup options:servers:otro mas:url = https://sd f"',
'-p:"tms smart setup options:servers:otro mas:enabled = true"',
'-p:"tms smart setup options:servers:remotes:type = api"',
'-p:"tms smart setup options:servers:remotes:url = https://www.tmssoftware.com"',
'-p:"tms smart setup options:servers:remotes:enabled = true"') -DifferenceObject $result


 tms config-write -p:"tms smart setup options:replace-servers={remotes:{type: api,url: https://www.tmssoftware.com}}"
 $result = tms config-read "tms smart setup options:servers:" -cmd

DiffText -ReferenceObject (
'-p:"tms smart setup options:servers:tms:enabled = true"',
'-p:"tms smart setup options:servers:community:enabled = false"',
'-p:"tms smart setup options:servers:remotes:type = api"',
'-p:"tms smart setup options:servers:remotes:url = https://www.tmssoftware.com"',
'-p:"tms smart setup options:servers:remotes:enabled = true"') -DifferenceObject $result


 tms config-write -clear -p:"tms smart setup options:add-servers={newone:{type: api,url: https://www.tmssoftware.com}}"
 $result = tms config-read "tms smart setup options:servers:" -cmd

DiffText -ReferenceObject (
'-p:"tms smart setup options:servers:tms:enabled = true"',
'-p:"tms smart setup options:servers:community:enabled = false"',
'-p:"tms smart setup options:servers:newone:type = api"',
'-p:"tms smart setup options:servers:newone:url = https://www.tmssoftware.com"',
'-p:"tms smart setup options:servers:newone:enabled = true"') -DifferenceObject $result

 tms config-write -p:"tms smart setup options:add-servers={remotes:{type: api,url: https://www.tmssoftware.com}}"
 $result = tms config-read "tms smart setup options:servers:" -cmd
 
DiffText -ReferenceObject (
'-p:"tms smart setup options:servers:tms:enabled = true"',
'-p:"tms smart setup options:servers:community:enabled = false"',
'-p:"tms smart setup options:servers:newone:type = api"',
'-p:"tms smart setup options:servers:newone:url = https://www.tmssoftware.com"',
'-p:"tms smart setup options:servers:newone:enabled = true"',
'-p:"tms smart setup options:servers:remotes:type = api"',
'-p:"tms smart setup options:servers:remotes:url = https://www.tmssoftware.com"',
'-p:"tms smart setup options:servers:remotes:enabled = true"') -DifferenceObject $result

 tms config-write -clear -p:"tms smart setup options:servers:newone:type=api" -p:"tms smart setup options:servers:newone:url=https://www.tmssoftware.com" -p:"tms smart setup options:servers:newone:enabled=true"

 $result = tms config-read "tms smart setup options:servers:" -cmd

DiffText -ReferenceObject (
'-p:"tms smart setup options:servers:tms:enabled = true"',
'-p:"tms smart setup options:servers:community:enabled = false"',
'-p:"tms smart setup options:servers:newone:type = api"',
'-p:"tms smart setup options:servers:newone:url = https://www.tmssoftware.com"',
'-p:"tms smart setup options:servers:newone:enabled = true"') -DifferenceObject $result
