. test.setup

tms config -print
tms server-add potato zipfile https://tmssoftware.com/potato.zip

tms server-enable tms false
try
{
  tms list-remote -json
  write-output "The command 'tms list-remote -json' should have failed because the server doesn't exist."
  exit 1
}
catch
{
  Write-Output "Correctly returned an error."
}