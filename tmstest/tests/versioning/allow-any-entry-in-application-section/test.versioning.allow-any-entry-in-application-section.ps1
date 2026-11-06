# A common thing we can do is to add new information fields to tmsbuild.yaml in the "Application" section. Say, a license url, an image or whatever
# Adding this automatically breaks old smartsetup versions, which will complain the field is unknown
# So we are relaxing the application section to allow anything. It is information anyway, and shouldn't break old smartsetups
# You can always require smartsetup vx.y to disable those old smartsetups from working.

. test.setup

$result = tms build

CheckTextHasString -Text $result -Message "*Unknown property ignored: `"awesome description`"*"
CheckLogHasString "Unknown property ignored: `"awesome description`""