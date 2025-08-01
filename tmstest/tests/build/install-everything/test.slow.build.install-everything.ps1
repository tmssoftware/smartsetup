# Install all the products and see that there are no errors.
# This is a slow test, so it should be named test.slow.build.install-everything.ps1
# It should be run with `tmstest -skip-slow` to skip it.

. test.setup

  tms install *    
  tms uninstall * #only if no errors. If there are errors, we leave it there for investigation.

