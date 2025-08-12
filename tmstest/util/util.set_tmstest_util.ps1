$Env:TMS_TEST_ROOT_DIR = $tmsTestRootDir

if ($IsWindows) {
  set-alias tmsutil "$tmsTestRootDir\util\delphi\tmstest_util\Win64\Debug\tmstest_util.exe"
}