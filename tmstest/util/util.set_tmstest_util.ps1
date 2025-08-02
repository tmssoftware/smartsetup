$Env:TMS_TEST_ROOT_DIR = $tmsTestRootDir

if ($IsWindows) {
  set-alias tmsutil "$tmsTestRootDir\util\delphi\tmsutil\Win64\Debug\tmstest_util.exe"
}