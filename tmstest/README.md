# Setting up the environment

1. tmstest runs on PowerShell. If in linux, you need to install PowerShell for linux.
2. For debugging, we recommend VSCode. Install the powershell extension.
3. Install tools
4. Setup the paths and aliases. You will have to do this from the **terminal** and from the **VSCode terminal**, since they load different user profiles.

for step 3, you currently need 
  * https://github.com/ajv-validator/ajv-cli
```shell
npm install -g ajv-cli
```


For step 4, go to the terminal you want to use (VSCode terminal or normal terminal) and type:

```shell
code $PROFILE
```
This will open your profile. Add the lines (adapting the paths for the machine)

```shell
set-alias tms E:\tms\smartsetup\tms\bin\Win64\Debug\tms.exe

$tmsTestRootDir = "E:\tms\tms-smartsetup\tmstest"
$env:TMSTEST_CODE="<reg-code>"
$env:TMSTEST_EMAIL="<reg-email>"

. $tmsTestRootDir/util/util.profile_startup.ps1
```

Once you have this done, **restart the terminal** (or VSCode). Now you should be able to type `$tmsTestRootDir` and see the value.

> [!IMPORTANT]
>If you save the reg/code in the profile, it will be stored in plain text wherever the profile is saved. You might prefer to set the variable in other manually.

# Defining which delphi versions and platforms to test
Testing every delphi version and platform might be prohibitive. So normally we will test the latest one, unless some test tests a specific version in particular.

To define what to test:
  * The delphi versions and platforms are defined in `tms.starting.config.yaml`. What we write here will affect all tests.
  * The delphi version used for `rsvars.bat` in tests that call msbuild directly are in `util.set_starting_config_yaml.ps1`

If the test has some specific need to test a particular delphi version, it can override the settings in `tms.starting.config.yaml` with their own, and not use the `TMS_RSVARS` variable or the command `msbuild` itself to test msbuild. 

# Running the tests

## Running from the console.

### Running all tests

```
tmstest
```
This will run all tests, and show which failed and which didn't.
A file *output.log* will be generated inside the `tmp-run\{test_folder}` for every test, with the output of the corresponding test.
Those folders might also have log files from smartsetup, if they run some smartsetup command.

### Running one test or a specific group of tests

```
tmstest build
```
this will run all tests that have `build` inside their name. We search for `test.*{parameter}*.tms` so in this case, this will run all tests matching `test.*build*.ps1`.

> [!NOTE]
> Don't run the tests directly, by executing them in the command line. They will refuse to run, because they need to be run outside the `tests` folder. Otherwise they would create their temporary files in the wrong place. To run them manually, call `tmstest name-of-the-test` instead. `tmstest` will basically copy the folder to tmp-run, and run it there.

### Slipping slow tests
```
tmstest -skip-slow
tmstest build -skip-slow
```
Will skip all slow tests.

### Debugging from the console

Sometimes, VSCode might not work or be a viable solution for debugging. You can use:
```shell
 Set-PSDebug -Trace 1 
```
To get a lot of what is happening in the console. Set it to 2 to be more verbose, and to 0 to reset to normal use.

## Running from Visual Studio Code

You can also use VSCode to debug the tests. To do so, you can open the workspace at the root: `tmstest.code-workspace`.

> [!NOTE]
> As you can't run the tests directly individually (you need to call them from `tmstest.ps1`), the workspace is configured to always run `tmstest.ps1`, no matter the active page. If you are in a particular test, VSCode will launch `tmstest.ps1 <filename_with_the_test.ps1>` instead of launching `filename_with_the_test.ps1` directly. You can change this in the folder `\.vscode\launch.json`

Currently `launch.json` is:
```json
 {
       ...
      "script": "${workspaceFolder}/tmstest.ps1 ${file}"
 }
```
So it will always launch tmstest.ps1, passing the active file as a parameter.
You can then run this normally: F5 starts the run, F10 steps over, F11 steps inside, etc.

> [!IMPORTANT]
> There are many "Run" commands in VS. F5 will work. Also pressing the triangle in "SmartSetup Tests" in the "Run and Debug" panel at the right will work. But there is a small triangle at the top left that won't call tmstest.ps1 and run the test directly. That will likely fail if isn't at the right place. If it fails, just use F5 instead to run it.

# Creating the tests

The test is just a script that runs commands, and if it returns an ErrorCode of 0, it is successful. If not, it failed. You can write to the screen during the test, but it won't show in the main `tmstest.ps1` app, unless you are running a single test. Anyway, the output will always be available in **output.log** in the `tmp-run\<test_name>` folder.

## Guidelines for creating tests

  * Tests must all start with the name **`test.`** 

  * Tests that take long to execute (like `tms install *`) should start with the name **`test.slow.`**. We can exclude those tests from running by calling `tmstest -skip-slow`.

  * Tests must not depend on other tests. A test must be able to run on its own, and not depend on the result of any other test. In the future, we might run the tests in parallel, so they can't assume any other test already completed.

  * Tests will run in a temp folder. The current folder will be the temp folder when called, so it must work that way. It can't assume the script is at the root of the test.

  * Tests should be inside of a folder with a similar name as the script. The folder name isn't important, but to make it simpler to edit it, they should have a similar name, but shorter. For example the test `test.build.framework-defines.ps1` can be inside a folder named `framework-defines`.  
  
  * Tests might be relocated in the folder hierarchy. We might move them to subfolders if they grow too much or reorganize the folder structure, and they should keep working. They shouldn't rely in being in any specific folder.

  * Do not specify delphi versions or platforms in the tmsbuild.yaml in the tests. We will manage this from a centralized location. 

  * When building products, either `skip register` or install to some `alternate registry key`. If installing to an alternate registry key, make sure to uninstall everything after installing.

  * If the test was because of a bug report, it should have a comment linking to it.

  * **The script must always start by calling `. test.setup` on the first line**. This will setup the environment, like for example making PowerShell stop on errors. **The `.` at the start of the line is important**, it means that the script will be sourced, instead of running on its own environment.
  
  * To **terminate** the script with an error, you can either call:
      * **`Write-Error`**: Will write the error in red, but **also stop the script with an error code**. You can specify a specific error code as parameter of `Write-Error` if you want
      * **throw "error message"** will exit with an exception.
      * **exit 1** will exit with error code 1. 

> [!IMPORTANT]
> The simplest way to terminate is with `Write-Error`, but there is a problem if inside a code that tests for fail to be the correct output:
> ```shell
>  try {
>    command
>    throw / Write-Error "command should have failed"
>  }
>  catch{}
>```
> As it can be seen, this exception (also from `Write-Error`) will be caught in the empty catch block and do noting. For those cases, you can use a "Error" variable, or use `Exit`:
> ```shell
>  try {
>    command
>    Write-Output "command should have failed"
>    exit 1
>  }
>  catch{}
>```
   
   * Do not use **`Write-Host`** in tests, as it can't be redirected. Use **`Write-Output`** instead.

> [!NOTE]
> The Write-Error behavior can be confusing. I would expect it to just write the error, not also terminate the script. But it does both. This is because we are terminating in non-terminating errors. 

## Utilities for writing tests

### Powershell
A lot of the stuff we need to do can be done directly in PowerShell. PowerShell commands return objects, not string (as a unix shell), so you can normally read the properties of the output. Some basic stuff:

   * Variables are written like `$variable`
   * String interpolation is done with `$()`. For example: `Write-Output "Skipped tests: $($skippedTests.Count)"`
   * You can write to the console with `Write-Output` and others. Each one writes to a different output stream (1 to 6)
   * **Beware of `Write-Host`**. This is designed to write to the console directly, not to the output stream. (Even if it can be redirected, it might be redirected in the wrong order) See https://learn.microsoft.com/en-us/powershell/scripting/learn/deep-dives/output-missing-from-transcript.
   * You can return an error with `Write-Error` or `"throw error message"`
   * You can do control flow with try/catch/finally
   * You can parse Json returned by a command with `| ConvertFrom-Json` and `| ConvertFrom-Json -AsHashtable`
   * You can read registry with `Get-ItemProperty -Path "HKCU:\Software\Embarcadero\BDS\$($BaseDelphiVersion)" -Name "RootDir"`
   * You can call a command with `& "command"` if it is in a string. You can source a command with `. command` (sourced commands run in the same environment as the calling script, so if you change a directory, it will stay changed when the command executes)
   * And a lot more... google and copilot are great here.


But besides this, we have our own utilities to make common stuff simple. `util.profile_startup.ps1` defines the following aliases:
   * **tmstest**: will call tmstest.ps1. One thing with PowerShell is that it doesn't run commands in the current folder, so adding an alias is the simplest way to know a command will be found.
   * **msbuild**: will build the projects in the current folder using msbuild.

   Some others might be added in the future to `util.profile_startup.ps1`

The following environment variables are set:
   * **TMS_RSVARS**: This is the location for rsvars.bat for the delphi version we are testing. That version is set in `util.set_starting_config_yaml.ps1`

> [!NOTE]
> We can't run rsvars from powershell. It runs, but the call is `cmd.exe rsvars.bat` which means that when the call ends, the variables rsvars set are forgotten. We already provide a command that runs msbuild.exe (`msbuild`) But if you need more control, to run msbuild directly, you need to create a .bat, and call it from the test script. Inside the bat you can write `"%TMS_RSVARS%"` to execute rsvars.

The following functions are defined in util.modules.psm1:
  * **Invoke-WithExitCodeIgnored**: Allows you to run a command that will fail and not crash.
  * **tmscredentials**: runs tms credentials so you can access tms repos.
  * **uninstall_and_check(products-remaining)**: uninstalls everything and checks that product-remaining products remain. Normally you will call this at the end of the script with products-remaining = 0.
  * **compare-files**: compares 2 files and returns an error if not the same.
  * **Assert-ValueIs**: Asserts 2 values are the same. Must be used as a filter, like `command | Assert-ValueIs("0")`
  * **Test-Result**: Checks if the result from a call contains some string

A starting tms.config.yaml is provided automatically to all `tms` calls, so you don't need to specify what is already specified there.

### Delphi
A command `tmstest_util` is provided that can be called from the scripts, to add specific delphi-only functionality we want to test, or stuff that is done simpler/faster in delphi. Currently tmstest_util has the commands:
  *  **delete-folder**: Deletes a folder, or moves the files to the locked folder if not possible.
  *  **clean-locked**: Deletes the locked folder.
