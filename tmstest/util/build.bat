@echo on
@:: Enable delayed expansion for !ERRORLEVEL!
@    setlocal enabledelayedexpansion

@echo build started
call "%TMS_RSVARS%"
%~1
%~2
%~3
%~4
@if !ERRORLEVEL! NEQ 0 (
@       echo Error %ERRORLEVEL% in rsvars. Exiting.
@        exit /b %ERRORLEVEL%
    )
msbuild  /p:DCC_UseMSBuildExternally=true
@if !ERRORLEVEL! NEQ 0 (
@       echo Error %ERRORLEVEL% in msbuild. Exiting.
@       exit /b %ERRORLEVEL%
    )
echo build finished