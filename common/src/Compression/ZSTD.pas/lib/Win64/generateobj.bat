set ZSTD=R:\zstd-1.5.6\lib
set TARGET=E:\tms\tms-smartsetup\common\src\Compression\ZSTD.pas\lib\Win64

REM Uncomment to use Visual Studio
REM set COMPILER="C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Tools\MSVC\14.40.33807\bin\Hostx64\x64\cl.exe"
REM set RSVARS="C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Auxiliary\Build\vcvars64.bat" 
REM set OBJ=.obj

REM Uncomment to use BCC
set COMPILER=bcc64x.exe
set RSVARS="C:\Program Files (x86)\Embarcadero\Studio\23.0\bin\rsvars.bat"
set OBJ=.o

REM set OPTIONS=-c -DDYNAMIC_BMI2=1 -DZSTD_LIB_MINIFY=1 -DNDEBUG -DZSTD_LEGACY_SUPPORT=0 -DZSTD_MULTITHREAD
set OPTIONS=-c -DDYNAMIC_BMI2=1 -DNDEBUG -DZSTD_LEGACY_SUPPORT=0 -DZSTD_MULTITHREAD 

REM Default options by clang pass --noexecstack but this isn't supported currently
REM Multithread support is complex in static linking: https://github.com/facebook/zstd/blob/dev/lib/README.md
REM 

call %RSVARS%

del %TARGET%\*%OBJ%

cd /D %ZSTD%\common
del *%OBJ%
%COMPILER% -O1 -fvectorize  %OPTIONS%  *.c
copy *%OBJ% %TARGET%\

cd %ZSTD%\decompress
del *%OBJ%
%COMPILER% -O2 %OPTIONS% *.c
copy *%OBJ% %TARGET%\

cd %ZSTD%\compress
del *%OBJ%
%COMPILER% -fvectorize %OPTIONS% *.c
copy *%OBJ% %TARGET%\
