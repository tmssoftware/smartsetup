// cSpell:disable

# Generate O files using MSYS2

The precompiled .o files that come with zstd releases are compiled with MSYS2, and we will use the same.
We could un7zip **libzstd_static.lib** and get the ".o" files from github releases directly if it wasn't because they build it non-multithreaded.

If in the future **libzstd_static.lib** comes multithreaded, it is just using 7zip to get the .o files.
 
## Fixing the build
For multithreaded builds, zstd uses some symbols declared with declspec(dllimport)
Those are an optimization so they link directly to the import lib for kernel32.dll (kernel32.lib),
and generate calls to for example __impl__beginthreadex instead of _beginthreadex. 
But as we are compiling directly without import lib, those __impl_ symbols cause it to crash. 
(and if we define them in our sources they will crash still, as they won't be in the same address as they are in kernel32.lib)

So we need to disable them.
1. Download zstd-version.tar.zst from https://github.com/facebook/zstd/releases/  
2. Extract it at `R:\zstd\src`
3. Edit `R:\zstd\src\build\cmake\CMakeLists.txt`
4. Search for
```cmake
# Always hide XXHash symbols
add_definitions(-DXXH_NAMESPACE=ZSTD_)
```
and add
```cmake
add_definitions(-D_KERNEL32_=" ")
add_definitions(-D_CRTIMP= " ")
add_definitions(-DDYNAMIC_BMI2=1) #allows optimized huf_decompress_amd64.o 
```
This should remove the dllimport stuff, and avoid __impl_ symbols.

## Setting up the environment.


 ```shell
 winget install --id=MSYS2.MSYS2  -e
 ```

launch msys2 ucrt64 from start menu. See https://www.msys2.org/docs/environments/


```shell
pacman -S --needed base-devel mingw-w64-ucrt-x86_64-toolchain
pacman -S --needed cmake
cd /r/zstd/src/build/cmake/
mkdir release-build
cd release-build
cmake -LH ..
cmake ..
make
```

check after cmake .. that it says:
```
-- Setting build type to 'Release' as none was specified.
-- ZSTD_LEGACY_SUPPORT defined!
-- ZSTD_MULTITHREAD_SUPPORT is enabled
```

## Getting the .o files
In windows explorer, go to R:\zstd\src\build\cmake\release-build\lib and open libzstd.a with 7zip
Delete the files in smartsetup\common\src\Compression\ZSTD.pas\lib\Win64 and copy the new ones.
open powershell
```shell
cd smartsetup\common\src\Compression\ZSTD.pas\lib\Win64
Get-ChildItem *.c.o | Rename-Item -NewName { $_.Name -replace '.c.o','.o' }
```