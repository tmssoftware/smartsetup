#include <iostream>
#include <tchar.h>
#include "MainMacroUnit.hpp"

#ifdef _WIN64
//#pragma link "zstd_ddict.o"  We aren't reading in the cppprojs yet
#pragma link "zstd_common.o"
#else
#pragma link "zstd_common.obj"
#endif

int _tmain(int argc, _TCHAR* argv[])
{

}
