#include <iostream>
#include <tchar.h>
#include "MainMacroUnit.hpp"

#ifdef _WIN64
#pragma link "unit42.o"
#else
#pragma link "unit42.obj"
#endif

int Test(); // defined in unit42

int _tmain(int argc, _TCHAR* argv[])
{
  std::cout << Test() << std::endl;
  return 0;
}
