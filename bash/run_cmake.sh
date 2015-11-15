CMAKE_PATH="`cygpath -p 'C:\Program Files (x86)\CMake\bin\'`"
PATH="$CMAKE_PATH:$PATH"; export PATH

GTEST_ROOT="C:/project/qfd/qfd_external.git/libs/qfd_infra/1.0.0"; export GTEST_ROOT

rm -rf CMake*
cmake -G "Visual Studio 14 2015 Win64" -DCMAKE_INSTALL_PREFIX:PATH=. -DVERBOSE=1 -DBUILD_DOCUMENTATION=1 .. 
