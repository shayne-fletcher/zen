#!/usr/bin/python

#  Copyright (C) Vladimir Prus 2006.
#  Distributed under the Boost Software License, Version 1.0. (See
#  accompanying file LICENSE_1_0.txt or copy at
#  http://www.boost.org/LICENSE_1_0.txt)

#  Regression test: if a library had no explicit sources, but
#  only <source> properties, it was built as it it were searched
#  library, and the specified sources were not compiled.
from BoostBuild import Tester, List

# Create a temporary working directory
t = Tester()

# Create the needed files
t.write("Jamroot", """
lib a : : <source>a.cpp ;
""")
t.write("a.cpp", """
#ifdef _WIN32
__declspec(dllexport)
#endif
void foo() {}

""")

t.run_build_system()
t.expect_addition("bin/$toolset/debug/a.obj")

t.rm("bin")

# Now try with <conditional>
t.write("Jamroot", """
rule test ( properties * )
{
    return <source>a.cpp ;
}
lib a : : <conditional>@test ;
""")

t.run_build_system()
t.expect_addition("bin/$toolset/debug/a.obj")

t.cleanup()
