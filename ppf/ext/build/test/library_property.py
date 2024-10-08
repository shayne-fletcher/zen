#!/usr/bin/python

# Copyright 2004 Vladimir Prus 
# Distributed under the Boost Software License, Version 1.0. 
# (See accompanying file LICENSE_1_0.txt or http://www.boost.org/LICENSE_1_0.txt) 

#  Test that the <library> property has no effect on "obj" targets.
#  Previously, it affected all targets, so
#
#     project : requirements <library>foo ;
#     exe a : a.cpp helper ;
#     obj helper : helper.cpp : <optimization>off ;
#
#  caused 'foo' to be built with and without optimization.
from BoostBuild import Tester, List

# Create a temporary working directory
t = Tester()

t.write("Jamfile", """ 
project
   : requirements <library>lib//x
   ;
exe a : a.cpp foo ;
obj foo : foo.cpp : <variant>release ; 
""")

t.write("a.cpp", """
void aux();
int main() { aux(); }
""")

t.write("foo.cpp", """
void gee();
void aux() { gee(); }
""")

t.write("project-root.jam", """ 
""")

t.write("lib/x.cpp", """
void
#if defined(_WIN32)
__declspec(dllexport)
#endif
gee() {}
""")

t.write("lib/Jamfile", """ 
lib x : x.cpp ;
""")

t.write("lib/project-root.jam", """ 
""")


t.run_build_system()
t.expect_addition("bin/$toolset/debug/a.exe")
t.expect_nothing("lib/bin/$toolset/release/x.obj")
t.cleanup()


