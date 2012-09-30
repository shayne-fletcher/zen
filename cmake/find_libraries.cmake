find_library(python python32.lib "C:/Python32/libs")
find_library(boost_python boost_python-vc100-mt-1_51.lib "C:/Boost/lib")
find_library(boost_python_d boost_python-vc100-mt-gd-1_51.lib "C:/Boost/lib")
find_library(boost_serialization boost_serialization-vc100-mt-1_51.lib "C:/Boost/lib")
find_library(boost_serialization_d boost_serialization-vc100-mt-gd-1_51.lib "C:/Boost/lib")

message ("-- Libraries found:
               python= ${python}
               boost_python= ${boost_python}
               boost_python_d= ${boost_python_d}
               boost_serialization= ${boost_serialization}
               boost_serialization_d= ${boost_serialization_d}")
