cmake_minimum_required(VERSION 2.8)

if(DEFINED ENV{ZEN_RUNTIME_OUTPUT_DIRECTORY})
  set (CMAKE_RUNTIME_OUTPUT_DIRECTORY $ENV{ZEN_RUNTIME_OUTPUT_DIRECTORY})
endif()
if(DEFINED ENV{ZEN_RUNTIME_ARCHIVE_DIRECTORY})
  set (CMAKE_RUNTIME_ARCHIVE_DIRECTORY $ENV{ZEN_RUNTIME_ARCHIVE_DIRECTORY})
endif()
if(DEFINED ENV{ZEN_RUNTIME_LIBRARY_DIRECTORY})
  set (CMAKE_RUNTIME_LIBRARY_DIRECTORY $ENV{ZEN_RUNTIME_LIBRARY_DIRECTORY})
endif()

project(zen)

include_directories ("c:/project/github/zen/")
include_directories ("c:/python32/include/")
include_directories ("c:/boost/include/boost-1_51_0/")

include("${CMAKE_CURRENT_LIST_DIR}/cmake/find_libraries.cmake")
include("${CMAKE_CURRENT_LIST_DIR}/cmake/install_libraries.cmake")

add_definitions(-DBOOST_ALL_NO_LIB=1 -D_CRT_SECURE_NO_WARNINGS=1)
add_definitions(-DBOOST_ALL_NO_LIB=1 -D_SCL_SECURE_NO_WARNINGS=1)

add_subdirectory(cpp/poly_var)
