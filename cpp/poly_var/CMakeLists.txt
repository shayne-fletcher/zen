#add_definitions("/EP")
add_library(poly_var SHARED config.h poly_var.h poly_var.cpp)
set_target_properties(poly_var 
  PROPERTIES 
    DEBUG_OUTPUT_NAME poly_var-d 
    COMPILE_DEFINITIONS POLY_VAR_SOURCE=1
)

add_executable(poly_var_test_c poly_var_test_c.c)
target_link_libraries(poly_var_test_c poly_var)
set_target_properties(poly_var_test_c PROPERTIES DEBUG_OUTPUT_NAME poly_var_test_c-d)

add_executable(poly_var_test_cpp poly_var_test_cpp.cpp)
target_link_libraries(poly_var_test_cpp poly_var)
set_target_properties(poly_var_test_cpp PROPERTIES DEBUG_OUTPUT_NAME poly_var_test_cpp-d)

add_executable(poly_var_parse poly_var_parse.cpp)
target_link_libraries(poly_var_parse poly_var)
set_target_properties(poly_var_parse PROPERTIES DEBUG_OUTPUT_NAME poly_var_parse-d)

add_library(_poly_var SHARED poly_var_module.cpp)
target_link_libraries(_poly_var 
  poly_var 
  debug ${boost_python_d} optimized ${boost_python} 
  ${python}
)
set_target_properties(_poly_var PROPERTIES PREFIX "" SUFFIX ".pyd")

configure_file("./__init__.py" ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/poly_var/__init__.py COPYONLY)
configure_file("./poly_var_test.py" ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/poly_var_test.py COPYONLY)

add_executable(poly_var_serialization_test poly_var_serialization_test.cpp)
target_link_libraries(poly_var_serialization_test poly_var debug ${boost_serialization_d} optimized ${boost_serialization})
set_target_properties(poly_var_serialization_test PROPERTIES DEBUG_OUTPUT_NAME poly_var_serialization_test_cpp-d)
