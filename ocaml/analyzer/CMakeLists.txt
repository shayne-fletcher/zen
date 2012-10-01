add_custom_command(

  OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/analyzer.exe

  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/analyzer.ml ${CMAKE_CURRENT_BINARY_DIR}/analyzer.ml
  COMMAND ocamlopt -o analyzer.exe analyzer.ml
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_BINARY_DIR}/analyzer.exe ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/analyzer.exe

  DEPENDS analyzer.ml

  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)

add_custom_target(analyzer_exe ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/analyzer.exe)
