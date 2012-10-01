add_custom_command(

  OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/fortune.exe

  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/fortune.ml ${CMAKE_CURRENT_BINARY_DIR}/fortune.ml
  COMMAND ocamlopt -o fortune.exe fortune.ml
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_BINARY_DIR}/fortune.exe ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/fortune.exe

  DEPENDS fortune.ml

  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)

add_custom_target(fortune_exe ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/fortune.exe)

add_custom_command(

  OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/strfile.exe

  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/strfile.ml ${CMAKE_CURRENT_BINARY_DIR}/strfile.ml
  COMMAND ocamlopt -o strfile.exe strfile.ml
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_BINARY_DIR}/strfile.exe ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/strfile.exe
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/fortunes ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/fortunes

  DEPENDS strfile.ml fortunes

  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)

add_custom_target(strfile_exe ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/strfile.exe)
