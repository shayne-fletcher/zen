add_custom_command(

  OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/recognizer.exe

  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/recognizer.ml ${CMAKE_CURRENT_BINARY_DIR}/recognizer.ml
  COMMAND ocamlopt -o recognizer.exe recognizer.ml
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_BINARY_DIR}/recognizer.exe ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/recognizer.exe

  DEPENDS recognizer.ml

  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)

add_custom_target(recognizer_exe ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/recognizer.exe)
