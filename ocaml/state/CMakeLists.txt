add_custom_command(

  OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/state.exe

  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/state.ml ${CMAKE_CURRENT_BINARY_DIR}/state.ml
  COMMAND ocamlopt -o state.exe state.ml
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_BINARY_DIR}/state.exe ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/state.exe

  DEPENDS state.ml

  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)

add_custom_target(state_exe ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/state.exe)
