#Current binary directory is C:/project/github/build/zen/debug/ocaml/embed

add_custom_command(
  OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/libmodwrap.lib

  # Use the byte-dll configuration from the thread,
  # http://caml.inria.fr/mantis/print_bug_page.php?bug_id=4847

  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/mod.ml ${CMAKE_CURRENT_BINARY_DIR}/mod.ml
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/modwrap.c ${CMAKE_CURRENT_BINARY_DIR}/modwrap.c
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/modwrap.def ${CMAKE_CURRENT_BINARY_DIR}/modwrap.def

  COMMAND ocamlc -c modwrap.c
  COMMAND ocamlc -cclib \"-implib -link /def:modwrap.def -link \" -output-obj -o ${CMAKE_CURRENT_BINARY_DIR}/libmodwrap.dll modwrap.obj mod.ml

  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_BINARY_DIR}/libmodwrap.dll ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/libmodwrap.dll

  DEPENDS mod.ml modwrap.c modwrap.def

  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)

add_custom_target(libmodwrap DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/libmodwrap.lib)

add_executable(main main.c)
add_dependencies(main libmodwrap)
target_link_libraries(main "${CMAKE_CURRENT_BINARY_DIR}/libmodwrap.lib")
