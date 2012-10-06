message("Build dir is ${CMAKE_BINARY_DIR}")

add_custom_command(

  OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/ocaml_poly_var.exe

  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/lexer.mll ${CMAKE_CURRENT_BINARY_DIR}/lexer.mll
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/parser.mly ${CMAKE_CURRENT_BINARY_DIR}/parser.mly
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/poly_var.mli ${CMAKE_CURRENT_BINARY_DIR}/poly_var.mli
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/poly_var.ml ${CMAKE_CURRENT_BINARY_DIR}/poly_var.ml
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/main.ml ${CMAKE_CURRENT_BINARY_DIR}/main.ml

  COMMAND ocamllex lexer.mll
  COMMAND ocamlyacc parser.mly
  COMMAND ocamlc -c poly_var.mli parser.mli lexer.ml parser.ml poly_var.ml main.ml

  COMMAND ocamlc -o ocaml_poly_var.exe lexer.cmo parser.cmo parser.cmo poly_var.cmo main.cmo

  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_BINARY_DIR}/ocaml_poly_var.exe ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/ocaml_poly_var.exe

  DEPENDS lexer.mll parser.mly poly_var.mli poly_var.ml main.ml

  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)

add_custom_target(ocaml_poly_var ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/ocaml_poly_var.exe)
