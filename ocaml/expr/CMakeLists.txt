add_custom_command(

  OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/expr.exe

  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/expr.ml ${CMAKE_CURRENT_BINARY_DIR}/expr.ml
  COMMAND ocamlopt -pp "camlp4o pa_extend.cmo" -I +camlp4 -o expr.exe expr.ml
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_BINARY_DIR}/expr.exe ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/expr.exe

  DEPENDS expr.ml

  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)

add_custom_target(expr_exe ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/expr.exe)

add_custom_command(

  OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/eval.exe

  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/eval.ml ${CMAKE_CURRENT_BINARY_DIR}/eval.ml
  COMMAND ocamlopt -pp "camlp4o pa_extend.cmo" -I +camlp4 -o eval.exe eval.ml
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_BINARY_DIR}/eval.exe ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/eval.exe

  DEPENDS eval.ml

  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)

add_custom_target(eval_exe ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/eval.exe)

add_custom_command(

  OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/expr2.exe

  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/expr2.ml ${CMAKE_CURRENT_BINARY_DIR}/expr2.ml
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/expr2_lexer.mll ${CMAKE_CURRENT_BINARY_DIR}/expr2_lexer.mll
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/expr2_parser.mly ${CMAKE_CURRENT_BINARY_DIR}/expr2_parser.mly
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/expr2_test.ml ${CMAKE_CURRENT_BINARY_DIR}/expr2_test.ml

  COMMAND ocamllex expr2_lexer.mll
  COMMAND ocamlyacc expr2_parser.mly
  COMMAND ocamlopt -o expr2.exe expr2.ml expr2_parser.mli expr2_lexer.ml expr2_parser.ml expr2_test.ml

  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_BINARY_DIR}/expr2.exe ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/expr2.exe

  DEPENDS expr2_lexer.mll expr2_parser.mly expr2.ml expr2_test.ml

  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)

add_custom_target(expr2_exe ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/expr2.exe)
