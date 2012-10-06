add_custom_command(

  OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/calc.exe

  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/lexer.mll ${CMAKE_CURRENT_BINARY_DIR}/lexer.mll
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/parser.mly ${CMAKE_CURRENT_BINARY_DIR}/parser.mly
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/calc.ml ${CMAKE_CURRENT_BINARY_DIR}/calc.ml

  COMMAND ocamllex lexer.mll
  COMMAND ocamlyacc parser.mly
  COMMAND ocamlc -c parser.mli
  COMMAND ocamlc -c lexer.ml
  COMMAND ocamlc -c parser.ml
  COMMAND ocamlc -c calc.ml
  COMMAND ocamlc -o calc.exe lexer.cmo parser.cmo calc.cmo

  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_BINARY_DIR}/calc.exe ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/calc.exe

  DEPENDS lexer.mll parser.mly calc.ml

  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)

add_custom_target(calc ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/calc.exe)
