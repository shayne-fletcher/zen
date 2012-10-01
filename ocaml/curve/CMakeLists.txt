add_custom_command(

  OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/curve.exe

  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/roots_sig.mli ${CMAKE_CURRENT_BINARY_DIR}/roots_sig.mli
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/roots.mli ${CMAKE_CURRENT_BINARY_DIR}/roots.mli
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/roots.ml ${CMAKE_CURRENT_BINARY_DIR}/roots.ml
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/dates_sig.mli ${CMAKE_CURRENT_BINARY_DIR}/dates_sig.mli
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/dates.mli ${CMAKE_CURRENT_BINARY_DIR}/dates.mli
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/dates.ml ${CMAKE_CURRENT_BINARY_DIR}/dates.ml
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/flows_sig.mli ${CMAKE_CURRENT_BINARY_DIR}/flows_sig.mli
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/flows.mli ${CMAKE_CURRENT_BINARY_DIR}/flows.mli
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/flows.ml ${CMAKE_CURRENT_BINARY_DIR}/flows.ml
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/interpolation_sig.mli ${CMAKE_CURRENT_BINARY_DIR}/interpolation_sig.mli
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/interpolation.mli ${CMAKE_CURRENT_BINARY_DIR}/interpolation.mli
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/interpolation.ml ${CMAKE_CURRENT_BINARY_DIR}/interpolation.ml
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/deals_sig.mli ${CMAKE_CURRENT_BINARY_DIR}/deals_sig.mli
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/deals.mli ${CMAKE_CURRENT_BINARY_DIR}/deals.mli
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/deals.ml ${CMAKE_CURRENT_BINARY_DIR}/deals.ml
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/curves_sig.mli ${CMAKE_CURRENT_BINARY_DIR}/curves_sig.mli
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/curves.mli ${CMAKE_CURRENT_BINARY_DIR}/curves.mli
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/curves.ml ${CMAKE_CURRENT_BINARY_DIR}/curves.ml
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/curve_sig.mli ${CMAKE_CURRENT_BINARY_DIR}/curve_sig.mli
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/curve.mli ${CMAKE_CURRENT_BINARY_DIR}/curve.mli
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/main.ml ${CMAKE_CURRENT_BINARY_DIR}/main.ml

  COMMAND ocamlopt -pp "camlp4o pa_extend.cmo" 
          -I +camlp4 -I c:/ocamlms/lib/site-lib/calendar 
          str.cmxa unix.cmxa calendarLib.cmx 
          -o curve.exe 
          roots_sig.mli roots.mli roots.ml 
          dates_sig.mli dates.mli dates.ml 
          flows_sig.mli flows.mli flows.ml 
          interpolation_sig.mli interpolation.mli interpolation.ml 
          deals_sig.mli deals.mli deals.ml 
          curves_sig.mli curves.mli curves.ml curve_sig.mli 
          curve.mli 
          main.ml

  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_BINARY_DIR}/curve.exe ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/curve.exe

  DEPENDS 
    roots_sig.mli roots.mli roots.ml 
    dates_sig.mli dates.mli dates.ml 
    flows_sig.mli flows.mli flows.ml 
    interpolation_sig.mli interpolation.mli interpolation.ml 
    deals_sig.mli deals.mli deals.ml 
    curves_sig.mli curves.mli curves.ml 
    curve.mli
    main.ml

  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)

add_custom_target(curve_exe ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/curve.exe)
