#if !defined(CONFIG_9ABBA8AE_3D5D_4EB3_B684_F3040EC84044_H)
#  define CONFIG_9ABBA8AE_3D5D_4EB3_B684_F3040EC84044_H

#  if defined(_WIN32)
#    if defined(POLY_VAR_SOURCE)
#      define POLY_VAR_DECL __declspec(dllexport)
#    else
#      define POLY_VAR_DECL __declspec(dllimport)
#    endif//defined(POLY_VAR_SOURCE)
#  endif//defined(_WIN32)

#if !defined(POLY_VAR_DECL)
#  define POLY_VAR_DECL
#endif //!defined(POLY_VAR_DECL)

#endif //!defined(CONFIG_9ABBA8AE_3D5D_4EB3_B684_F3040EC84044_H)
