set (srcs atm.F90  dyn.F90  esmApp.F90  esm.F90  ocn.F90  phy.F90)
ecbuild_add_executable (TARGET esmApp.x SOURCES ${srcs})
target_link_libraries (esmApp.x PRIVATE error_handling esmf)
target_include_directories(esmApp.x PUBLIC $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/error_handling)
