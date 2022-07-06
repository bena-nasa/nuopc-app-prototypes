set (srcs atm.F90  dyn.F90  deepApp.F90  esm.F90  ocn.F90  phy.F90 rad.F90 phyDrv.F90)
ecbuild_add_executable (TARGET deepApp.x SOURCES ${srcs})
target_link_libraries (deepApp.x PRIVATE error_handling esmf)
target_include_directories(deepApp.x PUBLIC $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/error_handling)
