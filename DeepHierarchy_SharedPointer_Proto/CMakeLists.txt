set (srcs atm.F90  dyn.F90  deep_sharedptr_app.F90  esm.F90  ocn.F90  phy.F90 rad.F90 phyDrv.F90)
ecbuild_add_executable (TARGET deep_sharedptr_app.x SOURCES ${srcs})
target_link_libraries (deep_sharedptr_app.x PRIVATE error_handling esmf)
target_include_directories(deep_sharedptr_app.x PUBLIC $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/error_handling)
