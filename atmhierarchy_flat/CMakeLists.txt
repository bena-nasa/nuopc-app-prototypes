set (srcs atm.F90  dyn.F90  atm_hierarchy_flat.F90  moist.F90 rad.F90)
ecbuild_add_executable (TARGET atm_hierarchy_flat.x SOURCES ${srcs})
target_link_libraries (atm_hierarchy_flat.x PRIVATE error_handling esmf)
target_include_directories(atm_hierarchy_flat.x PUBLIC $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/error_handling)
