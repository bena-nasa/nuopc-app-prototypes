set (srcs atm.F90 atm_hierarchy.F90 phy.F90)
ecbuild_add_executable (TARGET atm_hierarchy.x SOURCES ${srcs})
target_link_libraries (atm_hierarchy.x PRIVATE error_handling esmf MAPL_redu example_models)
target_include_directories(atm_hierarchy.x PUBLIC $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/error_handling)
