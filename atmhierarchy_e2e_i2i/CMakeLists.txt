set (srcs atm.F90 atm_hierarchy_e2e_i2i.F90)
ecbuild_add_executable (TARGET atm_hierarchy_e2e_i2i.x SOURCES ${srcs})
target_link_libraries (atm_hierarchy_e2e_i2i.x PRIVATE error_handling esmf MAPL_redu example_models)
target_include_directories(atm_hierarchy_e2e_i2i.x PUBLIC $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/error_handling)
