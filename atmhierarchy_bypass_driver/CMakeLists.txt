set (srcs atm.F90 atm_hierarchy_custom_run.F90)
ecbuild_add_executable (TARGET atm_hierarchy_custom_run.x SOURCES ${srcs})
target_link_libraries (atm_hierarchy_custom_run.x PRIVATE error_handling esmf MAPL_redu example_models)
target_include_directories(atm_hierarchy_custom_run.x PUBLIC $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/error_handling)
