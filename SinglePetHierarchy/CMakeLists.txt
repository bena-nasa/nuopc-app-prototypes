set (srcs atm.F90  dyn.F90  single_pet_esmApp.F90  esm.F90  ocn.F90  phy.F90)
ecbuild_add_executable (TARGET single_pet_esmApp.x SOURCES ${srcs})
target_link_libraries (single_pet_esmApp.x PRIVATE error_handling esmf)
target_include_directories(single_pet_esmApp.x PUBLIC $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/error_handling)
