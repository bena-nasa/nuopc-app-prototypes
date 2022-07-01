cmake_minimum_required (VERSION 3.17)
cmake_policy (SET CMP0053 NEW)
cmake_policy (SET CMP0054 NEW)

project (
  NUOPC_EXAMPLES
  VERSION 1.0.0
  LANGUAGES Fortran CXX C)  # Note - CXX is required for ESMF

# Set the default build type to release
if (NOT CMAKE_BUILD_TYPE)
  message (STATUS "Setting build type to 'Release' as none was specified.")
  set (CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build." FORCE)
  # Set the possible values of build type for cmake-gui
  set_property (CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS
    "Debug" "Release" "Aggressive")
endif ()

# mepo can now clone subrepos in three styles
set (ESMA_CMAKE_DIRS
  ESMA_cmake
  @ESMA_cmake
  ESMA_cmake@
  )

foreach (dir IN LISTS ESMA_CMAKE_DIRS)
  if (EXISTS ${CMAKE_CURRENT_LIST_DIR}/${dir})
    list (APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/${dir}")
    include (esma)
    set(NUOPC_EXAMPLE_STANDALONE TRUE)
  endif ()
endforeach ()

# build as standalone project
if (NOT COMMAND esma)
  include (esma OPTIONAL)
endif ()

ecbuild_declare_project()

add_subdirectory(HierarchyProto)
add_subdirectory(error_handling)
