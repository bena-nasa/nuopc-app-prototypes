esma_set_this (OVERRIDE example_models)
set (srcs dyn.F90 moist.F90 rad.F90)
esma_add_library(${this} SRCS ${srcs})
target_link_libraries(${this} PUBLIC esmf PRIVATE error_handling MAPL_redu)
