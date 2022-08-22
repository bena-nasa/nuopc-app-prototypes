esma_set_this (OVERRIDE MAPL_redu)
set (srcs MAPL_redu.F90)
esma_add_library(${this} SRCS ${srcs})
target_link_libraries(${this} PUBLIC esmf PRIVATE error_handling)
