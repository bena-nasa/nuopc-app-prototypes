esma_set_this (OVERRIDE error_handling)
set (srcs error_handling.F90)
esma_add_library(${this} SRCS ${srcs})
target_link_libraries(${this} PUBLIC esmf)

set (ERROR_HEADERS error_handling.h)
file(COPY ${ERROR_HEADERS} DESTINATION ${esma_include})
