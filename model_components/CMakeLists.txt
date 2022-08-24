esma_set_this (OVERRIDE example_models)
set (srcs dyn.F90 moist.F90 rad.F90 physics.F90 geoslike_physics.F90 moist_phy_med.F90 rad_phy_med.F90)
esma_add_library(${this} SRCS ${srcs})
target_link_libraries(${this} PUBLIC esmf PRIVATE error_handling MAPL_redu)
