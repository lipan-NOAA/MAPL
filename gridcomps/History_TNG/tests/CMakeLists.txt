add_definitions(-DUSE_MPI)

set (TEST_SRCS
    test_MAPL_HistoryConfig.pf
)

add_library (history_tng_extras
  MAPL_Initialize.F90
  )
target_link_libraries (history_tng_extras MAPL.shared MAPL_pFUnit
                                   esmf NetCDF::NetCDF_Fortran)

add_pfunit_ctest(History_TNG_tests.x
        TEST_SOURCES ${TEST_SRCS}
        LINK_LIBRARIES MAPL MAPL.pfio history_tng_extras MAPL_pFUnit MAPL.history_tng
        EXTRA_INITIALIZE Initialize
        EXTRA_USE MAPL_pFUnit_Initialize
        MAX_PES 4
        )


add_dependencies(build-tests history_tng_extras History_TNG_tests.x)

set_tests_properties(History_TNG_tests.x PROPERTIES LABELS "History_TNG")

# configure_file(test_GOCART_Tracers.rc test_GOCART_Tracers.rc COPYONLY)
# configure_file(field_table            field_table            COPYONLY)
# configure_file(GOCART_Tracers.yaml    GOCART_Tracers.yaml    COPYONLY)
# configure_file(NOAA_Config.yaml       NOAA_Config.yaml       COPYONLY)
