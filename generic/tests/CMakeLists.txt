set(MODULE_DIRECTORY "${esma_include}/MAPL.generic/tests")

set (test_srcs
    Test_ConcreteComposite.pf
    Test_CompositeComponent.pf
    test_SubGrid.pf
    test_SubField.pf
    test_SubFieldBundle.pf
    test_SubState.pf
  )


add_pfunit_ctest(MAPL.generic.tests
                TEST_SOURCES ${test_srcs}
		OTHER_SOURCES ${SRCS}
                LINK_LIBRARIES MAPL.base MAPL.pfio MAPL.generic MAPL.shared MAPL.pfunit
                EXTRA_INITIALIZE Initialize
                EXTRA_USE MAPL_pFUnit_Initialize
                MAX_PES 8
                )
set_target_properties(MAPL.generic.tests PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIRECTORY})

add_dependencies(build-tests MAPL.generic.tests)


