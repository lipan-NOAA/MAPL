esma_set_this (OVERRIDE MAPL.history_tng)

set (srcs
    MAPL_AbstractFieldEntry.F90
    MAPL_FieldRegistryEntry.F90
    MAPL_FieldRegistryEntryMap.F90
    MAPL_FieldRegistry.F90
    MAPL_FieldBundleEntry.F90
    MAPL_FieldBundleEntryMap.F90
    MAPL_FieldBundle.F90
    MAPL_Collection.F90
    MAPL_HistoryConfig.F90
    MAPL_HistoryWriter.F90
    )

esma_add_library (${this} SRCS ${srcs} DEPENDENCIES MAPL.shared MAPL.base MAPL.profiler TYPE SHARED)
target_link_libraries (${this} PUBLIC gftl gftl-shared ${ESMF_LIBRARIES}
                               PRIVATE OpenMP::OpenMP_Fortran MPI::MPI_Fortran)
target_include_directories (${this} PUBLIC ${INC_ESMF} ${INC_NETCDF}
          $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)

set_target_properties (${this} PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})

if (PFUNIT_FOUND)
    add_subdirectory(tests EXCLUDE_FROM_ALL)
endif ()

