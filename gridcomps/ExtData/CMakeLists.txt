<<<<<<< HEAD
esma_set_this (OVERRIDE MAPL.extdata)

set (srcs
     ExtDataFileStream.F90
     ExtDataRule.F90
     ExtDataDerived.F90
     ExtDataConfig.F90
     ExtDataGridCompNG.F90
     TimeStringConversion.F90
     ExtDataTypeDef.F90
     ExtDataOldTypesCreator.F90
     ExtDataBracket.F90
     ExtDataUpdatePointer.F90
     ExtDataAbstractFileHandler.F90
     ExtDataClimFileHandler.F90
     ExtDataSimpleFileHandler.F90
     ExtDataNode.F90
     ExtDataLgr.F90
     ExtDataConstants.F90
     ExtDataSample.F90
    )


esma_add_library (${this} SRCS ${srcs} DEPENDENCIES MAPL.shared MAPL.base TYPE SHARED)
target_link_libraries (${this} PUBLIC GFTL::gftl GFTL_SHARED::gftl-shared esmf NetCDF::NetCDF_Fortran
                               PRIVATE MPI::MPI_Fortran)
target_include_directories (${this} PUBLIC ${INC_ESMF} ${INC_NETCDF}
          $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)

set_target_properties (${this} PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})
