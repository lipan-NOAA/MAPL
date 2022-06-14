macro(run_case CASE)
    string(RANDOM LENGTH 24 tempdir)
    execute_process(
      COMMAND ${CMAKE_COMMAND} -E make_directory ${tempdir}
      COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_CURRENT_LIST_DIR}/test_cases/${CASE} ${tempdir}
      )
    if (EXISTS "${tempdir}/nproc.rc")
      file(READ "${tempdir}/nproc.rc" num_procs)
    else()
      set(num_procs "1")
    endif()
    file(APPEND "${tempdir}/CAP1.rc" "USE_EXTDATA2G: .true.")
    file(APPEND "${tempdir}/CAP2.rc" "USE_EXTDATA2G: .true.")
    execute_process(
      COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} ${num_procs} ${MY_BINARY_DIR}/ExtDataDriver.x
      RESULT_VARIABLE CMD_RESULT
      WORKING_DIRECTORY ${tempdir}
      #COMMAND_ECHO STDOUT
      )
    execute_process(
      COMMAND ${CMAKE_COMMAND} -E rm -rf ${tempdir}
      )
    if(CMD_RESULT)
       message(FATAL_ERROR "Error running ${CASE}")
    endif()
endmacro()
run_case(${TEST_CASE})
