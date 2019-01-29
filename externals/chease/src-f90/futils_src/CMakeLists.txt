project(futils)

set(SRCS
  futils.f90
  buffer.f90 
  vis3d.f90
  cutils.c
)

set(CMAKE_Fortran_MODULE_DIRECTORY 
  ${CMAKE_CURRENT_BINARY_DIR}/modules)

set(MODS ${CMAKE_CURRENT_BINARY_DIR}/modules PARENT_SCOPE)

add_library(futils STATIC ${SRCS})

install(TARGETS futils
  DESTINATION lib
  EXPORT futils-targets
  )
install(EXPORT futils-targets
  DESTINATION lib/cmake
  )

install(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/modules/ DESTINATION include)

include_directories(${MODS} ${EXTRA_INCS})

