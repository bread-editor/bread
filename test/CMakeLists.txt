# Find GTest
enable_testing()
find_package(GTest REQUIRED)

# breadtest executable compilation 
file (GLOB srcfiles *.cc)
file (GLOB _srcfiles */*.cc)
file (GLOB __srcfiles ../src/*/*.cc)
add_executable (breadtest ${srcfiles} ${_srcfiles} ${__srcfiles})

include_directories (${GTEST_INCLUDE_DIRS} ../src)
target_link_libraries (breadtest ${GTEST_BOTH_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT})
add_test (BreadTest breadtest)

# Copy test files to proper directory
add_custom_command (TARGET breadtest POST_BUILD
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_SOURCE_DIR}/test/Testfile.txt
  $<TARGET_FILE_DIR:breadtest>)