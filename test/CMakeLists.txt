# Find GTest
enable_testing()
find_package(GTest REQUIRED)

# breadtest executable compilation 
file (GLOB srcfiles *.cc)
add_executable (breadtest ${srcfiles})

include_directories (${GTEST_INCLUDE_DIRS})
target_link_libraries (breadtest ${GTEST_BOTH_LIBRARIES})
add_test (BreadTest breadtest)