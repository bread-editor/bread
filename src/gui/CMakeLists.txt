# Include qt
find_package (Qt5Widgets REQUIRED)

# qtbread executable compilation 
file (GLOB srcfiles *.cc)
add_executable (qtbread ${srcfiles})

include_directories (${Qt5Widgets_INCLUDE_DIRS})
target_link_libraries (qtbread Qt5::Widgets)

# Copy test file to output directory
add_custom_command (TARGET qtbread POST_BUILD
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_SOURCE_DIR}/src/gui/Testfile.txt
  $<TARGET_FILE_DIR:qtbread>)