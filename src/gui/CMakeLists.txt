# Versioning
set (rye_version_major 0)
set (rye_version_minor 1)
set (rye_version_patch 0)
set (rye_version ${rye_version_major}.${rye_version_minor}.${rye_version_patch})

# Include qt
find_package (Qt5Widgets REQUIRED)

# rye executable compilation 
file (GLOB srcfiles *.cc)
add_executable (rye ${srcfiles})

include_directories (${Qt5Widgets_INCLUDE_DIRS})
target_link_libraries (rye Qt5::Widgets)

# Copy test file to output directory
add_custom_command (TARGET rye POST_BUILD
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_SOURCE_DIR}/src/gui/Testfile.txt
  $<TARGET_FILE_DIR:rye>)