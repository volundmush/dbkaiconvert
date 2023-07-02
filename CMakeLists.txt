cmake_minimum_required(VERSION 3.22)

set(CPM_DOWNLOAD_VERSION 0.35.5)
set(CPM_DOWNLOAD_LOCATION "${CMAKE_BINARY_DIR}/cmake/CPM_${CPM_DOWNLOAD_VERSION}.cmake")

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

if(NOT (EXISTS ${CPM_DOWNLOAD_LOCATION}))
    message(STATUS "Downloading CPM.cmake")
    file(DOWNLOAD https://github.com/TheLartians/CPM.cmake/releases/download/v${CPM_DOWNLOAD_VERSION}/CPM.cmake ${CPM_DOWNLOAD_LOCATION})
endif()

include(${CPM_DOWNLOAD_LOCATION})

project(dbkaiconvert)

set(CMAKE_CXX_STANDARD 23)

add_subdirectory(../dbatk dbatk_build)
add_subdirectory(../dbat dbat_build)
find_library(LIBBSD NAMES bsd libbsd REQUIRED)
add_compile_definitions(HAVE_LIBBSD)

add_executable(dbkaiconvert main.cpp)
target_link_libraries(dbkaiconvert circlemud dbatk ${CIRCLE_LINK_LIBRARIES} ${DBATK_LINK_LIBRARIES})
target_include_directories(dbkaiconvert PUBLIC ${CIRCLE_INCLUDE_DIRS} ${DBATK_INCLUDE_DIRS})