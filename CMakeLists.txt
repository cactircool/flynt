cmake_minimum_required(VERSION 3.16)

project(flynt LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

include(FetchContent)
FetchContent_Declare(
  googletest
  GIT_REPOSITORY https://github.com/google/googletest.git
  GIT_TAG        90a41521142c978131f38c6da07b4eb96a9f1ff6
)
FetchContent_MakeAvailable(googletest)

file(GLOB_RECURSE HEADERS "${PROJECT_SOURCE_DIR}/include/*.h" "${PROJECT_SOURCE_DIR}/include/*.hpp")
file(GLOB_RECURSE SOURCES
  "${PROJECT_SOURCE_DIR}/src/*.c"
  "${PROJECT_SOURCE_DIR}/src/*.cpp"
  "${PROJECT_SOURCE_DIR}/test/*.c"
  "${PROJECT_SOURCE_DIR}/test/*.cpp"
)

add_executable(flynt
    ${HEADERS}
    ${SOURCES}
)

target_link_libraries(flynt gtest_main gtest)
include(GoogleTest)

include(GNUInstallDirs)
install(TARGETS flynt
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)
