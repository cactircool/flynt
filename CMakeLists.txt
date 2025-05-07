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

add_executable(flynt main.cpp
    token.h token.cpp
    lexer.h lexer.cpp
    slidingwindow.h
    preprocessor.h
    test/token.test.cpp
    test/slidingwindow.test.cpp
    test/lexer.test.cpp
    binarytree.h
    test/binarytree.test.cpp
)

target_link_libraries(flynt gtest_main gtest)
include(GoogleTest)

include(GNUInstallDirs)
install(TARGETS flynt
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)
