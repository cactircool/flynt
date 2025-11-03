# tests/CMakeLists.txt

cmake_minimum_required(VERSION 3.15) # safe to have this here

# Allow tests to include project headers
include_directories(${PROJECT_SOURCE_DIR}/include)

# ==== Fetch GoogleTest ====
include(FetchContent)

FetchContent_Declare(
  googletest
  # If you want to pin a version, you can use URL or GIT_TAG. This default will use the repo HEAD.
  # Example common pinned URL:
  # URL https://github.com/google/googletest/archive/refs/tags/release-1.14.0.zip
  GIT_REPOSITORY https://github.com/google/googletest.git
  GIT_TAG release-1.17.0  # pin to a stable release; change if you prefer another version
)

# Prevent overriding the parent project's compiler/linker settings
set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)

FetchContent_MakeAvailable(googletest)

# ==== Add test executable ====
file(GLOB TEST_SOURCES CONFIGURE_DEPENDS "${PROJECT_SOURCE_DIR}/tests/*.cpp")
add_executable(unit_tests ${TEST_SOURCES})

target_include_directories(unit_tests PRIVATE ${PROJECT_SOURCE_DIR}/include)
target_link_libraries(unit_tests PRIVATE GTest::gtest_main)

# Match same compiler options as the main target (optional)
if (MSVC)
    target_compile_options(unit_tests PRIVATE /W4 /permissive-)
else()
    target_compile_options(unit_tests PRIVATE -Wall -Wextra -Wpedantic)
endif()

# Ensure tests run in correct directory (optional)
set_target_properties(unit_tests PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}/tests_bin"
)

# Register with CTest
add_test(NAME all_unit_tests COMMAND unit_tests)
