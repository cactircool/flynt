cmake_minimum_required(VERSION 3.15)

# ---- Project settings ----
project(flynt
        VERSION 1.0.0
        DESCRIPTION "programming language perchance?"
        LANGUAGES CXX
)

# ---- Set C++ standard ----
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# ---- Define target ----
file(GLOB_RECURSE SRC_FILES CONFIGURE_DEPENDS
        "${PROJECT_SOURCE_DIR}/src/*.cpp"
)

add_executable(${PROJECT_NAME} ${SRC_FILES})

# ---- Include directories ----
target_include_directories(${PROJECT_NAME}
        PRIVATE
        ${PROJECT_SOURCE_DIR}/include
)

# ---- Compiler warnings (optional but recommended) ----
if (MSVC)
    target_compile_options(${PROJECT_NAME} PRIVATE /W4 /permissive-)
else()
    target_compile_options(${PROJECT_NAME} PRIVATE -Wall -Wextra -Wpedantic)
endif()

# ---- Output directories ----
set_target_properties(${PROJECT_NAME} PROPERTIES
        RUNTIME_OUTPUT_DIRECTORY "${PROJECT_SOURCE_DIR}/build"
)

# ---- Optional: Add install rules ----
install(TARGETS ${PROJECT_NAME} DESTINATION bin)
install(DIRECTORY ${PROJECT_SOURCE_DIR}/include/ DESTINATION include)

# ---- Optional: Add tests ----
# enable_testing()
# add_subdirectory(tests)