cmake_minimum_required(VERSION 3.15)

# ---- Project settings ----
project(flynt
        VERSION 1.0.0
        DESCRIPTION "programming language perchance?"
        LANGUAGES CXX
)

# ---- Set C++ standard ----
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Grab all .cpp files EXCEPT main.cpp
file(GLOB_RECURSE SRC_FILES CONFIGURE_DEPENDS
    "${PROJECT_SOURCE_DIR}/src/*.cpp"
    "${PROJECT_SOURCE_DIR}/src/*.c"
)

# Remove main.cpp from the list
list(FILTER SRC_FILES EXCLUDE REGEX ".*main\\.cpp$")

add_library(flynt_lib ${SRC_FILES})

target_include_directories(flynt_lib
    PUBLIC
    ${PROJECT_SOURCE_DIR}/include
)

# (optional but good) Apply same warnings
if (MSVC)
    target_compile_options(flynt_lib PRIVATE /W4 /permissive-)
else()
    target_compile_options(flynt_lib PRIVATE -Wall -Wextra -Wpedantic)
endif()

# Create the main executable that links the library
add_executable(${PROJECT_NAME} src/main.cpp)
target_link_libraries(${PROJECT_NAME} PRIVATE flynt_lib)

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
enable_testing()
add_subdirectory(tests)
