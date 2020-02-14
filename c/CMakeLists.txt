set(CURL_LIBRARY "-lcurl")
find_package(CURL REQUIRED)

add_library(leoext SHARED lib.c)

include_directories(${CURL_INCLUDE_DIR})
target_link_libraries(leoext ${CURL_LIBRARIES})

set(CMAKE_C_COMPILER gcc)
set(CMAKE_C_FLAGS "-m32 -L/usr/lib32")

set(CMAKE_SYSTEM_NAME Linux)

set(CMAKE_C_COMPILER gcc)
set(CMAKE_C_FLAGS -m32)

set(CMAKE_FIND_ROOT_PATH /usr/lib32)

set(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)
