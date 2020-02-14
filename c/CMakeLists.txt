set(CURL_LIBRARY "-lcurl")
find_package(CURL REQUIRED)

add_library(leoext SHARED lib.c)

include_directories(${CURL_INCLUDE_DIR})
target_link_libraries(leoext ${CURL_LIBRARIES})
set_target_properties(leoext PROPERTIES COMPILE_FLAGS "-m32" LINK_FLAGS "-m32")
