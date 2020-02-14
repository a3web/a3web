add_library(leoext SHARED lib.c)

set_target_properties(leoext PROPERTIES COMPILE_FLAGS "-m32" LINK_FLAGS "-m32")
