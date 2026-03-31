include Makefrag-JVMflags.mk
include Makefrag-source-files.mk


#########################################################################################
# check environment variable
#########################################################################################
ifndef ESSENT_JAR
$(error Please set environment variable ESSENT_JAR.)
endif

ESSENT_INCLUDES = -I$(lib_dir) -I/usr/include/c++/11 -I/usr/include/x86_64-linux-gnu/c++/11
LIBS = -L$(lib_dir) -L/usr/lib/gcc/x86_64-linux-gnu/11 -lpthread

CXXFLAGS = -O3 -std=c++17 -D__STDC_FORMAT_MACROS
CXXFLAGSO1 = -O1 -std=c++17 -D__STDC_FORMAT_MACROS
CXXFLAGSO0 = -O0 -std=c++17 -D__STDC_FORMAT_MACROS -fno-global-isel
CLANG_FLAGS = -fno-slp-vectorize -fbracket-depth=4096
CXXFLAGS += $(CLANG_FLAGS)
CXXFLAGSO1 += $(CLANG_FLAGS)
CXXFLAGSO0 += $(CLANG_FLAGS)
