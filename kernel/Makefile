base_dir=$(abspath .)
log_dir=$(base_dir)/logs
PREFIX=$(base_dir)/build
JSONPATH=$(base_dir)/json/
TXTPATH=$(base_dir)/txt/
YAMLPATH=$(base_dir)/yaml/
bin_dir=$(base_dir)/bin
CXX := clang++
AR := ar
CXXFLAGS := -std=c++17 -O3 -Wall -Wno-unused-function -Wno-unused-parameter -fPIC -funroll-loops -DPREFIX=\"$(PREFIX)\" -DJSONPATH=\"$(JSONPATH)\" -DTXTPATH=\"$(TXTPATH)\" -DYAMLPATH=\"$(YAMLPATH)\"
# CXXFLAGS += -g
# CXXFLAGS += -g -fsanitize=address
CXXFLAGS += -I/usr/include/c++/11 -I/usr/include/x86_64-linux-gnu/c++/11
LDFLAGS := -lyaml-cpp -lpthread
LDFLAGS += -L/usr/lib/gcc/x86_64-linux-gnu/11

include Makefile-gen.mk

OBJ_DIR := build

SRCS := \
  elfloader.cc \
  htif.cc \
  memif.cc \
  dtm.cc \
  syscall.cc \
  device.cc \
  rfb.cc \
  context.cc \
  htif_pthread.cc \
  htif_hexwriter.cc \
  dummy.cc \
  option_parser.cc \
  term.cc \
  tsi.cc \

OBJS := $(SRCS:%.cc=$(OBJ_DIR)/%.o)
OBJS_DEBUG := $(SRCS_DEBUG:%.cc=$(OBJ_DEBUG_DIR)/%.o)

LIB := libfesvr.a

KERNEL_BINS := $(addprefix kernel_,$(KERNELS))
KERNEL_OBJS := $(addprefix $(OBJ_DIR)/kernel_,$(addsuffix .o,$(KERNELS)))

.PHONY: clean

kernel_%: kernel_%.cc $(LIB) $(log_dir) $(bin_dir)
	@echo "Compile $@"
	/usr/bin/time $(CXX) $(CXXFLAGS) -o $(bin_dir)/$@ $< $(LIB) $(LDFLAGS) > $(log_dir)/compile_$@.log 2>&1

$(LIB): $(OBJS)
	@echo "Archiving $@"
	$(AR) rcs $@ $(OBJS)

$(OBJ_DIR)/%.o: %.cc | $(OBJ_DIR)
	@echo "Compiling $<"
	$(CXX) $(CXXFLAGS) -c $< -o $@

preprocess_tensor: preprocess_tensor.cc $(bin_dir)
	@echo "Compiling preprocess_tensor"
	$(CXX) $(CXXFLAGS) -o $(bin_dir)/$@ $< $(LDFLAGS)

$(OBJ_DIR):
	mkdir -p $(OBJ_DIR)

$(OBJ_DEBUG_DIR):
	mkdir -p $(OBJ_DEBUG_DIR)

$(log_dir):
	mkdir -p $(log_dir)

$(bin_dir):
	mkdir -p $(bin_dir)

clean:
	rm -rf $(OBJ_DIR) $(LIB) $(TARGETS) $(bin_dir)