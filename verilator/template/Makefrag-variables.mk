base_dir = $(shell pwd)
#########################################################################################
# check environment variable
#########################################################################################
ifndef INSTALLED_VERILATOR
$(error Please set environment variable INSTALLED_VERILATOR.)
endif

MODEL = TestHarness

VERILOG_FILE_PATH = $(base_dir)/$(MODEL).v

$(VERILOG_FILE_PATH):
	cp $(base_dir)/../verilogs/freechips.rocketchip.system.DefaultConfig.v $(base_dir)/$(MODEL).v

PLUSARGS_PATH = $(base_dir)/../verilogs/freechips.rocketchip.system.DefaultConfig.plusArgs

model_dir = $(base_dir)/$(MODEL)
model_mk = $(model_dir)/V$(MODEL).mk
model_header = $(model_dir)/V$(MODEL).h
LIBS = -L$(lib_dir) -L/usr/lib/gcc/x86_64-linux-gnu/11  -lpthread

ifndef OPTLEVEL
CXXFLAGS = -O3
else
CXXFLAGS = $(OPTLEVEL)
endif
CXXFLAGS += -std=c++17 -I$(lib_dir) -I/usr/include/c++/11 -I/usr/include/x86_64-linux-gnu/c++/11 -D__STDC_FORMAT_MACROS
CLANG_FLAGS = -fno-slp-vectorize -fbracket-depth=4096
CXXFLAGS += $(CLANG_FLAGS)

sim_vsrcs = \
	$(VERILOG_FILE_PATH) \
	$(base_dir)/vsrc/AsyncResetReg.v \
	$(base_dir)/vsrc/EICG_wrapper.v \
	$(base_dir)/vsrc/plusarg_reader.v \

sim_csrcs = \
	$(base_dir)/csrc/SimDTM.cc \
	$(base_dir)/csrc/SimJTAG.cc \
	$(base_dir)/csrc/remote_bitbang.cc \
	$(base_dir)/csrc/emulator_verilator.cc \


emulator_verilator = $(base_dir)/emulator_verilator$(OPTLEVEL)

# Run Verilator to produce a fast binary to emulate this circuit.
VERILATOR := $(INSTALLED_VERILATOR) --cc --exe
VERILATOR_FLAGS := --top-module $(MODEL) \
  +define+PRINTF_COND=\$$c\(\"verbose\",\"\&\&\"\,\"done_reset\"\) \
  +define+STOP_COND=\$$c\(\"done_reset\"\) --assert \
  --output-split 20000 \
  -Wno-fatal \
  -stats \
  --threads 1 \
  -Wno-STMTDLY --x-assign unique \
  -I$(base_dir)/vsrc -I$(base_dir)/rocket-chip/src/main/resources/vsrc \
  -O3 -CFLAGS "$(CXXFLAGS) -DVERILATOR -DTEST_HARNESS=V$(MODEL) -include $(base_dir)/csrc/verilator.h -include $(PLUSARGS_PATH)" \

