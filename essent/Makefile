base_dir=$(abspath .)
essent_dir=$(base_dir)/essent
lib_dir=$(base_dir)/../kernel
CXX = clang++
LINK = clang++
AR = ar

# Designs to compile
DESIGNS = \
	rocket21-8c \
	boom21-8small \
	boom21-8large \
	boom21-8mega \
	rocket21-6c \
	boom21-6small \
	boom21-6large \
	boom21-6mega \
	rocket21-4c \
	boom21-4small \
	boom21-4large \
	boom21-4mega \
	rocket21-2c \
	boom21-2small \
	boom21-2large \
	boom21-2mega \
	boom21-small \
	boom21-large \
	boom21-mega \
	rocket21-1c \
	rocket21-12c \
	rocket21-16c \
	rocket21-20c \
	boom21-12small \
	gemmini-8 \
	gemmini-16 \
	gemmini-32 \
	sha3-1 \
	sha3-2 \
	sha3-4 \

# Compile essent
ESSENT_JAR = $(essent_dir)/utils/bin/essent.jar
ESSENT_SOURCE = $(essent_dir)/src/main/scala

essent_jar: $(ESSENT_JAR)

$(ESSENT_JAR):
	cd $(essent_dir) && sbt assembly

MAKE_VARIABLES = ESSENT_JAR=$(ESSENT_JAR) CXX=$(CXX) LINK=$(LINK) AR=$(AR) lib_dir=$(lib_dir)

design = $(firstword $(subst _, ,$*))

# ESSENT compile
COMPILE_EMULATOR_ESSENT_TARGETS := $(foreach i,$(DESIGNS),compile_essent_$i)

${COMPILE_EMULATOR_ESSENT_TARGETS}: compile_essent_%:
	$(MAKE) -C $(design) -f Makefile-emulator.mk $(MAKE_VARIABLES) emulator_essent

CLEAN_ESSENT_TARGETS := $(foreach j,$(DESIGNS),clean_essent_$j)

${CLEAN_ESSENT_TARGETS}: clean_essent_%:
	$(MAKE) -C $(design) -f Makefile-emulator.mk $(MAKE_VARIABLES) clean_emulator_essent

.PHONY: $(COMPILE_EMULATOR_ESSENT_TARGETS) emulator_essent all clean clean_essent

emulator_essent: $(COMPILE_EMULATOR_ESSENT_TARGETS)

# Default
all: emulator_essent

clean_essent: $(CLEAN_ESSENT_TARGETS)

clean: clean_essent
