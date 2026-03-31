base_dir=$(abspath .)
lib_dir=$(base_dir)/../kernel
verilator_dir=$(base_dir)/verilator
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
	rocket21-24c \
	rocket21-28c \
	boom21-12small \
	gemmini-8 \
	gemmini-16 \
	gemmini-32 \
	sha3-1 \
	sha3-2 \
	sha3-4 \

# Compile verilator
VERILATOR_VERSION=5.016
VERILATOR_SRCDIR = $(verilator_dir)/verilator/src/verilator-$(VERILATOR_VERSION)
INSTALLED_VERILATOR=$(verilator_dir)/verilator/install/bin/verilator

verilator_install: $(INSTALLED_VERILATOR)

$(INSTALLED_VERILATOR): $(VERILATOR_SRCDIR)/bin/verilator
	$(MAKE) -C $(VERILATOR_SRCDIR) installbin installdata
	touch $@

$(VERILATOR_SRCDIR)/bin/verilator: $(VERILATOR_SRCDIR)/Makefile
	$(MAKE) -C $(VERILATOR_SRCDIR) verilator_bin
	touch $@

$(VERILATOR_SRCDIR)/Makefile: $(VERILATOR_SRCDIR)/configure
	mkdir -p $(dir $@)
	cd $(dir $@) && autoconf && ./configure --prefix=$(verilator_dir)/verilator/install

$(VERILATOR_SRCDIR)/configure: verilator/verilator-$(VERILATOR_VERSION).zip
	rm -rf $(dir $@)
	mkdir -p $(dir $@)
	unzip -q $^ -d $(dir $(VERILATOR_SRCDIR))

verilator/verilator-$(VERILATOR_VERSION).zip:
	mkdir -p $(dir $@)
	wget https://github.com/verilator/verilator/archive/refs/tags/v$(VERILATOR_VERSION).zip -O $@


MAKE_VARIABLES = INSTALLED_VERILATOR=$(INSTALLED_VERILATOR) CXX=$(CXX) LINK=$(LINK) AR=$(AR) lib_dir=$(lib_dir)

design = $(firstword $(subst _, ,$*))

# Verilator compile
COMPILE_EMULATOR_VERILATOR_TARGETS := $(foreach i,$(DESIGNS),compile_verilator_$i)

${COMPILE_EMULATOR_VERILATOR_TARGETS}: compile_verilator_%: 
	$(MAKE) -C $(design) -f Makefile-emulator.mk $(MAKE_VARIABLES) emulator_verilator

CLEAN_VERILATOR_TARGETS := $(foreach j,$(DESIGNS),clean_verilator_$j)

${CLEAN_VERILATOR_TARGETS}: clean_verilator_%:
	$(MAKE) -C $(design) -f Makefile-emulator.mk $(MAKE_VARIABLES) clean_emulator_verilator


.PHONY: $(COMPILE_EMULATOR_VERILATOR_TARGETS) emulator_verilator all clean clean_verilator

emulator_verilator: $(COMPILE_EMULATOR_VERILATOR_TARGETS)


# Default
all: emulator_verilator

clean_verilator: $(CLEAN_VERILATOR_TARGETS)

clean: clean_essent clean_verilator 
