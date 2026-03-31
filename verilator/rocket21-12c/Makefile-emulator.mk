include Makefrag-variables.mk

# Verilator
$(model_mk): $(sim_vsrcs) $(INSTALLED_VERILATOR) $(lib_dir)/libfesvr.a
	/usr/bin/time $(VERILATOR) $(VERILATOR_FLAGS) -Mdir $(model_dir) \
	-o $(emulator_verilator) $< $(sim_csrcs) $(lib_dir)/libfesvr.a -LDFLAGS "$(LIBS)" \
	-CFLAGS "-include $(model_header)" \
	> $(base_dir)/generate_file.log 2>&1
	touch $@


emulator_verilator: $(model_mk) $(lib_dir)/libfesvr.a
	/usr/bin/time $(MAKE) CXX=$(CXX) LINK=$(LINK) AR=$(AR) VM_PARALLEL_BUILDS=1 -C $(model_dir) -f V$(MODEL).mk > build_emulator_verilator$(OPTLEVEL).log 2>&1

.PHONY: all clean clean_emulator_verilator 

all: emulator_verilator

clean_emulator_verilator:
	rm $(emulator_verilator)
	rm -rf ./TestHarness


clean: clean_emulator_verilator 
