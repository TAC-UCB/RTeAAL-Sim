include Makefrag-variables.mk

$(lib_dir)/libfesvr.a:
	$(Make) -C $(lib_dir) libfesvr.a

# ESSENT
TestHarness.h:
	/usr/bin/time java $(JVM_FLAGS) -cp $(ESSENT_JAR) essent.Driver $(FIR_PATH) --output-dir $(base_dir) -O2 --essent-log-level info > essent.log 2>&1

emulator_essent: emulator_essent.cc TestHarness.h $(lib_dir)/libfesvr.a
	/usr/bin/time $(CXX) $(CXXFLAGS) $(ESSENT_INCLUDES) emulator_essent.cc $(lib_dir)/libfesvr.a -o emulator_essent $(LIBS) > compile.log 2>&1

emulator_essento1: emulator_essent.cc TestHarness.h $(lib_dir)/libfesvr.a
	/usr/bin/time $(CXX) $(CXXFLAGSO1) $(ESSENT_INCLUDES) emulator_essent.cc $(lib_dir)/libfesvr.a -o emulator_essento1 $(LIBS) > compileo1.log 2>&1

emulator_essento0: emulator_essent.cc TestHarness.h $(lib_dir)/libfesvr.a
	/usr/bin/time $(CXX) $(CXXFLAGSO0) $(ESSENT_INCLUDES) emulator_essent.cc $(lib_dir)/libfesvr.a -o emulator_essento0 $(LIBS) > compileo0.log 2>&1
.PHONY: all clean clean_emulator_essent

all: emulator_essent

clean_emulator_essent:
	rm -f ./emulator_essent
	rm -f ./TestHarness.h

clean: clean_emulator_essent
