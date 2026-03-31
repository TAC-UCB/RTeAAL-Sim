base_dir=$(abspath .)
essent_dir=$(base_dir)/..
essent_bin_dir=$(essent_dir)/utils/bin
firrtl_dir=$(essent_dir)/essent/firrtls

build_jar:
	cd $(essent_dir) && sbt assembly

# Designs to compile
DESIGNS = \
	rocketchip-8c \
	smallboom-8c \
	largeboom-8c \
	megaboom-8c \
	rocketchip-6c \
	smallboom-6c \
	largeboom-6c \
	megaboom-6c \
	rocketchip-4c \
	smallboom-4c \
	largeboom-4c \
	megaboom-4c \
	rocketchip-2c \
	smallboom-2c \
	largeboom-2c \
	megaboom-2c \
	smallboom-1c \
	largeboom-1c \
	megaboom-1c \
	rocketchip-1c \
	rocketchip-12c \
	rocketchip-16c \
	rocketchip-20c \
	smallboom-12c \
	gemmini-8 \
	gemmini-16 \
	gemmini-32 \
	sha3-1 \
	sha3-2 \
	sha3-4 \

FIRRTL_rocketchip_1c := $(firrtl_dir)/freechips.rocketchip.system.DefaultConfig.fir
FIRRTL_rocketchip_4c := $(firrtl_dir)/freechips.rocketchip.system.QuadCoreConfig.fir
FIRRTL_rocketchip_8c := $(firrtl_dir)/freechips.rocketchip.system.OctaCoreConfig.fir
FIRRTL_rocketchip_12c := $(firrtl_dir)/freechips.rocketchip.system.TwelveConfig.fir
FIRRTL_rocketchip_16c := $(firrtl_dir)/freechips.rocketchip.system.SixteenConfig.fir
FIRRTL_rocketchip_20c := $(firrtl_dir)/freechips.rocketchip.system.TwentyConfig.fir
FIRRTL_smallboom_1c := $(firrtl_dir)/freechips.rocketchip.system.SmallBoomConfig.fir
FIRRTL_smallboom_4c := $(firrtl_dir)/freechips.rocketchip.system.QuadSmallBoomConfig.fir
FIRRTL_smallboom_8c := $(firrtl_dir)/freechips.rocketchip.system.OctaSmallBoomConfig.fir
FIRRTL_smallboom_12c := $(firrtl_dir)/freechips.rocketchip.system.TwelveSmallBoomConfig.fir
FIRRTL_largeboom_1c := $(firrtl_dir)/freechips.rocketchip.system.LargeBoomConfig.fir
FIRRTL_largeboom_4c := $(firrtl_dir)/freechips.rocketchip.system.QuadLargeBoomConfig.fir
FIRRTL_largeboom_8c := $(firrtl_dir)/freechips.rocketchip.system.OctaLargeBoomConfig.fir
FIRRTL_gemmini_8 := $(firrtl_dir)/freechips.rocketchip.system.SmallRocketGemminiConfig.fir
FIRRTL_gemmini_16 := $(firrtl_dir)/freechips.rocketchip.system.RocketGemminiConfig.fir
FIRRTL_gemmini_32 := $(firrtl_dir)/freechips.rocketchip.system.LargeRocketGemminiConfig.fir
FIRRTL_sha3_1 := $(firrtl_dir)/freechips.rocketchip.system.RocketSHA3Config.fir
FIRRTL_sha3_2 := $(firrtl_dir)/freechips.rocketchip.system.RocketMedianSHA3Config.fir
FIRRTL_sha3_4 := $(firrtl_dir)/freechips.rocketchip.system.RocketLargeSHA3Config.fir

$(DESIGNS): %: tensor_% kernel_IU_% kernel_SU_% kernel_TI_% kernel_NU kernel_PSU kernel_OU kernel_RU preprocess_tensor
	$(base_dir)/bin/preprocess_tensor $@

# Function to get FIRRTL file for a design
get_firrtl = $(FIRRTL_$(subst -,_,$(patsubst tensor_%,%,$1)))
TENSOR_TARGETS := $(foreach i,$(DESIGNS),tensor_$i)
${TENSOR_TARGETS}: tensor_%: $(log_dir)
	@firrtl_file=$(call get_firrtl,$@); \
	cd $(essent_bin_dir) && /usr/bin/time ./essent -O1 $$firrtl_file > $(log_dir)/$@.log 2>&1
	touch $@

kernel_IU_%.cc kernel_SU_%.cc kernel_TI_%.cc: tensor_%
	@true