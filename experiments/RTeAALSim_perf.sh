#!/bin/bash
# <repeat> <core_id>
trap 'stty echo' EXIT
set -e
ARGS_DESIGNS=("rocketchip-8c")
KERNELS=("OU" "NU" "PSU" "IU" "SU" "TI" "RU")
BENCHS=("dhrystone")
# PERF_COMMAND="perf stat -e instructions,cycles,L1-dcache-loads,L1-dcache-stores,L1-dcache-load-misses,L1-icache-loads,L1-icache-load-misses,LLC-loads,LLC-load-misses,LLC-stores,LLC-store-misses,branch-instructions,branch-misses,l2_rqsts.code_rd_hit,l2_rqsts.code_rd_miss,l2_rqsts.miss,l2_rqsts.all_demand_data_rd,l2_rqsts.all_demand_miss"
# PERF_COMMAND="perf stat"
PERF_COMMAND="time"
REPEAT=$1; shift
if [ -z "$REPEAT" ]; then
    REPEAT=10
fi
CORE_ID=$1; shift
if [ -z "$CORE_ID" ]; then
    CORE_ID=0
fi

# create folder
current_date=$(date +%Y-%m-%d)

# select design
for ARGS_DESIGN in "${ARGS_DESIGNS[@]}"; do
    echo "DESIGN: $ARGS_DESIGN"
    for ((kernel_idx=0; kernel_idx<${#KERNELS[@]}; kernel_idx++)); do
        kernel="${KERNELS[kernel_idx]}"
        echo "KERNEL: $kernel"
        if [[ "$kernel" =~ ^(IU|SU|TI)$ ]]; then
            PROGRAM="../kernel/bin/kernel_${kernel}_${ARGS_DESIGN}"
        else
            PROGRAM="../kernel/bin/kernel_${kernel}"
        fi
        # check if program exists
        if [ ! -f "$PROGRAM" ]; then
            echo "Program $PROGRAM does not exist"
            continue
        fi
        output_dir="RTeAAL/${ARGS_DESIGN}_${kernel}/$current_date"
        mkdir -p "$output_dir"
        for bench in "${BENCHS[@]}"; do
            echo "Running bench: $bench"
            for i in $(seq 1 $REPEAT); do
                echo "Repeat: $i"
                taskset -c $CORE_ID $PERF_COMMAND $PROGRAM $ARGS_DESIGN "../benchmark/${bench}.riscv" > "$output_dir/${bench}-${i}.log" 2>&1
            done
        done
    done
done

# gemmini tests
# GEMMINI_SIZES=("8" "16" "32")
# for GEMMINI_SIZE in "${GEMMINI_SIZES[@]}"; do
#     echo "DESIGN: gemmini-${GEMMINI_SIZE}"
#     for ((kernel_idx=0; kernel_idx<${#KERNELS[@]}; kernel_idx++)); do
#         kernel="${KERNELS[kernel_idx]}"
#         echo "KERNEL: $kernel"
#         if [[ "$kernel" =~ ^(IU|SU|TI)$ ]]; then
#             PROGRAM="../kernel/bin/kernel_${kernel}_gemmini-${GEMMINI_SIZE}"
#         else
#             PROGRAM="../kernel/bin/kernel_gemmini-${GEMMINI_SIZE}"
#         fi

#         # check if program exists
#         if [ ! -f "$PROGRAM" ]; then
#             echo "Program $PROGRAM does not exist"
#             continue
#         fi
#         output_dir="RTeAAL/gemmini-${GEMMINI_SIZE}_${kernel}/$current_date"
#         mkdir -p "$output_dir"
#         for i in $(seq 1 $REPEAT); do
#             echo "Repeat: $i"
#             taskset -c $CORE_ID $PERF_COMMAND $PROGRAM gemmini-${GEMMINI_SIZE} "../benchmark/matrix_add-baremetal${GEMMINI_SIZE}" > "$output_dir/matrix_add-baremetal${GEMMINI_SIZE}-${i}.log" 2>&1
#         done
#     done
# done

# SHA3 tests
# SHA3_SIZES=("1" "2" "4")
# for SHA3_SIZE in "${SHA3_SIZES[@]}"; do
#     echo "DESIGN: sha3-${SHA3_SIZE}"
#     for ((kernel_idx=0; kernel_idx<${#KERNELS[@]}; kernel_idx++)); do
#         kernel="${KERNELS[kernel_idx]}"
#         echo "KERNEL: $kernel"
#         if [[ "$kernel" =~ ^(IU|SU|TI)$ ]]; then
#             PROGRAM="../kernel/bin/kernel_${kernel}_sha3-${SHA3_SIZE}"
#         else
#             PROGRAM="../kernel/bin/kernel_sha3-${SHA3_SIZE}"
#         fi
#         # check if program exists
#         if [ ! -f "$PROGRAM" ]; then
#             echo "Program $PROGRAM does not exist"
#             continue
#         fi
#         output_dir="RTeAAL/sha3-${SHA3_SIZE}_${kernel}/$current_date"
#         mkdir -p "$output_dir"
#         for i in $(seq 1 $REPEAT); do
#             echo "Repeat: $i"
#             taskset -c $CORE_ID $PERF_COMMAND $PROGRAM sha3-${SHA3_SIZE} "../benchmark/sha3-rocc.riscv" > "$output_dir/sha3-rocc-${i}.log" 2>&1
#         done
#     done
# done
