#!/bin/bash
# <design> <repeat> <core_id>
trap 'stty echo' EXIT
set -e
BASE_SIM=$1; shift
ARGS_DESIGNS=("rocket21-1c" "rocket21-4c")
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
    if [[ "$BASE_SIM" == *"essent"* ]]; then
        PROGRAM="../essent/${ARGS_DESIGN}/emulator_${BASE_SIM}"
    elif [[ "$BASE_SIM" == *"verilator"* ]]; then
        PROGRAM="../verilator/${ARGS_DESIGN}/emulator_${BASE_SIM}"
    else
        echo "Unknown BASE_SIM: $BASE_SIM"
        exit 1
    fi
    # check if program exists
    if [ ! -f "$PROGRAM" ]; then
        echo "Program $PROGRAM does not exist"
        continue
    fi
    output_dir="${BASE_SIM}/$ARGS_DESIGN/$current_date"
    mkdir -p "$output_dir"
    for bench in "${BENCHS[@]}"; do
        echo "Running bench: $bench"
        for i in $(seq 1 $REPEAT); do
            echo "Repeat: $i"
            taskset -c $CORE_ID $PERF_COMMAND $PROGRAM -c "../benchmark/${bench}.riscv" > "$output_dir/${bench}-${i}.log" 2>&1
        done
    done
done

# gemmini tests
# GEMMINI_SIZES=("8" "16" "32")
# for GEMMINI_SIZE in "${GEMMINI_SIZES[@]}"; do
#     echo "DESIGN: gemmini-${GEMMINI_SIZE}"
#     if [[ "$BASE_SIM" == *"essent"* ]]; then
#         PROGRAM="../essent/gemmini-${GEMMINI_SIZE}/emulator_${BASE_SIM}"
#     elif [[ "$BASE_SIM" == *"verilator"* ]]; then
#         PROGRAM="../verilator/gemmini-${GEMMINI_SIZE}/emulator_${BASE_SIM}"
#     else
#         echo "Unknown BASE_SIM: $BASE_SIM"
#         exit 1
#     fi
#     # check if program exists
#     if [ ! -f "$PROGRAM" ]; then
#         echo "Program $PROGRAM does not exist"
#         continue
#     fi
#     output_dir="${BASE_SIM}/gemmini-${GEMMINI_SIZE}/$current_date"
#     mkdir -p "$output_dir"
#     for i in $(seq 1 $REPEAT); do
#         echo "Repeat: $i"
#         taskset -c $CORE_ID $PERF_COMMAND $PROGRAM -c "../benchmark/matrix_add-baremetal${GEMMINI_SIZE}" > "$output_dir/matrix_add-baremetal${GEMMINI_SIZE}-${i}.log" 2>&1
#     done
# done

# # SHA3 tests
# SHA3_SIZES=("1" "2" "4")
# for SHA3_SIZE in "${SHA3_SIZES[@]}"; do
#     echo "DESIGN: sha3-${SHA3_SIZE}"
#     if [[ "$BASE_SIM" == *"essent"* ]]; then
#         PROGRAM="../essent/sha3-${SHA3_SIZE}/emulator_${BASE_SIM}"
#     elif [[ "$BASE_SIM" == *"verilator"* ]]; then
#         PROGRAM="../verilator/sha3-${SHA3_SIZE}/emulator_${BASE_SIM}"
#     else
#         echo "Unknown BASE_SIM: $BASE_SIM"
#         exit 1
#     fi
#     # check if program exists
#     if [ ! -f "$PROGRAM" ]; then
#         echo "Program $PROGRAM does not exist"
#         continue
#     fi
#     output_dir="${BASE_SIM}/sha3-${SHA3_SIZE}/$current_date"
#     mkdir -p "$output_dir"
#     for i in $(seq 1 $REPEAT); do
#         echo "Repeat: $i"
#         taskset -c $CORE_ID $PERF_COMMAND $PROGRAM -c "../benchmark/sha3-rocc.riscv" > "$output_dir/sha3-rocc-${i}.log" 2>&1
#     done
# done
