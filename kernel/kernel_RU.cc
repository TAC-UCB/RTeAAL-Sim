#include <iostream>
#include <vector>
#include <map>
#include <string>
#include <chrono>
#include <fstream>
#include <sstream>
#include <variant>
#include <yaml-cpp/yaml.h>
#include <filesystem>
#include <optional>
#include <cstddef>
#include <cstdint>
#include <chrono>
#include <random>

#include <linux/perf_event.h>
#include <sys/ioctl.h>
#include <sys/syscall.h>
#include <unistd.h>
#include <cstring>
#include <cerrno>
#include <csignal>

#include "dtm.h"
#include "json.hpp"

/* DTM related */ 
dtm_t* dtm;

void handle_sigterm(int sig) {
  dtm->stop();
}

// Parse the Json file for the mapping between DTM variable names and their indices
using DTMIndexMap = std::unordered_map<std::string, size_t>;
DTMIndexMap parse_json_dtm_indices(const std::string& path) {
    std::ifstream file(path);
    if (!file.is_open()) {
        throw std::runtime_error("Unable to read file: " + path);
    }

    nlohmann::json j;
    file >> j;

    DTMIndexMap map;
    for (auto it = j.begin(); it != j.end(); ++it) {
        map[it.key()] = it.value().get<size_t>();
    }

    return map;
}

void tick_dtm(dtm_t* dtm,
    const DTMIndexMap& dtm_index_map,
    std::vector<uint64_t>& databuf,
    bool done_reset
) {
    if (done_reset) {
        dtm_t::resp resp_bits;
        resp_bits.resp = static_cast<uint32_t>(databuf[dtm_index_map.at("SimDTM$$inst.debug_resp_bits_resp")]);
        resp_bits.data = static_cast<uint32_t>(databuf[dtm_index_map.at("SimDTM$$inst.debug_resp_bits_data")]);
        dtm->tick(databuf[dtm_index_map.at("SimDTM$$inst.debug_req_ready")] != 0,
                    databuf[dtm_index_map.at("SimDTM$$inst.debug_resp_valid")] != 0,
                    resp_bits);
        databuf[dtm_index_map.at("SimDTM$$inst.debug_resp_ready")] = 1;
        databuf[dtm_index_map.at("SimDTM$$inst.debug_req_valid")] = dtm->req_valid() ? 1 : 0;
        databuf[dtm_index_map.at("SimDTM$$inst.debug_req_bits_addr")] = static_cast<uint64_t>(dtm->req_bits().addr);
        databuf[dtm_index_map.at("SimDTM$$inst.debug_req_bits_op")] = static_cast<uint64_t>(dtm->req_bits().op);
        databuf[dtm_index_map.at("SimDTM$$inst.debug_req_bits_data")] = static_cast<uint64_t>(dtm->req_bits().data);
        databuf[dtm_index_map.at("SimDTM$$inst.exit")] = static_cast<uint64_t>(dtm->done() ? (dtm->exit_code() << 1 | 1) : 0);
    } else {
        databuf[dtm_index_map.at("SimDTM$$inst.debug_req_valid")] = 0;
        databuf[dtm_index_map.at("SimDTM$$inst.debug_resp_ready")] = 0;
        databuf[dtm_index_map.at("SimDTM$$inst.exit")] = 0;
    }
}

// Parse the Json file for the mapping between variable names and their indices
using VarIndexMap = std::unordered_map<std::string, std::vector<size_t>>;
VarIndexMap parse_json_var_indices(const std::string& path) {
    std::ifstream file(path);
    if (!file.is_open()) {
        throw std::runtime_error("Unable to read file: " + path);
    }

    nlohmann::json j;
    file >> j;

    VarIndexMap map;

    for (auto it = j.begin(); it != j.end(); ++it) {
        // Parse each value as a vector of size_t
        map[it.key()] = it.value().get<std::vector<size_t>>();
    }

    return map;
}

std::vector<uint64_t> load_txt_file(const std::string& filename, const std::string& prefix, std::optional<size_t> expected_size = std::nullopt) {
    std::filesystem::path filepath = TXTPATH + filename +"_" + prefix + ".txt";
    std::ifstream file(filepath);

    if (!file.is_open()) {
        std::cerr << "Failed to open file: " << filepath << "\n";
        std::exit(EXIT_FAILURE);
    }

    std::vector<uint64_t> values;

    std::string line;
    while (std::getline(file, line)) {
        std::stringstream ss(line);
        std::string token;

        while (std::getline(ss, token, ',')) {
            try {
                uint64_t val = std::stoull(token);
                values.push_back(val);
            } catch (const std::exception& e) {
                std::cerr << "Failed to parse number: '" << token << "', error: " << e.what() << "\n";
                std::exit(EXIT_FAILURE);
            }
        }
    }

    if (expected_size && values.size() != *expected_size) {
        std::cerr << "Expected " << *expected_size << " elements, but got " << values.size() << "\n";
        std::exit(EXIT_FAILURE);
    }

    return values;
}

std::vector<uint32_t> load_mux_file(const std::string& filename, const std::string& prefix, std::optional<size_t> expected_size = std::nullopt) {
    std::filesystem::path filepath = TXTPATH + filename +"_" + prefix + ".txt";
    std::ifstream file(filepath);

    if (!file.is_open()) {
        std::cerr << "Failed to open file: " << filepath << "\n";
        std::exit(EXIT_FAILURE);
    }

    std::vector<uint32_t> values;

    std::string line;
    while (std::getline(file, line)) {
        std::stringstream ss(line);
        std::string token;

        while (std::getline(ss, token, ',')) {
            try {
                uint64_t val = std::stoull(token);
                values.push_back(val);
            } catch (const std::exception& e) {
                std::cerr << "Failed to parse number: '" << token << "', error: " << e.what() << "\n";
                std::exit(EXIT_FAILURE);
            }
        }
    }

    if (expected_size && values.size() != *expected_size) {
        std::cerr << "Expected " << *expected_size << " elements, but got " << values.size() << "\n";
        std::exit(EXIT_FAILURE);
    }

    return values;
}

// case statement function
#if defined(__GNUC__) || defined(__clang__)
#define ALWAYS_INLINE __attribute__((always_inline)) inline
#else
#define ALWAYS_INLINE inline
#endif

ALWAYS_INLINE uint16_t op_ss_3(
    const uint8_t op_type,
    uint16_t counter,
    const uint32_t* __restrict& rptr,
    const uint64_t* __restrict databuf,
    uint64_t* __restrict output_buffer,
    uint64_t* __restrict memory_64,
    uint8_t* __restrict memory_8,
    const uint32_t* __restrict muxJT,
    std::vector<uint64_t>& scratch1,
    std::vector<uint64_t>& scratch2
) {
    switch (op_type) {
        // 0: add
        case 0: 
            {
                std::vector<uint32_t> input_data;
                for (size_t j = 0; j < 3; ++j) {
                    input_data.push_back(*rptr++);
                }
                uint64_t bwinfo = input_data[0];
                uint64_t bw = bwinfo >> 1;
                uint64_t shift = 64 - bw;
                uint8_t usint = bwinfo & 1;
            
                uint64_t lhs = databuf[input_data[1]];
                uint64_t rhs = databuf[input_data[2]];
            
                if (bw == 64) {
                    output_buffer[counter] = lhs + rhs;
                } else {
                    uint64_t lhs_extended = usint
                        ? static_cast<uint64_t>(static_cast<int64_t>(lhs << shift) >> shift)
                        : (lhs << shift) >> shift;
            
                    uint64_t rhs_extended = usint
                        ? static_cast<uint64_t>(static_cast<int64_t>(rhs << shift) >> shift)
                        : (rhs << shift) >> shift;
            
                    output_buffer[counter] = lhs_extended + rhs_extended;
                }
            }
            return 1;
        // 1: subtract
        case 1: 
            {
                // uint64_t bwinfo = *rptr++;
                // uint64_t bw = bwinfo >> 1;
                // uint64_t shift = 64 - bw;
                // uint8_t usint = bwinfo & 1;
            
                // uint64_t lhs = databuf[*rptr++];
                // uint64_t rhs = databuf[*rptr++];

                std::vector<uint32_t> input_data;
                for (size_t j = 0; j < 3; ++j) {
                    input_data.push_back(*rptr++);
                }

                uint64_t bwinfo = input_data[0];
                uint64_t bw = bwinfo >> 1;
                uint64_t shift = 64 - bw;
                uint8_t usint = bwinfo & 1;
            
                uint64_t lhs = databuf[input_data[1]];
                uint64_t rhs = databuf[input_data[2]];
            
                if (bw == 64) {
                    output_buffer[counter] = lhs - rhs;
                } else {
                    uint64_t lhs_extended = usint
                        ? static_cast<uint64_t>(static_cast<int64_t>(lhs << shift) >> shift)
                        : (lhs << shift) >> shift;
            
                    uint64_t rhs_extended = usint
                        ? static_cast<uint64_t>(static_cast<int64_t>(rhs << shift) >> shift)
                        : (rhs << shift) >> shift;
            
                    output_buffer[counter] = lhs_extended - rhs_extended;
                }
            }
            return 1;
        // 2: multiplication/division/modulus
        case 2: 
            {
                // uint64_t lhs = databuf[*rptr++];
                // uint64_t rhs = databuf[*rptr++];
                // output_buffer[counter] = lhs * rhs;
                std::uint64_t res;
                uint64_t opType = *rptr++;
                switch (opType) {
                    case 0: {
                        for (size_t j = 0; j < 2; ++j) {
                            if (j == 0) res = databuf[*rptr++];
                            else res *= databuf[*rptr++];
                        }
                        break;
                    }
                    case 1: {
                        for (size_t j = 0; j < 2; ++j) {
                            if (j == 0) res = static_cast<int64_t>(databuf[*rptr++]);
                            else res *= static_cast<int64_t>(databuf[*rptr++]);
                        }
                        break;
                    }
                    case 2: {
                        for (size_t j = 0; j < 2; ++j) {
                            if (j == 0) res = databuf[*rptr++];
                            else res /= databuf[*rptr++];
                        }
                        break;
                    }
                    case 3: {
                        for (size_t j = 0; j < 2; ++j) {
                            if (j == 0) res = static_cast<int64_t>(databuf[*rptr++]);
                            else res /= static_cast<int64_t>(databuf[*rptr++]);
                        }
                        break;
                    }
                    case 4: {
                        for (size_t j = 0; j < 2; ++j) {
                            if (j == 0) res = databuf[*rptr++];
                            else res %= databuf[*rptr++];
                        }
                        break;
                    }
                }
                output_buffer[counter] = res;
            }
            return 1;
        // 3: asSInt
        case 3: 
            {
                // uint64_t val = databuf[*rptr++];
                // size_t shift = *rptr++;

                std::vector<uint32_t> input_data;
                for (size_t j = 0; j < 2; ++j) {
                    input_data.push_back(*rptr++);
                }

                uint64_t val = databuf[input_data[0]];
                size_t shift = input_data[1];


                uint64_t extend_bit = (val >> shift) & 1;
                if (extend_bit == 1) {
                    uint64_t nonzero_bit = 64 - (shift + 1);
                    uint64_t extend_mask = ((1ULL << nonzero_bit) - 1) << (shift + 1);
                    output_buffer[counter] = val | extend_mask;
                } else {
                    output_buffer[counter] = val;
                }
            }
            return 1;
        // 4: lt
        case 4: 
            {
                // uint64_t lhs = databuf[*rptr++];
                // uint64_t rhs = databuf[*rptr++];
                // output_buffer[counter] = lhs < rhs;
                std::uint64_t res;
                for (size_t j = 0; j < 2; ++j) {
                    if (j == 0) {
                        res = databuf[*rptr++];
                    } else {
                        res = res < databuf[*rptr++];
                    }
                }
                output_buffer[counter] = res;
            }
            return 1;
        // 5: ltS
        case 5:
            {
                // int64_t lhs = static_cast<int64_t>(databuf[*rptr++]);
                // int64_t rhs = static_cast<int64_t>(databuf[*rptr++]);
                // output_buffer[counter] = lhs < rhs;
                std::int64_t res;
                for (size_t j = 0; j < 2; ++j) {
                    if (j == 0) {
                        res = static_cast<int64_t>(databuf[*rptr++]);
                    } else {
                        res = res < static_cast<int64_t>(databuf[*rptr++]);
                    }
                }
                output_buffer[counter] = res;
            }
            return 1;
        // 6: leq
        case 6: 
            {
                // uint64_t lhs = databuf[*rptr++];
                // uint64_t rhs = databuf[*rptr++];
                // output_buffer[counter] = lhs <= rhs;
                std::uint64_t res;
                for (size_t j = 0; j < 2; ++j) {
                    if (j == 0) {
                        res = databuf[*rptr++];
                    } else {
                        res = res <= databuf[*rptr++];
                    }
                }
                output_buffer[counter] = res;
            }
            return 1;
        // 7: leqS
        case 7:
            {
                // int64_t lhs = static_cast<int64_t>(databuf[*rptr++]);
                // int64_t rhs = static_cast<int64_t>(databuf[*rptr++]);
                // output_buffer[counter] = lhs <= rhs;
                std::int64_t res;
                for (size_t j = 0; j < 2; ++j) {
                    if (j == 0) {
                        res = static_cast<int64_t>(databuf[*rptr++]);
                    } else {
                        res = res <= static_cast<int64_t>(databuf[*rptr++]);
                    }
                }
                output_buffer[counter] = res;
            }
            return 1;
        // 8: eq
        case 8:
            {
                // uint64_t lhs = databuf[*rptr++];
                // uint64_t rhs = databuf[*rptr++];
                // output_buffer[counter] = lhs == rhs;
                std::uint64_t res;
                for (size_t j = 0; j < 2; ++j) {
                    if (j == 0) {
                        res = databuf[*rptr++];
                    } else {
                        res = res == databuf[*rptr++];
                    }
                }
                output_buffer[counter] = res;
            }
            return 1;
        // 9: neq
        case 9:
            {
                // uint64_t lhs = databuf[*rptr++];
                // uint64_t rhs = databuf[*rptr++];
                // output_buffer[counter] = lhs != rhs;
                std::uint64_t res;
                for (size_t j = 0; j < 2; ++j) {
                    if (j == 0) {
                        res = databuf[*rptr++];
                    } else {
                        res = res != databuf[*rptr++];
                    }
                }
                output_buffer[counter] = res;
            }
            return 1;
        // 10: shl
        case 10:
            {
                // uint64_t val = databuf[*rptr++];
                // uint64_t shift = databuf[*rptr++];
                // output_buffer[counter] = val << shift;
                std::uint64_t res;
                for (size_t j = 0; j < 2; ++j) {
                    if (j == 0) {
                        res = databuf[*rptr++];
                    } else {
                        res = res << databuf[*rptr++];
                    }
                }
                output_buffer[counter] = res;
            }
            return 1;
        // 11: shr
        case 11:
            {
                // uint64_t val = databuf[*rptr++];
                // uint64_t shift = databuf[*rptr++];
                // output_buffer[counter] = val >> shift;
                std::uint64_t res;
                for (size_t j = 0; j < 2; ++j) {
                    if (j == 0) {
                        res = databuf[*rptr++];
                    } else {
                        res = res >> databuf[*rptr++];
                    }
                }
                output_buffer[counter] = res;
            }
            return 1;
        // 12: shrS
        case 12:
            {
                // int64_t val = static_cast<int64_t>(databuf[*rptr++]);
                // uint64_t shift = databuf[*rptr++];
                // output_buffer[counter] = static_cast<uint64_t>(val >> shift);
                std::int64_t res;
                for (size_t j = 0; j < 2; ++j) {
                    if (j == 0) {
                        res = static_cast<int64_t>(databuf[*rptr++]);
                    } else {
                        res = res >> databuf[*rptr++];
                    }
                }
                output_buffer[counter] = static_cast<uint64_t>(res);
            }
            return 1;
        // 13: and
        case 13:
            {
                // uint64_t lhs = databuf[*rptr++];
                // uint64_t rhs = databuf[*rptr++];
                // output_buffer[counter] = lhs & rhs;
                std::uint64_t res;
                for (size_t j = 0; j < 2; ++j) {
                    if (j == 0) {
                        res = databuf[*rptr++];
                    } else {
                        res = res & databuf[*rptr++];
                    }
                }
                output_buffer[counter] = res;
            }
            return 1;
        // 14: or
        case 14:
            {
                // uint64_t lhs = databuf[*rptr++];
                // uint64_t rhs = databuf[*rptr++];
                // output_buffer[counter] = lhs | rhs;
                std::uint64_t res;
                for (size_t j = 0; j < 2; ++j) {
                    if (j == 0) {
                        res = databuf[*rptr++];
                    } else {
                        res = res | databuf[*rptr++];
                    }
                }
                output_buffer[counter] = res;
            }
            return 1;
        // 15: xor
        case 15: 
            {
                // uint64_t lhs = databuf[*rptr++];
                // uint64_t rhs = databuf[*rptr++];
                // output_buffer[counter] = lhs ^ rhs;
                std::uint64_t res;
                for (size_t j = 0; j < 2; ++j) {
                    if (j == 0) {
                        res = databuf[*rptr++];
                    } else {
                        res = res ^ databuf[*rptr++];
                    }
                }
                output_buffer[counter] = res;
            }
            return 1;
        // 16: xorr
        case 16:
            {
                uint64_t val = databuf[*rptr++];
                #if defined(__GNUC__) || defined(__clang__)
                            uint32_t ones = __builtin_popcountll(val);
                #else
                            uint32_t ones = 0;
                            while (val) {
                                ones += val & 1;
                                val >>= 1;
                            }
                #endif
                output_buffer[counter] = static_cast<uint64_t>(ones & 1);
            }
            return 1;
        // 17: cat
        case 17:
            {
                // uint32_t x = *rptr++;
                // uint32_t y = *rptr++;
                // uint32_t z = *rptr++;
                // output_buffer[counter] = (databuf[x] << z) | databuf[y];
                std::vector<uint32_t> input_data;
                for (size_t j = 0; j < 3; ++j) {
                    input_data.push_back(*rptr++);
                }
                output_buffer[counter] = (databuf[input_data[0]] << input_data[2]) | databuf[input_data[1]];
            }
            return 1;
        // 18: bits
        case 18:
            {
                // uint64_t x = *rptr++;
                // uint64_t y = *rptr++;
                // uint64_t z = *rptr++;
                // output_buffer[counter] = (databuf[x] >> y) & databuf[z];
                std::vector<uint32_t> input_data;
                for (size_t j = 0; j < 3; ++j) {
                    input_data.push_back(*rptr++);
                }
                output_buffer[counter] = (databuf[input_data[0]] >> input_data[1]) & databuf[input_data[2]];
            }
            return 1;
        // 19: mux
        case 19:
            {
                // const uint32_t* x = rptr++;
                // const uint32_t* y = rptr++;
                // const uint32_t* z = rptr++;
                // output_buffer[counter] = databuf[*x] > 0 ? databuf[*y] : databuf[*z];
                std::vector<uint32_t> input_data;
                for (size_t j = 0; j < 3; ++j) {
                    input_data.push_back(*rptr++);
                }
                output_buffer[counter] = databuf[input_data[0]] ? databuf[input_data[1]] : databuf[input_data[2]];
            }
            return 1;
        // 20: assign
        case 20:
            {
                output_buffer[counter] = databuf[*rptr++];
            }
            return 1;
        // 21: memr
        case 21:
            {
                std::vector<uint32_t> input_data;
                for (size_t j = 0; j < 4; ++j) {
                    input_data.push_back(*rptr++);
                }

                size_t tmp = input_data[1] + (static_cast<size_t>(databuf[input_data[3]]) & input_data[2]);
                if (input_data[0]==0) {
                    output_buffer[counter] = memory_64[tmp];
                } else {
                    output_buffer[counter] = static_cast<uint64_t>(memory_8[tmp]);
                }
            }
            return 1;
        // 22: or chain 
        case 22:
            {
                size_t x = *rptr++;
                uint64_t acc = 0;
                for (size_t j = 0; j < x; ++j) {
                    acc |= databuf[*rptr++];
                }
                output_buffer[counter] = acc;
            }
            return 1;
        // 23: xor chain
        case 23: 
            {
                size_t x = *rptr++;
                uint64_t acc = databuf[*rptr++];
                for (size_t j = 0; j < x-1; ++j) {
                    acc ^= databuf[*rptr++];
                }
                output_buffer[counter] = acc;
            }
            return 1;
        // 24: muxJT
        case 24:
            {
                std::vector<uint32_t> input_data;
                for (size_t j = 0; j < 3; ++j) {
                    input_data.push_back(*rptr++);
                }
                size_t tmp = (databuf[input_data[0]] & input_data[1]) + input_data[2];
                output_buffer[counter] = databuf[muxJT[tmp]];
            }
            return 1;
        // 25: dshl
        case 25:
            {
                size_t n = *rptr++;
                size_t shift_idx = *rptr++;
                size_t shift_mask_idx = *rptr++;
                uint64_t shift_op = databuf[shift_idx] & databuf[shift_mask_idx];
                size_t output_bw = *rptr++;
                size_t min_words = (output_bw + 63) / 64;

                // resize scratch buffer for block indices
                scratch1.resize(n);
                for (size_t k = 0; k < n; ++k) {
                    scratch1[k] = databuf[*rptr++];
                }

                if (__builtin_expect(n == 0 || shift_op == 0, 0)) {
                    for (size_t w = 0; w < min_words; ++w) {
                        output_buffer[counter + w] = (w < n) ? scratch1[w] : 0ULL;
                    }
                    return min_words;
                }

                size_t word_shift = shift_op / 64;
                uint32_t bit_shift = shift_op & 63;

                size_t result_len = n + word_shift + 1;
                if (result_len < min_words) result_len = min_words;

                scratch2.assign(result_len, 0ULL);

                uint64_t carry = 0;
                for (size_t w = 0; w < n; ++w) {
                    uint64_t val = scratch1[w];
                    uint64_t shifted = val << bit_shift;
                    scratch2[w + word_shift] |= shifted | carry;
                    carry = (bit_shift == 0) ? 0 : (val >> (64 - bit_shift));
                }
                if (bit_shift != 0) {
                    scratch2[n + word_shift] |= carry;
                }

                for (size_t w = 0; w < min_words; ++w) {
                    output_buffer[counter + w] = scratch2[w];
                }
                return min_words;
            }
        // 26: dshr
        case 26:
            {
                size_t input_len = *rptr++;
                size_t shift_idx = *rptr++;
                size_t shift_mask_idx = *rptr++;
                uint64_t shift = databuf[shift_idx] & databuf[shift_mask_idx];
                size_t output_bw = *rptr++;

                size_t word_shift = shift / 64;
                size_t bit_shift = shift & 63;
                size_t num_words = (output_bw + 63) / 64;

                scratch1.resize(input_len);
                for (size_t k = 0; k < input_len; ++k) {
                    scratch1[k] = databuf[*rptr++];
                }

                for (size_t w = 0; w < num_words; ++w) {
                    size_t src_idx = w + word_shift;
                    uint64_t low = (src_idx < input_len) ? scratch1[src_idx] : 0ULL;
                    uint64_t high = (bit_shift > 0 && (src_idx + 1) < input_len) ? scratch1[src_idx + 1] : 0ULL;
                    uint64_t val = (bit_shift == 0) ? low : ((low >> bit_shift) | (high << (64 - bit_shift)));
                    output_buffer[counter + w] = val;
                }
                return num_words;
            }
        // 27: dshrS
        case 27:
            {
                size_t input_len = *rptr++;
                size_t shift_idx = *rptr++;
                size_t shift_mask_idx = *rptr++;
                uint64_t shift = databuf[shift_idx] & databuf[shift_mask_idx];
                size_t output_bw = *rptr++;

                size_t word_shift = shift / 64;
                size_t bit_shift = shift & 63;
                size_t num_words = (output_bw + 63) / 64;

                scratch1.resize(input_len);
                for (size_t k = 0; k < input_len; ++k) {
                    scratch1[k] = databuf[*rptr++];
                }

                uint64_t sign_ext_val = (scratch1.back() & (1ULL << 63)) ? UINT64_MAX : 0;

                for (size_t w = 0; w < num_words; ++w) {
                    size_t src_idx = w + word_shift;
                    uint64_t low = (src_idx < input_len) ? scratch1[src_idx] : sign_ext_val;
                    uint64_t high = 0;
                    if (bit_shift > 0) {
                        high = ((src_idx + 1) < input_len) ? scratch1[src_idx + 1] : sign_ext_val;
                    }
                    int64_t val = (bit_shift == 0) ? low : (low >> bit_shift) | (high << (64 - bit_shift));
                    output_buffer[counter + w] = val;
                }
                return num_words;
            }
        // 28: memw
        case 28:
            {
                std::vector<uint32_t> input_data;
                for (size_t j = 0; j < 7; ++j) {
                    input_data.push_back(*rptr++);
                }

                size_t tmp = input_data[3] + (static_cast<size_t>(databuf[input_data[5]]) & input_data[4]);
                if (databuf[input_data[0]] != 0 && databuf[input_data[1]] != 0) {
                    if (input_data[2] == 0) {
                        memory_64[tmp] = databuf[input_data[6]];
                    } else {
                        memory_8[tmp] = static_cast<uint8_t>(databuf[input_data[6]]);
                    }
                }
            }
            return 1;
        default:
            return 0;
    }
}

template<typename T>
std::vector<T> loadVectorFromJson(const std::string& filename) {
    std::ifstream file(filename);
    if (!file) throw std::runtime_error("Failed to open file: " + filename);

    nlohmann::json j;
    file >> j;
    return j.get<std::vector<T>>();
}

// Load size_t from JSON file (plain number)
size_t loadSizeTFromJson(const std::string& filename) {
    std::ifstream file(filename);
    if (!file) {
        std::cerr << "Failed to open file: " << filename << "\n";
        return false;
    }

    nlohmann::json j;
    file >> j;
    return j.get<size_t>();
}

// Profiling Functions
int open_perf_counter(uint32_t type, uint64_t config) {
    struct perf_event_attr pe;
    memset(&pe, 0, sizeof(struct perf_event_attr));
    pe.type = type;
    pe.size = sizeof(struct perf_event_attr);
    pe.config = config;
    pe.disabled = 1;
    pe.exclude_kernel = 1;
    pe.exclude_hv = 1;

    int fd = syscall(__NR_perf_event_open, &pe, 0, -1, -1, 0);
    return fd;
}

int64_t read_counter(int fd) {
    int64_t count;
    if (read(fd, &count, sizeof(count)) == -1) {
        perror("read");
        return -1;
    }
    return count;
}

int open_cache_perf_counter(uint64_t cache, uint64_t op, uint64_t result) {
    struct perf_event_attr pe{};
    memset(&pe, 0, sizeof(struct perf_event_attr));
    pe.type = PERF_TYPE_HW_CACHE;
    pe.size = sizeof(struct perf_event_attr);
    pe.config = cache | (op << 8) | (result << 16);
    pe.disabled = 1;
    pe.exclude_kernel = 0;
    pe.exclude_hv = 1;

    int fd = syscall(__NR_perf_event_open, &pe, 0, -1, -1, 0);
    if (fd == -1) {
        std::cerr << "perf_event_open failed: " << strerror(errno) << "\n";
    }
    return fd;
}

void initialize_memory(std::vector<uint8_t>& memory_8, std::vector<uint64_t>& memory_64) {
    std::random_device rd;
    std::mt19937 gen(rd());

    // For uint8_t values (0 to 255)
    std::uniform_int_distribution<uint16_t> dist8(0, 255);  // note: use uint16_t for dist range

    for (auto& byte : memory_8) {
        byte = static_cast<uint8_t>(dist8(gen));
    }

    // For uint64_t values (full range or a subset if desired)
    std::uniform_int_distribution<uint64_t> dist64(0, UINT64_MAX);

    for (auto& word : memory_64) {
        word = dist64(gen);
    }
}


int main(int argc, char* argv[]) {
    if (argc < 3) {
        std::cerr << "Usage: " << argv[0] << " <design name> <benchmark>\n";
        return 1;
    }

    printf("[INFO] Simulate Design: %s\n", argv[1]);
    std::string prefix = argv[1];
    
    // Initialize DTM
    int htif_argc = argc - 1;
    char** htif_argv = &argv[1];
    dtm = new dtm_t(htif_argc, htif_argv);
    std::cout << "[INFO] DTM initialized " << std::endl;
    signal(SIGTERM, handle_sigterm);

    /* Loading Tensors */
    auto start = std::chrono::steady_clock::now();

    // Load DTM index map
    std::filesystem::path dtm_index_path = JSONPATH "dtmSignal_" + prefix + ".json";
    if (!std::filesystem::exists(dtm_index_path)) {
        std::cerr << "DTM index file not found: " << dtm_index_path << std::endl;
        return 1;
    }
    DTMIndexMap dtm_index_map;
    dtm_index_map = parse_json_dtm_indices(dtm_index_path.string());

    // Load Reg index map
    std::filesystem::path reg_index_path = JSONPATH "InputRegMap_" + prefix + ".json";
    if (!std::filesystem::exists(reg_index_path)) {
        std::cerr << "Reg index file not found: " << reg_index_path << std::endl;
        return 1;
    }
    VarIndexMap reg_index_map;
    reg_index_map = parse_json_var_indices(reg_index_path.string());

    // Find S rank
    size_t S_shape = loadSizeTFromJson(JSONPATH+prefix+"_S_shape.json");
    std::cout << "Shape for rank ID " << S_shape << std::endl;
    // Load layerInput tensor
    std::vector<uint64_t> databuf = load_txt_file("layerIn", prefix, S_shape);
    std::cout << "Loaded " << databuf.size() << " input values.\n";
    // Load mux JumpTable
    std::vector<uint32_t> muxJT = load_mux_file("muxJT", prefix);
    std::cout << "Loaded " << muxJT.size() << " muxJT values.\n";

    // Declare layerOutput Buffer
    size_t max_s = loadSizeTFromJson(JSONPATH+prefix+"_max_s.json");
    std::vector<uint64_t> output_buffer(max_s, 0);
    size_t layer_num = loadSizeTFromJson(JSONPATH+prefix+"_layer_num.json"); 
    std::vector<uint32_t> s = loadVectorFromJson<uint32_t>(JSONPATH+prefix+"_unswizzled_sptr.json");
    std::vector<uint32_t> r = loadVectorFromJson<uint32_t>(JSONPATH+prefix+"_unswizzled_rptr.json");
    std::vector<uint8_t> n = loadVectorFromJson<uint8_t>(JSONPATH+prefix+"_unswizzled_nptr.json");
    std::vector<uint16_t> ss_lens = loadVectorFromJson<uint16_t>(JSONPATH+prefix+"_unswizzled_ss_lens_ptr.json");

    // Memory Array Declaration
    // TODO: change them to random initialization after debugging
    size_t size8 = 0;
    size_t size64 = 0;
    if (prefix == "rocketchip-1c") {
        size8 = 268457796;
        size64 = 4798;
    } else if (prefix == "rocketchip-2c") {
        size8 =  268474894;
        size64 = 9506;
    } else if (prefix == "rocketchip-4c") {
        size8 = 268509090;
        size64 = 18922;
    } else if (prefix == "rocketchip-6c") {
        size8 = 268544102;
        size64 = 28306;
    } else if (prefix == "rocketchip-8c") {
        size8 = 268578202;
        size64 = 37674;
    } else if (prefix == "rocketchip-12c") {
        size8 = 268646258;
        size64 = 56410;
    } else if (prefix == "rocketchip-16c") {
        size8 = 268714778;
        size64 = 76170;
    } else if (prefix == "rocketchip-20c") {
        size8 = 268782690;
        size64 = 94906;
    } else if (prefix == "smallboom-1c") {
        size8 = 268457712;
        size64 = 11840;
    } else if (prefix == "smallboom-2c") {
        size8 = 268474726;
        size64 = 23590;
    } else if (prefix == "smallboom-4c") {
        size8 = 268509586;
        size64 = 47042;
    } else if (prefix == "smallboom-6c") {
        size8 = 268543334;
        size64 = 70462;
    } else if (prefix == "smallboom-8c") {
        size8 = 268577570;
        size64 = 94906;
    } else if (prefix == "smallboom-12c") {
        size8 = 268644898;
        size64 = 141746;
    } else if (prefix == "mediumboom-1c") {
        size8 = 268457840;
        size64 = 11968;
    } else if (prefix == "largeboom-1c") {
        size8 = 268474528;
        size64 = 23054;
    } else if (prefix == "largeboom-2c") {
        size8 = 268509126;
        size64 = 46018;
    } else if (prefix == "largeboom-4c") {
        size8 = 268577138;
        size64 = 92858;
    } else if (prefix == "largeboom-6c") {
        size8 = 268644406;
        size64 = 138674;
    } else if (prefix == "largeboom-8c") {
        size8 = 268713578;
        size64 = 185514;
    } else if (prefix == "megaboom-1c") {
        size8 = 268475872;
        size64 = 27982;
    } else if (prefix == "megaboom-2c") {
        size8 = 268510798;
        size64 = 56834;
    } else if (prefix == "megaboom-4c") {
        size8 = 268581018;
        size64 = 113466;
    } else if (prefix == "megaboom-6c") {
        size8 = 268649070;
        size64 = 169074;
    } else if (prefix == "megaboom-8c") {
        size8 = 268720914;
        size64 = 227154;
    } else if (prefix == "rocketchip-1c-small") {
        size8 = 268444964;
        size64 = 1310;
    } else if (prefix == "rocketchip-2c-small") {
        size8 = 268449230;
        size64 = 2530;
    } else if (prefix == "rocketchip-4c-small") {
        size8 =  268457762;
        size64 = 4970;
    } else if (prefix == "rocketchip-6c-small") {
        size8 =  268467110;
        size64 = 7378;
    } else if (prefix == "rocketchip-8c-small") {
        size8 =  268475546;
        size64 = 9770;
    } else if (prefix == "rocketchip-10c-small") {
        size8 =  268483910;
        size64 = 12162;
    } else if (prefix == "rocketchip-12c-small") {
        size8 =  268492274;
        size64 = 14554;
    } else if (prefix == "gemmini-8") {
        size8 = 268509786;
        size64 = 6060;
    } else if (prefix == "gemmini-16") {
        size8 = 268558938;
        size64 = 6060;
    } else if (prefix == "gemmini-32") {
        size8 = 268657232;
        size64 = 6060;
    } else if (prefix == "sha3-1" || prefix == "sha3-2" || prefix == "sha3-4") {
        size8 =  268457874;
        size64 = 4816;
    } else {
        std::cerr << "Unknown design prefix: " << prefix << ". Please provide a valid design prefix.\n";
        return 1;
    }
    std::vector<uint8_t> memory_8(size8, 0);
    std::vector<uint64_t> memory_64(size64, 0); 

    initialize_memory(memory_8, memory_64);

    std::vector<uint64_t> scratch1;
    std::vector<uint64_t> scratch2;

    uint64_t* __restrict databuf_ptr = databuf.data();
    uint64_t* __restrict outbuf_ptr = output_buffer.data(); 
    uint64_t* __restrict memory_64_ptr = memory_64.data();
    uint8_t* __restrict memory_8_ptr = memory_8.data();
    const uint32_t* __restrict muxJT_ptr = muxJT.data();

    databuf_ptr = (uint64_t*)__builtin_assume_aligned(databuf_ptr, 64);
    outbuf_ptr = (uint64_t*)__builtin_assume_aligned(outbuf_ptr, 64);
    memory_64_ptr = (uint64_t*)__builtin_assume_aligned(memory_64_ptr, 64);
    memory_8_ptr = (uint8_t*)__builtin_assume_aligned(memory_8_ptr, 64);
    muxJT_ptr = (const uint32_t*)__builtin_assume_aligned(muxJT_ptr, 64);

    auto end = std::chrono::steady_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::nanoseconds>(end - start);

    std::cout << "Time taken for tensor loading: " << duration.count() << " nanoseconds\n\n";

    std::cout << "\n";
    std::cout << "==================== Simulation Starts! ====================\n";
    std::cout << "\n";

    // int instr_fd = open_perf_counter(PERF_TYPE_HARDWARE, PERF_COUNT_HW_INSTRUCTIONS);
    // if (instr_fd < 0) {
    //     std::cerr << "Failed to open instructions counter: " << strerror(errno) << "\n";
    //     return 1;
    // }

    // int cycles_fd = open_perf_counter(PERF_TYPE_HARDWARE, PERF_COUNT_HW_CPU_CYCLES);
    // if (cycles_fd < 0) {
    //     std::cerr << "Failed to open cycles counter\n";
    //     return 1;
    // }

    // // L1D read access
    // int l1d_read_access_fd = open_cache_perf_counter(
    //     PERF_COUNT_HW_CACHE_L1D,
    //     PERF_COUNT_HW_CACHE_OP_READ,
    //     PERF_COUNT_HW_CACHE_RESULT_ACCESS
    // );

    // // L1D read miss
    // int l1d_read_miss_fd = open_cache_perf_counter(
    //     PERF_COUNT_HW_CACHE_L1D,
    //     PERF_COUNT_HW_CACHE_OP_READ,
    //     PERF_COUNT_HW_CACHE_RESULT_MISS
    // );

    // if (l1d_read_access_fd < 0 || l1d_read_miss_fd < 0) {
    //     return 1;
    // }

    // ioctl(instr_fd, PERF_EVENT_IOC_RESET, 0);
    // ioctl(instr_fd, PERF_EVENT_IOC_ENABLE, 0);
    // ioctl(cycles_fd, PERF_EVENT_IOC_RESET, 0);
    // ioctl(cycles_fd, PERF_EVENT_IOC_ENABLE, 0);

    // ioctl(l1d_read_access_fd, PERF_EVENT_IOC_RESET, 0);
    // ioctl(l1d_read_miss_fd, PERF_EVENT_IOC_RESET, 0);
    // ioctl(l1d_read_access_fd, PERF_EVENT_IOC_ENABLE, 0);
    // ioctl(l1d_read_miss_fd, PERF_EVENT_IOC_ENABLE, 0);

    uint64_t sim_cycles = -1;
    uint64_t cycle = 0;
    auto start_sim = std::chrono::steady_clock::now();

    bool io_success = false;
    bool done_reset = false;
    uint64_t async_reset_cycles = 2;
    uint64_t sync_reset_cycles = 10;

    // Simulation code here
    while (cycle < sim_cycles) {
        // if (cycle > 205615) {
        //   printf("Cycle: %lu\n", cycle);
        // }
        const uint32_t* __restrict rptr = r.data();
        const uint8_t* __restrict nptr = n.data();
        const uint32_t* __restrict s_ptr = s.data();
        const uint16_t* __restrict ss_ptr = ss_lens.data();

        rptr = (const uint32_t*)__builtin_assume_aligned(rptr, 64);
        nptr = (const uint8_t*)__builtin_assume_aligned(nptr, 64);
        s_ptr = (const uint32_t*)__builtin_assume_aligned(s_ptr, 64);
        ss_ptr = (const uint16_t*)__builtin_assume_aligned(ss_ptr, 64);

        if (done_reset && (dtm->done() || io_success)) {
            break;
        }
        done_reset = !(cycle < (async_reset_cycles + sync_reset_cycles));
        io_success = databuf[dtm_index_map.at("SimDTM$$inst.exit")] == 1;
        databuf[reg_index_map.at("reset")[0]] = done_reset ? 0 : 1;
        for (size_t i = 0; i < layer_num; ++i) {
            uint16_t counter = 0;
            uint32_t layer_len = *ss_ptr++;
            // std::cout << "layer_len: " << layer_len << std::endl;
            for (size_t ss = 0; ss < layer_len; ++ss) {
                // std::cout << "ss: " << ss << std::endl;
                // std::cout << "n: " << nptr << std::endl;
                uint8_t op_type = *nptr++;
                counter += op_ss_3(op_type, counter, rptr, databuf_ptr, outbuf_ptr, memory_64_ptr, memory_8_ptr, muxJT_ptr, scratch1, scratch2);
            }
            // std::cout << "counter: " << counter << std::endl;
            for (size_t j = 0; j < counter; ++j) {
                databuf_ptr[*s_ptr++] = outbuf_ptr[j];
            }
        }
        tick_dtm(dtm, dtm_index_map, databuf, done_reset);
        cycle++;
    }

    auto end_sim = std::chrono::steady_clock::now();
    auto duration_sim = std::chrono::duration_cast<std::chrono::nanoseconds>(end_sim - start_sim);

    std::cout << "Time taken for Simulation: " << duration_sim.count() << " nanoseconds\n\n";

    // ioctl(instr_fd, PERF_EVENT_IOC_DISABLE, 0);
    // ioctl(cycles_fd, PERF_EVENT_IOC_DISABLE, 0);

    // ioctl(l1d_read_access_fd, PERF_EVENT_IOC_DISABLE, 0);
    // ioctl(l1d_read_miss_fd, PERF_EVENT_IOC_DISABLE, 0);

    // int64_t instructions = read_counter(instr_fd);
    // int64_t cycles = read_counter(cycles_fd);

    // uint64_t accesses = 0, misses = 0;
    // ssize_t ret1 = read(l1d_read_access_fd, &accesses, sizeof(uint64_t));
    // if (ret1 != sizeof(uint64_t)) {
    //     std::cerr << "Failed to read L1D access counter\n";
    // }

    // ssize_t ret2 = read(l1d_read_miss_fd, &misses, sizeof(uint64_t));
    // if (ret2 != sizeof(uint64_t)) {
    //     std::cerr << "Failed to read L1D miss counter\n";
    // }

    // std::cout << "Instructions: " << instructions << "\n";
    // std::cout << "CPU Cycles: " << cycles << "\n";

    // std::cout << "L1D Read Accesses: " << accesses << "\n";
    // std::cout << "L1D Read Misses: " << misses << "\n";

    // close(instr_fd);
    // close(cycles_fd);
    // close(l1d_read_access_fd);
    // close(l1d_read_miss_fd);

    int ret = 0;
    if (dtm->exit_code()) {
        std::cerr << "*** FAILED *** via dtm (code = " << dtm->exit_code() << ") after " << cycle << "cycles" << std::endl;
        ret = dtm->exit_code();
    }
    else if (cycle == sim_cycles) {
        std::cerr << "*** FAILED *** via cycle (timeout) after " << cycle << " cycles" << std::endl;
        ret = 2;
    }
    else {
        std::cerr << "Completed after " << cycle << " cycles" << std::endl;
    }

    if (dtm) delete dtm;

    return ret;
}
