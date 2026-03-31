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

#define LR(lhs, rhs) \
        uint32_t lhs = *rptr++;     \
        uint32_t rhs = *rptr++;

#define COPY24() \
    databuf_ptr[*s_ptr++] = outbuf_ptr[j];\
    databuf_ptr[*s_ptr++] = outbuf_ptr[j + 1];\
    databuf_ptr[*s_ptr++] = outbuf_ptr[j + 2];\
    databuf_ptr[*s_ptr++] = outbuf_ptr[j + 3];\
    databuf_ptr[*s_ptr++] = outbuf_ptr[j + 4];\
    databuf_ptr[*s_ptr++] = outbuf_ptr[j + 5];\
    databuf_ptr[*s_ptr++] = outbuf_ptr[j + 6];\
    databuf_ptr[*s_ptr++] = outbuf_ptr[j + 7];\
    databuf_ptr[*s_ptr++] = outbuf_ptr[j + 8];\
    databuf_ptr[*s_ptr++] = outbuf_ptr[j + 9];\
    databuf_ptr[*s_ptr++] = outbuf_ptr[j + 10];\
    databuf_ptr[*s_ptr++] = outbuf_ptr[j + 11];\
    databuf_ptr[*s_ptr++] = outbuf_ptr[j + 12];\
    databuf_ptr[*s_ptr++] = outbuf_ptr[j + 13];\
    databuf_ptr[*s_ptr++] = outbuf_ptr[j + 14];\
    databuf_ptr[*s_ptr++] = outbuf_ptr[j + 15];\
    databuf_ptr[*s_ptr++] = outbuf_ptr[j + 16];\
    databuf_ptr[*s_ptr++] = outbuf_ptr[j + 17];\
    databuf_ptr[*s_ptr++] = outbuf_ptr[j + 18];\
    databuf_ptr[*s_ptr++] = outbuf_ptr[j + 19];\
    databuf_ptr[*s_ptr++] = outbuf_ptr[j + 20];\
    databuf_ptr[*s_ptr++] = outbuf_ptr[j + 21];\
    databuf_ptr[*s_ptr++] = outbuf_ptr[j + 22];\
    databuf_ptr[*s_ptr++] = outbuf_ptr[j + 23]

#define COPY16() \
    databuf_ptr[*s_ptr++] = outbuf_ptr[j];     \
    databuf_ptr[*s_ptr++] = outbuf_ptr[j + 1]; \
    databuf_ptr[*s_ptr++] = outbuf_ptr[j + 2]; \
    databuf_ptr[*s_ptr++] = outbuf_ptr[j + 3]; \
    databuf_ptr[*s_ptr++] = outbuf_ptr[j + 4]; \
    databuf_ptr[*s_ptr++] = outbuf_ptr[j + 5]; \
    databuf_ptr[*s_ptr++] = outbuf_ptr[j + 6]; \
    databuf_ptr[*s_ptr++] = outbuf_ptr[j + 7]; \
    databuf_ptr[*s_ptr++] = outbuf_ptr[j + 8]; \
    databuf_ptr[*s_ptr++] = outbuf_ptr[j + 9]; \
    databuf_ptr[*s_ptr++] = outbuf_ptr[j + 10];\
    databuf_ptr[*s_ptr++] = outbuf_ptr[j + 11];\
    databuf_ptr[*s_ptr++] = outbuf_ptr[j + 12];\
    databuf_ptr[*s_ptr++] = outbuf_ptr[j + 13];\
    databuf_ptr[*s_ptr++] = outbuf_ptr[j + 14];\
    databuf_ptr[*s_ptr++] = outbuf_ptr[j + 15]

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

#if defined(__GNUC__) || defined(__clang__)
#define ALWAYS_INLINE __attribute__((always_inline)) inline
#else
#define ALWAYS_INLINE inline
#endif

ALWAYS_INLINE void add_loop(
   //  const uint64_t* __restrict& rptr,
   const uint32_t* __restrict& rptr,
   const uint64_t* __restrict databuf,
   uint64_t* __restrict output_buffer,
   const uint16_t start,
   const uint16_t end
) {
    // 0: addition
   {
        for (uint32_t i = start; i < end; ++i) {
            uint64_t bwinfo = *rptr++;
            uint64_t bw = bwinfo >> 1;
            uint64_t shift = 64 - bw;
            uint8_t usint = bwinfo & 1;
        
            uint64_t lhs = databuf[*rptr++];
            uint64_t rhs = databuf[*rptr++];
        
            if (bw == 64) {
                output_buffer[i] = lhs + rhs;
            } else {
                uint64_t lhs_extended = usint
                    ? static_cast<uint64_t>(static_cast<int64_t>(lhs << shift) >> shift)
                    : (lhs << shift) >> shift;
        
                uint64_t rhs_extended = usint
                    ? static_cast<uint64_t>(static_cast<int64_t>(rhs << shift) >> shift)
                    : (rhs << shift) >> shift;
        
                output_buffer[i] = lhs_extended + rhs_extended;
            }
        }
   }
}

#if defined(__GNUC__) || defined(__clang__)
#define ALWAYS_INLINE __attribute__((always_inline)) inline
#else
#define ALWAYS_INLINE inline
#endif

ALWAYS_INLINE void sub_loop(
  //  const uint64_t* __restrict& rptr,
  const uint32_t* __restrict& rptr,
  const uint64_t* __restrict databuf,
  uint64_t* __restrict output_buffer,
  const uint16_t start,
  const uint16_t end
) {
   // 1: subtraction
  {
        for (uint32_t i = start; i < end; ++i) {
            uint64_t bwinfo = *rptr++;
            uint64_t bw = bwinfo >> 1;
            uint64_t shift = 64 - bw;
            uint8_t usint = bwinfo & 1;
        
            uint64_t lhs = databuf[*rptr++];
            uint64_t rhs = databuf[*rptr++];
        
            if (bw == 64) {
                output_buffer[i] = lhs - rhs;
            } else {
                uint64_t lhs_extended = usint
                    ? static_cast<uint64_t>(static_cast<int64_t>(lhs << shift) >> shift)
                    : (lhs << shift) >> shift;
        
                uint64_t rhs_extended = usint
                    ? static_cast<uint64_t>(static_cast<int64_t>(rhs << shift) >> shift)
                    : (rhs << shift) >> shift;
        
                output_buffer[i] = lhs_extended - rhs_extended;
            }
        }
  }
}

#if defined(__GNUC__) || defined(__clang__)
#define ALWAYS_INLINE __attribute__((always_inline)) inline
#else
#define ALWAYS_INLINE inline
#endif

ALWAYS_INLINE void mdr_loop(
  //  const uint64_t* __restrict& rptr,
  const uint32_t* __restrict& rptr,
  const uint64_t* __restrict databuf,
  uint64_t* __restrict output_buffer,
  const uint16_t start,
  const uint16_t end
) {
   // 2: multiplication
  {
    for (uint32_t i = start; i < end; ++i) {
        // uint64_t lhs = databuf[*rptr++];
        // uint64_t rhs = databuf[*rptr++];
        uint64_t opType = *rptr++;

        LR(l, r);
        uint64_t lhs = databuf[l];
        uint64_t rhs = databuf[r];
        switch (opType) {
            case 0: {
                output_buffer[i] = lhs * rhs;
                break;
            }
            case 1: {
                output_buffer[i] = static_cast<uint64_t>(static_cast<int64_t>(lhs) * static_cast<int64_t>(rhs));
                break;
            }
            case 2: {
                output_buffer[i] = lhs / rhs;
                break;
            }
            case 3: {
                output_buffer[i] = static_cast<uint64_t>(static_cast<int64_t>(lhs) / static_cast<int64_t>(rhs));
                break;
            }
            case 4: {
                output_buffer[i] = lhs % rhs;
                break;
            }
        }
    }
  }
}

#if defined(__GNUC__) || defined(__clang__)
#define ALWAYS_INLINE __attribute__((always_inline)) inline
#else
#define ALWAYS_INLINE inline
#endif

ALWAYS_INLINE void mulS_loop(
  //  const uint64_t* __restrict& rptr,
  const uint32_t* __restrict& rptr,
  const uint64_t* __restrict databuf,
  uint64_t* __restrict output_buffer,
  const uint16_t start,
  const uint16_t end
) {
   // 3: signed multiplication
  {
        for (uint32_t i = start; i < end; ++i) {
            int64_t lhs = static_cast<int64_t>(databuf[*rptr++]);
            int64_t rhs = static_cast<int64_t>(databuf[*rptr++]);
            output_buffer[i] = static_cast<uint64_t>(lhs * rhs);
        }
  }
}

#if defined(__GNUC__) || defined(__clang__)
#define ALWAYS_INLINE __attribute__((always_inline)) inline
#else
#define ALWAYS_INLINE inline
#endif

ALWAYS_INLINE void lt_loop(
  //  const uint64_t* __restrict& rptr,
  const uint32_t* __restrict& rptr,
  const uint64_t* __restrict databuf,
  uint64_t* __restrict output_buffer,
  const uint16_t start,
  const uint16_t end
) {
   // 4; lt
  {
        for (uint32_t i = start; i < end; ++i) {
            uint64_t lhs = databuf[*rptr++];
            uint64_t rhs = databuf[*rptr++];
            output_buffer[i] = lhs < rhs;
        }
  }
}

#if defined(__GNUC__) || defined(__clang__)
#define ALWAYS_INLINE __attribute__((always_inline)) inline
#else
#define ALWAYS_INLINE inline
#endif

ALWAYS_INLINE void ltS_loop(
  //  const uint64_t* __restrict& rptr,
  const uint32_t* __restrict& rptr,
  const uint64_t* __restrict databuf,
  uint64_t* __restrict output_buffer,
  const uint16_t start,
  const uint16_t end
) {
   // 5; ltS
  {
       for (uint32_t i = start; i < end; ++i) {
            int64_t lhs = static_cast<int64_t>(databuf[*rptr++]);
            int64_t rhs = static_cast<int64_t>(databuf[*rptr++]);
            output_buffer[i] = lhs < rhs;
        }
  }
}

#if defined(__GNUC__) || defined(__clang__)
#define ALWAYS_INLINE __attribute__((always_inline)) inline
#else
#define ALWAYS_INLINE inline
#endif

ALWAYS_INLINE void leq_loop(
  //  const uint64_t* __restrict& rptr,
  const uint32_t* __restrict& rptr,
  const uint64_t* __restrict databuf,
  uint64_t* __restrict output_buffer,
  const uint16_t start,
  const uint16_t end
) {
   // 6: leq
  {
       for (uint32_t i = start; i < end; ++i) {
            uint64_t lhs = databuf[*rptr++];
            uint64_t rhs = databuf[*rptr++];
            output_buffer[i] = lhs <= rhs;
        }
  }
}

#if defined(__GNUC__) || defined(__clang__)
#define ALWAYS_INLINE __attribute__((always_inline)) inline
#else
#define ALWAYS_INLINE inline
#endif

ALWAYS_INLINE void leqS_loop(
  //  const uint64_t* __restrict& rptr,
  const uint32_t* __restrict& rptr,
  const uint64_t* __restrict databuf,
  uint64_t* __restrict output_buffer,
  const uint16_t start,
  const uint16_t end
) {
   // 7: leqS
  {
       for (uint32_t i = start; i < end; ++i) {
            int64_t lhs = static_cast<int64_t>(databuf[*rptr++]);
            int64_t rhs = static_cast<int64_t>(databuf[*rptr++]);
            output_buffer[i] = lhs <= rhs;
        }
  }
}

#if defined(__GNUC__) || defined(__clang__)
#define ALWAYS_INLINE __attribute__((always_inline)) inline
#else
#define ALWAYS_INLINE inline
#endif

ALWAYS_INLINE void eq_loop(
  //  const uint64_t* __restrict& rptr,
  const uint32_t* __restrict& rptr,
  const uint64_t* __restrict databuf,
  uint64_t* __restrict output_buffer,
  const uint16_t start,
  const uint16_t end
) {
   // 8: eq
  {
       for (uint32_t i = start; i < end; ++i) {
            uint64_t lhs = databuf[*rptr++];
            uint64_t rhs = databuf[*rptr++];
            output_buffer[i] = lhs == rhs;
        }
  }
}

#if defined(__GNUC__) || defined(__clang__)
#define ALWAYS_INLINE __attribute__((always_inline)) inline
#else
#define ALWAYS_INLINE inline
#endif

ALWAYS_INLINE void neq_loop(
  //  const uint64_t* __restrict& rptr,
  const uint32_t* __restrict& rptr,
  const uint64_t* __restrict databuf,
  uint64_t* __restrict output_buffer,
  const uint16_t start,
  const uint16_t end
) {
   // 9: neq
  {
        for (uint32_t i = start; i < end; ++i) {
            uint64_t lhs = databuf[*rptr++];
            uint64_t rhs = databuf[*rptr++];
            output_buffer[i] = lhs != rhs;
        }
  }
}


#if defined(__GNUC__) || defined(__clang__)
#define ALWAYS_INLINE __attribute__((always_inline)) inline
#else
#define ALWAYS_INLINE inline
#endif

ALWAYS_INLINE void shl_loop(
  //  const uint64_t* __restrict& rptr,
  const uint32_t* __restrict& rptr,
  const uint64_t* __restrict databuf,
  uint64_t* __restrict output_buffer,
  const uint16_t start,
  const uint16_t end
) {
   // 10: shl
  {
        for (uint32_t i = start; i < end; ++i) {
            uint64_t val = databuf[*rptr++];
            uint64_t shift = databuf[*rptr++];
            output_buffer[i] = val << shift;
        }
  }
}

#if defined(__GNUC__) || defined(__clang__)
#define ALWAYS_INLINE __attribute__((always_inline)) inline
#else
#define ALWAYS_INLINE inline
#endif

ALWAYS_INLINE void shr_loop(
  //  const uint64_t* __restrict& rptr,
  const uint32_t* __restrict& rptr,
  const uint64_t* __restrict databuf,
  uint64_t* __restrict output_buffer,
  const uint16_t start,
  const uint16_t end
) {
   // 11: shr
  {
        for (uint32_t i = start; i < end; ++i) {
            uint64_t val = databuf[*rptr++];
            uint64_t shift = databuf[*rptr++];
            output_buffer[i] = val >> shift;
        }
  }
}

#if defined(__GNUC__) || defined(__clang__)
#define ALWAYS_INLINE __attribute__((always_inline)) inline
#else
#define ALWAYS_INLINE inline
#endif

ALWAYS_INLINE void shrS_loop(
  //  const uint64_t* __restrict& rptr,
  const uint32_t* __restrict& rptr,
  const uint64_t* __restrict databuf,
  uint64_t* __restrict output_buffer,
  const uint16_t start,
  const uint16_t end
) {
   // 12: shrS
  {
        for (uint32_t i = start; i < end; ++i) {
            int64_t val = static_cast<int64_t>(databuf[*rptr++]);
            uint64_t shift = databuf[*rptr++];
            // Perform arithmetic right shift
            output_buffer[i] = static_cast<uint64_t>(val >> shift);
        }
  }
}

#if defined(__GNUC__) || defined(__clang__)
#define ALWAYS_INLINE __attribute__((always_inline)) inline
#else
#define ALWAYS_INLINE inline
#endif

ALWAYS_INLINE void and_loop(
  //  const uint64_t* __restrict& rptr,
  const uint32_t* __restrict& rptr,
  const uint64_t* __restrict databuf,
  uint64_t* __restrict output_buffer,
  const uint16_t start,
  const uint16_t end
) {
   // 13: and
  {
        uint32_t i = start;
        uint32_t limit = end - ((end - start) & 7);

        for (; i < limit; i += 8) {
            uint64_t lhs0 = databuf[*rptr++];
            uint64_t rhs0 = databuf[*rptr++];
            output_buffer[i] = lhs0 & rhs0;

            uint64_t lhs1 = databuf[*rptr++];
            uint64_t rhs1 = databuf[*rptr++];
            output_buffer[i + 1] = lhs1 & rhs1;

            uint64_t lhs2 = databuf[*rptr++];
            uint64_t rhs2 = databuf[*rptr++];
            output_buffer[i + 2] = lhs2 & rhs2;

            uint64_t lhs3 = databuf[*rptr++];
            uint64_t rhs3 = databuf[*rptr++];
            output_buffer[i + 3] = lhs3 & rhs3;

            uint64_t lhs4 = databuf[*rptr++];
            uint64_t rhs4 = databuf[*rptr++];
            output_buffer[i + 4] = lhs4 & rhs4;

            uint64_t lhs5 = databuf[*rptr++];
            uint64_t rhs5 = databuf[*rptr++];
            output_buffer[i + 5] = lhs5 & rhs5;

            uint64_t lhs6 = databuf[*rptr++];
            uint64_t rhs6 = databuf[*rptr++];
            output_buffer[i + 6] = lhs6 & rhs6;

            uint64_t lhs7 = databuf[*rptr++];
            uint64_t rhs7 = databuf[*rptr++];
            output_buffer[i + 7] = lhs7 & rhs7;
        }

        // cleanup loop
        for (; i < end; ++i) {
            uint64_t lhs = databuf[*rptr++];
            uint64_t rhs = databuf[*rptr++];
            output_buffer[i] = lhs & rhs;
        }
  }
}

#if defined(__GNUC__) || defined(__clang__)
#define ALWAYS_INLINE __attribute__((always_inline)) inline
#else
#define ALWAYS_INLINE inline
#endif

ALWAYS_INLINE void or_loop(
  //  const uint64_t* __restrict& rptr,
  const uint32_t* __restrict& rptr,
  const uint64_t* __restrict databuf,
  uint64_t* __restrict output_buffer,
  const uint16_t start,
  const uint16_t end
) {
   // 14: or
  {
    //    for (uint32_t i = start; i < end; ++i) {
    //        uint64_t lhs = databuf[*rptr++];
    //        uint64_t rhs = databuf[*rptr++];
    //        output_buffer[i] = lhs | rhs;
    //    }
        uint32_t i = start;
        uint32_t limit = end - ((end - start) & 7);

        for (; i < limit; i += 8) {
            uint64_t lhs0 = databuf[*rptr++];
            uint64_t rhs0 = databuf[*rptr++];
            output_buffer[i] = lhs0 | rhs0;

            uint64_t lhs1 = databuf[*rptr++];
            uint64_t rhs1 = databuf[*rptr++];
            output_buffer[i + 1] = lhs1 | rhs1;

            uint64_t lhs2 = databuf[*rptr++];
            uint64_t rhs2 = databuf[*rptr++];
            output_buffer[i + 2] = lhs2 | rhs2;

            uint64_t lhs3 = databuf[*rptr++];
            uint64_t rhs3 = databuf[*rptr++];
            output_buffer[i + 3] = lhs3 | rhs3;

            uint64_t lhs4 = databuf[*rptr++];
            uint64_t rhs4 = databuf[*rptr++];
            output_buffer[i + 4] = lhs4 | rhs4;

            uint64_t lhs5 = databuf[*rptr++];
            uint64_t rhs5 = databuf[*rptr++];
            output_buffer[i + 5] = lhs5 | rhs5;

            uint64_t lhs6 = databuf[*rptr++];
            uint64_t rhs6 = databuf[*rptr++];
            output_buffer[i + 6] = lhs6 | rhs6;

            uint64_t lhs7 = databuf[*rptr++];
            uint64_t rhs7 = databuf[*rptr++];
            output_buffer[i + 7] = lhs7 | rhs7;
        }

        // cleanup loop
        for (; i < end; ++i) {
            uint64_t lhs = databuf[*rptr++];
            uint64_t rhs = databuf[*rptr++];
            output_buffer[i] = lhs | rhs;
        }
  }
}

#if defined(__GNUC__) || defined(__clang__)
#define ALWAYS_INLINE __attribute__((always_inline)) inline
#else
#define ALWAYS_INLINE inline
#endif

ALWAYS_INLINE void xor_loop(
  //  const uint64_t* __restrict& rptr,
  const uint32_t* __restrict& rptr,
  const uint64_t* __restrict databuf,
  uint64_t* __restrict output_buffer,
  const uint16_t start,
  const uint16_t end
) {
   // 15: xor
  {
    //    for (uint32_t i = start; i < end; ++i) {
    //        uint64_t lhs = databuf[*rptr++];
    //        uint64_t rhs = databuf[*rptr++];
    //        output_buffer[i] = lhs ^ rhs;
    //    }
        uint32_t i = start;
        uint32_t limit = end - ((end - start) & 7);

        for (; i < limit; i += 8) {
            uint64_t lhs0 = databuf[*rptr++];
            uint64_t rhs0 = databuf[*rptr++];
            output_buffer[i] = lhs0 ^ rhs0;

            uint64_t lhs1 = databuf[*rptr++];
            uint64_t rhs1 = databuf[*rptr++];
            output_buffer[i + 1] = lhs1 ^ rhs1;

            uint64_t lhs2 = databuf[*rptr++];
            uint64_t rhs2 = databuf[*rptr++];
            output_buffer[i + 2] = lhs2 ^ rhs2;

            uint64_t lhs3 = databuf[*rptr++];
            uint64_t rhs3 = databuf[*rptr++];
            output_buffer[i + 3] = lhs3 ^ rhs3;

            uint64_t lhs4 = databuf[*rptr++];
            uint64_t rhs4 = databuf[*rptr++];
            output_buffer[i + 4] = lhs4 ^ rhs4;

            uint64_t lhs5 = databuf[*rptr++];
            uint64_t rhs5 = databuf[*rptr++];
            output_buffer[i + 5] = lhs5 ^ rhs5;

            uint64_t lhs6 = databuf[*rptr++];
            uint64_t rhs6 = databuf[*rptr++];
            output_buffer[i + 6] = lhs6 ^ rhs6;

            uint64_t lhs7 = databuf[*rptr++];
            uint64_t rhs7 = databuf[*rptr++];
            output_buffer[i + 7] = lhs7 ^ rhs7;
        }

        // cleanup loop
        for (; i < end; ++i) {
            uint64_t lhs = databuf[*rptr++];
            uint64_t rhs = databuf[*rptr++];
            output_buffer[i] = lhs ^ rhs;
        }
  }
}

#if defined(__GNUC__) || defined(__clang__)
#define ALWAYS_INLINE __attribute__((always_inline)) inline
#else
#define ALWAYS_INLINE inline
#endif

ALWAYS_INLINE void xorr_loop(
  //  const uint64_t* __restrict& rptr,
  const uint32_t* __restrict& rptr,
  const uint64_t* __restrict databuf,
  uint64_t* __restrict output_buffer,
  const uint16_t start,
  const uint16_t end
) {
   // 16: xorr
  {
       for (uint32_t i = start; i < end; ++i) {
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
                       output_buffer[i] = static_cast<uint64_t>(ones & 1);
       }
  }
}

#if defined(__GNUC__) || defined(__clang__)
#define ALWAYS_INLINE __attribute__((always_inline)) inline
#else
#define ALWAYS_INLINE inline
#endif

ALWAYS_INLINE void cat_loop(
  //  const uint64_t* __restrict& rptr,
  const uint32_t* __restrict& rptr,
  const uint64_t* __restrict databuf,
  uint64_t* __restrict output_buffer,
  const uint16_t start,
  const uint16_t end
) {
   // 17: cat
  {
    //    for (uint32_t i = start; i < end; ++i) {
    //        uint16_t x = *rptr++;
    //        uint16_t y = *rptr++;
    //        uint16_t z = *rptr++;
    //        output_buffer[i] = (databuf[x] << z) | databuf[y];
    //    }
         uint32_t i = start;
        for (; i + 7 < end; i += 8) {
            {
                size_t x = *rptr++;
                size_t y = *rptr++;
                uint32_t z = *rptr++;
                output_buffer[i] = (databuf[x] << z) | databuf[y];
            }
            {
                size_t x = *rptr++;
                size_t y = *rptr++;
                uint32_t z = *rptr++;
                output_buffer[i + 1] = (databuf[x] << z) | databuf[y];
            }
            {
                size_t x = *rptr++;
                size_t y = *rptr++;
                uint32_t z = *rptr++;
                output_buffer[i + 2] = (databuf[x] << z) | databuf[y];
            }
            {
                size_t x = *rptr++;
                size_t y = *rptr++;
                uint32_t z = *rptr++;
                output_buffer[i + 3] = (databuf[x] << z) | databuf[y];
            }
            {
                size_t x = *rptr++;
                size_t y = *rptr++;
                uint32_t z = *rptr++;
                output_buffer[i + 4] = (databuf[x] << z) | databuf[y];
            }
            {
                size_t x = *rptr++;
                size_t y = *rptr++;
                uint32_t z = *rptr++;
                output_buffer[i + 5] = (databuf[x] << z) | databuf[y];
            }
            {
                size_t x = *rptr++;
                size_t y = *rptr++;
                uint32_t z = *rptr++;
                output_buffer[i + 6] = (databuf[x] << z) | databuf[y];
            }
            {
                size_t x = *rptr++;
                size_t y = *rptr++;
                uint32_t z = *rptr++;
                output_buffer[i + 7] = (databuf[x] << z) | databuf[y];
            }
        }

        // Tail case for leftover iterations
        for (; i < end; ++i) {
            size_t x = *rptr++;
            size_t y = *rptr++;
            uint32_t z = *rptr++;
            output_buffer[i] = (databuf[x] << z) | databuf[y];
        }
  }
}

#if defined(__GNUC__) || defined(__clang__)
#define ALWAYS_INLINE __attribute__((always_inline)) inline
#else
#define ALWAYS_INLINE inline
#endif

ALWAYS_INLINE void bits_loop(
  //  const uint64_t* __restrict& rptr,
  const uint32_t* __restrict& rptr,
  const uint64_t* __restrict databuf,
  uint64_t* __restrict output_buffer,
  const uint16_t start,
  const uint16_t end
) {
   // 18: bits
  {
        uint32_t i = start;
        for (; i + 7 < end; i += 8) {
            {
                size_t x = *rptr++;
                uint32_t y = *rptr++;
                uint64_t z = databuf[*rptr++];
                output_buffer[i] = (databuf[x] >> y) & z;
            }
            {
                size_t x = *rptr++;
                uint32_t y = *rptr++;
                uint64_t z = databuf[*rptr++];
                output_buffer[i + 1] = (databuf[x] >> y) & z;
            }
            {
                size_t x = *rptr++;
                uint32_t y = *rptr++;
                uint64_t z = databuf[*rptr++];
                output_buffer[i + 2] = (databuf[x] >> y) & z;
            }
            {
                size_t x = *rptr++;
                uint32_t y = *rptr++;
                uint64_t z = databuf[*rptr++];
                output_buffer[i + 3] = (databuf[x] >> y) & z;
            }
            {
                size_t x = *rptr++;
                uint32_t y = *rptr++;
                uint64_t z = databuf[*rptr++];
                output_buffer[i + 4] = (databuf[x] >> y) & z;
            }
            {
                size_t x = *rptr++;
                uint32_t y = *rptr++;
                uint64_t z = databuf[*rptr++];
                output_buffer[i + 5] = (databuf[x] >> y) & z;
            }
            {
                size_t x = *rptr++;
                uint32_t y = *rptr++;
                uint64_t z = databuf[*rptr++];
                output_buffer[i + 6] = (databuf[x] >> y) & z;
            }
            {
                size_t x = *rptr++;
                uint32_t y = *rptr++;
                uint64_t z = databuf[*rptr++];
                output_buffer[i + 7] = (databuf[x] >> y) & z;
            }
        }

        // Remainder loop for leftover iterations
        for (; i < end; ++i) {
            size_t x = *rptr++;
            uint32_t y = *rptr++;
            uint64_t z = databuf[*rptr++];
            output_buffer[i] = (databuf[x] >> y) & z;
        }
  }
}

#if defined(__GNUC__) || defined(__clang__)
#define ALWAYS_INLINE __attribute__((always_inline)) inline
#else
#define ALWAYS_INLINE inline
#endif

ALWAYS_INLINE void mux_loop(
  //  const uint64_t* __restrict& rptr,
  const uint32_t* __restrict& rptr,
  const uint64_t* __restrict databuf,
  uint64_t* __restrict output_buffer,
  const uint16_t start,
  const uint16_t end
) {
   // 19: mux
  {
    //    for (uint32_t i = start; i < end; ++i) {
    //        const uint16_t* x = rptr++;
    //        const uint16_t* y = rptr++;
    //        const uint16_t* z = rptr++;
    //        output_buffer[i] = databuf[*x] ? databuf[*y] : databuf[*z];
    //    }
        uint32_t i = start;
        uint32_t limit = end - ((end - start) & 7);  // ensure multiple of 4

        for (; i < limit; i += 8) {
            const uint32_t* x0 = rptr++;
            const uint32_t* y0 = rptr++;
            const uint32_t* z0 = rptr++;
            output_buffer[i] = databuf[*x0] ? databuf[*y0] : databuf[*z0];

            const uint32_t* x1 = rptr++;
            const uint32_t* y1 = rptr++;
            const uint32_t* z1 = rptr++;
            output_buffer[i + 1] = databuf[*x1] ? databuf[*y1] : databuf[*z1];

            const uint32_t* x2 = rptr++;
            const uint32_t* y2 = rptr++;
            const uint32_t* z2 = rptr++;
            output_buffer[i + 2] = databuf[*x2] ? databuf[*y2] : databuf[*z2];

            const uint32_t* x3 = rptr++;
            const uint32_t* y3 = rptr++;
            const uint32_t* z3 = rptr++;
            output_buffer[i + 3] = databuf[*x3] ? databuf[*y3] : databuf[*z3];

            const uint32_t* x4 = rptr++;
            const uint32_t* y4 = rptr++;
            const uint32_t* z4 = rptr++;
            output_buffer[i + 4] = databuf[*x4] ? databuf[*y4] : databuf[*z4];

            const uint32_t* x5 = rptr++;
            const uint32_t* y5 = rptr++;
            const uint32_t* z5 = rptr++;
            output_buffer[i + 5] = databuf[*x5] ? databuf[*y5] : databuf[*z5];

            const uint32_t* x6 = rptr++;
            const uint32_t* y6 = rptr++;
            const uint32_t* z6 = rptr++;
            output_buffer[i + 6] = databuf[*x6] ? databuf[*y6] : databuf[*z6];

            const uint32_t* x7 = rptr++;
            const uint32_t* y7 = rptr++;
            const uint32_t* z7 = rptr++;
            output_buffer[i + 7] = databuf[*x7] ? databuf[*y7] : databuf[*z7];
        }

        // cleanup
        for (; i < end; ++i) {
            const uint32_t* x = rptr++;
            const uint32_t* y = rptr++;
            const uint32_t* z = rptr++;
            output_buffer[i] = databuf[*x] ? databuf[*y] : databuf[*z];
        }
  }
}

#if defined(__GNUC__) || defined(__clang__)
#define ALWAYS_INLINE __attribute__((always_inline)) inline
#else
#define ALWAYS_INLINE inline
#endif

ALWAYS_INLINE void assign_loop(
  //  const uint64_t* __restrict& rptr,
  const uint32_t* __restrict& rptr,
  const uint64_t* __restrict databuf,
  uint64_t* __restrict output_buffer,
  const uint16_t start,
  const uint16_t end
) {
   // 20: assign
  {
       for (uint32_t i = start; i < end; ++i) {
           output_buffer[i] = databuf[*rptr++];
       }
    // uint32_t i = start;

    // for (; i + 3 < end; i += 4) {
    //     output_buffer[i]     = databuf[*rptr++];
    //     output_buffer[i + 1] = databuf[*rptr++];
    //     output_buffer[i + 2] = databuf[*rptr++];
    //     output_buffer[i + 3] = databuf[*rptr++];
    // }

    // // handle remaining elements (0 to 3)
    // for (; i < end; ++i) {
    //     output_buffer[i] = databuf[*rptr++];
    // }
  }
}

#if defined(__GNUC__) || defined(__clang__)
#define ALWAYS_INLINE __attribute__((always_inline)) inline
#else
#define ALWAYS_INLINE inline
#endif

ALWAYS_INLINE void stop_loop(
  //  const uint64_t* __restrict& rptr,
  const uint32_t* __restrict& rptr,
  const uint64_t* __restrict databuf,
  uint64_t* __restrict output_buffer,
  const uint16_t start,
  const uint16_t end
) {
   // 21: stop
  {
    //    for (uint32_t i = start; i < end; ++i) {
    //        rptr++;
    //    }
    rptr += (end-start);
  }
}

#if defined(__GNUC__) || defined(__clang__)
#define ALWAYS_INLINE __attribute__((always_inline)) inline
#else
#define ALWAYS_INLINE inline
#endif

ALWAYS_INLINE void orchain_loop(
  //  const uint64_t* __restrict& rptr,
  const uint32_t* __restrict& rptr,
  const uint64_t* __restrict databuf,
  uint64_t* __restrict output_buffer,
  const uint16_t start,
  const uint16_t end
) {
   // 22: orchain
  {
       for (uint32_t i = start; i < end; ++i) {
           // size_t x = *rptr++;
           uint16_t x = *rptr++;
        //    std::cout << "or chain: " << x << std::endl;
           uint64_t acc = 0;
           for (size_t j = 0; j < x; ++j) {
               acc |= databuf[*rptr++];
           }
           output_buffer[i] = acc;
       }
  }
}

#if defined(__GNUC__) || defined(__clang__)
#define ALWAYS_INLINE __attribute__((always_inline)) inline
#else
#define ALWAYS_INLINE inline
#endif

ALWAYS_INLINE void xorchain_loop(
  //  const uint64_t* __restrict& rptr,
  const uint32_t* __restrict& rptr,
  const uint64_t* __restrict databuf,
  uint64_t* __restrict output_buffer,
  const uint16_t start,
  const uint16_t end
) {
   // 23: xorchain
  {
    for (uint32_t i = start; i < end; ++i) {
        uint16_t x = *rptr++;
        // std::cout << "xor chain: " << x << std::endl;
        uint64_t acc = databuf[*rptr++];
        for (size_t j = 0; j < x-1; ++j) {
            acc ^= databuf[*rptr++];
        }
        output_buffer[i] = acc;
    }
  }
}


#if defined(__GNUC__) || defined(__clang__)
#define ALWAYS_INLINE __attribute__((always_inline)) inline
#else
#define ALWAYS_INLINE inline
#endif

ALWAYS_INLINE void muxchain_loop(
  //  const uint64_t* __restrict& rptr,
  const uint32_t* __restrict& rptr,
  const uint64_t* __restrict databuf,
  uint64_t* __restrict output_buffer,
  const uint32_t* __restrict muxJT,
  const uint16_t start,
  const uint16_t end
) {
   // 24: muxchain
  {
       for (uint32_t i = start; i < end; ++i) {
            size_t x = *rptr++;
            size_t x_mask = *rptr++;
            size_t y = *rptr++;
            size_t tmp = (databuf[x] & x_mask) + y;
            output_buffer[i] = databuf[muxJT[tmp]];
       }
  }
}

#if defined(__GNUC__) || defined(__clang__)
#define ALWAYS_INLINE __attribute__((always_inline)) inline
#else
#define ALWAYS_INLINE inline
#endif

ALWAYS_INLINE void memw_loop(
  //  const uint64_t* __restrict& rptr,
  const uint32_t* __restrict& rptr,
  const uint64_t* __restrict databuf,
  uint64_t* __restrict output_buffer,
  uint64_t* __restrict memory_64,
  uint8_t* __restrict memory_8,
  const uint16_t start,
  const uint16_t end
) {
   // 26: memw
  {
        for (uint32_t i = start; i < end; ++i) {
            size_t u = *rptr++;
            size_t v = *rptr++;
            size_t w = *rptr++;
            size_t x = *rptr++;
            size_t y_mask = *rptr++;

            size_t y = *rptr++;
            size_t z = *rptr++;
            size_t tmp = x + (static_cast<size_t>(databuf[y]) & y_mask);
            if (databuf[u] != 0 && databuf[v] != 0) {
                if (w == 0) {
                    memory_64[tmp] = databuf[z];
                } else {
                    memory_8[tmp] = static_cast<uint8_t>(databuf[z]);
                }
            }
        }
  }
}

#if defined(__GNUC__) || defined(__clang__)
#define ALWAYS_INLINE __attribute__((always_inline)) inline
#else
#define ALWAYS_INLINE inline
#endif

ALWAYS_INLINE void memr_loop(
  //  const uint64_t* __restrict& rptr,
  const uint32_t* __restrict& rptr,
  const uint64_t* __restrict databuf,
  uint64_t* __restrict output_buffer,
  uint64_t* __restrict memory_64,
  uint8_t* __restrict memory_8,
  const uint16_t start,
  const uint16_t end
) {
   // 27: memr
  {
        for (uint32_t i = start; i < end; ++i) {
            size_t x = *rptr++;
            size_t y = *rptr++;
            size_t z_mask = *rptr++;

            size_t z = *rptr++;
            size_t tmp = y + (static_cast<size_t>(databuf[z]) & z_mask);
            if (x == 0) {
                output_buffer[i] = memory_64[tmp];
            } else {
                output_buffer[i] = static_cast<uint64_t>(memory_8[tmp]);
            }
        }
  }
}

#if defined(__GNUC__) || defined(__clang__)
#define ALWAYS_INLINE __attribute__((always_inline)) inline
#else
#define ALWAYS_INLINE inline
#endif
ALWAYS_INLINE void asSInt_loop(
   //  const uint64_t* __restrict& rptr,
   const uint32_t* __restrict& rptr,
   const uint64_t* __restrict databuf,
   uint64_t* __restrict output_buffer,
   const uint16_t start,
   const uint16_t end
) {
    // 28: sign extend
   {
       for (uint32_t i = start; i < end; ++i) {
            size_t x = *rptr++;
            size_t y = *rptr++;
            uint64_t shift = y;
            uint64_t val = databuf[x];
            uint64_t extend_bit = (val >> shift) & 1;
            if (extend_bit == 1) {
                uint64_t nonzero_bit = 64 - (shift + 1);
                uint64_t extend_mask = ((1ULL << nonzero_bit) - 1) << (shift + 1);
                output_buffer[i] = val | extend_mask;
            } else {
                output_buffer[i] = val;
            }
        }
   }
}

#if defined(__GNUC__) || defined(__clang__)
#define ALWAYS_INLINE __attribute__((always_inline)) inline
#else
#define ALWAYS_INLINE inline
#endif
 ALWAYS_INLINE uint16_t dshl_loop(
   const uint32_t* __restrict& rptr,
   const uint64_t* __restrict databuf,
   uint64_t* __restrict output_buffer,
   std::vector<uint64_t>& scratch1,
   std::vector<uint64_t>& scratch2,
   const uint16_t start,
   uint16_t out_count,
   const uint16_t end
 ) {
     // 33: dshl
    {
        for (size_t i = out_count; i < end; ++i) {
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
                    output_buffer[out_count + w] = (w < n) ? scratch1[w] : 0ULL;
                }
                out_count += min_words;
                continue;
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
                output_buffer[out_count + w] = scratch2[w];
            }

            out_count += min_words;
        }
    }
    return out_count;
 }


#if defined(__GNUC__) || defined(__clang__)
#define ALWAYS_INLINE __attribute__((always_inline)) inline
#else
#define ALWAYS_INLINE inline
#endif
ALWAYS_INLINE uint16_t dshr_loop(
   const uint32_t* __restrict& rptr,
   const uint64_t* __restrict databuf,
   uint64_t* __restrict output_buffer,
   std::vector<uint64_t>& scratch1,
   const uint16_t start,
   uint16_t out_count,
   const uint16_t end
) {
   // 34: dshr
   {
        for (uint32_t i = start; i < end; ++i) {
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
                output_buffer[out_count + w] = val;
            }
            out_count += num_words;
        }
   }
   return out_count;
}

#if defined(__GNUC__) || defined(__clang__)
#define ALWAYS_INLINE __attribute__((always_inline)) inline
#else
#define ALWAYS_INLINE inline
#endif
ALWAYS_INLINE uint16_t dshrS_loop(
   const uint32_t* __restrict& rptr,
   const uint64_t* __restrict databuf,
   uint64_t* __restrict output_buffer,
   std::vector<uint64_t>& scratch1,
   const uint16_t start,
   uint16_t out_count,
   const uint16_t end
) {
   // 35: dshrS
   {
        for (uint32_t i = start; i < end; ++i) {
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
                output_buffer[out_count + w] = val;
            }
            out_count += num_words;
        }
   }
   return out_count;
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
    std::vector<uint32_t> new_s = loadVectorFromJson<uint32_t>(JSONPATH+prefix+"_sptr.json");
    std::vector<uint32_t> new_r = loadVectorFromJson<uint32_t>(JSONPATH+prefix+"_rptr.json");

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
    // std::cout << "here?" << std::endl;

    std::vector<uint64_t> scratch1;
    std::vector<uint64_t> scratch2;

    uint64_t* __restrict databuf_ptr = databuf.data();
    uint64_t* __restrict outbuf_ptr = output_buffer.data(); 
    uint64_t* __restrict memory_64_ptr = memory_64.data();
    uint8_t* __restrict memory_8_ptr = memory_8.data();
    const uint32_t* __restrict muxJT_ptr = muxJT.data();
    std::uint16_t out_count;
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
        // printf("Cycle: %lu\n", cycle);
        const uint32_t* __restrict rptr = new_r.data();
        const uint32_t* __restrict s_ptr = new_s.data();

        rptr = (const uint32_t*)__builtin_assume_aligned(rptr, 64);
        s_ptr = (const uint32_t*)__builtin_assume_aligned(s_ptr, 64);

        if (done_reset && (dtm->done() || io_success)) {
            break;
        }
        done_reset = !(cycle < (async_reset_cycles + sync_reset_cycles));
        io_success = databuf[dtm_index_map.at("SimDTM$$inst.exit")] == 1;
        databuf[reg_index_map.at("reset")[0]] = done_reset ? 0 : 1;
        
// INSERT_INNER_DIMS_HERE

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
