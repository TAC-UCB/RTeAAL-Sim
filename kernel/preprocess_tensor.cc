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

#include <sys/ioctl.h>
#include <sys/syscall.h>
#include <unistd.h>
#include <cstring>
#include <cerrno>
#include <csignal>

#include "json.hpp"

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

// Fiber structure
struct Fiber;
using Payload = std::variant<int, Fiber>;

struct Fiber {
    std::vector<uint32_t> coords;
    std::vector<Payload> payloads;
};


// Recursive collection
template <typename CoordT, typename PayloadT>
size_t collect_leaf_data(const Fiber& fiber, std::vector<CoordT>& leaf_coords, std::vector<PayloadT>& leaf_payloads) {
    static_assert((std::is_same_v<CoordT, uint8_t> || std::is_same_v<CoordT, uint16_t> || std::is_same_v<CoordT, uint32_t>),
                  "CoordT must be uint8_t or uint16_t or uint32_t");
    static_assert((std::is_same_v<PayloadT, uint8_t> || std::is_same_v<PayloadT, uint16_t> || std::is_same_v<PayloadT, uint32_t>),
                  "PayloadT must be uint8_t or uint16_t or uint32_t");

    size_t max_leaf_size = 0;
    for (size_t i = 0; i < fiber.coords.size(); ++i) {
        const auto& payload = fiber.payloads[i];
        if (std::holds_alternative<int>(payload)) {
            leaf_coords.push_back(static_cast<CoordT>(fiber.coords[i]));
            leaf_payloads.push_back(static_cast<PayloadT>(std::get<int>(payload)));

            size_t leaf_count = 1;
            size_t j = i + 1;
            while (j < fiber.payloads.size() && std::holds_alternative<int>(fiber.payloads[j])) {
                ++leaf_count;
                ++j;
            }
            max_leaf_size = std::max(max_leaf_size, leaf_count);
        } else {
            size_t child_max = collect_leaf_data<CoordT, PayloadT>(std::get<Fiber>(payload), leaf_coords, leaf_payloads);
            max_leaf_size = std::max(max_leaf_size, child_max);
        }
    }
    return max_leaf_size;
}

std::vector<size_t> compute_start_and_end_indices(const YAML::Node& tensor_root) {
    std::vector<size_t> start_indices;
    size_t current_offset = 0;

    for (const auto& item : tensor_root) {
        const auto& fiber = item["fiber"];
        const auto& payloads = fiber["payloads"];

        start_indices.push_back(current_offset);

        if (payloads.IsSequence()) {
            for (const auto& subfiber : payloads) {
                if (subfiber["fiber"] && subfiber["fiber"]["coords"]) {
                    const auto& coords = subfiber["fiber"]["coords"];
                    current_offset += coords.size();
                    start_indices.push_back(current_offset);
                }
            }
        }
    }

    return start_indices;
}

// Recursive YAML loader
Fiber parse_fiber(const YAML::Node& node) {
    Fiber fiber;
    const auto& coords_node = node["coords"];
    const auto& payloads_node = node["payloads"];

    if (!coords_node || !payloads_node) {
        throw std::runtime_error("Missing coords or payloads in fiber.");
    }

    // Load coords
    for (const auto& c : coords_node) {
        fiber.coords.push_back(c.as<uint32_t>());
    }

    // Load payloads
    for (const auto& p : payloads_node) {
        if (p.IsMap() && p["fiber"]) {
            // Recursive fiber
            fiber.payloads.push_back(parse_fiber(p["fiber"]));
        } else if (p.IsScalar()) {
            // Leaf integer payload
            fiber.payloads.push_back(p.as<int>());
        } else {
            throw std::runtime_error("Unexpected payload structure.");
        }
    }

    return fiber;
}

size_t get_shape_for_rank_id(const YAML::Node& tensor, const std::string& target_id) {
    const auto& rank_ids = tensor["rank_ids"];
    const auto& shape = tensor["shape"];

    if (!rank_ids || !shape || rank_ids.size() != shape.size()) {
        std::cerr << "Malformed tensor metadata.\n";
        return -1;
    }

    for (std::size_t i = 0; i < rank_ids.size(); ++i) {
        if (rank_ids[i].as<std::string>() == target_id) {
            return shape[i].as<int>();
        }
    }
    return -1;
}

std::vector<uint64_t> load_txt_file(const std::string& filename, const std::string& prefix) {
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

// ins_map, inr_map, is_map are passed by reference and will be mutated/filled.
// ON, n, s, r, databuf are inputs.
void fill_maps(
    const std::vector<size_t>& layer_pointer,
    const std::vector<uint8_t>& n,
    const std::vector<uint32_t>& s,
    const std::vector<uint32_t>& r,
    const std::vector<uint64_t>& databuf,
    std::map<uint16_t, std::map<uint8_t, std::vector<uint32_t>>>& ins_map,
    std::map<uint16_t, std::map<uint8_t, std::vector<uint32_t>>>& inr_map,
    std::unordered_map<uint16_t, std::vector<uint32_t>>& is_map
) {
    size_t r_iter = 0;

    for (size_t i = 0; i + 1 < layer_pointer.size(); ++i) {
        // for (size_t i = 0; i + 1 < 87; ++i) {
        std::map<uint8_t, std::vector<uint32_t>> ns_map;
        std::map<uint8_t, std::vector<uint32_t>> ns_map2;
        // std::map<uint8_t, std::vector<size_t>> nr_map;
        std::map<uint8_t, std::vector<uint32_t>> nr_map;
        std::vector<uint32_t> s_vec;
        bool up = true;
        size_t dshc = 0;

        for (size_t j = layer_pointer[i]; j < layer_pointer[i + 1]; ++j) {
            ns_map2[n[j]].push_back(s[j]);

            size_t opcode = n[j];

            switch (opcode) {
                // case 0: case 1: {
                //     nr_map[opcode].push_back(databuf[r[r_iter++]]);
                //     nr_map[opcode].push_back(r[r_iter++]);
                //     nr_map[opcode].push_back(r[r_iter++]);
                //     ns_map[opcode].push_back(s[j]);
                //     break;
                // }
                case 0: case 1: case 2: {
                    nr_map[opcode].push_back(databuf[r[r_iter++]]);
                    nr_map[opcode].push_back(r[r_iter++]);
                    nr_map[opcode].push_back(r[r_iter++]);
                    ns_map[opcode].push_back(s[j]);
                    break;
                }
                // case 2: case 3: case 4: case 5: case 6: case 7: case 8:
                // case 9: case 10: case 11: case 12: case 13: case 14: case 15: case 28: case 29: case 30: {
                //     nr_map[opcode].push_back(r[r_iter++]);
                //     nr_map[opcode].push_back(r[r_iter++]);
                //     ns_map[opcode].push_back(s[j]);
                //     break;
                // }
                case 4: case 5: case 6: case 7: case 8:
                case 9: case 10: case 11: case 12: case 13: case 14: case 15: {
                    nr_map[opcode].push_back(r[r_iter++]);
                    nr_map[opcode].push_back(r[r_iter++]);
                    ns_map[opcode].push_back(s[j]);
                    break;
                }
                case 24: {
                    nr_map[opcode].push_back(r[r_iter++]);
                    nr_map[opcode].push_back(databuf[r[r_iter++]]);
                    nr_map[opcode].push_back(databuf[r[r_iter++]]);
                    ns_map[opcode].push_back(s[j]);
                    break;
                }
                // case 27: {
                //     nr_map[opcode].push_back(r[r_iter++]);
                //     nr_map[opcode].push_back(databuf[r[r_iter++]]);
                //     ns_map[opcode].push_back(s[j]);
                //     break;
                // }
                 case 3: {
                    nr_map[opcode].push_back(r[r_iter++]);
                    nr_map[opcode].push_back(databuf[r[r_iter++]]);
                    ns_map[opcode].push_back(s[j]);
                    break;
                }
                // case 16: case 20: case 21: {
                //     nr_map[opcode].push_back(r[r_iter++]);
                //     ns_map[opcode].push_back(s[j]);
                //     break;
                // }
                case 16: case 20: {
                    nr_map[opcode].push_back(r[r_iter++]);
                    ns_map[opcode].push_back(s[j]);
                    break;
                }
                case 19: {
                    nr_map[opcode].push_back(r[r_iter++]);
                    nr_map[opcode].push_back(r[r_iter++]);
                    nr_map[opcode].push_back(r[r_iter++]);
                    ns_map[opcode].push_back(s[j]);
                    break;
                }
                case 18: {
                    nr_map[opcode].push_back(r[r_iter++]);
                    nr_map[opcode].push_back(databuf[r[r_iter++]]);
                    nr_map[opcode].push_back(r[r_iter++]);
                    ns_map[opcode].push_back(s[j]);
                    break;
                }
                case 17: {
                    nr_map[opcode].push_back(r[r_iter++]);
                    nr_map[opcode].push_back(r[r_iter++]);
                    nr_map[opcode].push_back(databuf[r[r_iter++]]);
                    ns_map[opcode].push_back(s[j]);
                    break;
                }
                // case 25: {
                //     nr_map[opcode].push_back(r[r_iter++]);
                //     nr_map[opcode].push_back(r[r_iter++]);
                //     nr_map[opcode].push_back(databuf[r[r_iter++]]);
                //     nr_map[opcode].push_back(r[r_iter++]);
                //     nr_map[opcode].push_back(r[r_iter++]);
                //     nr_map[opcode].push_back(r[r_iter++]);
                //     nr_map[opcode].push_back(r[r_iter++]);
                //     ns_map[opcode].push_back(s[j]);
                //     break;
                // }
                 case 28: {
                    nr_map[opcode].push_back(r[r_iter++]);
                    nr_map[opcode].push_back(r[r_iter++]);
                    nr_map[opcode].push_back(databuf[r[r_iter++]]);
                    nr_map[opcode].push_back(databuf[r[r_iter++]]);
                    nr_map[opcode].push_back(databuf[r[r_iter++]]);
                    nr_map[opcode].push_back(r[r_iter++]);
                    nr_map[opcode].push_back(r[r_iter++]);
                    ns_map[opcode].push_back(s[j]);
                    break;
                }
                // case 26: {
                //     nr_map[opcode].push_back(databuf[r[r_iter++]]);
                //     nr_map[opcode].push_back(r[r_iter++]);
                //     nr_map[opcode].push_back(r[r_iter++]);
                //     nr_map[opcode].push_back(r[r_iter++]);
                //     ns_map[opcode].push_back(s[j]);
                //     break;
                // }
                case 21: {
                    nr_map[opcode].push_back(databuf[r[r_iter++]]);
                    nr_map[opcode].push_back(databuf[r[r_iter++]]);
                    nr_map[opcode].push_back(databuf[r[r_iter++]]);
                    nr_map[opcode].push_back(r[r_iter++]);
                    ns_map[opcode].push_back(s[j]);
                    break;
                }
                case 22: case 23: {
                    uint32_t input_len_idx = r[r_iter++];
                    size_t input_len = static_cast<size_t>(databuf[input_len_idx]);
                    nr_map[opcode].push_back(input_len);
                    for (size_t inp = 0; inp < input_len; ++inp) {
                        nr_map[opcode].push_back(r[r_iter++]);
                    }
                    ns_map[opcode].push_back(s[j]);
                    break;
                }
                default: {
                    if (up && dshc == 0) {
                        ns_map[opcode].push_back(s[j]);
                        uint32_t input_len_idx = r[r_iter++];
                        size_t input_len = static_cast<size_t>(databuf[input_len_idx]);
                        nr_map[opcode].push_back(input_len);
                        nr_map[opcode].push_back(r[r_iter++]);
                        nr_map[opcode].push_back(r[r_iter++]);
                        uint32_t outbw_idx = r[r_iter++];
                        size_t outbw = static_cast<size_t>(databuf[outbw_idx]);
                        nr_map[opcode].push_back(outbw);
                        size_t outb = (outbw + 63) / 64;
                        dshc = outb - 1;
                        up = false;

                        for (size_t inp = 0; inp < input_len; ++inp) {
                            nr_map[opcode].push_back(r[r_iter++]);
                        }
                    } else {
                        if (dshc > 0) dshc--;
                        if (dshc == 0) up = true;
                    }
                    break;
                }
            }
        }

        // Aggregate all values from ns_map2 into s_vec
        for (const auto& [_, val] : ns_map2) {
            if (_ != 28) // Exclude opcode 28
                s_vec.insert(s_vec.end(), val.begin(), val.end());
        }

        // Update input maps with the computed data
        is_map[i] = std::move(s_vec);
        ins_map[i] = std::move(ns_map);
        inr_map[i] = std::move(nr_map);
    }
}

// unswizzled map
void unswizzled_fill_maps(
    const std::vector<size_t>& layer_pointer,
    const std::vector<uint8_t>& n,
    const std::vector<uint32_t>& s,
    const std::vector<uint32_t>& r,
    const std::vector<uint64_t>& databuf,
    std::vector<uint32_t>& ss_counts,
    std::vector<uint8_t>& updated_n,
    std::vector<uint32_t>& updated_r
) {
    size_t r_iter = 0;

    for (size_t i = 0; i + 1 < layer_pointer.size(); ++i) {
        bool up = true;
        size_t dshc = 0;
        std::vector<uint32_t> in_s_vec;

        // std::cout << "Processing layer " << i << ": from " << layer_pointer[i] << " to " << layer_pointer[i + 1] << "\n";

        for (size_t j = layer_pointer[i]; j < layer_pointer[i + 1]; ++j) {

            size_t opcode = n[j];

            // if (i == 21) {
            //     std::cout << "Opcode: " << static_cast<int>(opcode) << ", r_iter: " << r_iter << "\n";
            // }

            switch (opcode) {
                case 0: case 1: case 2: {
                    in_s_vec.push_back(s[j]);
                    updated_n.push_back(opcode);
                    updated_r.push_back(databuf[r[r_iter++]]);
                    updated_r.push_back(r[r_iter++]);
                    updated_r.push_back(r[r_iter++]);
                    break;
                }
                case 4: case 5: case 6: case 7: case 8:
                case 9: case 10: case 11: case 12: case 13: case 14: case 15:  {
                    in_s_vec.push_back(s[j]);
                    updated_n.push_back(opcode);
                    updated_r.push_back(r[r_iter++]);
                    updated_r.push_back(r[r_iter++]);
                    break;
                }
                case 24: {
                    in_s_vec.push_back(s[j]);
                    updated_n.push_back(opcode);
                    updated_r.push_back(r[r_iter++]);
                    updated_r.push_back(databuf[r[r_iter++]]);
                    updated_r.push_back(databuf[r[r_iter++]]);
                    break;
                }
                case 3: {
                    in_s_vec.push_back(s[j]);
                    updated_n.push_back(opcode);
                    updated_r.push_back(r[r_iter++]);
                    updated_r.push_back(databuf[r[r_iter++]]);
                    break;
                }
                case 16: case 20: {
                    in_s_vec.push_back(s[j]);
                    updated_n.push_back(opcode);
                    updated_r.push_back(r[r_iter++]);
                    break;
                }
                case 19: {
                    in_s_vec.push_back(s[j]);
                    updated_n.push_back(opcode);
                    updated_r.push_back(r[r_iter++]);
                    updated_r.push_back(r[r_iter++]);
                    updated_r.push_back(r[r_iter++]);
                    break;
                }
                case 18: {
                    in_s_vec.push_back(s[j]);
                    updated_n.push_back(opcode);
                    updated_r.push_back(r[r_iter++]);
                    updated_r.push_back(databuf[r[r_iter++]]);
                    updated_r.push_back(r[r_iter++]);
                    break;
                }
                case 17: {
                    in_s_vec.push_back(s[j]);
                    updated_n.push_back(opcode);
                    updated_r.push_back(r[r_iter++]);
                    updated_r.push_back(r[r_iter++]);
                    updated_r.push_back(databuf[r[r_iter++]]);
                    break;
                }
                case 28: {
                    in_s_vec.push_back(s[j]);
                    updated_n.push_back(opcode);
                    updated_r.push_back(r[r_iter++]);
                    updated_r.push_back(r[r_iter++]);
                    updated_r.push_back(databuf[r[r_iter++]]);
                    updated_r.push_back(databuf[r[r_iter++]]);
                    updated_r.push_back(databuf[r[r_iter++]]);
                    updated_r.push_back(r[r_iter++]);
                    updated_r.push_back(r[r_iter++]);
                    break;
                }
                case 21: {
                    in_s_vec.push_back(s[j]);
                    updated_n.push_back(opcode);
                    updated_r.push_back(databuf[r[r_iter++]]);
                    updated_r.push_back(databuf[r[r_iter++]]);
                    updated_r.push_back(databuf[r[r_iter++]]);
                    updated_r.push_back(r[r_iter++]);
                    break;
                }
                case 22: case 23: {
                    in_s_vec.push_back(s[j]);
                    updated_n.push_back(opcode);
                    uint32_t input_len_idx = r[r_iter++];
                    size_t input_len = static_cast<size_t>(databuf[input_len_idx]);
                    // if (i == 21) {
                    //     std::cout << "Input length for opcode " << static_cast<int>(opcode) << ": " << input_len << "\n";
                    // }   
                    updated_r.push_back(input_len);
                    for (size_t inp = 0; inp < input_len; ++inp) {
                        updated_r.push_back(r[r_iter++]);
                    }
                    break;
                }
                default: {
                    if (up && dshc == 0) {
                        in_s_vec.push_back(s[j]);
                        updated_n.push_back(opcode);
                        uint32_t input_len_idx = r[r_iter++];
                        size_t input_len = static_cast<size_t>(databuf[input_len_idx]);
                        updated_r.push_back(input_len);
                        updated_r.push_back(r[r_iter++]);
                        updated_r.push_back(r[r_iter++]);
                        uint32_t outbw_idx = r[r_iter++];
                        size_t outbw = static_cast<size_t>(databuf[outbw_idx]);
                        updated_r.push_back(outbw);
                        size_t outb = (outbw + 63) / 64;
                        dshc = outb - 1;
                        up = false;

                        for (size_t inp = 0; inp < input_len; ++inp) {
                            updated_r.push_back(r[r_iter++]);
                        }
                    } else {
                        if (dshc > 0) dshc--;
                        if (dshc == 0) up = true;
                    }
                    break;
                }
            }
        }
        ss_counts.push_back(in_s_vec.size());
    }
}

std::vector<uint32_t> build_new_r(const std::map<uint16_t, std::map<uint8_t, std::vector<uint32_t>>>& inr_map) {
    std::vector<uint32_t> new_r;

    for (const auto& [i_key, nr_map] : inr_map) {
        for (const auto& [n_key, vec] : nr_map) {
            if (n_key >= 0 && n_key < 29) {
                new_r.insert(new_r.end(), vec.begin(), vec.end());
            } else {
                std::cerr << "Warning: n_key " << n_key << " out of bounds [0..28]" << std::endl;
            }
        }
    }

    return new_r;
}

std::vector<uint32_t> build_all_lens(const std::map<uint16_t, std::map<uint8_t, std::vector<uint32_t>>>& ins_map) {
    std::vector<uint32_t> all_lens;

    for (const auto& [_i_key, ns_map] : ins_map) {
        std::vector<uint32_t> lens(29, 0);
        for (const auto& [n_key, vec] : ns_map) {
            if (n_key < 29) {
                lens[n_key] = vec.size();
            } else {
                std::cerr << "Warning: n_key " << n_key << " out of bounds [0..33]" << std::endl;
            }
        }
        all_lens.insert(all_lens.end(), lens.begin(), lens.end());
    }
    return all_lens;
}

std::unordered_map<uint8_t, size_t> buildMap(const std::vector<uint8_t>& op_type) {
    std::unordered_map<uint8_t, size_t> result;
    for (size_t i = 0; i < op_type.size(); ++i) {
        result[op_type[i]] = i;  // overwrites if op_type[i] appears multiple times
    }
    return result;
}

std::vector<uint32_t> partition_len_extraction(
    const std::map<uint8_t, std::vector<uint32_t>>& ns_map,
    std::vector<uint8_t>& op_type
) 
{
    std::vector<uint32_t> lens(op_type.size(), 0);
    std::sort(op_type.begin(), op_type.end());
    std::unordered_map<uint8_t, size_t> op_type_index = buildMap(op_type);

    for (const auto& [n_key, vec] : ns_map) {
        // for (const auto& val : op_type) {
            lens[op_type_index[n_key]] = vec.size();
        // }
    }
    return lens;
}  


std::vector<uint32_t> build_all_len_array(
    const std::vector<uint32_t>& all_lens,
    uint8_t op_type_num
) 
{
    std::vector<uint32_t> all_len_output;
    all_len_output.reserve(all_lens.size() * 2);

    all_len_output.push_back(0);  // Start with 0
    size_t sum = 0;
    // size_t start_idx = 0;
    size_t accum_op = 0;

    for (size_t i = 0; i < all_lens.size(); ++i) {
        size_t x = all_lens[i];

        if (i % op_type_num == 0) {
            accum_op = sum;

            if (!all_len_output.empty()) {
                all_len_output.back() = 0;
                // start_idx = 0;
            }
        }

        sum += x;
        uint16_t in_num = sum - accum_op;

        if (i < all_lens.size() - 1) {
            all_len_output.push_back(in_num);
            // start_idx = in_num;
            all_len_output.push_back(in_num);
        } else {
            all_len_output.push_back(in_num);
        }
    }
    return all_len_output;
}

std::vector<uint32_t> partition_len_generation(
    const std::map<uint16_t, std::map<uint8_t, std::vector<uint32_t>>>& ins_map,
    std::vector<std::vector<uint8_t>>& op_types,
    std::vector<size_t>& layer_divider
) 
{
    std::vector<uint32_t> all_lens;
    // for (const auto& [_i_key, ns_map] : ins_map) {
    for (size_t i = 1; i < layer_divider.size(); ++i) {
        uint32_t val = layer_divider[i];
        uint32_t pre_val = layer_divider[i-1];
        printf("Processing layer %zu: from %u to %u\n", i-1, pre_val, val);
        // if (_i_key < val && _i_key >= pre_val) {
        for (size_t _i_key = pre_val ; _i_key < val; ++_i_key) {
            std::vector<uint32_t> lens = partition_len_extraction(ins_map.at(_i_key), op_types[i-1]);
            std::vector<uint32_t> part_lens = build_all_len_array(lens, op_types[i-1].size());
            // std::cout << lens.size() << std::endl;
            all_lens.insert(all_lens.end(), part_lens.begin(), part_lens.end());
        }
    }
    // }
    return all_lens;
}

std::vector<uint32_t> generate_all_len(const std::vector<uint32_t>& all_lens) {
    std::vector<uint32_t> all_len;
    all_len.reserve(all_lens.size() * 2);

    // size_t empty_count = 0;
    // size_t total_count = 0;

    all_len.push_back(0);
    size_t sum = 0;
    size_t start_idx = 0;
    size_t accum_op = 0;

    for (size_t i = 0; i < all_lens.size(); ++i) {
        uint32_t x = all_lens[i];
        if (i % 29 == 0) {
            accum_op = sum;
            if (!all_len.empty()) {
                all_len.back() = 0;
                start_idx = 0;
            }
        }

        sum += x;

        uint32_t in_num = sum - accum_op;

        if (i < all_lens.size() - 1) {
            all_len.push_back(in_num);
            if ((in_num - start_idx) == 0) {
                // ++empty_count;
            }
            // ++total_count;
            start_idx = in_num;
            all_len.push_back(in_num);
        } else {
            all_len.push_back(in_num);
            if ((in_num - start_idx) == 0) {
                // ++empty_count;
            }
            // ++total_count;
        }
    }

    return all_len;
}

void print_new_r(const std::vector<size_t>& new_r) {
    for (size_t i = 0; i < new_r.size(); ++i) {
        std::cout << new_r[i];
        if (i + 1 != new_r.size()) {
            std::cout << ", ";
        }
    }
    std::cout << std::endl;
}

std::vector<std::vector<uint16_t>> map_to_vector(const std::unordered_map<uint8_t, std::vector<uint16_t>>& is_map) {
    // Determine max key in is_map
    size_t max_key = 0;
    for (const auto& kv : is_map) {
        if (kv.first > max_key) {
            max_key = kv.first;
        }
    }

    // Create a vector sized to max_key + 1
    std::vector<std::vector<uint16_t>> is_vec(max_key + 1);

    // Copy contents from map to vector by key/index
    for (const auto& kv : is_map) {
        is_vec[kv.first] = kv.second;
    }

    return is_vec;
}

std::vector<uint32_t> map_to_vector_flatten(const std::unordered_map<uint16_t, std::vector<uint32_t>>& is_map) {
    // Determine max key
    uint16_t max_key = 0;
    for (const auto& kv : is_map) {
        if (kv.first > max_key) {
            max_key = kv.first;
        }
    }

    // Compute total size
    size_t total_size = 0;
    for (uint16_t i = 0; i <= max_key; ++i) {
        auto it = is_map.find(i);
        if (it != is_map.end()) {
            total_size += it->second.size();
        }
    }

    // Allocate flat vector
    std::vector<uint32_t> flat;
    flat.reserve(total_size);

    // Append vectors in key order (0 to max_key)
    for (uint16_t i = 0; i <= max_key; ++i) {
        auto it = is_map.find(i);
        if (it != is_map.end()) {
            flat.insert(flat.end(), it->second.begin(), it->second.end());
        }
    }

    return flat;
}

template <typename T>
bool dumpVectorToJson(const std::vector<T>& vec, const std::string& filename) {
    nlohmann::json j = vec;

    std::ofstream file(filename);
    if (!file) {
        std::cerr << "Failed to open file: " << filename << "\n";
        return false;
    }

    file << j.dump(4); // pretty-print with 4 spaces
    return true;
}

bool dumpSizeTToJson(size_t value, const std::string& filename) {
    nlohmann::json j = value;

    std::ofstream file(filename);
    if (!file) {
        std::cerr << "Failed to open file: " << filename << "\n";
        return false;
    }

    file << j.dump(); // no pretty-print needed for a single number
    return true;
}

int main(int argc, char* argv[]) {

    printf("[INFO] Performing Data Pre-processing for Design: %s\n", argv[1]);
    std::string prefix = argv[1];

    // Load simluation tensors
    std::filesystem::path OIM_yaml_path = YAMLPATH "operationInputMask_" + prefix + "_maskedIn_full_func.yaml";
    std::filesystem::path OM_yaml_path = YAMLPATH "operationMask_" + prefix + "_maskedIn_full_func.yaml";

    YAML::Node OIM_tensor_yaml = YAML::LoadFile(OIM_yaml_path.string());
    YAML::Node OM_tensor_yaml = YAML::LoadFile(OM_yaml_path.string());

    const auto& OIM_root_node = OIM_tensor_yaml["tensor"]["root"];
    if (!OIM_root_node || !OIM_root_node[0]["fiber"]) {
        std::cerr << "Malformed root fiber.\n";
        return 1;
    }

    const auto& OM_root_node = OM_tensor_yaml["tensor"]["root"];
    if (!OM_root_node || !OM_root_node[0]["fiber"]) {
        std::cerr << "Malformed root fiber.\n";
        return 1;
    }

    // Find S rank
    size_t S_shape = get_shape_for_rank_id(OIM_tensor_yaml["tensor"], "S");
    std::cout << "Shape for rank ID " << S_shape << std::endl;
    // CSR layer metadata tensor
    std::vector<size_t> layer_pointer = compute_start_and_end_indices(OM_root_node);

    Fiber OIM_root_fiber = parse_fiber(OIM_root_node[0]["fiber"]);
    Fiber OM_root_fiber = parse_fiber(OM_root_node[0]["fiber"]);

    // Collect Contiguous Rank S, N, R
    std::vector<uint8_t> o;
    std::vector<uint32_t> r;
    std::vector<uint32_t> s;
    std::vector<uint8_t> n;
    collect_leaf_data(OIM_root_fiber, o, r);
    size_t max_s = collect_leaf_data(OM_root_fiber, s, n);
    std::cout << "Number of Operations:  " << n.size() << std::endl;
    std::cout << "Number of Input values:  " << r.size() << std::endl;
    std::cout << "Max S aka size for layerOutput " << max_s << std::endl;
    std::cout << "Number of layers: " << layer_pointer.size() - 1 << std::endl;
    size_t layer_num = layer_pointer.size() - 1;
    std::vector<uint64_t> databuf = load_txt_file("layerIn", prefix);
    std::cout << "Loaded " << databuf.size() << " input values.\n";

    // create output json directory if not exist
    if (!std::filesystem::exists(JSONPATH)) {
        std::filesystem::create_directories(JSONPATH);
    }

    std::cout << "here.\n";

    // for unswizzled kernel
    std::vector<uint32_t> ss_lens;
    std::vector<uint8_t> updated_n;
    std::vector<uint32_t> updated_r;
    unswizzled_fill_maps(layer_pointer, n, s, r, databuf, ss_lens, updated_n, updated_r); 
    if (dumpVectorToJson(updated_r, JSONPATH+prefix+"_unswizzled_rptr.json")) {
        std::cout << "Vector dumped to unswizzled_rptr.json\n";
    }

    if (dumpVectorToJson(updated_n, JSONPATH+prefix+"_unswizzled_nptr.json")) {
        std::cout << "Vector dumped to unswizzled_nptr.json\n";
    }

    if (dumpVectorToJson(s, JSONPATH+prefix+"_unswizzled_sptr.json")) {
        std::cout << "Vector dumped to unswizzled_sptr.json\n";
    }

    if (dumpVectorToJson(ss_lens, JSONPATH+prefix+"_unswizzled_ss_lens_ptr.json")) {
        std::cout << "Vector dumped to unswizzled_ss_lens_ptr.json\n";
    }

    if (dumpSizeTToJson(max_s, JSONPATH+prefix+"_max_s.json")) {
        std::cout << "Number saved to max_s.json\n";
    }

    if (dumpSizeTToJson(layer_num, JSONPATH+prefix+"_layer_num.json")) {
        std::cout << "Number saved to layer_num.json\n";
    }

    if (dumpSizeTToJson(S_shape, JSONPATH+prefix+"_S_shape.json")) {
        std::cout << "Number saved to S_shape.json\n";
    }

    // Start Rank Swizzling
    std::map<uint16_t, std::map<uint8_t, std::vector<uint32_t>>> ins_map;
    std::map<uint16_t, std::map<uint8_t, std::vector<uint32_t>>> inr_map;
    std::unordered_map<uint16_t, std::vector<uint32_t>> is_map;
    fill_maps(layer_pointer, n, s, r, databuf, ins_map, inr_map, is_map);
    std::vector<uint32_t> new_s = map_to_vector_flatten(is_map);

    // deallocate old tensors to save memory usage
    o = {};
    r = {};
    s = {};
    n = {};
    layer_pointer = {};

    std::vector<uint32_t> new_r = build_new_r(inr_map);
    std::vector<uint32_t> all_lens = build_all_lens(ins_map);
    std::vector<uint32_t> all_len = generate_all_len(all_lens);

    if (dumpVectorToJson(new_r, JSONPATH+prefix+"_rptr.json")) {
        std::cout << "Vector dumped to rptr.json\n";
    }

    if (dumpVectorToJson(all_len, JSONPATH+prefix+"_fptr.json")) {
        std::cout << "Vector dumped to fptr.json\n";
    }

    if (dumpVectorToJson(new_s, JSONPATH+prefix+"_sptr.json")) {
        std::cout << "Vector dumped to sptr.json\n";
    }

    if (dumpSizeTToJson(max_s, JSONPATH+prefix+"_max_s.json")) {
        std::cout << "Number saved to max_s.json\n";
    }

    if (dumpSizeTToJson(layer_num, JSONPATH+prefix+"_layer_num.json")) {
        std::cout << "Number saved to layer_num.json\n";
    }

    if (dumpSizeTToJson(S_shape, JSONPATH+prefix+"_S_shape.json")) {
        std::cout << "Number saved to S_shape.json\n";
    }

    return 0;
}
