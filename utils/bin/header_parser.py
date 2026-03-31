import re
import json

def extract_structs(text):
    structs = {}
    lines = text.splitlines()
    i = 0
    while i < len(lines):
        line = lines[i].strip()
        if line.startswith("typedef struct"):
            header = line
            if "{" not in line:
                i += 1
                header += " " + lines[i].strip()

            match = re.match(r"typedef\s+struct\s+(\w+)\s*{", header)
            if not match:
                i += 1
                continue
            struct_decl_name = match.group(1)

            body_lines = []
            brace_count = 1 if "{" in line else 0
            while i + 1 < len(lines):
                i += 1
                l = lines[i]
                brace_count += l.count("{")
                brace_count -= l.count("}")
                body_lines.append(l)
                if brace_count == 0:
                    break

            typedef_line = lines[i].strip()
            typedef_match = re.match(r"}\s*(\w+);", typedef_line)
            struct_typedef_name = typedef_match.group(1) if typedef_match else struct_decl_name

            structs[struct_typedef_name] = extract_fields("\n".join(body_lines))

        i += 1
    return structs

def extract_fields(body):
    fields = []
    field_pattern = re.compile(r"([a-zA-Z_][\w<>\d$]*)\s+([a-zA-Z_][\w$]*)(\[(\d+)\])?;")
    for line in body.splitlines():
        line = line.strip()
        if not line or line.startswith("//") or '(' in line:
            continue  # skip empty/comment/constructor
        m = field_pattern.match(line)
        if m:
            field_type, field_name, _, array_size = m.groups()
            fields.append((field_type, field_name, array_size))
    return fields

def unroll_structs(structs, top_struct, prefix=""):
    flat_fields = []

    def recurse(struct_name, current_prefix):
        fields = structs.get(struct_name, [])
        for field_type, field_name, array_size in fields:
            full_name = f"{current_prefix}.{field_name}" if current_prefix else field_name
            if field_name == "assert_exit_code":
              continue
            if field_type in structs:
                recurse(field_type, full_name)
            else:
                flat_fields.append((field_type, full_name, array_size))

    recurse(top_struct, prefix)
    return flat_fields


if __name__ == "__main__":
    # args
    import argparse
    parser = argparse.ArgumentParser(description="Essent Header Parser")
    parser.add_argument("name", help="Design's name")
    args = parser.parse_args()
    HEADER_FILE = "TestHarness.h" 
    with open(HEADER_FILE, "r") as f:
        content = f.read()
    structs = extract_structs(content)

    top_struct = "TestHarness"
    if top_struct not in structs:
        print(f"Error: Struct '{top_struct}' not found.")
        exit(1)

    flat_fields = unroll_structs(structs, top_struct)

    with open("input_bw.txt", "w") as out_file:
        for field_type, name, array_size in flat_fields:
            if array_size:
              out_file.write(f"{field_type} {name}[{array_size}]\n")

    with open("input_m.txt", "w") as out_file:
        for field_type, name, array_size in flat_fields:
            if array_size:
              out_file.write(f"{name}[{array_size}]\n")
    
    with open("memory.txt", "w") as out_file:
        for field_type, name, array_size in flat_fields:
            if array_size:
                out_file.write(f"{field_type} {name}[{array_size}]\n")

    out_dict = {}
    # open memory.txt
    with open("memory.txt", "r") as f:
        lines = f.readlines()
    for line in lines:
        full_name = line.split(" ")[1]

        match = re.match(r"^(.*)\[(\d+)\]$", full_name)
        if match:
            prefix = match.group(1)
            length = int(match.group(2))
        else:
            raise ValueError(f"Invalid format: {full_name}")
        if length & (length - 1) != 0:
            assert False, f"Array size {length} is not a power of 2: {full_name}"
        out_dict[prefix] = length
    # write to json file
    with open(f"mem_array_len_{args.name}.json", "w") as f:
        json.dump(out_dict, f, indent=4)

    input_file = "input_bw.txt"                     # Input file
    output_json_signals = "split_signals_memory.json"  # JSON file for new signals
    mapping_file = "split_signal_mapping_memory.json"  # JSON file for dictionary

    with open(input_file, "r") as f:
        lines = f.readlines()

    output_signals = []
    mapping_dict = {}

    for line in lines:
        line = line.strip()
        if not line:
            continue

        match = re.match(r"UInt<(\d+)> (.+)", line)
        if not match:
            continue

        bitwidth = int(match.group(1))
        name = match.group(2)

        # Check for array part
        array_match = re.match(r"(.*?)(\[[^\]]+\])$", name)
        if array_match:
            base_name = array_match.group(1)
            array_suffix = array_match.group(2)
        else:
            base_name = name
            array_suffix = ""

        if bitwidth <= 64:
            full_name = base_name + array_suffix
            output_signals.append({"bitwidth": bitwidth, "name": full_name})
            mapping_dict[name] = [full_name]
        else:
            remaining = bitwidth
            index = 0
            chunks = []
            while remaining > 0:
                chunk_size = min(64, remaining)
                split_name = f"{base_name}_+=+_{index}{array_suffix}"
                output_signals.append({"bitwidth": chunk_size, "name": split_name})
                chunks.append(split_name)
                remaining -= chunk_size
                index += 1
            mapping_dict[name] = chunks

    # Write signals to JSON
    with open(output_json_signals, "w") as f:
        json.dump(output_signals, f, indent=2)

    # Write mapping dictionary to JSON
    with open(mapping_file, "w") as f:
        json.dump(mapping_dict, f, indent=2)

    print(f"✅ Wrote {len(output_signals)} signals to {output_json_signals}")
    print(f"✅ Wrote signal mapping to {mapping_file}")

    input_file = "split_signals_memory.json"           # Your input file
    output_file = f"accum_length_dict_{args.name}.json"     # Output dictionary file

    dict1 = {}  # bitwidth <= 8
    dict2 = {}  # bitwidth > 8
    mem8_sync = []
    mem64_sync = []
    length1 = 0
    length2 = 0

    with open(input_file, 'r') as f:
        data = json.load(f)


    for entry in data:
        bitwidth = entry['bitwidth']
        name_str = entry['name']
        
        # Extract array name and index from brackets
        match = re.match(r"^(.*)\[(\d+)\]$", name_str)
        if match:
            signal_base = match.group(1)
            array_size = int(match.group(2))

            # for array size not power of 2, padding to next power of 2
            if array_size & (array_size - 1) != 0:
                aligned_array_size = 1 << (array_size - 1).bit_length()
                if bitwidth <= 8:
                    # length1 + array_size ~ length1 + aligned_array_size
                    for i in range(aligned_array_size - array_size):
                        mem8_sync.append(length1 + array_size + i)
                else:
                    # length2 + array_size ~ length2 + aligned_array_size
                    for i in range(aligned_array_size - array_size):
                        mem64_sync.append(length2 + array_size + i)
                array_size = aligned_array_size

            if bitwidth <= 8:
                if signal_base not in dict1:
                    dict1[signal_base] = length1
                    length1 += array_size
            else:
                if signal_base not in dict2:
                    dict2[signal_base] = length2
                    length2 += array_size

    # Write the output to JSON file
    with open(output_file, "w") as f:
        json.dump({**dict1, **dict2}, f, indent=2)

    print(f"✅ Wrote accumulated lengths to: {output_file}")
    print(f"u8 memory array length: {length1}")
    print(f"u64 memory array length: {length2}")
