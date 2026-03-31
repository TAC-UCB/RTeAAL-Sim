# Verilator Baseline

This directory contains the build infrastructure for compiling RTL designs with Verilator, which is the first baseline in the evaluation.

## Building

First, install Verilator (fetched and built automatically):
```bash
make verilator_install
```

Then compile a specific design:
```bash
make compile_verilator_<design>
```

### Compilation Logs

Two log files are generated in the design's directory when compiling:
- `generate_file.log` — timing and peak memory usage of Verilog elaboration and Verilator code generation
- `build_emulator_verilator.log` — timing and peak memory usage of C++ compilation into a simulator binary

## Adding a New RTL Design

**1. Copy the template folder**

```bash
cp -r template <design>
```

**2. Add the Verilog and plusArgs files**

Place the design's `.v` and `.plusArgs` files in `verilator/verilogs/`.

**3. Update the file paths in the design folder**

In `<design>/Makefrag-variables.mk`, update the Verilog and plusArgs file names to match the new design:
```makefile
$(VERILOG_FILE_PATH):
	cp $(base_dir)/../verilogs/<your-design>.v $(base_dir)/$(MODEL).v

PLUSARGS_PATH = $(base_dir)/../verilogs/<your-design>.plusArgs
```

**4. Register the design in the Makefile**

In `verilator/Makefile`, add the design name to the `DESIGNS` list.
