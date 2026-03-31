# ESSENT Baseline

This directory contains the build infrastructure for compiling RTL designs with ESSENT, which is the second baseline in the evaluation.

## Building

First, build the ESSENT JAR:
```bash
make essent_jar
```

Then compile a specific design:
```bash
make compile_essent_<design>
```

### Compilation Logs

Two log files are generated in the design's directory when compiling:
- `essent.log` — timing and peak memory usage of TestHarness header generation (FIRRTL → C++ header via ESSENT)
- `compile.log` — timing and peak memory usage of C++ compilation (compiling the generated header into a simulator binary)

## Adding a New RTL Design

**1. Copy the template folder**

```bash
cp -r template <design>
```

**2. Add the FIRRTL file**

Place the design's `.fir` file in `essent/firrtls/`.

**3. Update the FIRRTL path in the design folder**

In `<design>/Makefrag-source-files.mk`, update `FIR_PATH` to point to the new FIRRTL file:
```makefile
FIR_PATH = $(base_dir)/../firrtls/<your-design>.fir
```

**4. Register the design in the Makefile**

In `essent/Makefile`, add the design name to the `DESIGNS` list.
