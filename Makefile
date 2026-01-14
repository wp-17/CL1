# Project Configuration
PRJ       := cl1
PRJ_DIR   := $(CURDIR)
BUILD_DIR := ./build/$(VCC)
VSRC_DIR  := ./vsrc
WAVE_DIR  := ./wave
CPUTOP    := Cl1Top
DUMP_WAVE := 1

CONFIG_DBG = n

$(shell mkdir -p $(WAVE_DIR))

# Tools
MILL      := $(or $(shell which mill), ./mill) # Use global mill if available, otherwise use local ./mill
MKDIR     := mkdir -p
RM        := rm -rf
MAKE      ?= make
VCC       ?= verilator
WAVE      ?= gtkwave

# Phony Targets
.PHONY: all verilog help reformat checkformat clean run

# Generate Verilog
FIRTOOL_VERSION = 1.105.0
FIRTOOL_PATCH_DIR = $(shell pwd)/patch/firtool

verilog:
	@echo "Generating Verilog files..."
	$(MKDIR) $(VSRC_DIR)
	@./patch/update-firtool.sh $(FIRTOOL_VERSION) $(FIRTOOL_PATCH_DIR)
	CHISEL_FIRTOOL_PATH=$(FIRTOOL_PATCH_DIR)/firtool-$(FIRTOOL_VERSION)/bin \
	$(MILL) -i $(PRJ).runMain Elaborate --target-dir $(VSRC_DIR) --throw-on-first-error
	sed -i '/difftest\.sv/d' $(VSRC_DIR)/$(CPUTOP).sv
	sed -i '/Stat\.v/d' $(VSRC_DIR)/$(CPUTOP).sv
	
# Show Help for Elaborate
help:
	@echo "Displaying help for Elaborate..."
	$(MILL) -i $(PRJ).runMain Elaborate --help

# Reformat Code
reformat:
	@echo "Reformatting code..."
	$(MILL) -i __.reformat

# Check Code Format
checkformat:
	@echo "Checking code format..."
	$(MILL) -i __.checkFormat

# Clean Build Artifacts
clean:
	@echo "Cleaning build artifacts..."
	$(RM) $(BUILD_DIR)
	$(RM) $(VSRC_DIR)


RTLSRC_CPU  		:= $(VSRC_DIR)/$(CPUTOP).sv

.PHONY: $(RTLSRC_CPU)

-include ./soc/soc.mk

ifeq ($(CONFIG_DBG), y)
 	-include ./riscv_dbg/dbg.mk
endif


riscv_dbg/remote_bitbang/librbs_veri.so: INCLUDE_DIRS =./
riscv_dbg/remote_bitbang/librbs_veri.so:
	$(MAKE) -C riscv_dbg/remote_bitbang all
	mv riscv_dbg/remote_bitbang/librbs.so $@

riscv_dbg/remote_bitbang/librbs_vcs.so:
riscv_dbg/remote_bitbang/librbs_vcs.so:
	$(MAKE) -C riscv_dbg/remote_bitbang all INCLUDE_DIRS="./ $(if $(VCS_HOME),$(VCS_HOME)/include,)"
	mv riscv_dbg/remote_bitbang/librbs.so $@

ifeq ($(CONFIG_DBG), y)
  ifeq ($(VCC), verilator)
    BITBANG_SO := $(abspath riscv_dbg/remote_bitbang/librbs_veri.so)
  else ifeq ($(VCC), vcs)
    BITBANG_SO := $(abspath riscv_dbg/remote_bitbang/librbs_vcs.so)
  endif
  TEST_CASE = while_loop
  TEST_NAME = loop
  VF += -DRISCV_DEBUG
  VCS_OUT = log/vcs_run.log
  OPENOCD_OUT = log/openocd_run.log
  OPENOCD_CFG = riscv_dbg/openocd_cfg/dm_compliance_test.cfg
endif



VTOP := top
COMPILE_OUT := $(BUILD_DIR)/compile.log
BIN := $(BUILD_DIR)/$(VTOP)

ifeq ($(VCC), verilator)
	VF += $(addprefix +incdir+, $(RTLSRC_INCDIR)) \
	--Wno-lint --Wno-UNOPTFLAT --Wno-BLKANDNBLK --Wno-COMBDLY --Wno-MODDUP \
	./cl1/src/cc/verilator/main.cpp \
	./cl1/src/cc/verilator/difftest.cpp \
	-CFLAGS -I$(abspath ./cl1/src/cc/verilator/include) \
	--timescale 1ns/1ps \
	--autoflush \
	--x-assign unique \
	--trace --trace-fst \
	--build -j 0 --exe --timing --cc \
	--Mdir $(BUILD_DIR) \
	--top-module $(VTOP) -o $(VTOP)
else ifeq ($(VCC), vcs)
	VF += $(addprefix +incdir+, $(RTLSRC_INCDIR)) \
	+vc -full64 -sverilog +v2k -timescale=1ns/1ps \
	-LDFLAGS -Wl,--no-as-needed \
	+lint=TFIPC-L \
	-lca -kdb \
	-CC "$(if $(VCS_HOME), -I$(VCS_HOME)//include,)" \
	-debug_access -l $(COMPILE_OUT) \
	-Mdir=$(BUILD_DIR) \
	-top $(VTOP) -o $(BUILD_DIR)/$(VTOP)
else 
	$(error unsupport VCC)
endif


$(BIN): $(RTLSRC_CPU) $(RTLSRC_DBG) $(RTLSRC_SOC) $(BITBANG_SO)
	$(VCC) $(RTLSRC_CPU) $(RTLSRC_DBG) $(BITBANG_SO) $(RTLSRC_SOC) $(VF)

bin: $(BIN)

REF ?= ./utils/riscv32-spike-so
TEST_CASE ?= dummy
TEST_NAME ?= dummy
WAVE_TYPE ?= fst

CURRENT_TIME := $(shell date +%s)

ifneq ($(DUMP_WAVE),)
RUN_ARGS += +$(WAVE_TYPE)
endif
RUN_ARGS += --diff
RUN_ARGS += +firmware=./test/$(TEST_CASE)/build/$(TEST_NAME).hex
RUN_ARGS += --image=./test/$(TEST_CASE)/build/$(TEST_NAME).bin
RUN_ARGS += --ref=$(REF)

# Test Targets (run, gdb, latest)
run: $(BIN)
	$(BIN) $(RUN_ARGS)

dbg_test: $(BIN)
	@mkdir -p log
	@echo "[INFO] Starting dbg_test simulation..."
	@(  \
	    echo "[CMD] $(BIN) $(RUN_ARGS) +verbose &> $(VCS_OUT) &"; \
		VCS_PID=""; \
		cleanup() { \
			echo "[INFO] Cleaning up..."; \
			[ -n "$$VCS_PID" ] && kill $$VCS_PID 2>/dev/null; \
			exit 1; \
		}; \
		trap 'echo; cleanup' INT TERM; \
		./$(BIN) $(RUN_ARGS)  +verbose &> $(VCS_OUT) & \
		VCS_PID=$$!; \
		echo "[INFO] VCS PID: $$VCS_PID"; \
		until grep -q "Listening on port" $(VCS_OUT); do sleep 1; done; \
		echo "[INFO] Detected 'Listening on port', launching OpenOCD..."; \
		./tools/openocd -f $(OPENOCD_CFG) |& tee $(OPENOCD_OUT); \
		wait $$VCS_PID; \
	)
	@echo "[INFO] Simulation completed."


#TODO: add test submodule
test: bin
	@for dir in $(CL2_TEST_DIR); do \
		$(MAKE) -C $$dir run ARCH=riscv32-cl2 || exit 1; \
	done


wave:
	$(WAVE) $(WAVE_DIR)/$(VTOP).$(WAVE_TYPE)

.PHONY: $(BIN) wave
