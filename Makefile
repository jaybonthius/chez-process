SCHEME ?= scheme
CC ?= cc
CAP ?=
OBJ ?= build/obj
LIBDIRS := src::$(OBJ)/src:tests/lib::$(OBJ)/tests/lib
RUN := $(SCHEME) --libdirs $(LIBDIRS) --program
TEST_ENV ?=
BUILD_TIMEOUT ?= 1800
TEST_TIMEOUT ?= 900
BENCH_TIMEOUT ?= 1800
TESTS := $(sort $(wildcard tests/*-test.sps))

SHARED_OBJECT := $(OBJ)/src/process/libchez-process.so
SLOW_ACCESS := $(OBJ)/tests/lib/slow-access.so
C_BENCH := $(OBJ)/bench/c-bench
CFLAGS ?= -O2
SHARED_CFLAGS := -std=gnu11 -fPIC -shared -fvisibility=hidden -Wall -Wextra -pthread
SANITIZE_CFLAGS := -O1 -g -fno-omit-frame-pointer -fsanitize=address,undefined \
  -fno-sanitize-recover=all
# Chez itself is not built with the sanitizers, so their runtimes are
# preloaded; Chez frees its heap only at exit, so leaks are not reported.
SANITIZE_ENV := ASAN_OPTIONS=detect_leaks=0 \
  LD_PRELOAD=$(shell $(CC) -print-file-name=libasan.so):$(shell $(CC) -print-file-name=libubsan.so)

.PHONY: build test sanitize bench clean $(TESTS:%=run/%)

# Phony so that Chez, which tracks what each object was built against,
# decides what is stale.
build: $(SHARED_OBJECT) $(SLOW_ACCESS)
	$(CAP) timeout $(BUILD_TIMEOUT) $(SCHEME) -q --script build.ss $(OBJ)

$(SHARED_OBJECT): src/process/shim.c
	@mkdir -p $(@D)
	$(CAP) timeout $(BUILD_TIMEOUT) $(CC) $(CFLAGS) $(SHARED_CFLAGS) -o $@ $< -ldl

$(SLOW_ACCESS): tests/fixtures/slow-access.c
	@mkdir -p $(@D)
	$(CAP) timeout $(BUILD_TIMEOUT) $(CC) -O2 -std=gnu11 -fPIC -shared -Wall -o $@ $< -ldl

# One process per test file, so a crash or leaked global state stays inside
# its file.
test: $(TESTS:%=run/%)

$(TESTS:%=run/%): run/%: build
	@$(CAP) timeout $(TEST_TIMEOUT) env SCHEME=$(SCHEME) $(TEST_ENV) $(RUN) ./$(OBJ)/$(*:.sps=.so)

# A build of its own, so that the ordinary one is never left sanitized.
sanitize:
	@$(MAKE) --no-print-directory test OBJ=build/sanitize CFLAGS="$(SANITIZE_CFLAGS)" \
	  TEST_ENV="$(SANITIZE_ENV)"

$(C_BENCH): bench/bench.c
	@mkdir -p $(@D)
	$(CAP) timeout $(BUILD_TIMEOUT) $(CC) -O2 -std=gnu11 -Wall -o $@ $<

bench: build $(C_BENCH)
	$(CAP) timeout $(BENCH_TIMEOUT) $(RUN) ./$(OBJ)/bench/bench.so $(C_BENCH)

clean:
	rm -rf build
