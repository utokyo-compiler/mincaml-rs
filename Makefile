TESTS_DIR := ./tests
TESTS := $(wildcard $(TESTS_DIR)/*.ml)
MLI := ./tests/pervasives.mli

build:
	cargo build

$(TESTS_DIR)/%.wasm: $(TESTS_DIR)/%.ml build
	cargo run -- -i $(MLI) -i $< -o $@
	wasm-tools validate $@

test: build $(TESTS:$(TESTS_DIR)/%.ml=$(TESTS_DIR)/%.wasm)
	@echo "All tests passed"

.PHONY: test build
