TESTS_DIR := ./tests
TESTS := $(wildcard $(TESTS_DIR)/*.ml)
MLI := ./tests/pervasives.mli

$(TESTS_DIR)/%.wasm: $(TESTS_DIR)/%.ml
	cargo run -- -i $(MLI) -i $< -o $@
	wasm-tools validate $@

test: $(TESTS:$(TESTS_DIR)/%.ml=$(TESTS_DIR)/%.wasm)
	@echo "All tests passed"

.PHONY: test
