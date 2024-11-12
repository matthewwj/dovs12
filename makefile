# Makefile for building the project using dune

.PHONY: all clean
OUTPUT_DIR := llvm_outputs

# Default target: builds the project
all:
	@echo "Building the project..."
	dune build
	

# Clean target: cleans the build artifacts
clean:
	dune clean

run: 
	@echo "Testing semant and codegen"

test: 
	@echo "Testing semant and codegen"
	dune build
	./_build/default/main.exe
#	clang $(OUTPUT_DIR)/testreadint.ll $(OUTPUT_DIR)/main.c -o $(OUTPUT_DIR)/output1
#	./$(OUTPUT_DIR)/output1