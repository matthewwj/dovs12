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
	./_build/default/semanttest.exe
	

test: 
	@echo "Testing semant and codegen"
	./_build/default/codegentest.exe
	clang $(OUTPUT_DIR)/test5.ll $(OUTPUT_DIR)/main.c -o $(OUTPUT_DIR)/output2
	./$(OUTPUT_DIR)/output2
