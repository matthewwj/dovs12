# Makefile for building the project using dune

.PHONY: all clean

# Default target: builds the project
all:
	@echo "Building the project..."
	dune build
	

# Clean target: cleans the build artifacts
clean:
	dune clean

run: 
	@echo "Testing semant"
	
	@echo "----------------------------------"
	
