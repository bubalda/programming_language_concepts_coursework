.PHONY: all build run repl debug test clean rebuild install help

# Default target
all: build test run

# Build the project
build:
	stack build

# Run the REPL
run:
	stack run

# Run the REPL (alias)
repl:
	stack run

# Run in debug mode (shows tokens, AST, and evaluation at each step)
debug:
	stack run -- --debug

# Run the test suite
test:
	stack test

# Clean build artifacts
clean:
	stack clean

# Clean then rebuild from scratch
rebuild: clean build

# Install GHC and dependencies via Stack (first time setup)
install:
	stack setup
	stack build --only-dependencies

# Display available make targets
help:
	@echo ""
	@echo "Available targets:"
	@echo "  make build     - Build the project"
	@echo "  make run       - Start the REPL"
	@echo "  make repl      - Start the REPL (alias for run)"
	@echo "  make debug     - Start the REPL in debug mode"
	@echo "  make test      - Run the test suite"
	@echo "  make clean     - Remove build artifacts"
	@echo "  make rebuild   - Clean then rebuild from scratch"
	@echo "  make install   - Set up GHC and install dependencies (first time)"
	@echo "  make all       - Build and run tests (default)"
	@echo ""
