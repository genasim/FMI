# Source and object directories
SRC_IMPL := src/impl
SRC_PUBLIC := src/public
BIN_DIR := bin

# Compiler and linker
CXX := g++
CXXFLAGS := -I$(SRC_PUBLIC) -Wall -std=c++11 -g
LDFLAGS :=

# Ensure the bin directory exists
$(shell mkdir -p $(BIN_DIR))

# Find all .cpp files in the src/impl directory and subdirectories, transform into .o files in bin directory
SOURCES := $(shell find $(SRC_IMPL) -type f -name '*.cpp')
OBJECTS := $(SOURCES:$(SRC_IMPL)/%.cpp=$(BIN_DIR)/%.o)

# Main source file
MAIN_SRC := src/main.cpp
MAIN_OBJ := $(BIN_DIR)/main.o

# Target executable name
TARGET := bin/executable.out

all: $(TARGET)

$(TARGET): $(OBJECTS) $(MAIN_OBJ)
	$(CXX) $(OBJECTS) $(MAIN_OBJ) -o $@ $(LDFLAGS)

# Rule for compiling source files to object files in bin directory
$(BIN_DIR)/%.o: $(SRC_IMPL)/%.cpp
	@mkdir -p $(@D)
	$(CXX) $(CXXFLAGS) -c $< -o $@

# Rule for compiling the main.cpp file to its object file in bin directory
$(MAIN_OBJ): $(MAIN_SRC)
	@mkdir -p $(@D)
	$(CXX) $(CXXFLAGS) -c $< -o $@

.PHONY: clean
clean:
	@rm -rf $(BIN_DIR)
	@echo "removed bin/ directory"
	@rm -rf src/assets
	@echo "removed src/assets/ directory"
