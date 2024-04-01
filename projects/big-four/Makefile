# Makefile for compiling C++ project with specific directory structure

# Source and object directories
SRC_IMPL := src/impl
SRC_PUBLIC := src/public
BIN_DIR := bin

# Compiler and linker
CXX := g++
CXXFLAGS := -I$(SRC_PUBLIC) -Wall -std=c++11
LDFLAGS :=

# Ensure the bin directory exists
$(shell mkdir -p $(BIN_DIR))

# Find all .cpp files in the private directory, transform into .o files in bin directory
SOURCES := $(wildcard $(SRC_IMPL)/*.cpp)
OBJECTS := $(patsubst $(SRC_IMPL)/%.cpp,$(BIN_DIR)/%.o,$(SOURCES))

# Main source file
MAIN_SRC := src/main.cpp
MAIN_OBJ := $(BIN_DIR)/main.o

# Target executable name
TARGET := executable

# .PHONY: all clean

all: $(TARGET)

$(TARGET): $(OBJECTS) $(MAIN_OBJ)
	$(CXX) $(OBJECTS) $(MAIN_OBJ) -o $@ $(LDFLAGS)

# Rule for compiling source files to object files in bin directory
$(BIN_DIR)/%.o: $(SRC_IMPL)/%.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

# Rule for compiling the main.cpp file to its object file in bin directory
$(MAIN_OBJ): $(MAIN_SRC)
	$(CXX) $(CXXFLAGS) -c $< -o $@

clean:
	rm -rf $(TARGET) $(BIN_DIR)
