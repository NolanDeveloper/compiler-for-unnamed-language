SOURSES = $(wildcard src/*)
OBJECTS = $(patsubst src/%.cpp, build/%.o, $(SOURSES))

CXX = clang++
CXXFLAGS = -std=c++11 -g -Iinclude -ferror-limit=2
LDLIBS = 


LLVM_CONFIG_COMPILE := $$(/usr/lib/llvm-3.7/bin/llvm-config --cxxflags)
LLVM_CONFIG_LINK := $$(/usr/lib/llvm-3.7/bin/llvm-config --cxxflags --ldflags --system-libs --libs core)

build/a.out: $(OBJECTS)
	$(CXX) $^ -o $@ $(CXXFLAGS) $(LDLIBS) $(LLVM_CONFIG_LINK)

build/%.o: src/%.cpp
	$(CXX) -c $< -o $@ $(CXXFLAGS) $(LLVM_CONFIG_COMPILE)
	$(CXX) -MM $< $(CXXFLAGS) $(LLVM_CONFIG_COMPILE) \
		| sed 's/\(.*\.o:\)/build\/\1/g' > build/$*.d 

-include $(wildcard build/*.d)
