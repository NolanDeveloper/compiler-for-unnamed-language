SOURSES = $(wildcard src/*)
OBJECTS = $(patsubst src/%.cpp, build/%.o, $(SOURSES))

CXX = clang++
CXXFLAGS = -std=c++14 -g -Iinclude
LDLIBS = 

build/a.out: $(OBJECTS)
	$(CXX) $(CXXFLAGS) $(LDLIBS) $^ -o $@

build/%.o: src/%.cpp
	$(CXX) $(CXXFLAGS) $(LDLIBS) -c $< -o $@
	$(CXX) $(CXXFLAGS) -MM $< | sed 's/\(.*\.o:\)/build\/\1/g' > build/$*.d 

-include $(wildcard build/*.d)
