#include "utils.hpp"

std::string operator ""_s(const char * str, size_t ) { 
    return { str }; 
}
