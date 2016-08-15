#pragma once

#include <memory>
#include <string>
#include <utility>

template <typename T>
using Ptr = std::unique_ptr<T>;

template <typename T, typename... Args>
std::unique_ptr<T> make_unique(Args &&... args) {
    return std::unique_ptr<T>(new T(std::forward<decltype(args)>(args)...));
}

std::string operator ""_s(const char * str, size_t);
