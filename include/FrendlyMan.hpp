#pragma once

#include <iostream>
#include <iomanip>
#include <string>


class FrendlyMan {
    std::string name;

public:
    FrendlyMan(std::string name) : name(move(name)) { }

    void hello(int n) {
        using namespace std;
        for (int i = 0; i < n; ++i)
            cout << setw(3) << i << ": Hello, " << name << "!\n";
    }
};
