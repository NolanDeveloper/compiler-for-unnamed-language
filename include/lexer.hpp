#pragma once

#include "utils.hpp"

#include <string>
#include <vector>
#include <memory>

struct Token { 
    virtual ~Token() { }
};

struct End_of_file : Token { };
struct Kw_int : Token { };
struct Kw_float : Token { };
struct Kw_for : Token { };
struct Kw_while : Token { };
struct Kw_if : Token { };
struct Kw_return : Token { };
struct P_comma : Token { };
struct P_semicolon : Token { };
struct P_lparen : Token { };
struct P_rparen : Token { };
struct P_lbracket : Token { };
struct P_rbracket : Token { };
struct P_assign : Token { };
struct P_plus : Token { };
struct P_minus : Token { };
struct P_multiply : Token { };
struct P_divide : Token { };

struct Token_with_text : Token {
    const std::string text;
    Token_with_text(std::string text) : text(move(text)) { }
};

struct name_id : Token_with_text {
    name_id(std::string text) : Token_with_text(text) { }
};

struct int_literal : Token_with_text {
    int_literal(std::string text) : Token_with_text(text) { }
};

struct float_literal : Token_with_text {
    float_literal(std::string text) : Token_with_text(text) { }
};

// Breaks text of program into tokens.
std::vector<Ptr<Token>> tokenize(const std::string & text);
