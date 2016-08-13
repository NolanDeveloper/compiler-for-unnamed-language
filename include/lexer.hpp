#pragma once

#include "utils.hpp"

#include <string>
#include <vector>
#include <memory>

struct token { 
    virtual ~token() { }
};

struct end_of_file : token { };
struct kw_int : token { };
struct kw_float : token { };
struct kw_for : token { };
struct kw_while : token { };
struct kw_if : token { };
struct kw_return : token { };
struct p_comma : token { };
struct p_semicolon : token { };
struct p_lparen : token { };
struct p_rparen : token { };
struct p_lbracket : token { };
struct p_rbracket : token { };
struct p_assign : token { };
struct p_plus : token { };
struct p_minus : token { };
struct p_multiply : token { };
struct p_divide : token { };

struct token_with_text : token {
    const std::string text;
    token_with_text(std::string text) : text(move(text)) { }
};

struct name_id : token_with_text {
    name_id(std::string text) : token_with_text(text) { }
};

struct int_literal : token_with_text {
    int_literal(std::string text) : token_with_text(text) { }
};

struct float_literal : token_with_text {
    float_literal(std::string text) : token_with_text(text) { }
};

// Breaks text of program into tokens.
std::vector<ptr<token>> tokenize(const std::string & text);
