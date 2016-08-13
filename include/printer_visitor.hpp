#pragma once

#include "parser.hpp"

#include <iostream>
#include <string>

class printer : public visitor {
    std::ostream & os;
    int tabs = 0;

public:
    printer(std::ostream & os) : os(os) { }

    void add_tab() { ++tabs; }
    void delete_tab() { if (tabs != 0) --tabs; }
    void println(const std::string & s);

    void visit(const unit & node) override;
    void visit(const function_declaration & node) override;
    void visit(const variable_declaration_statement & node) override;
    void visit(const expression_statement & node) override;
    void visit(const return_statement & node) override;
    void visit(const if_statement & node) override;
    void visit(const while_statement & node) override;
    void visit(const for_statement & node) override;
    void visit(const compound_statement & node) override;
    void visit(const variable_expression & node) override;
    void visit(const assignment_expression & node) override;
    void visit(const float_literal_expression & node) override;
    void visit(const int_literal_expression & node) override;
    void visit(const call_expression & node) override;
    void visit(const binary_expression & node) override;
    void visit(const cast_expression & node) override;
};

