#pragma once

#include "parser.hpp"

#include <iostream>
#include <string>

class Printer : public Visitor {
    std::ostream & os;
    int tabs = 0;

public:
    Printer(std::ostream & os) : os(os) { }

    void add_tab() { ++tabs; }
    void delete_tab() { if (tabs != 0) --tabs; }
    void println(const std::string & s);

    void visit(const Unit & node) override;
    void visit(const Function_declaration & node) override;
    void visit(const Variable_declaration_statement & node) override;
    void visit(const Expression_statement & node) override;
    void visit(const Return_statement & node) override;
    void visit(const If_statement & node) override;
    void visit(const While_statement & node) override;
    void visit(const For_statement & node) override;
    void visit(const Compound_statement & node) override;
    void visit(const Variable_expression & node) override;
    void visit(const Assignment_expression & node) override;
    void visit(const Float_literal_expression & node) override;
    void visit(const Int_literal_expression & node) override;
    void visit(const Call_expression & node) override;
    void visit(const Recursive_call_expression & node) override;
    void visit(const Binary_expression & node) override;
    void visit(const Cast_expression & node) override;
};

