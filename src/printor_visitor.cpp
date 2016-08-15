#include "printer_visitor.hpp"

using namespace std;

void Printer::println(const string & s) {
    for (int i = 0; i < tabs; ++i)
        os << "  ";
    os << s << '\n';
}

void Printer::visit(const Unit & node) {
    println("{ Unit");
    add_tab();
    println("function_declarations = [");
    add_tab();
    if (node.function_declarations.empty()) {
        println("<none>");
    } else {
        for (size_t i = 0; i < node.function_declarations.size(); ++i)
            node.function_declarations[i]->accept(*this);
    }
    delete_tab();
    println("]");
    delete_tab();
    println("}");
}

void Printer::visit(const Function_declaration & node) {
    println("{ Function_declaration");
    add_tab();
    println("return_type = \""_s + str(node.return_type) + "\"");
    println("name = \""_s + node.name + "\"");
    println("parameters = [");
    add_tab();
    if (node.parameters.empty()) {
        println("<none>");
    } else {
        for (size_t i = 0; i < node.parameters.size(); ++i) {
            node.parameters[i]->accept(*this);
        }
    }
    delete_tab();
    println("]");
    println("body =");
    add_tab();
    if (!node.body) {
        println("<none>");
    } else {
        node.body->accept(*this);
    }
    delete_tab();
    delete_tab();
    println("}");
}

void Printer::visit(const Variable_declaration_statement & node) {
    println("{ Variable_declaration_statement");
    add_tab();
    println("type = \""_s + str(node.type) + "\"");
    println("name = \""_s + node.name + "\"");
    println("initialization = ");
    add_tab();
    if (!node.initialization)
        println("<none>");
    else
        node.initialization->accept(*this);
    delete_tab();
    delete_tab();
    println("}");
}

void Printer::visit(const Expression_statement & node) {
    println("{ Expression_statement");
    add_tab();
    println("expr =");
    add_tab();
    node.expr->accept(*this);
    delete_tab();
    delete_tab();
    println("}");
}

void Printer::visit(const Return_statement & node) {
    println("{ Return_statement");
    add_tab();
    println("value =");
    add_tab();
    node.value->accept(*this);
    delete_tab();
    delete_tab();
    println("}");
}

void Printer::visit(const If_statement & node) {
    println("{ If_statement");
    add_tab();
    println("condition =");
    add_tab();
    node.condition->accept(*this);
    delete_tab();
    println("body =");
    add_tab();
    node.body->accept(*this);
    delete_tab();
    delete_tab();
    println("}");
}

void Printer::visit(const While_statement & node) {
    println("{ While_statement");
    add_tab();
    println("condition =");
    add_tab();
    node.condition->accept(*this);
    delete_tab();
    println("body =");
    add_tab();
    node.body->accept(*this);
    delete_tab();
    delete_tab();
    println("}");
}

void Printer::visit(const For_statement & node) {
    println("{ For_statement");
    add_tab();
    println("initialization =");
    add_tab();
    node.initialization->accept(*this);
    delete_tab();
    println("condition =");
    add_tab();
    node.condition->accept(*this);
    delete_tab();
    println("step =");
    add_tab();
    node.step->accept(*this);
    delete_tab();
    println("body =");
    add_tab();
    node.body->accept(*this);
    delete_tab();
    delete_tab();
    println("}");
}

void Printer::visit(const Compound_statement & node) {
    println("{ Compound_statement");
    add_tab();
    println("statements = [");
    add_tab();
    if (node.statements.empty()) {
        println("<none>");
    } else {
        for (size_t i = 0; i < node.statements.size(); ++i) {
            node.statements[i]->accept(*this);
        }
    }
    delete_tab();
    println("]");
    delete_tab();
    println("}");
}

void Printer::visit(const Variable_expression & node) {
    println("{ Variable_expression");
    add_tab();
    println("type = \""_s + str(node.type) + "\"");
    println("decl = ");
    add_tab();
    node.decl->accept(*this);
    delete_tab();
    delete_tab();
    println("}");
}

void Printer::visit(const Assignment_expression & node) {
    println("{ assignement_expression");
    add_tab();
    println("type = \""_s + str(node.type) + "\"");
    println("decl = ");
    add_tab();
    node.decl->accept(*this);
    delete_tab();
    println("initial_value =");
    add_tab();
    node.value->accept(*this);
    delete_tab();
    delete_tab();
    println("}");
}

void Printer::visit(const Float_literal_expression & node) {
    println("{ Float_literal_expression");
    add_tab();
    println("type = \""_s + str(node.type) + "\"");
    println("value = \""_s + to_string(node.value) + "\"");
    delete_tab();
    println("}");
}

void Printer::visit(const Int_literal_expression & node) {
    println("{ Int_literal_expression");
    add_tab();
    println("type = \""_s + str(node.type) + "\"");
    println("value = \""_s + to_string(node.value) + "\"");
    delete_tab();
    println("}");
}

void Printer::visit(const Call_expression & node) {
    println("{ Call_expression");
    add_tab();
    println("type = \""_s + str(node.type) + "\"");
    println("decl = ");
    add_tab();
    node.decl->accept(*this);
    delete_tab();
    println("arguments = [");
    add_tab();
    if (node.arguments.empty())
        println("<none>");
    else {
        for (size_t i = 0; i < node.arguments.size(); ++i) {
            node.arguments[i]->accept(*this);
        }
    }
    delete_tab();
    println("]");
    delete_tab();
    println("}");
}

void Printer::visit(const Binary_expression & node) {
    println("{ Binary_expression");
    add_tab();
    println("type = \""_s + str(node.type) + "\"");
    println("operation = \""_s + str(node.operation) + "\"");
    println("lhs = ");
    add_tab();
    node.lhs->accept(*this);
    delete_tab();
    println("rhs = ");
    add_tab();
    node.rhs->accept(*this);
    delete_tab();
    delete_tab();
    println("}");
}

void Printer::visit(const Cast_expression & node) {
    println("{ Cast_expression");
    add_tab();
    println("type = \""_s + str(node.type) + "\"");
    println("expr =");
    add_tab();
    node.expr->accept(*this);
    delete_tab();
    delete_tab();
    println("}");
}

