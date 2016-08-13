#include "printer_visitor.hpp"

using namespace std;

void printer::println(const string & s) {
    for (int i = 0; i < tabs; ++i)
        os << "  ";
    os << s << '\n';
}

void printer::visit(const unit & node) {
    println("{ unit");
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

void printer::visit(const function_declaration & node) {
    println("{ function_declaration");
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

void printer::visit(const variable_declaration_statement & node) {
    println("{ variable_declaration_statement");
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

void printer::visit(const expression_statement & node) {
    println("{ expression_statement");
    add_tab();
    println("expr =");
    add_tab();
    node.expr->accept(*this);
    delete_tab();
    delete_tab();
    println("}");
}

void printer::visit(const return_statement & node) {
    println("{ return_statement");
    add_tab();
    println("value =");
    add_tab();
    node.value->accept(*this);
    delete_tab();
    delete_tab();
    println("}");
}

void printer::visit(const if_statement & node) {
    println("{ if_statement");
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

void printer::visit(const while_statement & node) {
    println("{ while_statement");
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

void printer::visit(const for_statement & node) {
    println("{ for_statement");
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

void printer::visit(const compound_statement & node) {
    println("{ compound_statement");
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

void printer::visit(const variable_expression & node) {
    println("{ variable_expression");
    add_tab();
    println("type = \""_s + str(node.type) + "\"");
    println("decl = ");
    add_tab();
    node.decl->accept(*this);
    delete_tab();
    delete_tab();
    println("}");
}

void printer::visit(const assignment_expression & node) {
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

void printer::visit(const float_literal_expression & node) {
    println("{ float_literal_expression");
    add_tab();
    println("type = \""_s + str(node.type) + "\"");
    println("value = \""_s + to_string(node.value) + "\"");
    delete_tab();
    println("}");
}

void printer::visit(const int_literal_expression & node) {
    println("{ int_literal_expression");
    add_tab();
    println("type = \""_s + str(node.type) + "\"");
    println("value = \""_s + to_string(node.value) + "\"");
    delete_tab();
    println("}");
}

void printer::visit(const call_expression & node) {
    println("{ call_expression");
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

void printer::visit(const binary_expression & node) {
    println("{ binary_expression");
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

void printer::visit(const cast_expression & node) {
    println("{ cast_expression");
    add_tab();
    println("type = \""_s + str(node.type) + "\"");
    println("expr =");
    add_tab();
    node.expr->accept(*this);
    delete_tab();
    delete_tab();
    println("}");
}

