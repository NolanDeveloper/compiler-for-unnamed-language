#pragma once

#include "lexer.hpp"

#include <string>
#include <vector>
#include <iostream>

using token_iterator = std::vector<ptr<token>>::const_iterator;

struct expression;
struct unit;
struct function_declaration;
struct variable_declaration_statement;
struct expression_statement;
struct return_statement;
struct if_statement;
struct while_statement;
struct for_statement;
struct compound_statement;
struct statement;
struct variable_expression;
struct assignment_expression;
struct float_literal_expression;
struct int_literal_expression;
struct call_expression;
struct binary_expression;
struct cast_expression;

struct visitor {
    virtual void visit(const unit & node) = 0;
    virtual void visit(const function_declaration & node) = 0;
    virtual void visit(const variable_declaration_statement & node) = 0;
    virtual void visit(const expression_statement & node) = 0;
    virtual void visit(const return_statement & node) = 0;
    virtual void visit(const if_statement & node) = 0;
    virtual void visit(const while_statement & node) = 0;
    virtual void visit(const for_statement & node) = 0;
    virtual void visit(const compound_statement & node) = 0;
    virtual void visit(const variable_expression & node) = 0;
    virtual void visit(const assignment_expression & node) = 0;
    virtual void visit(const float_literal_expression & node) = 0;
    virtual void visit(const int_literal_expression & node) = 0;
    virtual void visit(const call_expression & node) = 0;
    virtual void visit(const binary_expression & node) = 0;
    virtual void visit(const cast_expression & node) = 0;
    virtual ~visitor() { }
};

struct ast_node { 
    virtual ~ast_node() { };
    virtual void accept(visitor & v) const = 0;
};

enum class type_node { INT, FLOAT };

std::string str(type_node t);

struct expression : ast_node { 
    type_node type;
    expression(type_node type) : type(type) { }
};

enum class opcode { PLUS, MINUS, MULTIPLY, DIVIDE };

std::string str(opcode op);

type_node get_result_type(type_node lhs, type_node rhs);

struct cast_expression : expression {
    const ptr<expression> expr;
    cast_expression(type_node type, ptr<expression> expr);
    void accept(visitor & v) const override;
};

struct binary_expression : expression { 
    const opcode operation;
    const ptr<expression> lhs;
    const ptr<expression> rhs;
    binary_expression(opcode operation, ptr<expression> lhs, ptr<expression> rhs);
    void accept(visitor & v) const override;
};

struct call_expression : expression { 
    const function_declaration * decl;
    const std::vector<ptr<expression>> arguments;
    call_expression(function_declaration * decl, std::vector<ptr<expression>> arguments);
    void accept(visitor & v) const override;
};

struct int_literal_expression : expression { 
    const int value;
    int_literal_expression(int value);
    void accept(visitor & v) const override;
};

struct float_literal_expression : expression {
    const float value;
    float_literal_expression(float value);
    void accept(visitor & v) const override;
};

struct assignment_expression : expression { 
    const variable_declaration_statement * decl;
    const ptr<expression> value;
    assignment_expression(variable_declaration_statement * decl, ptr<expression> value);
    void accept(visitor & v) const override;
};

struct variable_expression : expression { 
    const variable_declaration_statement * decl;
    variable_expression(variable_declaration_statement * decl);
    void accept(visitor & v) const override;
};

struct statement : ast_node { };

struct compound_statement : statement { 
    const std::vector<ptr<statement>> statements;
    compound_statement(std::vector<ptr<statement>> statements);
    void accept(visitor & v) const override;
};

struct for_statement : statement { 
    const ptr<expression> initialization;
    const ptr<expression> condition;
    const ptr<expression> step;
    const ptr<statement> body;
    for_statement(ptr<expression> initialization, ptr<expression> condition,
            ptr<expression> step, ptr<statement> body);
    void accept(visitor & v) const override;
};

struct while_statement : statement { 
    const ptr<expression> condition;
    const ptr<statement> body;
    while_statement(ptr<expression> condition, ptr<statement> body);
    void accept(visitor & v) const override;
};

struct if_statement : statement {
    const ptr<expression> condition;
    const ptr<statement> body;
    if_statement(ptr<expression> condition, ptr<statement> body);
    void accept(visitor & v) const override;
};

struct return_statement : statement {
    const ptr<expression> value;
    return_statement(ptr<expression> value);
    void accept(visitor & v) const override;
};

struct expression_statement : statement {
    const ptr<expression> expr;
    expression_statement(ptr<expression> expr);
    void accept(visitor & v) const override;
};

struct variable_declaration_statement : statement {
    const type_node type;
    const std::string name;
    const ptr<expression> initialization;
    variable_declaration_statement(type_node type, std::string name, 
            ptr<expression> initialization);
    void accept(visitor & v) const override;
};

struct function_declaration : ast_node {
    const type_node return_type;
    const std::string name;
    const std::vector<ptr<variable_declaration_statement>> parameters;
    ptr<statement> body;
    function_declaration(type_node return_type, std::string name,
            std::vector<ptr<variable_declaration_statement>> parameters, 
            ptr<statement> body);
    void accept(visitor & v) const override;
};

struct unit : ast_node {
    const std::vector<ptr<function_declaration>> function_declarations;
    unit(std::vector<ptr<function_declaration>> function_declarations);
    void accept(visitor & v) const override;
};

struct context {
    std::vector<std::vector<variable_declaration_statement *>> variables;
    std::vector<function_declaration *> functions;
    type_node current_function_return_type;

    context() { variables.emplace_back(); }

    variable_declaration_statement * lookup_variable(const std::string & name);

    void push_context() { variables.emplace_back(); }
    void pop_context() { variables.pop_back(); }

    void enter_function_declaration(type_node return_type);

    ptr<expression> create_default_value(type_node type);

    void act_on_variable_declaration(ptr<statement> & stmt, type_node type,
            std::string name, ptr<expression> initial_value);

    void act_on_functon_declaration(ptr<function_declaration> & func_decl,
            type_node return_type, std::string name, 
            std::vector<ptr<variable_declaration_statement>> parameters,
            ptr<statement> body);

    void act_on_call_expression(ptr<expression> & expr, std::string name, 
            std::vector<ptr<expression>> arguments);

    void act_on_assignment_expression(ptr<expression> & expr, 
            std::string name, ptr<expression> value);

    void act_on_variable_expression(ptr<expression> & expr, std::string name);

    void act_on_binary_expression(ptr<expression> & expr, 
            opcode operation, ptr<expression> lhs, ptr<expression> rhs);

    void act_on_return_statement(ptr<statement> & stmt, ptr<expression> value);

};

struct parser {
    context semantic_actions;

    template <typename T>
    bool parse_token(token_iterator & it, token_iterator end) {
        if (!(dynamic_cast<T *>(it->get()))) 
            return false;
        ++it;
        return true;
    }

    template <typename T>
    bool parse_token(token_iterator & it, token_iterator end, T * & token) {
        if (!(token = dynamic_cast<T *>(it->get())))
            return false;
        ++it;
        return true;
    }

    // name-id ::= '[a-zA-Z_]\w*'
    bool parse_name_id(token_iterator & it, token_iterator end, std::string & name);

    // int_literal ::= '\-?\d+'
    bool parse_int_literal(token_iterator & it, token_iterator end, 
            ptr<expression> & int_lit);

    // float_literal ::= '\-?\d+.\d*'
    bool parse_float_literal(token_iterator & it, token_iterator end,
            ptr<expression> & float_lit);

    // literal ::= int_literal | float_literal
    bool parse_literal(token_iterator & it, token_iterator end, ptr<expression> & lit);

    // type ::= 'int' | 'float'
    bool parse_type(token_iterator & it, token_iterator end, type_node & type);

    // additive-op ::= '+' | '-'
    bool parse_additive_op(token_iterator & it, token_iterator end, opcode & op);

    // multiplicative-op ::= '*' | '/'
    bool parse_multiplicative_op(token_iterator & it, token_iterator end, opcode & op);

    // expression-list ::= additive-expression ',' expression-list
    //                   | additive-expression
    bool parse_expression_list(token_iterator & it, token_iterator end,
            std::vector<ptr<expression>> & arguments);

    //  primary-expression ::= literal
    //                       | '(' additive-expression ')'
    //                       | type '(' additive-expression ')'
    //                       | name-id '(' expression-list ')'
    //                       | name-id '(' ')'
    //                       | name-id '=' additive-expression
    //                       | name-id
    bool parse_primary_expression(token_iterator & it, token_iterator end,
            ptr<expression> & expr);

    // multiplicative-expression ::= multiplicative-expression multiplciative-op 
    //                               primary-expression
    //                             | primary-expression
    bool parse_multiplicative_expression(token_iterator & it, token_iterator end,
            ptr<expression> & expr);

    // additive-expression ::= additive-expression additive-op multiplicative-expression
    //                       | multiplicative-expression
    bool parse_additive_expression(token_iterator & it, token_iterator end, 
            ptr<expression> & expr);

    // for-statement ::= for '(' additive-expression 
    //                       ';' additive-expression 
    //                       ';' additive-expression 
    //                       ')' compound-statement
    bool parse_for_statement(token_iterator & it, token_iterator end, 
            ptr<statement> & stmt);

    // while-statement ::= while '(' additive-expression ')' compound-statement
    bool parse_while_statement(token_iterator & it, token_iterator end, 
            ptr<statement> & stmt);

    // if-statement ::= if '(' additive-expression ')' compound-statement
    bool parse_if_statement(token_iterator & it, token_iterator end,
            ptr<statement> & stmt);

    // return-statement ::= return additive-expression ';'
    bool parse_return_statement(token_iterator & it, token_iterator end,
            ptr<statement> & stmt);

    // variable-declaration ::= type name-id ';'
    //                        | type name-id '=' additive-expression ';'
    bool parse_variable_declaration(token_iterator & it, token_iterator end, 
            ptr<statement> & stmt);

    // expression-statement ::= additive-expression ';'
    bool parse_expression_statement(token_iterator & it, token_iterator end,
            ptr<statement> & stmt);

    // statement ::= for-statement
    //             | while-statement
    //             | if-statement
    //             | return-statement
    //             | compound-statement
    //             | variable-declaration
    //             | expression-statement
    bool parse_statement(token_iterator & it, token_iterator end, ptr<statement> & stmt);

    // statements ::= statement statements
    //              | statement
    bool parse_statements(token_iterator & it, token_iterator end,
            std::vector<ptr<statement>> & statements);

    // compound-statement ::= '{' statements '}'
    bool parse_compound_statement(token_iterator & it, token_iterator end,
            ptr<statement> & stmt);

    // function-parameter ::= type name-id
    bool parse_function_parameter(token_iterator & it, token_iterator end,
            ptr<variable_declaration_statement> & parameter);

    // parameter-list ::= function-parameter, parameter-list
    //                  | function-parameter
    bool parse_parameter_list(token_iterator & it, token_iterator end,
            std::vector<ptr<variable_declaration_statement>> & parameters);

    // function-declaration ::= type name-id ( parameter-list ) compound-statement
    //                        | type name-id ( ) compound-statement
    bool parse_function_declaration(token_iterator & it, token_iterator end, 
            ptr<function_declaration> & func_decl);

    // unit ::= function-declaration unit
    //        | function-declaration
    bool parse_unit(token_iterator & it, token_iterator end, 
            std::vector<ptr<function_declaration>> & declarations);

    bool parse(token_iterator it, token_iterator end, ptr<unit> & translation_unit);
};
