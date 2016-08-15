#pragma once

#include "lexer.hpp"

#include <string>
#include <vector>
#include <iostream>

using Token_iterator = std::vector<Ptr<Token>>::const_iterator;

struct Expression;
struct Unit;
struct Function_declaration;
struct Variable_declaration_statement;
struct Expression_statement;
struct Return_statement;
struct If_statement;
struct While_statement;
struct For_statement;
struct Compound_statement;
struct Statement;
struct Variable_expression;
struct Assignment_expression;
struct Float_literal_expression;
struct Int_literal_expression;
struct Call_expression;
struct Binary_expression;
struct Cast_expression;

struct Visitor {
    virtual void visit(const Unit & node) = 0;
    virtual void visit(const Function_declaration & node) = 0;
    virtual void visit(const Variable_declaration_statement & node) = 0;
    virtual void visit(const Expression_statement & node) = 0;
    virtual void visit(const Return_statement & node) = 0;
    virtual void visit(const If_statement & node) = 0;
    virtual void visit(const While_statement & node) = 0;
    virtual void visit(const For_statement & node) = 0;
    virtual void visit(const Compound_statement & node) = 0;
    virtual void visit(const Variable_expression & node) = 0;
    virtual void visit(const Assignment_expression & node) = 0;
    virtual void visit(const Float_literal_expression & node) = 0;
    virtual void visit(const Int_literal_expression & node) = 0;
    virtual void visit(const Call_expression & node) = 0;
    virtual void visit(const Binary_expression & node) = 0;
    virtual void visit(const Cast_expression & node) = 0;
    virtual ~Visitor() { }
};

struct Ast_node { 
    virtual ~Ast_node() { };
    virtual void accept(Visitor & v) const = 0;
};

enum class Type_node { INT, FLOAT };

std::string str(Type_node t);

struct Expression : Ast_node { 
    Type_node type;
    Expression(Type_node type) : type(type) { }
};

enum class Opcode { PLUS, MINUS, MULTIPLY, DIVIDE };

std::string str(Opcode op);

Type_node get_result_type(Type_node lhs, Type_node rhs);

struct Cast_expression : Expression {
    const Ptr<Expression> expr;
    Cast_expression(Type_node type, Ptr<Expression> expr);
    void accept(Visitor & v) const override;
};

struct Binary_expression : Expression { 
    const Opcode operation;
    const Ptr<Expression> lhs;
    const Ptr<Expression> rhs;
    Binary_expression(Opcode operation, Ptr<Expression> lhs, Ptr<Expression> rhs);
    void accept(Visitor & v) const override;
};

struct Call_expression : Expression { 
    const Function_declaration * decl;
    const std::vector<Ptr<Expression>> arguments;
    Call_expression(Function_declaration * decl, std::vector<Ptr<Expression>> arguments);
    void accept(Visitor & v) const override;
};

struct Int_literal_expression : Expression { 
    const int value;
    Int_literal_expression(int value);
    void accept(Visitor & v) const override;
};

struct Float_literal_expression : Expression {
    const float value;
    Float_literal_expression(float value);
    void accept(Visitor & v) const override;
};

struct Assignment_expression : Expression { 
    const Variable_declaration_statement * decl;
    const Ptr<Expression> value;
    Assignment_expression(Variable_declaration_statement * decl, Ptr<Expression> value);
    void accept(Visitor & v) const override;
};

struct Variable_expression : Expression { 
    const Variable_declaration_statement * decl;
    Variable_expression(Variable_declaration_statement * decl);
    void accept(Visitor & v) const override;
};

struct Statement : Ast_node { };

struct Compound_statement : Statement { 
    const std::vector<Ptr<Statement>> statements;
    Compound_statement(std::vector<Ptr<Statement>> statements);
    void accept(Visitor & v) const override;
};

struct For_statement : Statement { 
    const Ptr<Expression> initialization;
    const Ptr<Expression> condition;
    const Ptr<Expression> step;
    const Ptr<Statement> body;
    For_statement(Ptr<Expression> initialization, Ptr<Expression> condition,
            Ptr<Expression> step, Ptr<Statement> body);
    void accept(Visitor & v) const override;
};

struct While_statement : Statement { 
    const Ptr<Expression> condition;
    const Ptr<Statement> body;
    While_statement(Ptr<Expression> condition, Ptr<Statement> body);
    void accept(Visitor & v) const override;
};

struct If_statement : Statement {
    const Ptr<Expression> condition;
    const Ptr<Statement> body;
    If_statement(Ptr<Expression> condition, Ptr<Statement> body);
    void accept(Visitor & v) const override;
};

struct Return_statement : Statement {
    const Ptr<Expression> value;
    Return_statement(Ptr<Expression> value);
    void accept(Visitor & v) const override;
};

struct Expression_statement : Statement {
    const Ptr<Expression> expr;
    Expression_statement(Ptr<Expression> expr);
    void accept(Visitor & v) const override;
};

struct Variable_declaration_statement : Statement {
    const Type_node type;
    const std::string name;
    const Ptr<Expression> initialization;
    Variable_declaration_statement(Type_node type, std::string name, 
            Ptr<Expression> initialization);
    void accept(Visitor & v) const override;
};

struct Function_declaration : Ast_node {
    const Type_node return_type;
    const std::string name;
    const std::vector<Ptr<Variable_declaration_statement>> parameters;
    Ptr<Statement> body;
    Function_declaration(Type_node return_type, std::string name,
            std::vector<Ptr<Variable_declaration_statement>> parameters, 
            Ptr<Statement> body);
    void accept(Visitor & v) const override;
};

struct Unit : Ast_node {
    const std::vector<Ptr<Function_declaration>> function_declarations;
    Unit(std::vector<Ptr<Function_declaration>> function_declarations);
    void accept(Visitor & v) const override;
};

struct context {
    std::vector<std::vector<Variable_declaration_statement *>> variables;
    std::vector<Function_declaration *> functions;
    Type_node current_function_return_type;

    context() { variables.emplace_back(); }

    Variable_declaration_statement * lookup_variable(const std::string & name);

    void push_context() { variables.emplace_back(); }
    void pop_context() { variables.pop_back(); }

    void enter_function_declaration(Type_node return_type);

    Ptr<Expression> create_default_value(Type_node type);

    void act_on_variable_declaration(Ptr<Statement> & stmt, Type_node type,
            std::string name, Ptr<Expression> initial_value);

    void act_on_functon_declaration(Ptr<Function_declaration> & func_decl,
            Type_node return_type, std::string name, 
            std::vector<Ptr<Variable_declaration_statement>> parameters,
            Ptr<Statement> body);

    void act_on_call_expression(Ptr<Expression> & expr, std::string name, 
            std::vector<Ptr<Expression>> arguments);

    void act_on_assignment_expression(Ptr<Expression> & expr, 
            std::string name, Ptr<Expression> value);

    void act_on_variable_expression(Ptr<Expression> & expr, std::string name);

    void act_on_binary_expression(Ptr<Expression> & expr, 
            Opcode operation, Ptr<Expression> lhs, Ptr<Expression> rhs);

    void act_on_return_statement(Ptr<Statement> & stmt, Ptr<Expression> value);

};

struct parser {
    context semantic_actions;

    template <typename T>
    bool parse_token(Token_iterator & it, Token_iterator end) {
        if (!(dynamic_cast<T *>(it->get()))) 
            return false;
        ++it;
        return true;
    }

    template <typename T>
    bool parse_token(Token_iterator & it, Token_iterator end, T * & Token) {
        if (!(Token = dynamic_cast<T *>(it->get())))
            return false;
        ++it;
        return true;
    }

    // name-id ::= '[a-zA-Z_]\w*'
    bool parse_name_id(Token_iterator & it, Token_iterator end, std::string & name);

    // int_literal ::= '\-?\d+'
    bool parse_int_literal(Token_iterator & it, Token_iterator end, 
            Ptr<Expression> & int_lit);

    // float_literal ::= '\-?\d+.\d*'
    bool parse_float_literal(Token_iterator & it, Token_iterator end,
            Ptr<Expression> & float_lit);

    // literal ::= int_literal | float_literal
    bool parse_literal(Token_iterator & it, Token_iterator end, Ptr<Expression> & lit);

    // type ::= 'int' | 'float'
    bool parse_type(Token_iterator & it, Token_iterator end, Type_node & type);

    // additive-op ::= '+' | '-'
    bool parse_additive_op(Token_iterator & it, Token_iterator end, Opcode & op);

    // multiplicative-op ::= '*' | '/'
    bool parse_multiplicative_op(Token_iterator & it, Token_iterator end, Opcode & op);

    // Expression-list ::= additive-Expression ',' Expression-list
    //                   | additive-Expression
    bool parse_expression_list(Token_iterator & it, Token_iterator end,
            std::vector<Ptr<Expression>> & arguments);

    //  primary-Expression ::= literal
    //                       | '(' additive-Expression ')'
    //                       | type '(' additive-Expression ')'
    //                       | name-id '(' Expression-list ')'
    //                       | name-id '(' ')'
    //                       | name-id '=' additive-Expression
    //                       | name-id
    bool parse_primary_expression(Token_iterator & it, Token_iterator end,
            Ptr<Expression> & expr);

    // multiplicative-Expression ::= multiplicative-Expression multiplciative-op 
    //                               primary-Expression
    //                             | primary-Expression
    bool parse_multiplicative_expression(Token_iterator & it, Token_iterator end,
            Ptr<Expression> & expr);

    // additive-Expression ::= additive-Expression additive-op multiplicative-Expression
    //                       | multiplicative-Expression
    bool parse_additive_expression(Token_iterator & it, Token_iterator end, 
            Ptr<Expression> & expr);

    // for-Statement ::= for '(' additive-Expression 
    //                       ';' additive-Expression 
    //                       ';' additive-Expression 
    //                       ')' compound-Statement
    bool parse_for_statement(Token_iterator & it, Token_iterator end, 
            Ptr<Statement> & stmt);

    // while-Statement ::= while '(' additive-Expression ')' compound-Statement
    bool parse_while_statement(Token_iterator & it, Token_iterator end, 
            Ptr<Statement> & stmt);

    // if-Statement ::= if '(' additive-Expression ')' compound-Statement
    bool parse_if_statement(Token_iterator & it, Token_iterator end,
            Ptr<Statement> & stmt);

    // return-Statement ::= return additive-Expression ';'
    bool parse_return_statement(Token_iterator & it, Token_iterator end,
            Ptr<Statement> & stmt);

    // variable-declaration ::= type name-id ';'
    //                        | type name-id '=' additive-Expression ';'
    bool parse_variable_declaration(Token_iterator & it, Token_iterator end, 
            Ptr<Statement> & stmt);

    // Expression-Statement ::= additive-Expression ';'
    bool parse_expression_statement(Token_iterator & it, Token_iterator end,
            Ptr<Statement> & stmt);

    // Statement ::= for-Statement
    //             | while-Statement
    //             | if-Statement
    //             | return-Statement
    //             | compound-Statement
    //             | variable-declaration
    //             | Expression-Statement
    bool parse_statement(Token_iterator & it, Token_iterator end, Ptr<Statement> & stmt);

    // statements ::= Statement statements
    //              | Statement
    bool parse_statements(Token_iterator & it, Token_iterator end,
            std::vector<Ptr<Statement>> & statements);

    // compound-Statement ::= '{' statements '}'
    bool parse_compound_statement(Token_iterator & it, Token_iterator end,
            Ptr<Statement> & stmt);

    // function-parameter ::= type name-id
    bool parse_function_parameter(Token_iterator & it, Token_iterator end,
            Ptr<Variable_declaration_statement> & parameter);

    // parameter-list ::= function-parameter, parameter-list
    //                  | function-parameter
    bool parse_parameter_list(Token_iterator & it, Token_iterator end,
            std::vector<Ptr<Variable_declaration_statement>> & parameters);

    // function-declaration ::= type name-id ( parameter-list ) compound-Statement
    //                        | type name-id ( ) compound-Statement
    bool parse_function_declaration(Token_iterator & it, Token_iterator end, 
            Ptr<Function_declaration> & func_decl);

    // Unit ::= function-declaration Unit
    //        | function-declaration
    bool parse_unit(Token_iterator & it, Token_iterator end, 
            std::vector<Ptr<Function_declaration>> & declarations);

    bool parse(Token_iterator it, Token_iterator end, Ptr<Unit> & translation_unit);
};
