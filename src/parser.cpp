#include "parser.hpp"

#include <algorithm>

using namespace std;

string str(type_node t) { 
    switch (t) {
    case type_node::INT: return "int";
    case type_node::FLOAT: return "float";
    }
}

string str(opcode op) {
    switch (op) {
    case opcode::PLUS: return "+";
    case opcode::MINUS: return "-";
    case opcode::MULTIPLY: return "*";
    case opcode::DIVIDE: return "/";
    }
}

type_node get_result_type(type_node lhs, type_node rhs) {
    if (lhs == type_node::FLOAT || rhs == type_node::FLOAT) 
        return type_node::FLOAT;
    return type_node::INT;
}

cast_expression::cast_expression(type_node type, ptr<expression> expr)
        : expression(type), expr(move(expr)) { }

binary_expression::binary_expression(opcode operation, ptr<expression> lhs, 
        ptr<expression> rhs)
        : expression(get_result_type(lhs->type, rhs->type))
        , operation(operation), lhs(move(lhs)), rhs(move(rhs)) { }

call_expression::call_expression(function_declaration * decl, 
        vector<ptr<expression>> arguments)
        : expression(decl->return_type)
        , decl(decl)
        , arguments(move(arguments)) { }

int_literal_expression::int_literal_expression(int value) 
        : expression(type_node::INT), value(value) { }

float_literal_expression::float_literal_expression(float value) 
        : expression(type_node::FLOAT), value(value) { }

assignment_expression::assignment_expression(variable_declaration_statement * decl, 
        ptr<expression> value)
        : expression(decl->type), decl(decl), value(move(value)) { }

variable_expression::variable_expression(variable_declaration_statement * decl) 
        : expression(decl->type), decl(decl) { }

compound_statement::compound_statement(std::vector<ptr<statement>> statements)
        : statements(move(statements)) { }

for_statement::for_statement(ptr<expression> initialization, ptr<expression> condition,
        ptr<expression> step, ptr<statement> body)
        : initialization(move(initialization))
        , condition(move(condition))
        , step(move(step))
        , body(move(body)) { }

while_statement::while_statement(ptr<expression> condition, ptr<statement> body)
        : condition(move(condition)) , body(move(body)) { }

if_statement::if_statement(ptr<expression> condition, ptr<statement> body)
        : condition(move(condition)) , body(move(body)) { }

return_statement::return_statement(ptr<expression> value) : value(move(value)) { }

expression_statement::expression_statement(ptr<expression> expr) : expr(move(expr)) { }

variable_declaration_statement::variable_declaration_statement(type_node type, 
        std::string name, ptr<expression> initialization)
        : type(type)
        , name(move(name))
        , initialization(move(initialization)) { }

function_declaration::function_declaration(type_node return_type, std::string name,
        std::vector<ptr<variable_declaration_statement>> parameters, ptr<statement> body)
        : return_type(return_type)
        , name(move(name))
        , parameters(move(parameters))
        , body(move(body)) { }

unit::unit(std::vector<ptr<function_declaration>> function_declarations)
        : function_declarations(move(function_declarations)) { }

void unit::accept(visitor & v) const { v.visit(*this); }
void function_declaration::accept(visitor & v) const { v.visit(*this); }
void variable_declaration_statement::accept(visitor & v) const { v.visit(*this); }
void expression_statement::accept(visitor & v) const { v.visit(*this); }
void return_statement::accept(visitor & v) const { v.visit(*this); }
void if_statement::accept(visitor & v) const { v.visit(*this); }
void while_statement::accept(visitor & v) const { v.visit(*this); }
void for_statement::accept(visitor & v) const { v.visit(*this); }
void compound_statement::accept(visitor & v) const { v.visit(*this); }
void variable_expression::accept(visitor & v) const { v.visit(*this); }
void assignment_expression::accept(visitor & v) const { v.visit(*this); }
void float_literal_expression::accept(visitor & v) const { v.visit(*this); }
void int_literal_expression::accept(visitor & v) const { v.visit(*this); }
void call_expression::accept(visitor & v) const { v.visit(*this); }
void binary_expression::accept(visitor & v) const { v.visit(*this); }
void cast_expression::accept(visitor & v) const { v.visit(*this); }

variable_declaration_statement * context::lookup_variable(const string & name) {
    for (auto & scope : variables) {
        // Find variable declaration
        auto begin = scope.begin();
        auto end = scope.end();
        auto it = find_if(begin, end,
                [&name] (variable_declaration_statement * decl) {
                    return name == decl->name;
                });
        if (it != end) {
            return *it;
        }
    }
    return nullptr;
}

ptr<expression> context::create_default_value(type_node type) {
    switch (type) {
    case type_node::INT:
        return make_unique<int_literal_expression>(0);
    case type_node::FLOAT:
        return make_unique<float_literal_expression>(0.f);
    }
}

void context::act_on_variable_declaration(ptr<statement> & stmt, type_node type,
        string name, ptr<expression> initial_value) {
    auto begin = variables.back().begin();
    auto end = variables.back().end();
    bool duplicate_name = find_if(begin, end,
            [&name] (variable_declaration_statement * decl) {
                return name == decl->name;
            }) != end;
    if (duplicate_name) {
        cout << "Variable has duplicate name: " << name << '\n';
        exit(-1);
    }
    if (initial_value && type != initial_value->type) {
        cout << "Type of initial value doesn't match with variable type.\n";
        exit(-1);
    }
    if (!initial_value) {
        initial_value = create_default_value(type);
    }
    auto new_stmt = make_unique<variable_declaration_statement>(
        type, move(name), move(initial_value));
    variables.back().push_back(new_stmt.get());
    stmt = move(new_stmt);
}

void context::act_on_functon_declaration(ptr<function_declaration> & func_decl,
        type_node return_type, string name, 
        vector<ptr<variable_declaration_statement>> parameters,
        ptr<statement> body) {
    auto begin = functions.begin();
    auto end = functions.end();
    bool duplicate_name = find_if(begin, end,
            [&name] (function_declaration * decl) {
                return name == decl->name;
            }) != end;
    if (duplicate_name) {
        cout << "Function has duplicate name: " << name << '\n';
        exit(-1);
    }
    func_decl = make_unique<function_declaration>(
            return_type, move(name), move(parameters), move(body));
    functions.push_back(func_decl.get());
}

void context::act_on_call_expression(ptr<expression> & expr, string name, 
        vector<ptr<expression>> arguments) {
    // Find function declaration for this call
    auto begin = functions.begin();
    auto end = functions.end();
    auto it = find_if(begin, end,
            [&name] (function_declaration * decl) {
                return name == decl->name;
            });
    if (it == end) {
        cout << "No function with such name: " << name << '\n';
        exit(-1);
    }
    // Check amount of arguments is correct
    auto presented = arguments.size();
    auto required = (**it).parameters.size();
    if (presented != required) {
        cout << "Wrong amount of arguments for call of function: " << name << '\n';
        cout << required << " required, but " << presented << " presented.\n";
        exit(-1);
    }
    expr = make_unique<call_expression>(*it, move(arguments));
}

void context::act_on_assignment_expression(ptr<expression> & expr, 
        string name, ptr<expression> value) {
    variable_declaration_statement * decl = lookup_variable(name);
    if (!decl) {
        cout << "No variable with such name: " << name << '\n';
        exit(-1);
    }
    if (decl->type != value->type) {
        cout << "Type of assignable value doesn't match with type of variable.\n";
        exit(-1);
    }
    expr = make_unique<assignment_expression>(decl, move(value));
}

void context::act_on_variable_expression(ptr<expression> & expr, string name) {
    variable_declaration_statement * decl = lookup_variable(name);
    if (!decl) {
        cout << "No variable with such name: " << name << '\n';
        exit(-1);
    }
    expr = make_unique<variable_expression>(decl);
}

void context::act_on_binary_expression(ptr<expression> & expr, 
        opcode operation, ptr<expression> lhs, ptr<expression> rhs) {
    if (lhs->type != rhs->type) {
        cout << "Operands of binary expression have different type.\n";
        exit(-1);
    }
    expr = make_unique<binary_expression>(operation, move(lhs), move(rhs));
}

void context::enter_function_declaration(type_node return_type) {
    current_function_return_type = return_type;
}

void context::act_on_return_statement(ptr<statement> & stmt, ptr<expression> value) {
    if (current_function_return_type != value->type) {
        cout << "Returning value has wrong type: " << str(value->type) << '\n';
        cout << "while expected: " << str(current_function_return_type) << '\n';
        exit(-1);
    }
    stmt = make_unique<return_statement>(move(value));
}

// name-id ::= '[a-zA-Z_]\w*'
bool parser::parse_name_id(token_iterator & it, token_iterator end, string & name) {
    name_id * name_id_token;
    if (!parse_token<name_id>(it, end, name_id_token)) return false;
    name = move(name_id_token->text);
    return true;
}

// int_literal ::= '\-?\d+'
bool parser::parse_int_literal(token_iterator & it, token_iterator end, 
        ptr<expression> & int_lit) {
    int_literal * lit;
    if (!parse_token<int_literal>(it, end, lit)) return false;
    int_lit = make_unique<int_literal_expression>(stoi(lit->text));
    return true;
}

// float_literal ::= '\-?\d+.\d*'
bool parser::parse_float_literal(token_iterator & it, token_iterator end,
        ptr<expression> & float_lit) {
    float_literal * lit;
    if (!parse_token<float_literal>(it, end, lit)) return false;
    float_lit = make_unique<float_literal_expression>(stof(lit->text));
    return true;
}

// literal ::= int_literal | float_literal
bool parser::parse_literal(token_iterator & it, token_iterator end, ptr<expression> & lit) {
    return parse_int_literal(it, end, lit) || parse_float_literal(it, end, lit);
}

// type ::= 'int' | 'float'
bool parser::parse_type(token_iterator & it, token_iterator end, type_node & type) {
    if (parse_token<kw_int>(it, end)) return type = type_node::INT, true;
    if (parse_token<kw_float>(it, end)) return type = type_node::FLOAT, true;
    return false;
}

// additive-op ::= '+' | '-'
bool parser::parse_additive_op(token_iterator & it, token_iterator end, opcode & op) {
    if (parse_token<p_plus>(it, end)) return op = opcode::PLUS, true;
    if (parse_token<p_minus>(it, end)) return op = opcode::MINUS, true;
    return false;
}

// multiplicative-op ::= '*' | '/'
bool parser::parse_multiplicative_op(token_iterator & it, token_iterator end, opcode & op) {
    if (parse_token<p_multiply>(it, end)) return op = opcode::MULTIPLY, true;
    if (parse_token<p_divide>(it, end)) return op = opcode::DIVIDE, true;
    return false;
}

// expression-list ::= additive-expression ',' expression-list
//                   | additive-expression
bool parser::parse_expression_list(token_iterator & it, token_iterator end,
        vector<ptr<expression>> & arguments) {
    ptr<expression> expr;
    token_iterator t = it;
    if (!parse_additive_expression(it, end, expr)) return false;
    arguments.push_back(move(expr));
    if (parse_token<p_comma>(it, end))
        if (!parse_expression_list(it, end, arguments)) return it = t, false;
    return true;
}

//  primary-expression ::= literal
//                       | '(' additive-expression ')'
//                       | type '(' additive-expression ')'
//                       | name-id '(' expression-list ')'
//                       | name-id '(' ')'
//                       | name-id '=' additive-expression
//                       | name-id
bool parser::parse_primary_expression(token_iterator & it, token_iterator end,
        ptr<expression> & expr) {
    token_iterator t = it;
    if (parse_literal(it, end, expr)) return true;
    type_node type;
    if (parse_type(it, end, type)) {
        if (!parse_token<p_lparen>(it, end)) return it = t, false;
        if (!parse_additive_expression(it, end, expr)) return it = t, false;
        if (!parse_token<p_rparen>(it, end)) return it = t, false;
        expr = make_unique<cast_expression>(type, move(expr));
        return true;
    }
    if (parse_token<p_lparen>(it, end))
        return parse_additive_expression(it, end, expr) && parse_token<p_rparen>(it, end);
    string name;
    if (!parse_name_id(it, end, name)) return false;
    if (parse_token<p_lparen>(it, end)) {
        vector<ptr<expression>> arguments;
        if (parse_token<p_rparen>(it, end)) {
            semantic_actions.act_on_call_expression(expr, move(name), move(arguments));
            return true;
        }
        if (!parse_expression_list(it, end, arguments)) return it = t, false; 
        if (!parse_token<p_rparen>(it, end)) return it = t, false;
        semantic_actions.act_on_call_expression(expr, move(name), move(arguments));
        return true;
    }
    if (parse_token<p_assign>(it, end)) {
        ptr<expression> value;
        if (!parse_additive_expression(it, end, value)) return it = t, false;
        semantic_actions.act_on_assignment_expression(
                expr, move(name), move(value));
    } else {
        semantic_actions.act_on_variable_expression(expr, move(name));
    }
    return true;
}

// multiplicative-expression ::= multiplicative-expression multiplciative-op 
//                               primary-expression
//                             | primary-expression
bool parser::parse_multiplicative_expression(token_iterator & it, token_iterator end,
        ptr<expression> & expr) {
    opcode operation;
    ptr<expression> rhs;
    ptr<expression> lhs;
    token_iterator t = it;
    if (!parse_primary_expression(it, end, lhs)) return false;
    while (parse_multiplicative_op(it, end, operation)) {
        if (!parse_primary_expression(it, end, rhs)) 
            return it = t, false;
        semantic_actions.act_on_binary_expression(expr, operation, move(lhs), move(rhs));
        lhs = move(expr);
    }
    expr = move(lhs);
    return true;
}

// additive-expression ::= additive-expression additive-op multiplicative-expression
//                       | multiplicative-expression
bool parser::parse_additive_expression(token_iterator & it, token_iterator end, 
        ptr<expression> & expr) {
    opcode operation;
    ptr<expression> rhs;
    ptr<expression> lhs;
    token_iterator t = it;
    if (!parse_multiplicative_expression(it, end, lhs)) return false;
    while (parse_additive_op(it, end, operation)) {
        if (!parse_multiplicative_expression(it, end, rhs)) 
            return it = t, false;
        semantic_actions.act_on_binary_expression(expr, operation, move(lhs), move(rhs));
        lhs = move(expr);
    }
    expr = move(lhs);
    return true;
}

// for-statement ::= for '(' additive-expression 
//                       ';' additive-expression 
//                       ';' additive-expression 
//                       ')' compound-statement
bool parser::parse_for_statement(token_iterator & it, token_iterator end, 
        ptr<statement> & stmt) {
    ptr<expression> initialization;
    ptr<expression> condition;
    ptr<expression> step;
    ptr<statement> body;
    token_iterator t = it;
    if (!parse_token<kw_for>(it, end)) return false;
    if (!parse_token<p_lparen>(it, end)) return it = t, false;
    if (!parse_additive_expression(it, end, initialization)) return it = t, false;
    if (!parse_token<p_semicolon>(it, end)) return it = t, false;
    if (!parse_additive_expression(it, end, condition)) return it = t, false;
    if (!parse_token<p_semicolon>(it, end)) return it = t, false;
    if (!parse_additive_expression(it, end, step)) return it = t, false;
    if (!parse_token<p_rparen>(it, end)) return it = t, false;
    if (!parse_compound_statement(it, end, body)) return it = t, false;
    stmt = make_unique<for_statement>(
            move(initialization), move(condition), move(step), move(body));
    return true;
}

// while-statement ::= while '(' additive-expression ')' compound-statement
bool parser::parse_while_statement(token_iterator & it, token_iterator end, 
        ptr<statement> & stmt) {
    ptr<expression> condition;
    ptr<statement> body;
    token_iterator t = it;
    if (!parse_token<kw_while>(it, end)) return false;
    if (!parse_token<p_lparen>(it, end)) return it = t, false;
    if (!parse_additive_expression(it, end, condition)) return it = t, false;
    if (!parse_token<p_rparen>(it, end)) return it = t, false;
    if (!parse_compound_statement(it, end, body)) return it = t, false;
    stmt = make_unique<while_statement>(move(condition), move(body));
    return true;
}

// if-statement ::= if '(' additive-expression ')' compound-statement
bool parser::parse_if_statement(token_iterator & it, token_iterator end,
        ptr<statement> & stmt) {
    ptr<expression> condition;
    ptr<statement> body;
    token_iterator t = it;
    if (!parse_token<kw_if>(it, end)) return false;
    if (!parse_token<p_lparen>(it, end)) return it = t, false;
    if (!parse_additive_expression(it, end, condition)) return it = t, false;
    if (!parse_token<p_rparen>(it, end)) return it = t, false;
    if (!parse_compound_statement(it, end, body)) return it = t, false;
    stmt = make_unique<if_statement>(move(condition), move(body));
    return true;
}

// return-statement ::= return additive-expression ';'
bool parser::parse_return_statement(token_iterator & it, token_iterator end,
        ptr<statement> & stmt) {
    ptr<expression> return_value;
    token_iterator t = it;
    if (!parse_token<kw_return>(it, end)) return false;
    if (!parse_additive_expression(it, end, return_value)) return it = t, false;
    if (!parse_token<p_semicolon>(it, end)) return it = t, false;
    semantic_actions.act_on_return_statement(stmt, move(return_value));
    return true;
}

// variable-declaration ::= type name-id ';'
//                        | type name-id '=' additive-expression ';'
bool parser::parse_variable_declaration(token_iterator & it, token_iterator end, 
        ptr<statement> & stmt) {
    type_node type;
    string name;
    ptr<expression> initialization;
    token_iterator t = it;
    if (!parse_type(it, end, type)) return false;
    if (!parse_name_id(it, end, name)) return it = t, false;
    if (parse_token<p_assign>(it, end))
        if (!parse_additive_expression(it, end, initialization)) return it = t, false;
    if (!parse_token<p_semicolon>(it, end)) return it = t, false;
    semantic_actions.act_on_variable_declaration(
            stmt, type, move(name), move(initialization));
    return true;
}

// expression-statement ::= additive-expression ';'
bool parser::parse_expression_statement(token_iterator & it, token_iterator end,
        ptr<statement> & stmt) {
    ptr<expression> expr;
    token_iterator t = it;
    if (!parse_additive_expression(it, end, expr)) return false;
    if (!parse_token<p_semicolon>(it, end)) return it = t, false;
    stmt = make_unique<expression_statement>(move(expr));
    return true;
}

// statement ::= for-statement
//             | while-statement
//             | if-statement
//             | return-statement
//             | compound-statement
//             | variable-declaration
//             | expression-statement
bool parser::parse_statement(token_iterator & it, token_iterator end, ptr<statement> & stmt) {
    return parse_for_statement(it, end, stmt) ||
        parse_while_statement(it, end, stmt) ||
        parse_if_statement(it, end, stmt) ||
        parse_return_statement(it, end, stmt) ||
        parse_compound_statement(it, end, stmt) ||
        parse_variable_declaration(it, end, stmt) ||
        parse_expression_statement(it, end, stmt);
}

// statements ::= statement statements
//              | statement
bool parser::parse_statements(token_iterator & it, token_iterator end,
        vector<ptr<statement>> & statements) {
    ptr<statement> stmt;
    if (!parse_statement(it, end, stmt)) return false;
    statements.push_back(move(stmt));
    parse_statements(it, end, statements);
    return true;
}

// compound-statement ::= '{' statements '}'
bool parser::parse_compound_statement(token_iterator & it, token_iterator end,
        ptr<statement> & stmt) {
    vector<ptr<statement>> statements;
    token_iterator t = it;
    if (!parse_token<p_lbracket>(it, end)) return false;
    if (!parse_statements(it, end, statements)) return it = t, false;
    if (!parse_token<p_rbracket>(it, end)) return it = t, false;
    stmt = make_unique<compound_statement>(move(statements));
    return true;
}

// function-parameter ::= type name-id
bool parser::parse_function_parameter(token_iterator & it, token_iterator end,
        ptr<variable_declaration_statement> & parameter) {
    type_node type;
    string name;
    token_iterator t = it;
    if (!parse_type(it, end, type)) return false;
    if (!parse_name_id(it, end, name)) return it = t, false;
    // TODO: check name issues
    parameter = make_unique<variable_declaration_statement>(type, move(name), nullptr);
    semantic_actions.variables.back().push_back(parameter.get());
    return true;
}

// parameter-list ::= function-parameter, parameter-list
//                  | function-parameter
bool parser::parse_parameter_list(token_iterator & it, token_iterator end,
        vector<ptr<variable_declaration_statement>> & parameters) {
    ptr<variable_declaration_statement> parameter;
    if (!parse_function_parameter(it, end, parameter)) return false;
    // TODO: move checking code into semantic_actions
    bool previously_declared_name = find_if(
            parameters.begin(), parameters.end(),
            [&parameter] (const ptr<variable_declaration_statement> & p) {
                return parameter->name == p->name;
            }) != parameters.end();
    if (previously_declared_name) {
        cout << "Two parameters have same name: " << parameter->name << '\n';
        exit(-1);
    }
    parameters.push_back(move(parameter));
    if (parse_token<p_comma>(it, end)) {
        if (!parse_parameter_list(it, end, parameters))
            return false;
    }
    return true;
}

// function-declaration ::= type name-id ( parameter-list ) compound-statement
//                        | type name-id ( ) compound-statement
bool parser::parse_function_declaration(token_iterator & it, token_iterator end, 
        ptr<function_declaration> & func_decl) {
    type_node return_type;
    string name;
    vector<ptr<variable_declaration_statement>> parameters;
    ptr<statement> body;
    token_iterator t = it;
    if (!parse_type(it, end, return_type)) return false;
    if (!parse_name_id(it, end, name)) return it = t, false;
    if (!parse_token<p_lparen>(it, end)) return it = t, false;
    if (!parse_token<p_rparen>(it, end)) {
        semantic_actions.push_context();
        if (!parse_parameter_list(it, end, parameters)) return it = t, false;
        if (!parse_token<p_rparen>(it, end)) return it = t, false;
    } else {
        semantic_actions.push_context();
    }
    semantic_actions.enter_function_declaration(return_type);
    if (!parse_compound_statement(it, end, body)) return it = t, false;
    semantic_actions.pop_context();
    semantic_actions.act_on_functon_declaration(
            func_decl, return_type, move(name), move(parameters), move(body));
    return true;
}

// unit ::= function-declaration unit
//        | function-declaration
bool parser::parse_unit(token_iterator & it, token_iterator end, 
        vector<ptr<function_declaration>> & declarations) {
    ptr<function_declaration> func_decl;
    if (!parse_function_declaration(it, end, func_decl)) return false;
    declarations.push_back(move(func_decl));
    parse_unit(it, end, declarations);
    return true;
}

bool parser::parse(token_iterator it, token_iterator end, ptr<unit> & translation_unit) {
    vector<ptr<function_declaration>> declarations;
    if (!parse_unit(it, end, declarations)) return false;
    translation_unit = make_unique<unit>(move(declarations));
    return true;
}

