#include "parser.hpp"

#include <algorithm>

using namespace std;

string str(Type_node t) { 
    switch (t) {
    case Type_node::INT: return "int";
    case Type_node::FLOAT: return "float";
    }
}

string str(Opcode op) {
    switch (op) {
    case Opcode::PLUS: return "+";
    case Opcode::MINUS: return "-";
    case Opcode::MULTIPLY: return "*";
    case Opcode::DIVIDE: return "/";
    }
}

Type_node get_result_type(Type_node lhs, Type_node rhs) {
    if (lhs == Type_node::FLOAT || rhs == Type_node::FLOAT) 
        return Type_node::FLOAT;
    return Type_node::INT;
}

Cast_expression::Cast_expression(Type_node type, Ptr<Expression> expr)
        : Expression(type), expr(move(expr)) { }

Binary_expression::Binary_expression(Opcode operation, Ptr<Expression> lhs, 
        Ptr<Expression> rhs)
        : Expression(get_result_type(lhs->type, rhs->type))
        , operation(operation), lhs(move(lhs)), rhs(move(rhs)) { }

Call_expression::Call_expression(Function_declaration * decl, 
        vector<Ptr<Expression>> arguments)
        : Expression(decl->return_type)
        , decl(decl)
        , arguments(move(arguments)) { }

Recursive_call_expression::Recursive_call_expression(Type_node current_function_return_type, 
        vector<Ptr<Expression>> arguments)
        : Expression(current_function_return_type)
        , arguments(move(arguments)) { }

Int_literal_expression::Int_literal_expression(int value) 
        : Expression(Type_node::INT), value(value) { }

Float_literal_expression::Float_literal_expression(float value) 
        : Expression(Type_node::FLOAT), value(value) { }

Assignment_expression::Assignment_expression(Variable_declaration_statement * decl, 
        Ptr<Expression> value)
        : Expression(decl->type), decl(decl), value(move(value)) { }

Variable_expression::Variable_expression(Variable_declaration_statement * decl) 
        : Expression(decl->type), decl(decl) { }

Compound_statement::Compound_statement(std::vector<Ptr<Statement>> statements)
        : statements(move(statements)) { }

For_statement::For_statement(Ptr<Expression> initialization, Ptr<Expression> condition,
        Ptr<Expression> step, Ptr<Statement> body)
        : initialization(move(initialization))
        , condition(move(condition))
        , step(move(step))
        , body(move(body)) { }

While_statement::While_statement(Ptr<Expression> condition, Ptr<Statement> body)
        : condition(move(condition)) , body(move(body)) { }

If_statement::If_statement(Ptr<Expression> condition, Ptr<Statement> body)
        : condition(move(condition)) , body(move(body)) { }

Return_statement::Return_statement(Ptr<Expression> value) : value(move(value)) { }

Expression_statement::Expression_statement(Ptr<Expression> expr) : expr(move(expr)) { }

Variable_declaration_statement::Variable_declaration_statement(Type_node type, 
        std::string name, Ptr<Expression> initialization)
        : type(type)
        , name(move(name))
        , initialization(move(initialization)) { }

Function_declaration::Function_declaration(Type_node return_type, std::string name,
        std::vector<Ptr<Variable_declaration_statement>> parameters, Ptr<Statement> body)
        : return_type(return_type)
        , name(move(name))
        , parameters(move(parameters))
        , body(move(body)) { }

Unit::Unit(std::vector<Ptr<Function_declaration>> function_declarations)
        : function_declarations(move(function_declarations)) { }

void Unit::accept(Visitor & v) const { v.visit(*this); }
void Function_declaration::accept(Visitor & v) const { v.visit(*this); }
void Variable_declaration_statement::accept(Visitor & v) const { v.visit(*this); }
void Expression_statement::accept(Visitor & v) const { v.visit(*this); }
void Return_statement::accept(Visitor & v) const { v.visit(*this); }
void If_statement::accept(Visitor & v) const { v.visit(*this); }
void While_statement::accept(Visitor & v) const { v.visit(*this); }
void For_statement::accept(Visitor & v) const { v.visit(*this); }
void Compound_statement::accept(Visitor & v) const { v.visit(*this); }
void Variable_expression::accept(Visitor & v) const { v.visit(*this); }
void Assignment_expression::accept(Visitor & v) const { v.visit(*this); }
void Float_literal_expression::accept(Visitor & v) const { v.visit(*this); }
void Int_literal_expression::accept(Visitor & v) const { v.visit(*this); }
void Call_expression::accept(Visitor & v) const { v.visit(*this); }
void Recursive_call_expression::accept(Visitor & v) const { v.visit(*this); }
void Binary_expression::accept(Visitor & v) const { v.visit(*this); }
void Cast_expression::accept(Visitor & v) const { v.visit(*this); }

Variable_declaration_statement * Context::lookup_variable(const string & name) {
    for (auto & scope : variables) {
        // Find variable declaration
        auto begin = scope.begin();
        auto end = scope.end();
        auto it = find_if(begin, end,
                [&name] (Variable_declaration_statement * decl) {
                    return name == decl->name;
                });
        if (it != end) {
            return *it;
        }
    }
    return nullptr;
}

Ptr<Expression> Context::create_default_value(Type_node type) {
    switch (type) {
    case Type_node::INT:
        return make_unique<Int_literal_expression>(0);
    case Type_node::FLOAT:
        return make_unique<Float_literal_expression>(0.f);
    }
}

void Context::act_on_variable_declaration(Ptr<Statement> & stmt, Type_node type,
        string name, Ptr<Expression> initial_value) {
    auto begin = variables.back().begin();
    auto end = variables.back().end();
    bool duplicate_name = find_if(begin, end,
            [&name] (Variable_declaration_statement * decl) {
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
    auto new_stmt = make_unique<Variable_declaration_statement>(
        type, move(name), move(initial_value));
    variables.back().push_back(new_stmt.get());
    stmt = move(new_stmt);
}

void Context::act_before_function_body(Type_node return_type, string name, 
            vector<Ptr<Variable_declaration_statement>> parameters) {
    current_function_return_type = return_type;
    current_function_name = move(name);
    current_function_parameters = move(parameters);
}

void Context::act_on_functon_declaration(Ptr<Function_declaration> & func_decl,
        Ptr<Statement> body) {
    auto begin = functions.begin();
    auto end = functions.end();
    bool duplicate_name = find_if(begin, end,
            [this] (Function_declaration * decl) {
                return current_function_name == decl->name;
            }) != end;
    if (duplicate_name) {
        cout << "Function has duplicate name: " << current_function_name << '\n';
        exit(-1);
    }
    func_decl = make_unique<Function_declaration>(
            current_function_return_type, move(current_function_name), 
            move(current_function_parameters), move(body));
    functions.push_back(func_decl.get());
}

void Context::act_on_call_expression(Ptr<Expression> & expr, string name, 
        vector<Ptr<Expression>> arguments) {
    if (current_function_name != name) { // not recursive call
        // Find function declaration for this call
        auto begin = functions.begin();
        auto end = functions.end();
        auto it = find_if(begin, end,
                [&name] (Function_declaration * decl) {
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
        // Check types of arguments
        for (size_t i = 0; i < presented; ++i) {
            if ((**it).parameters[i]->type != arguments[i]->type) {
                cout << "Wrong type of argument " << i << " in call of function "
                     << name << ". " << str((**it).parameters[i]->type) 
                     << " was expected, but " << str(arguments[i]->type) << " occured.\n";
                exit(-1);
            }
        }
        expr = make_unique<Call_expression>(*it, move(arguments));
    } else { // recursive call
        // Check amount of arguments is correct
        auto presented = arguments.size();
        auto required = current_function_parameters.size();
        if (presented != required) {
            cout << "Wrong amount of arguments for call of function: " << name << '\n';
            cout << required << " required, but " << presented << " presented.\n";
            exit(-1);
        }
        // Check types of arguments
        for (size_t i = 0; i < presented; ++i) {
            if (current_function_parameters[i]->type != arguments[i]->type) {
                cout << "Wrong type of argument " << i << " in call of function "
                     << name << ". " << str(current_function_parameters[i]->type) 
                     << " was expected, but " << str(arguments[i]->type) << " occured.\n";
                exit(-1);
            }
        }
        expr = make_unique<Recursive_call_expression>(
                current_function_return_type, move(arguments));
    }
}

void Context::act_on_assignment_expression(Ptr<Expression> & expr, 
        string name, Ptr<Expression> value) {
    Variable_declaration_statement * decl = lookup_variable(name);
    if (!decl) {
        cout << "No variable with such name: " << name << '\n';
        exit(-1);
    }
    if (decl->type != value->type) {
        cout << "Type of assignable value doesn't match with type of variable.\n";
        exit(-1);
    }
    expr = make_unique<Assignment_expression>(decl, move(value));
}

void Context::act_on_variable_expression(Ptr<Expression> & expr, string name) {
    Variable_declaration_statement * decl = lookup_variable(name);
    if (!decl) {
        cout << "No variable with such name: " << name << '\n';
        exit(-1);
    }
    expr = make_unique<Variable_expression>(decl);
}

void Context::act_on_binary_expression(Ptr<Expression> & expr, 
        Opcode operation, Ptr<Expression> lhs, Ptr<Expression> rhs) {
    if (lhs->type != rhs->type) {
        cout << "Operands of binary Expression have different type.\n";
        exit(-1);
    }
    expr = make_unique<Binary_expression>(operation, move(lhs), move(rhs));
}

void Context::act_on_return_statement(Ptr<Statement> & stmt, Ptr<Expression> value) {
    if (current_function_return_type != value->type) {
        cout << "Returning value has wrong type: " << str(value->type) << '\n';
        cout << "while expected: " << str(current_function_return_type) << '\n';
        exit(-1);
    }
    stmt = make_unique<Return_statement>(move(value));
}

// name-id ::= '[a-zA-Z_]\w*'
bool parser::parse_name_id(Token_iterator & it, Token_iterator end, string & name) {
    name_id * name_id_token;
    if (!parse_token<name_id>(it, end, name_id_token)) return false;
    name = move(name_id_token->text);
    return true;
}

// int_literal ::= '\-?\d+'
bool parser::parse_int_literal(Token_iterator & it, Token_iterator end, 
        Ptr<Expression> & int_lit) {
    int_literal * lit;
    if (!parse_token<int_literal>(it, end, lit)) return false;
    int_lit = make_unique<Int_literal_expression>(stoi(lit->text));
    return true;
}

// float_literal ::= '\-?\d+.\d*'
bool parser::parse_float_literal(Token_iterator & it, Token_iterator end,
        Ptr<Expression> & float_lit) {
    float_literal * lit;
    if (!parse_token<float_literal>(it, end, lit)) return false;
    float_lit = make_unique<Float_literal_expression>(stof(lit->text));
    return true;
}

// literal ::= int_literal | float_literal
bool parser::parse_literal(Token_iterator & it, Token_iterator end, Ptr<Expression> & lit) {
    return parse_int_literal(it, end, lit) || parse_float_literal(it, end, lit);
}

// type ::= 'int' | 'float'
bool parser::parse_type(Token_iterator & it, Token_iterator end, Type_node & type) {
    if (parse_token<Kw_int>(it, end)) return type = Type_node::INT, true;
    if (parse_token<Kw_float>(it, end)) return type = Type_node::FLOAT, true;
    return false;
}

// additive-op ::= '+' | '-'
bool parser::parse_additive_op(Token_iterator & it, Token_iterator end, Opcode & op) {
    if (parse_token<P_plus>(it, end)) return op = Opcode::PLUS, true;
    if (parse_token<P_minus>(it, end)) return op = Opcode::MINUS, true;
    return false;
}

// multiplicative-op ::= '*' | '/'
bool parser::parse_multiplicative_op(Token_iterator & it, Token_iterator end, Opcode & op) {
    if (parse_token<P_multiply>(it, end)) return op = Opcode::MULTIPLY, true;
    if (parse_token<P_divide>(it, end)) return op = Opcode::DIVIDE, true;
    return false;
}

// Expression-list ::= additive-Expression ',' Expression-list
//                   | additive-Expression
bool parser::parse_expression_list(Token_iterator & it, Token_iterator end,
        vector<Ptr<Expression>> & arguments) {
    Ptr<Expression> expr;
    Token_iterator t = it;
    if (!parse_additive_expression(it, end, expr)) return false;
    arguments.push_back(move(expr));
    if (parse_token<P_comma>(it, end))
        if (!parse_expression_list(it, end, arguments)) return it = t, false;
    return true;
}

//  primary-Expression ::= literal
//                       | '(' additive-Expression ')'
//                       | type '(' additive-Expression ')'
//                       | name-id '(' Expression-list ')'
//                       | name-id '(' ')'
//                       | name-id '=' additive-Expression
//                       | name-id
bool parser::parse_primary_expression(Token_iterator & it, Token_iterator end,
        Ptr<Expression> & expr) {
    Token_iterator t = it;
    if (parse_literal(it, end, expr)) return true;
    Type_node type;
    if (parse_type(it, end, type)) {
        if (!parse_token<P_lparen>(it, end)) return it = t, false;
        if (!parse_additive_expression(it, end, expr)) return it = t, false;
        if (!parse_token<P_rparen>(it, end)) return it = t, false;
        expr = make_unique<Cast_expression>(type, move(expr));
        return true;
    }
    if (parse_token<P_lparen>(it, end))
        return parse_additive_expression(it, end, expr) && parse_token<P_rparen>(it, end);
    string name;
    if (!parse_name_id(it, end, name)) return false;
    if (parse_token<P_lparen>(it, end)) {
        vector<Ptr<Expression>> arguments;
        if (parse_token<P_rparen>(it, end)) {
            semantic_actions.act_on_call_expression(expr, move(name), move(arguments));
            return true;
        }
        if (!parse_expression_list(it, end, arguments)) return it = t, false; 
        if (!parse_token<P_rparen>(it, end)) return it = t, false;
        semantic_actions.act_on_call_expression(expr, move(name), move(arguments));
        return true;
    }
    if (parse_token<P_assign>(it, end)) {
        Ptr<Expression> value;
        if (!parse_additive_expression(it, end, value)) return it = t, false;
        semantic_actions.act_on_assignment_expression(
                expr, move(name), move(value));
    } else {
        semantic_actions.act_on_variable_expression(expr, move(name));
    }
    return true;
}

// multiplicative-Expression ::= multiplicative-Expression multiplciative-op 
//                               primary-Expression
//                             | primary-Expression
bool parser::parse_multiplicative_expression(Token_iterator & it, Token_iterator end,
        Ptr<Expression> & expr) {
    Opcode operation;
    Ptr<Expression> rhs;
    Ptr<Expression> lhs;
    Token_iterator t = it;
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

// additive-Expression ::= additive-Expression additive-op multiplicative-Expression
//                       | multiplicative-Expression
bool parser::parse_additive_expression(Token_iterator & it, Token_iterator end, 
        Ptr<Expression> & expr) {
    Opcode operation;
    Ptr<Expression> rhs;
    Ptr<Expression> lhs;
    Token_iterator t = it;
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

// for-Statement ::= for '(' additive-Expression 
//                       ';' additive-Expression 
//                       ';' additive-Expression 
//                       ')' compound-Statement
bool parser::parse_for_statement(Token_iterator & it, Token_iterator end, 
        Ptr<Statement> & stmt) {
    Ptr<Expression> initialization;
    Ptr<Expression> condition;
    Ptr<Expression> step;
    Ptr<Statement> body;
    Token_iterator t = it;
    if (!parse_token<Kw_for>(it, end)) return false;
    if (!parse_token<P_lparen>(it, end)) return it = t, false;
    if (!parse_additive_expression(it, end, initialization)) return it = t, false;
    if (!parse_token<P_semicolon>(it, end)) return it = t, false;
    if (!parse_additive_expression(it, end, condition)) return it = t, false;
    if (!parse_token<P_semicolon>(it, end)) return it = t, false;
    if (!parse_additive_expression(it, end, step)) return it = t, false;
    if (!parse_token<P_rparen>(it, end)) return it = t, false;
    if (!parse_compound_statement(it, end, body)) return it = t, false;
    stmt = make_unique<For_statement>(
            move(initialization), move(condition), move(step), move(body));
    return true;
}

// while-Statement ::= while '(' additive-Expression ')' compound-Statement
bool parser::parse_while_statement(Token_iterator & it, Token_iterator end, 
        Ptr<Statement> & stmt) {
    Ptr<Expression> condition;
    Ptr<Statement> body;
    Token_iterator t = it;
    if (!parse_token<Kw_while>(it, end)) return false;
    if (!parse_token<P_lparen>(it, end)) return it = t, false;
    if (!parse_additive_expression(it, end, condition)) return it = t, false;
    if (!parse_token<P_rparen>(it, end)) return it = t, false;
    if (!parse_compound_statement(it, end, body)) return it = t, false;
    stmt = make_unique<While_statement>(move(condition), move(body));
    return true;
}

// if-Statement ::= if '(' additive-Expression ')' compound-Statement
bool parser::parse_if_statement(Token_iterator & it, Token_iterator end,
        Ptr<Statement> & stmt) {
    Ptr<Expression> condition;
    Ptr<Statement> body;
    Token_iterator t = it;
    if (!parse_token<Kw_if>(it, end)) return false;
    if (!parse_token<P_lparen>(it, end)) return it = t, false;
    if (!parse_additive_expression(it, end, condition)) return it = t, false;
    if (!parse_token<P_rparen>(it, end)) return it = t, false;
    if (!parse_compound_statement(it, end, body)) return it = t, false;
    stmt = make_unique<If_statement>(move(condition), move(body));
    return true;
}

// return-Statement ::= return additive-Expression ';'
bool parser::parse_return_statement(Token_iterator & it, Token_iterator end,
        Ptr<Statement> & stmt) {
    Ptr<Expression> return_value;
    Token_iterator t = it;
    if (!parse_token<Kw_return>(it, end)) return false;
    if (!parse_additive_expression(it, end, return_value)) return it = t, false;
    if (!parse_token<P_semicolon>(it, end)) return it = t, false;
    semantic_actions.act_on_return_statement(stmt, move(return_value));
    return true;
}

// variable-declaration ::= type name-id ';'
//                        | type name-id '=' additive-Expression ';'
bool parser::parse_variable_declaration(Token_iterator & it, Token_iterator end, 
        Ptr<Statement> & stmt) {
    Type_node type;
    string name;
    Ptr<Expression> initialization;
    Token_iterator t = it;
    if (!parse_type(it, end, type)) return false;
    if (!parse_name_id(it, end, name)) return it = t, false;
    if (parse_token<P_assign>(it, end))
        if (!parse_additive_expression(it, end, initialization)) return it = t, false;
    if (!parse_token<P_semicolon>(it, end)) return it = t, false;
    semantic_actions.act_on_variable_declaration(
            stmt, type, move(name), move(initialization));
    return true;
}

// Expression-Statement ::= additive-Expression ';'
bool parser::parse_expression_statement(Token_iterator & it, Token_iterator end,
        Ptr<Statement> & stmt) {
    Ptr<Expression> expr;
    Token_iterator t = it;
    if (!parse_additive_expression(it, end, expr)) return false;
    if (!parse_token<P_semicolon>(it, end)) return it = t, false;
    stmt = make_unique<Expression_statement>(move(expr));
    return true;
}

// Statement ::= for-Statement
//             | while-Statement
//             | if-Statement
//             | return-Statement
//             | compound-Statement
//             | variable-declaration
//             | Expression-Statement
bool parser::parse_statement(Token_iterator & it, Token_iterator end, Ptr<Statement> & stmt) {
    return parse_for_statement(it, end, stmt) ||
        parse_while_statement(it, end, stmt) ||
        parse_if_statement(it, end, stmt) ||
        parse_return_statement(it, end, stmt) ||
        parse_compound_statement(it, end, stmt) ||
        parse_variable_declaration(it, end, stmt) ||
        parse_expression_statement(it, end, stmt);
}

// statements ::= Statement statements
//              | Statement
bool parser::parse_statements(Token_iterator & it, Token_iterator end,
        vector<Ptr<Statement>> & statements) {
    Ptr<Statement> stmt;
    if (!parse_statement(it, end, stmt)) return false;
    statements.push_back(move(stmt));
    parse_statements(it, end, statements);
    return true;
}

// compound-Statement ::= '{' statements '}'
bool parser::parse_compound_statement(Token_iterator & it, Token_iterator end,
        Ptr<Statement> & stmt) {
    vector<Ptr<Statement>> statements;
    Token_iterator t = it;
    if (!parse_token<P_lbracket>(it, end)) return false;
    if (!parse_statements(it, end, statements)) return it = t, false;
    if (!parse_token<P_rbracket>(it, end)) return it = t, false;
    stmt = make_unique<Compound_statement>(move(statements));
    return true;
}

// function-parameter ::= type name-id
bool parser::parse_function_parameter(Token_iterator & it, Token_iterator end,
        Ptr<Variable_declaration_statement> & parameter) {
    Type_node type;
    string name;
    Token_iterator t = it;
    if (!parse_type(it, end, type)) return false;
    if (!parse_name_id(it, end, name)) return it = t, false;
    // TODO: check name issues
    parameter = make_unique<Variable_declaration_statement>(type, move(name), nullptr);
    semantic_actions.variables.back().push_back(parameter.get());
    return true;
}

// parameter-list ::= function-parameter, parameter-list
//                  | function-parameter
bool parser::parse_parameter_list(Token_iterator & it, Token_iterator end,
        vector<Ptr<Variable_declaration_statement>> & parameters) {
    Ptr<Variable_declaration_statement> parameter;
    if (!parse_function_parameter(it, end, parameter)) return false;
    // TODO: move checking code into semantic_actions
    bool previously_declared_name = find_if(
            parameters.begin(), parameters.end(),
            [&parameter] (const Ptr<Variable_declaration_statement> & p) {
                return parameter->name == p->name;
            }) != parameters.end();
    if (previously_declared_name) {
        cout << "Two parameters have same name: " << parameter->name << '\n';
        exit(-1);
    }
    parameters.push_back(move(parameter));
    if (parse_token<P_comma>(it, end)) {
        if (!parse_parameter_list(it, end, parameters))
            return false;
    }
    return true;
}

// function-declaration ::= type name-id ( parameter-list ) compound-Statement
//                        | type name-id ( ) compound-Statement
bool parser::parse_function_declaration(Token_iterator & it, Token_iterator end, 
        Ptr<Function_declaration> & func_decl) {
    Type_node return_type;
    string name;
    vector<Ptr<Variable_declaration_statement>> parameters;
    Ptr<Statement> body;
    Token_iterator t = it;
    if (!parse_type(it, end, return_type)) return false;
    if (!parse_name_id(it, end, name)) return it = t, false;
    if (!parse_token<P_lparen>(it, end)) return it = t, false;
    if (!parse_token<P_rparen>(it, end)) {
        semantic_actions.push_context();
        if (!parse_parameter_list(it, end, parameters)) return it = t, false;
        if (!parse_token<P_rparen>(it, end)) return it = t, false;
    } else {
        semantic_actions.push_context();
    }
    semantic_actions.act_before_function_body(return_type, move(name), move(parameters));
    if (!parse_compound_statement(it, end, body)) return it = t, false;
    semantic_actions.pop_context();
    semantic_actions.act_on_functon_declaration(func_decl, move(body));
    return true;
}

// Unit ::= function-declaration Unit
//        | function-declaration
bool parser::parse_unit(Token_iterator & it, Token_iterator end, 
        vector<Ptr<Function_declaration>> & declarations) {
    Ptr<Function_declaration> func_decl;
    if (!parse_function_declaration(it, end, func_decl)) return false;
    declarations.push_back(move(func_decl));
    parse_unit(it, end, declarations);
    return true;
}

bool parser::parse(Token_iterator it, Token_iterator end, Ptr<Unit> & translation_unit) {
    vector<Ptr<Function_declaration>> declarations;
    if (!parse_unit(it, end, declarations)) return false;
    translation_unit = make_unique<Unit>(move(declarations));
    return true;
}

