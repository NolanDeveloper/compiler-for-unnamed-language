#include "utils.hpp"
#include "lexer.hpp"
#include "parser.hpp"
#include "printer_visitor.hpp"

#include "llvm/IR/Verifier.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"

#include <vector>
#include <cassert>
#include <memory>
#include <iostream>
#include <algorithm>
#include <sstream>
#include <map>

using namespace std;

struct code_generator_visitor : visitor {
    ostream & os; // todo: remove
    llvm::LLVMContext & context = llvm::getGlobalContext();
    llvm::IRBuilder<> ir_builder = llvm::IRBuilder<>(llvm::getGlobalContext());
    ptr<llvm::Module> module = 
        make_unique<llvm::Module>("seagull", llvm::getGlobalContext());
    map<const ast_node *, llvm::Value *> values;
    map<const function_declaration *, llvm::Function *> functions;
    vector<llvm::Value *> stack_of_values;

    code_generator_visitor(ostream & os) : os(os) { }

    llvm::Type * get_llvm_type(type_node t) {
        switch (t) {
        case type_node::INT:
            return llvm::Type::getInt32Ty(llvm::getGlobalContext());
        case type_node::FLOAT:
            return llvm::Type::getFloatTy(llvm::getGlobalContext());
        }
    }

    llvm::Value * get_default_constant(type_node t) {
        switch (t) {
        case type_node::INT: 
            return llvm::ConstantInt::get(
                    llvm::Type::getInt32Ty(llvm::getGlobalContext()), 0, true);
        case type_node::FLOAT: 
            return llvm::ConstantFP::get(
                    llvm::Type::getFloatTy(llvm::getGlobalContext()), 0.f);
        }
    }

    void visit(const unit & node) override { 
        for (const auto & declaration : node.function_declarations) {
            declaration->accept(*this);
        }
        module->dump();
    }

    void visit(const function_declaration & node) override { 
        llvm::Type * return_type = get_llvm_type(node.return_type);
        vector<llvm::Type *> argument_types(node.parameters.size());
        int i = 0;
        for (const auto & p : node.parameters)
            argument_types[i++] = get_llvm_type(p->type);
        llvm::FunctionType * function_type = 
            llvm::FunctionType::get(return_type, argument_types, /* is_vararg */ false);
        llvm::Function * function = llvm::Function::Create( 
                function_type, llvm::Function::InternalLinkage, 
                node.name, module.get());
        functions[&node] = function;
        i = 0;
        for (auto & arg : function->args()) {
            arg.setName(node.parameters[i]->name);
            values[node.parameters[i].get()] = & arg;
            ++i;
        }
        llvm::BasicBlock * function_body = 
            llvm::BasicBlock::Create(llvm::getGlobalContext(), "function_body", function);
        ir_builder.SetInsertPoint(function_body);
        node.body->accept(*this);
        llvm::verifyFunction(*function);
    }

    void visit(const variable_declaration_statement & node) override { 
        node.initialization->accept(*this);
        llvm::Value * value = stack_of_values.back();
        stack_of_values.pop_back();
        values[&node] = value;
    }

    void visit(const expression_statement & node) override { 
        node.expr->accept(*this);
        stack_of_values.pop_back();
    }

    void visit(const return_statement & node) override { 
        node.value->accept(*this);
        ir_builder.CreateRet(stack_of_values.back());
        stack_of_values.pop_back();
    }

    void visit(const if_statement & node) override {
        node.condition->accept(*this);
        llvm::Value * value = stack_of_values.back();
        stack_of_values.pop_back();
        llvm::Value * condition = ir_builder.CreateFCmpONE(
                value, get_default_constant(node.condition->type), "ifcond");
        llvm::Function * current_function = ir_builder.GetInsertBlock()->getParent();
        llvm::BasicBlock * then = llvm::BasicBlock::Create(
                llvm::getGlobalContext(), "then", current_function);
        llvm::BasicBlock * merge = llvm::BasicBlock::Create(
                llvm::getGlobalContext(), "merge", current_function); 
        ir_builder.CreateCondBr(value, then, merge);
        ir_builder.SetInsertPoint(then);
        node.body->accept(*this);
        ir_builder.CreateBr(merge);
        ir_builder.SetInsertPoint(merge);
    }

    void visit(const while_statement & node) override { 
        node.condition->accept(*this);
        llvm::Value * value = stack_of_values.back();
        stack_of_values.pop_back();
        llvm::Value * condition = ir_builder.CreateFCmpONE(
                value, get_default_constant(node.condition->type), "ifcond");
        llvm::Function * current_function = ir_builder.GetInsertBlock()->getParent();
        llvm::BasicBlock * loop = llvm::BasicBlock::Create(
                llvm::getGlobalContext(), "loop", current_function);
        ir_builder.SetInsertPoint(loop);
        llvm::BasicBlock * then = llvm::BasicBlock::Create(
                llvm::getGlobalContext(), "then", current_function);
        llvm::BasicBlock * _else = llvm::BasicBlock::Create(
                llvm::getGlobalContext(), "else", current_function);
        ir_builder.CreateCondBr(value, then, _else);
        ir_builder.SetInsertPoint(then);
        node.body->accept(*this);
        ir_builder.CreateBr(loop);
        ir_builder.SetInsertPoint(_else);
    } 

    void visit(const for_statement & node) override { 
        node.initialization->accept(*this);
        llvm::Value * initialization = stack_of_values.back();
        stack_of_values.pop_back();
        node.condition->accept(*this);
        llvm::Value * condition = stack_of_values.back();
        stack_of_values.pop_back();
        llvm::Function * current_function = ir_builder.GetInsertBlock()->getParent();
        llvm::BasicBlock * loop = llvm::BasicBlock::Create(
               llvm::getGlobalContext(), "for_loop", current_function);
        ir_builder.SetInsertPoint(loop);
        llvm::BasicBlock * then = llvm::BasicBlock::Create(
                llvm::getGlobalContext(), "then", current_function);
        llvm::BasicBlock * _else = llvm::BasicBlock::Create(
                llvm::getGlobalContext(), "else", current_function);
        ir_builder.CreateCondBr(condition, then, _else);
        ir_builder.SetInsertPoint(then);
        node.body->accept(*this);
        node.step->accept(*this);
        ir_builder.CreateBr(loop);
        ir_builder.SetInsertPoint(_else);
    }

    void visit(const compound_statement & node) override { 
        for (const auto & stmt : node.statements)
            stmt->accept(*this);
    }

    void visit(const variable_expression & node) override {
        stack_of_values.push_back(values[node.decl]);
    }

    void visit(const assignment_expression & node) override { 
        node.value->accept(*this);
        llvm::Value * value = stack_of_values.back();
        values[node.decl] = value;
    }

    void visit(const float_literal_expression & node) override { 
        llvm::Value * value = llvm::ConstantFP::get(
            llvm::Type::getFloatTy(llvm::getGlobalContext()),
            node.value);
        stack_of_values.push_back(value);
    }

    void visit(const int_literal_expression & node) override {
        llvm::Value * value = llvm::ConstantInt::get(
            llvm::Type::getInt32Ty(llvm::getGlobalContext()),
            node.value, true);
        stack_of_values.push_back(value);
    }

    void visit(const call_expression & node) override { 
        llvm::Function * function = functions[node.decl];       
        vector<llvm::Value *> arguments;
        arguments.reserve(node.decl->parameters.size());
        for (const auto & var : node.arguments) {
            var->accept(*this);
            arguments.push_back(stack_of_values.back());
            stack_of_values.pop_back();
        }
        llvm::Value * call = ir_builder.CreateCall(function, arguments, "call");
        stack_of_values.push_back(call);
    }

    void visit(const binary_expression & node) override { 
        node.lhs->accept(*this);
        llvm::Value * lhs = stack_of_values.back();
        stack_of_values.pop_back();
        node.rhs->accept(*this);
        llvm::Value * rhs = stack_of_values.back();
        stack_of_values.pop_back();
        llvm::Value * operation;
        switch (node.operation) {
        case opcode::PLUS:     operation = ir_builder.CreateAdd(lhs, rhs, "add"); break;
        case opcode::MINUS:    operation = ir_builder.CreateSub(lhs, rhs, "sub"); break;
        case opcode::MULTIPLY: operation = ir_builder.CreateMul(lhs, rhs, "mul"); break;
        case opcode::DIVIDE:   operation = ir_builder.CreateSDiv(lhs, rhs, "div"); break;
        }
        stack_of_values.push_back(operation);
    }

    void visit(const cast_expression & node) override { 
        node.expr->accept(*this);
        llvm::Type * dest_type = get_llvm_type(node.type);
        llvm::Value * value = stack_of_values.back();
        stack_of_values.pop_back();
        llvm::Value * cast;
        switch (node.type) {
        case type_node::INT:
            cast = ir_builder.CreateFPToSI(value, dest_type, "cast");
            break;
        case type_node::FLOAT:
            cast = ir_builder.CreateSIToFP(value, dest_type, "cast");
            break;
        }
        stack_of_values.push_back(cast);
    };
};

int main() {
    ostringstream oss;
    oss << cin.rdbuf();
    vector<ptr<token>> tokens = tokenize(oss.str());
    ptr<unit> translation_unit;
    parser parser;
    //cout << parser.parse(tokens.cbegin(), tokens.cend(), translation_unit) << '\n';
    parser.parse(tokens.cbegin(), tokens.cend(), translation_unit);
    //printer prnt(cout);
    //translation_unit->accept(prnt);
    code_generator_visitor codegen(cout);
    translation_unit->accept(codegen);
    return 0;
}
