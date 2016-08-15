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
#include <fstream>
#include <algorithm>
#include <sstream>
#include <map>
#include <getopt.h>

using namespace std;
using namespace llvm;

struct Code_generator_visitor : Visitor {
    ostream & os; // todo: remove
    LLVMContext & context = getGlobalContext();
    IRBuilder<> ir_builder = IRBuilder<>(getGlobalContext());
    Ptr<Module> module = 
        make_unique<Module>("seagull", getGlobalContext());
    map<const Ast_node *, Value *> values;
    map<const Function_declaration *, Function *> functions;
    Value * return_value;

    Code_generator_visitor(ostream & os) : os(os) { }

    Type * get_llvm_type(Type_node t) {
        switch (t) {
        case Type_node::INT:
            return Type::getInt32Ty(getGlobalContext());
        case Type_node::FLOAT:
            return Type::getFloatTy(getGlobalContext());
        }
    }

    Value * get_default_constant(Type_node t) {
        switch (t) {
        case Type_node::INT: 
            return ConstantInt::get(
                    Type::getInt32Ty(getGlobalContext()), 0, true);
        case Type_node::FLOAT: 
            return ConstantFP::get(
                    Type::getFloatTy(getGlobalContext()), 0.f);
        }
    }

    Value * isZero(Value * value) {
        if (value->getType()->isIntegerTy()) {
            return ir_builder.CreateICmpNE(
                    value, ConstantInt::get(value->getType(), 0), "cond");
        } else {
            return ir_builder.CreateFCmpOLT(
                    value, ConstantFP::get(value->getType(), 0), "cond");
        }
        assert(false);
    }

    void visit(const Unit & node) override { 
        for (const auto & declaration : node.function_declarations) {
            declaration->accept(*this);
        }
        module->dump();
    }

    void visit(const Function_declaration & node) override { 
        Type * return_type = get_llvm_type(node.return_type);
        vector<Type *> argument_types(node.parameters.size());
        int i = 0;
        for (const auto & p : node.parameters)
            argument_types[i++] = PointerType::getUnqual(get_llvm_type(p->type));
        FunctionType * function_type = 
            FunctionType::get(return_type, argument_types, /* is_vararg */ false);
        Function * function = Function::Create( 
                function_type, Function::ExternalLinkage,
                node.name, module.get());
        functions[&node] = function;
        i = 0;
        for (auto & arg : function->args()) {
            arg.setName(node.parameters[i]->name);
            values[node.parameters[i].get()] = & arg;
            ++i;
        }
        BasicBlock * function_body = 
            BasicBlock::Create(getGlobalContext(), "function_body", function);
        ir_builder.SetInsertPoint(function_body);
        node.body->accept(*this);
        verifyFunction(*function);
    }

    void visit(const Variable_declaration_statement & node) override { 
        Value * variable_ptr = ir_builder.CreateAlloca(get_llvm_type(node.type));
        node.initialization->accept(*this);
        ir_builder.CreateStore(return_value, variable_ptr);
        values[&node] = variable_ptr;
    }

    void visit(const Expression_statement & node) override { 
        node.expr->accept(*this);
    }

    void visit(const Return_statement & node) override { 
        node.value->accept(*this);
        ir_builder.CreateRet(return_value);
    }

    void visit(const If_statement & node) override {
        node.condition->accept(*this);
        Value * condition = isZero(return_value);
        Function * current_function = ir_builder.GetInsertBlock()->getParent();
        BasicBlock * then = BasicBlock::Create(
                getGlobalContext(), "then", current_function);
        BasicBlock * merge = BasicBlock::Create(
                getGlobalContext(), "merge", current_function); 
        ir_builder.CreateCondBr(condition, then, merge);
        ir_builder.SetInsertPoint(then);
        node.body->accept(*this);
        ir_builder.CreateBr(merge);
        ir_builder.SetInsertPoint(merge);
    }

    void visit(const While_statement & node) override { 
        node.condition->accept(*this);
        Value * condition = isZero(return_value);
        Function * current_function = ir_builder.GetInsertBlock()->getParent();
        BasicBlock * loop = BasicBlock::Create(
                getGlobalContext(), "loop", current_function);
        ir_builder.SetInsertPoint(loop);
        BasicBlock * then = BasicBlock::Create(
                getGlobalContext(), "then", current_function);
        BasicBlock * _else = BasicBlock::Create(
                getGlobalContext(), "else", current_function);
        ir_builder.CreateCondBr(condition, then, _else);
        ir_builder.SetInsertPoint(then);
        node.body->accept(*this);
        ir_builder.CreateBr(loop);
        ir_builder.SetInsertPoint(_else);
    } 

    void visit(const For_statement & node) override { 
        node.initialization->accept(*this);
        Value * initialization = return_value;
        Function * current_function = ir_builder.GetInsertBlock()->getParent();
        BasicBlock * loop = BasicBlock::Create(
                getGlobalContext(), "for_loop", current_function);
        ir_builder.CreateBr(loop);
        ir_builder.SetInsertPoint(loop);
        node.condition->accept(*this);
        Value * condition = isZero(return_value);
        BasicBlock * then = BasicBlock::Create(
                getGlobalContext(), "then", current_function);
        BasicBlock * _else = BasicBlock::Create(
                getGlobalContext(), "else", current_function);
        ir_builder.CreateCondBr(condition, then, _else);
        ir_builder.SetInsertPoint(then);
        node.body->accept(*this);
        node.step->accept(*this);
        ir_builder.CreateBr(loop);
        ir_builder.SetInsertPoint(_else);
    }

    void visit(const Compound_statement & node) override { 
        for (const auto & stmt : node.statements)
            stmt->accept(*this);
    }

    void visit(const Variable_expression & node) override {
        return_value = ir_builder.CreateLoad(values[node.decl]);
    }

    void visit(const Assignment_expression & node) override { 
        node.value->accept(*this);
        Value * value = ir_builder.CreateStore(return_value, values[node.decl]);
    }

    void visit(const Float_literal_expression & node) override { 
        Value * value = ConstantFP::get(
            Type::getFloatTy(getGlobalContext()),
            node.value);
        return_value = value;
    }

    void visit(const Int_literal_expression & node) override {
        Value * value = ConstantInt::get(
            Type::getInt32Ty(getGlobalContext()),
            node.value, true);
        return_value = value;
    }

    void visit(const Call_expression & node) override { 
        Function * function = functions[node.decl];       
        vector<Value *> arguments;
        arguments.reserve(node.decl->parameters.size());
        for (const auto & var : node.arguments) {
            Value * mem = ir_builder.CreateAlloca(get_llvm_type(var->type));
            arguments.push_back(mem);
            var->accept(*this);
            ir_builder.CreateStore(return_value, mem);
        }
        Value * call = ir_builder.CreateCall(function, arguments, "call");
        return_value = call;
    }

    void visit(const Binary_expression & node) override { 
        node.lhs->accept(*this);
        Value * lhs = return_value;
        node.rhs->accept(*this);
        Value * rhs = return_value;
        Value * operation;
        switch (node.type) {
        case Type_node::INT:
            switch (node.operation) {
            case Opcode::PLUS:     operation = ir_builder.CreateAdd(lhs, rhs, "add"); break;
            case Opcode::MINUS:    operation = ir_builder.CreateSub(lhs, rhs, "sub"); break;
            case Opcode::MULTIPLY: operation = ir_builder.CreateMul(lhs, rhs, "mul"); break;
            case Opcode::DIVIDE:   operation = ir_builder.CreateSDiv(lhs, rhs, "div"); break;
            }
            break;
        case Type_node::FLOAT:
            switch (node.operation) {
            case Opcode::PLUS:     operation = ir_builder.CreateFAdd(lhs, rhs, "add"); break;
            case Opcode::MINUS:    operation = ir_builder.CreateFSub(lhs, rhs, "sub"); break;
            case Opcode::MULTIPLY: operation = ir_builder.CreateFMul(lhs, rhs, "mul"); break;
            case Opcode::DIVIDE:   operation = ir_builder.CreateFDiv(lhs, rhs, "div"); break;
            }
            break;
        }
        return_value = operation;
    }

    void visit(const Cast_expression & node) override { 
        node.expr->accept(*this);
        Type * dest_type = get_llvm_type(node.type);
        Value * value = return_value;
        Value * cast;
        switch (node.type) {
        case Type_node::INT:
            cast = ir_builder.CreateFPToSI(value, dest_type, "cast");
            break;
        case Type_node::FLOAT:
            cast = ir_builder.CreateSIToFP(value, dest_type, "cast");
            break;
        }
        return_value = cast;
    };
};

enum class Output_type {
    llvm_ir, ast
};

int main(int argc, char ** argv) {
    static const option long_options[] = {
        { "type", required_argument, nullptr, 't' },
        { "file", required_argument, nullptr, 'f' },
    };
    Output_type type = Output_type::llvm_ir;
    string filename;
    while (char c = getopt_long(argc, argv, "t:f:", long_options, nullptr)) {
        switch (c) {
        case 't':
            if (!strcmp(optarg, "llvm-ir")) {
                type = Output_type::llvm_ir;
            } else if (!strcmp(optarg, "ast")) {
                type = Output_type::ast;
            } else {
                cout << "Provided wrong parameter for \"--type\" argument: " << optarg
                     << "\nallowed:\n\tllvm-ir\n\tast\n";
            }
            break;
        case 'f': filename = optarg; break;
        case ':':
            cout << "Option " << optopt << " requires operand.\n";
            break;
        case -1: goto break_inner;
        default: return -1;
        }
    }
break_inner:
    ostringstream oss;
    ifstream fs(filename);
    oss << fs.rdbuf();
    vector<Ptr<Token>> tokens = tokenize(oss.str());
    Ptr<Unit> translation_unit;
    parser parser;
    parser.parse(tokens.cbegin(), tokens.cend(), translation_unit);
    switch (type) {
    case Output_type::llvm_ir:
        {
            Code_generator_visitor codegen(cout);
            translation_unit->accept(codegen);
        }
        break;
    case Output_type::ast:
        {
            Printer prnt(cout);
            translation_unit->accept(prnt);
        }
        break;
    }
    return 0;
}
